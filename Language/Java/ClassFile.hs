{-# LANGUAGE DoRec #-}
module Language.Java.ClassFile (getClass, nameConstructor) where

import Language.Java.Syntax
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Control.Monad
import Data.Word
import Control.Applicative ((<$>), (<$), (<*>)) -- (<*) (see parseFull)
import Data.Array
import Data.Flags
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import Data.List

parseFull :: Parser a -> Parser a
-- language-java depends on Parsec 2, which is not Applicative...
-- parseFull p = p <* eof
parseFull p = 
  do x <- p
     eof
     return x

parseType :: Parser Type
parseType = 
  (PrimType ByteT <$ char 'B') <|>
  (PrimType CharT <$ char 'C') <|>
  (PrimType DoubleT <$ char 'D') <|>
  (PrimType FloatT <$ char 'F') <|>
  (PrimType IntT <$ char 'I') <|>
  (PrimType LongT <$ char 'J') <|>
  (PrimType ShortT <$ char 'S') <|>
  (PrimType BooleanT <$ char 'Z') <|>
  (RefType <$> ArrayType <$> (char '[' >> parseType)) <|>
  (RefType <$> ClassRefType <$> (char 'L' >> parseClassType'))

  where parseClassType' = 
          do x <- parseClassType
             _ <- char ';'
             return x

readType :: String -> Type
readType s = case parse (parseFull parseType) "" s of
  Right x -> x
  Left e -> error $ show e

parseClassType :: Parser ClassType
parseClassType = 
  do parts <- (many1 alphaNum) `sepBy` (char '/')
     let ids = map (\ part -> (Ident part, [])) parts
     return $ ClassType ids

readClassType :: String -> ClassType
readClassType s = case parse (parseFull parseClassType) "" s of
  Right x -> x
  Left e -> error $ show e
  
parseMethodType :: Parser (Maybe Type, [Type])
parseMethodType = do
  args <- between (char '(') (char ')') (many parseType)
  ret <- (Nothing <$ char 'V') <|> (Just <$> parseType) 
  return (ret, args)
  
readMethodType s = case parse (parseFull parseMethodType) "" s of
  Right x -> x
  Left e -> error $ show e

type ConstIdx = Word16    
getConstIdx = getWord16be

data ConstantValue = ValueInteger Word32
                   | ValueFloat Float
                   | ValueLong Word64
                   | ValueDouble Double
                   | ValueString String
                   deriving Show

data Constant = ConstantClass { constClass :: ClassType }
              | ConstantFieldRef { constClass :: ClassType, nameAndTypeIdx :: ConstIdx }
              | ConstantMethodRef { constClass :: ClassType, nameAndTypeIdx :: ConstIdx }
              | ConstantInterfaceMethodRef { constClass :: ClassType, nameAndTypeIdx :: ConstIdx }
              | ConstantValue ConstantValue
              | ConstantString String
              | ConstantNameAndType { nameIdx :: ConstIdx, descIdx :: ConstIdx }
              | Unknown Word8
              deriving Show
  
type ConstTable = Array Word16 Constant
type ClassImporter a = ReaderT ConstTable Get a

getMany f = 
  do count <- lift getWord16be
     forM [1..count] $ const f

getConstant :: ConstTable -> Get Constant
getConstant constTable = 
  do tag <- getWord8
     case tag of
       1 -> ConstantString <$> getString
       3 -> ConstantValue <$> ValueInteger <$> getWord32be
       4 -> ConstantValue <$> ValueFloat <$> getFloat32be
       5 -> ConstantValue <$> ValueLong <$> getWord64be
       6 -> ConstantValue <$> ValueDouble <$> getFloat64be
       7 -> ConstantClass <$> readClassType <$> getStringRef
       8 -> ConstantValue <$> ValueString <$> getStringRef
       9 -> ConstantFieldRef <$> getType  <*> getConstIdx
       10 -> ConstantMethodRef <$> getType <*> getConstIdx
       11 -> ConstantInterfaceMethodRef <$> getType <*> getConstIdx
       12 -> ConstantNameAndType <$> getConstIdx <*> getConstIdx
       tag -> error $ unwords ["Unknown tag type:", show tag] -- return $ Unknown tag
  where 
    getString = toString <$> (getByteString =<< (fromInteger . fromIntegral <$> getWord16be))
    
    lookupString idx = case constTable!idx of
      ConstantString s -> s
      
    getStringRef = lookupString <$> getConstIdx
      
    getType = lookupType <$> getConstIdx
    lookupType = constClass . (constTable!)
                 
getConstants :: Get ConstTable
getConstants = do    
  count <- getWord16be
  rec 
    -- According to the Java .class spec, count is, for some reason, one larger than the actual number of constants
    consts <- listArray (1, count - 1) <$> (forM [1..count-1] $ const $ getConstant consts)
  return consts
  
classObject :: ClassType
classObject = ClassType [(Ident "java", []), (Ident "lang", []), (Ident "Object", [])]

nameConstructor :: String
nameConstructor = "<init>"

getClassType :: ClassImporter ClassType
getClassType = constClass <$> getConstantRef

getClass :: Get TypeDecl
getClass = 
  do (_major, _minor) <- getHeader
     constTable <- getConstants
     flip runReaderT constTable $
       do (isInterface, modifiers) <- getToplevel
     
          ClassType [(this, _)] <- getClassType
          superClass <- getClassType
          let super = if superClass == classObject then Nothing else Just (ClassRefType superClass)
     
          ifaces <- getMany (ClassRefType <$> getClassType)
          fields <- getMany getField
          methods <- catMaybes <$> getMany (getMethod this)
          
          _attributes <- getAttributes
     
          return $ if isInterface
                     then InterfaceTypeDecl $ InterfaceDecl modifiers this [] ifaces (InterfaceBody [])
                     else ClassTypeDecl $ ClassDecl modifiers this [] super ifaces (ClassBody $ map MemberDecl $ fields ++ methods)

getHeader = 
  do signature <- replicateM 4 getWord8
     unless (signature == [0xCA, 0xFE, 0xBA, 0xBE]) $ 
       error "Not a class file"
     minor <- getWord16be
     major <- getWord16be
     return (major, minor)
    
getConstantRef :: ClassImporter Constant
getConstantRef = 
  do idx <- lift getConstIdx
     asks (!idx)
                       
getStringRef :: ClassImporter String                       
getStringRef = 
  do const <- getConstantRef
     case const of
       ConstantString s -> return s
     
data Attribute = AttrConstantValue ConstantValue
               | AttrExceptions [ClassType]
               | AttrSynthetic
               deriving Show
                        
isAttrSynthetic AttrSynthetic = True
isAttrSynthetic _ = False
     
isAttrConstantValue (AttrConstantValue _) = True
isAttrConstantValue _ = False
                    
isAttrExceptions (AttrExceptions _) = True
isAttrExceptions _ = False
                        
getAttribute :: ClassImporter (Maybe Attribute)
getAttribute = 
  do name <- getStringRef
     len <- lift getWord32be 
     case name of
       "Synthetic" -> 
         return $ Just $ AttrSynthetic
       "ConstantValue" -> 
         do const <- getConstantRef
            return $ Just $ AttrConstantValue $ 
              case const of
                ConstantValue val -> val
       "Exceptions" ->
         Just <$> AttrExceptions <$> getMany getClassType
       _ -> 
         do lift $ skip (fromIntegral len)
            return Nothing
     
getAttributes = catMaybes <$> getMany getAttribute     
     
getField :: ClassImporter MemberDecl
getField = 
  do modifiers <- getModifiers
     name <- getStringRef
     ty <- readType <$> getStringRef
     attributes <- getAttributes
     let init = 
           case find isAttrConstantValue attributes of
             Nothing -> Nothing
             Just (AttrConstantValue val) -> Just $ InitExp $ Lit $ case val of
               ValueInteger x -> Int $ fromIntegral x
               ValueLong x -> Word $ fromIntegral x
               ValueString s -> String s
               ValueFloat x -> Float $ realToFrac x
               ValueDouble x -> Double x
     return $ FieldDecl modifiers ty [VarDecl (VarId $ Ident name) init]   

getMethod :: Ident -> ClassImporter (Maybe MemberDecl)
getMethod cls =
  do modifiers <- getModifiers
     name <- getStringRef
     let isConstructor = name == nameConstructor
     (ret, args) <- readMethodType <$> getStringRef
     let formals = zipWith (\i ty -> FormalParam [] ty False (VarId $ Ident $ 'p':show i)) [(1::Integer)..] args
     attributes <- getAttributes
     let exceptions = case find (isAttrExceptions) attributes of
           Nothing -> []
           Just (AttrExceptions tys) -> map ClassRefType tys
     return $
       if any isAttrSynthetic attributes then Nothing
         else Just $ 
           if isConstructor 
             then ConstructorDecl modifiers [] cls formals exceptions (ConstructorBody Nothing [])
             else MethodDecl modifiers [] ret (Ident name) formals exceptions (MethodBody Nothing)

parseModifiers :: Word16 -> [Modifier]
parseModifiers flags =
  catMaybes [0x0001 ~> Public,
             0x0002 ~> Private,
             0x0004 ~> Protected,
             0x0008 ~> Static,
             0x0010 ~> Final,
             -- TODO: How is this represented in language-java?
             -- 0x0020 ~> Synchronized, 
             0x0040 ~> Volatile,
             0x0080 ~> Transient,
             0x0100 ~> Native,
             0x0400 ~> Abstract,
             0x0800 ~> StrictFP]
  
  where flag ~> mod | flag .~. flags = Just mod
                    | otherwise = Nothing
      
getModifiers = parseModifiers <$> lift getWord16be

getToplevel = 
  do flags <- lift getWord16be     
     let iface = 0x0200 .~. flags
         modifiers = parseModifiers flags
     return (iface, modifiers)
