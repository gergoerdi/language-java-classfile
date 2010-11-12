{-# LANGUAGE DoRec #-}
module Language.Java.ClassFile (
  ImportedType(..),
  -- * Classfile parsing
  getClass,
  parseClasses,
  parseJar,
  -- * Magic identifiers
  nameConstructor, nameClassConstructor
  ) where

import Language.Java.Syntax
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Data.Word
import Control.Applicative ((<$>), (<$), (<*>)) -- (<*) (see parseFull)
import Data.Array
import Data.Flags
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import Data.List
import Codec.Archive.LibZip


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
  do parts <- (many1 alphaNum) `sepBy` (char '/' <|> char '$')
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

getConstant :: ConstTable -> Get (Word16, Constant)
getConstant constTable = 
  do tag <- getWord8
     case tag of
       1 -> noSkip <$> ConstantString <$> getString
       3 -> noSkip <$> ConstantValue <$> ValueInteger <$> getWord32be
       4 -> noSkip <$> ConstantValue <$> ValueFloat <$> getFloat32be
       5 -> skip <$> ConstantValue <$> ValueLong <$> getWord64be
       6 -> skip <$> ConstantValue <$> ValueDouble <$> getFloat64be
       7 -> noSkip <$> ConstantClass <$> readClassType <$> getStringRef
       8 -> noSkip <$> ConstantValue <$> ValueString <$> getStringRef
       9 -> noSkip <$> (ConstantFieldRef <$> getType  <*> getConstIdx)
       10 -> noSkip <$> (ConstantMethodRef <$> getType <*> getConstIdx)
       11 -> noSkip <$> (ConstantInterfaceMethodRef <$> getType <*> getConstIdx)
       12 -> noSkip <$> (ConstantNameAndType <$> getConstIdx <*> getConstIdx)
       tag -> fail $ unwords ["Unknown tag type:", show tag]
  where 
    getString = toString <$> (getByteString =<< (fromInteger . fromIntegral <$> getWord16be))
    
    lookupString idx = case constTable!idx of
      ConstantString s -> s
      
    getStringRef = lookupString <$> getConstIdx
      
    getType = lookupType <$> getConstIdx
    lookupType = constClass . (constTable!)
    
    noSkip x = (1, x)
    skip x = (2, x)
                 
getConstants :: Get ConstTable
getConstants = do    
  count <- getWord16be
  rec 
    -- According to the Java .class spec, count is, for some reason, one larger than the actual number of constants
    consts <- array (1, count - 1) <$> (fromToM 1 (count - 1) $ \i -> 
      do (step, const) <- getConstant consts
         return (step, (i, const)))
  return consts
  
fromToM from to f | from > to = return []
                  | otherwise = 
  do (step, x) <- f from
     liftM (x:) $ fromToM (from + step) to f
  
classObject :: ClassType
classObject = ClassType [(Ident "java", []), (Ident "lang", []), (Ident "Object", [])]

-- |Constructors are encoded in .class files as member functions of the name \"@\<init>@\".
nameConstructor :: String
nameConstructor = "<init>"

-- |Class constructors are encoded in .class files as static member functions of the name \"@\<clinit>@\".
nameClassConstructor :: String
nameClassConstructor = "<clinit>"

getClassType :: ClassImporter ClassType
getClassType = constClass <$> getConstantRef

getHeader = 
  do signature <- replicateM 4 getWord8
     unless (signature == [0xCA, 0xFE, 0xBA, 0xBE]) $ 
       error "Not a class file"
     minor <- getWord16be
     major <- getWord16be
     return (major, minor)
    
getConstantRefMaybe :: ClassImporter (Maybe Constant)
getConstantRefMaybe =
  do idx <- lift getConstIdx
     if idx == 0 then return Nothing else Just <$> asks (!idx)

getConstantRef :: ClassImporter Constant
getConstantRef = fromJust <$> getConstantRefMaybe
                       
getStringRefMaybe ::ClassImporter (Maybe String)                 
getStringRefMaybe = 
  do const <- getConstantRefMaybe
     return $ case const of
       Just (ConstantString s) -> Just s
       _ -> Nothing
                 
getStringRef :: ClassImporter String
getStringRef = fromJust <$> getStringRefMaybe
     
data InnerClass = InnerClass { innerInner :: ClassType, 
                               innerOuter :: Maybe ClassType,
                               innerName :: Maybe String,
                               innerFlags :: Word16 }
                  deriving Show
     
data Attribute = AttrConstantValue ConstantValue
               | AttrExceptions [ClassType]
               | AttrSynthetic
               | AttrInnerClasses [InnerClass]
               deriving Show
                        
isAttrSynthetic AttrSynthetic = True
isAttrSynthetic _ = False
     
isAttrConstantValue (AttrConstantValue _) = True
isAttrConstantValue _ = False
                    
isAttrExceptions (AttrExceptions _) = True
isAttrExceptions _ = False                     
                     
isAttrInnerClasses (AttrInnerClasses _) = True
isAttrInnerClasses _ = False

getInnerClass :: ClassImporter InnerClass
getInnerClass =
  do inner <- getClassType
     outer <- fmap constClass <$> getConstantRefMaybe
     name <- getStringRefMaybe
     flags <- lift getWord16be
     return $ InnerClass inner outer name flags

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
       "InnerClasses" ->
         do Just <$> AttrInnerClasses <$> getMany getInnerClass            
       _ -> 
         do lift $ skip (fromIntegral len)
            return Nothing
     
getAttributes = catMaybes <$> getMany getAttribute     
     
getField :: ClassImporter (Maybe MemberDecl)
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
     return $ unlessSynthetic attributes $ 
       FieldDecl modifiers ty [VarDecl (VarId $ Ident name) init]   

getMethod :: Ident -> ClassImporter (Maybe MemberDecl)
getMethod cls =
  do modifiers <- getModifiers
     name <- getStringRef
     let isConstructor = name `elem` [nameConstructor, nameClassConstructor]
     (ret, args) <- readMethodType <$> getStringRef
     let formals = zipWith (\i ty -> FormalParam [] ty False (VarId $ Ident $ 'p':show i)) [(1::Integer)..] args
     attributes <- getAttributes
     let exceptions = case find (isAttrExceptions) attributes of
           Nothing -> []
           Just (AttrExceptions tys) -> map ClassRefType tys
     return $ unlessSynthetic attributes $
       if isConstructor 
         then ConstructorDecl modifiers [] cls formals exceptions (ConstructorBody Nothing [])
         else MethodDecl modifiers [] ret (Ident name) formals exceptions (MethodBody Nothing)

unlessSynthetic :: [Attribute] -> a -> Maybe a
unlessSynthetic attributes x = if any isAttrSynthetic attributes then Nothing else Just x

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

data ImportedType = Toplevel TypeDecl
                  | Inner ClassType TypeDecl

type DeclMap = [(ClassType, ImportedType)]

getClass_ :: Maybe DeclMap -> Get (ClassType, ImportedType)
getClass_ declMap = 
  do (_major, _minor) <- getHeader
     constTable <- getConstants
     flip runReaderT constTable $
       do (isInterface, modifiers) <- getToplevel
     
          classType@(ClassType parts) <- getClassType
          let (this, _) = last parts
          superClass <- getClassType
          let super = if superClass == classObject then Nothing else Just (ClassRefType superClass)
     
          ifaces <- getMany (ClassRefType <$> getClassType)
          fields <- catMaybes <$> getMany getField
          methods <- catMaybes <$> getMany (getMethod this)
          
          attributes <- getAttributes
          let innerClasses = 
                case find isAttrInnerClasses attributes of
                  Nothing -> []
                  Just (AttrInnerClasses ics) -> ics                  
              innerDecls = mapMaybe (toInnerDecl classType) innerClasses
              outer = getOuter classType innerClasses
              
          let members = fields ++ methods ++ innerDecls
              decl = if isInterface
                       then InterfaceTypeDecl $ InterfaceDecl modifiers this [] ifaces (InterfaceBody members)
                       else ClassTypeDecl $ ClassDecl modifiers this [] super ifaces (ClassBody $ map MemberDecl members)
     
          return (classType, case outer of
                     Nothing -> Toplevel decl
                     Just outer -> Inner outer decl)
          
  where toInnerDecl classType ic = 
          do outer <- innerOuter ic             
             guard $ outer == classType
             Inner _ inner <- lookup (innerInner ic) =<< declMap
             let modifiers = parseModifiers $ innerFlags ic
             return $ case inner of
               ClassTypeDecl classDecl -> MemberClassDecl $ classDecl `withClassModifiers` modifiers
               InterfaceTypeDecl ifaceDecl -> MemberInterfaceDecl $ ifaceDecl `withInterfaceModifiers` modifiers
        
        (ClassDecl _ id tys super ifaces body) `withClassModifiers` ms = ClassDecl ms id tys super ifaces body
        (InterfaceDecl _ id tys ifaces body) `withInterfaceModifiers` ms = InterfaceDecl ms id tys ifaces body

        getOuter classType ics = innerInner <$> find (\ ic -> innerInner ic == classType) ics

-- | Parse a bunch of binary .class files
--
-- Because inner class declarations are actually described in separate .class files, 
-- this function needs all relevant .class files. Inner classes in the result are 
-- represented as proper 'MemberDecl's.
parseClasses :: [ByteString] -> [(ClassType, TypeDecl)]
parseClasses streams = mapMaybe toTypeDecl declMap
  where declMap = map run streams
        run stream = runGet (getClass_ $ Just declMap) stream

toTypeDecl :: (ClassType, ImportedType) -> Maybe (ClassType, TypeDecl)
toTypeDecl (classType, (Toplevel decl)) = Just (classType, decl)
toTypeDecl _ = Nothing

-- | Parse a binary .class file into a 'TypeDecl'.
getClass :: Get (ClassType, ImportedType)
getClass = getClass_ Nothing

-- | Parse all classes in a .jar file, and return a list of 'CompilationUnit's corresponding to non-inner classes.
parseJar :: FilePath -> IO [CompilationUnit]
parseJar jarPath = undefined
