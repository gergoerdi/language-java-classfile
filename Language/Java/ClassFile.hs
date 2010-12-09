{-# LANGUAGE DoRec #-}
module Language.Java.ClassFile (
  -- * Classfile parsing
  ClassImportResult(..),
  InnerType(..),
  getClass,
  -- parseClasses,
  -- parseJar,
  -- * Magic identifiers
  nameConstructor, nameClassConstructor
  ) where

import Language.Java.Syntax
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.ByteString.UTF8 (toString)
import Data.Maybe
import Control.Monad
import Data.Word
import Control.Applicative ((<$>), (<$))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Flags
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import Data.List
import Control.DeepSeq
import Data.Monoid

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
  (RefType <$> ClassRefType <$> (char 'L' >> parseClassTypeEnd))

readType :: String -> Type
readType s = case parse (parseFull parseType) "" s of
  Right x -> x
  Left e -> error $ show e

parseRefType :: Parser RefType
parseRefType =
  ArrayType <$> (char '[' >> parseType) <|>
  ClassRefType <$> parseClassType    

readRefType :: String -> RefType             
readRefType s = 
  case parse (parseFull parseRefType) "" s of
    Right x -> x
    Left e -> error $ unwords [show s, show e]

parseClassType :: Parser ClassType
parseClassType = 
  do parts <- (many1 $ alphaNum <|> oneOf "-_") `sepBy` (char '/' <|> char '$')
     let ids = map (\ part -> (Ident part, [])) parts
     return $ ClassType ids

parseClassTypeEnd =
  do x <- parseClassType
     _ <- char ';'
     return x

  
parseMethodType :: Parser (Maybe Type, [Type])
parseMethodType = 
  do args <- between (char '(') (char ')') (many parseType)
     ret <- (Nothing <$ char 'V') <|> (Just <$> parseType) 
     return (ret, args)
  
readMethodType s = case parse (parseFull parseMethodType) "" s of
  Right x -> x
  Left e -> error $ show e

type ConstIdx = IntMap.Key
getConstIdx = fromIntegral <$> getWord16be

data ConstantValue = ValueInteger Word32
                   | ValueFloat Float
                   | ValueLong Word64
                   | ValueDouble Double
                   | ValueString String
                   deriving Show

data Constant = ConstantClass { constRefType :: RefType }
              | ConstantValue ConstantValue
              | ConstantString String
              deriving Show
  
type ConstTable = IntMap Constant
type ClassImporter a = ReaderT ConstTable Get a

getMany f = 
  do count <- lift getWord16be
     forM [1..count] $ const f

getConstant :: ConstTable -> Get (ConstIdx, Maybe Constant)
getConstant constTable = 
  do tag <- getWord8
     case tag of
       1 -> noSkip <$> Just <$> ConstantString <$> getString
       3 -> noSkip <$> Just <$> ConstantValue <$> ValueInteger <$> getWord32be
       4 -> noSkip <$> Just <$> ConstantValue <$> ValueFloat <$> getFloat32be
       5 -> skip <$> Just <$> ConstantValue <$> ValueLong <$> getWord64be
       6 -> skip <$> Just <$> ConstantValue <$> ValueDouble <$> getFloat64be
       7 -> noSkip <$> Just <$> ConstantClass <$> readRefType <$> getStringRef
       8 -> noSkip <$> Just <$> ConstantValue <$> ValueString <$> getStringRef
       
       9 -> noSkip <$> (getConstIdx >> getConstIdx >> return Nothing)
       10 -> noSkip <$> (getConstIdx >> getConstIdx >> return Nothing)
       11 -> noSkip <$> (getConstIdx >> getConstIdx >> return Nothing)
       12 -> noSkip <$> (getConstIdx >> getConstIdx >> return Nothing)
       
       tag -> fail $ unwords ["Unknown tag type:", show tag]
  where 
    getString = toString <$> (getByteString =<< (fromInteger . fromIntegral <$> getWord16be))
    
    -- lookupString idx = case IntMap.lookup idx constTable of
    --   Just (ConstantString s) -> s
      
    -- getStringRef = lookupString <$> getConstIdx

    getStringRef = do idx <- getConstIdx
                      let Just (ConstantString s) = IntMap.lookup idx constTable
                      return s
      
    noSkip x = (1, x)
    skip x = (2, x)
                 
getConstants :: Get ConstTable
getConstants = 
  do count <- fromIntegral <$> getWord16be
     rec 
       -- According to the Java .class spec, count is, for some reason, one larger than the actual number of constants
       consts <- IntMap.fromAscList <$> mapMaybe filter <$> (fromToM 1 (count - 1) $ \i -> 
         do (step, const) <- getConstant consts
            return (step, (i, const)))
     return consts
     
  where filter (_, Nothing) = Nothing
        filter (i, Just c) = Just (i, c)
  
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

constClassType con = let ClassRefType cls = constRefType con in cls

getClassTypeMaybe :: ClassImporter (Maybe ClassType)
getClassTypeMaybe = fmap constClassType <$> getConstantRefMaybe

getClassType :: ClassImporter ClassType
getClassType = constClassType <$> fromMaybe (error "getClassType") <$> getConstantRefMaybe

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
     if idx == 0 then return Nothing else asks (IntMap.lookup idx)

getConstantRef :: ClassImporter Constant
getConstantRef = fromMaybe (error "getConstantRef") <$> getConstantRefMaybe
                       
getStringRefMaybe ::ClassImporter (Maybe String)                 
getStringRefMaybe = 
  do const <- getConstantRefMaybe
     return $ case const of
       Just (ConstantString s) -> Just s
       _ -> Nothing
                 
getStringRef :: ClassImporter String
getStringRef = fromMaybe (fail "getStringRef") <$> getStringRefMaybe
     
data InnerClass = InnerClass { innerInner :: ClassType, 
                               innerOuter :: Maybe ClassType,
                               innerName :: Maybe String,
                               innerFlags :: Word16 }
                  deriving Show
     
data Attribute = AttrConstantValue ConstantValue
               | AttrExceptions [ClassType]
               | AttrSynthetic
               | AttrInnerClasses [InnerClass]
               | AttrSourceFile String
               deriving Show
                        
getInnerClass :: ClassImporter InnerClass
getInnerClass =
  do inner <- getClassType
     outer <- fmap constClassType <$> getConstantRefMaybe
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
         Just <$> AttrInnerClasses <$> getMany getInnerClass            
       "SourceFile" ->
         Just <$> AttrSourceFile <$> getStringRef
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
  where isAttrConstantValue (AttrConstantValue _) = True
        isAttrConstantValue _ = False                     

getMethod :: Ident -> ClassImporter (Maybe MemberDecl)
getMethod cls =
  do modifiers <- getModifiers
     name <- getStringRef
     let isConstructor = name `elem` [nameConstructor, nameClassConstructor]
     (ret, args) <- readMethodType <$> getStringRef
     let formals = zipWith (\i ty -> FormalParam [] ty False (VarId $ Ident $ 'p':show i)) [(1::Integer)..] args
     attributes <- getAttributes
     
     let getAttrExceptions (AttrExceptions tys) = Just tys
         getAttrExceptions _ = Nothing
         exceptions = case mapFirst getAttrExceptions attributes of
           Nothing -> []
           Just tys -> map ClassRefType tys
             
     return $ unlessSynthetic attributes $
       if isConstructor 
         then ConstructorDecl modifiers [] cls formals exceptions (ConstructorBody Nothing [])
         else MethodDecl modifiers [] ret (Ident name) formals exceptions (MethodBody Nothing)

unlessSynthetic :: [Attribute] -> a -> Maybe a
unlessSynthetic attributes x = if any isAttrSynthetic attributes then Nothing else Just x
  where isAttrSynthetic AttrSynthetic = True
        isAttrSynthetic _ = False

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

-- | Descriptor of an inner (nested) type. 
-- 
-- The actual definition of inner types are not in the .class file, but
-- in separate ones for each type. E.g. if class @Foo@ is defined
-- inside class @Bar@, then the file @Bar.class@ will contain an
-- 'InnerType' for Foo, and the actual definition is in
-- @Bar$Foo.class@.
data InnerType = InnerType [Modifier] ClassType
               deriving Show
                        
data ClassImportResult = ClassImportResult 
                         { sourcePath :: Maybe String,
                           outerType :: Maybe ClassType,
                           typeDecl :: TypeDecl,
                           innerTypes :: [InnerType] }
                       deriving Show
  
instance NFData Constant where
  rnf (ConstantClass ty) = ty `seq` ()
  rnf (ConstantValue value) = value `deepseq` ()
  rnf (ConstantString s) = s `deepseq` ()    

instance NFData ConstantValue where
  rnf (ValueString s) = s `deepseq` ()
  rnf (ValueInteger n) = n `seq` ()
  rnf (ValueLong n) = n `seq` ()
  rnf (ValueFloat f) = f `seq` ()
  rnf (ValueDouble f) = f `seq` ()
  
mapFirst f = getFirst . mconcat . map (First . f)
  
-- | Parse a binary .class file into an 'ImportedType'.
getClass :: Get (ClassType, ClassImportResult)
getClass = 
  do (_major, _minor) <- getHeader
     constTable <- getConstants
     constTable `deepseq` flip runReaderT constTable $
       do (isInterface, modifiers) <- getToplevel
     
          classType@(ClassType parts) <- getClassType
          let (this, _) = last parts
          superClass <- getClassTypeMaybe
          let super = case superClass of
                Nothing -> Nothing
                Just superClass -> if superClass /= classObject then Just $ ClassRefType superClass else Nothing
     
          ifaces <- getMany (ClassRefType <$> getClassType)
          fields <- catMaybes <$> getMany getField
          methods <- catMaybes <$> getMany (getMethod this)
          
          attributes <- getAttributes
          let sourcePath = mapFirst getSourceFile attributes
                where getSourceFile (AttrSourceFile path) = Just path
                      getSourceFile _ = Nothing
          
          let innerClasses = fromMaybe [] $ mapFirst getInnerClasses attributes
                where getInnerClasses (AttrInnerClasses ics) = Just ics
                      getInnerClasses _ = Nothing
              innerDecls = mapMaybe (toInnerType classType) innerClasses
              outer = getOuter classType innerClasses                      
              
          let members = fields ++ methods
              decl = if isInterface
                       then InterfaceTypeDecl $ InterfaceDecl modifiers this [] ifaces (InterfaceBody members)
                       else ClassTypeDecl $ ClassDecl modifiers this [] super ifaces (ClassBody $ map MemberDecl members)
     
          return (classType, ClassImportResult sourcePath outer decl innerDecls)
          
  where toInnerType classType ic = 
          do outer <- innerOuter ic             
             guard $ outer == classType
             let inner = innerInner ic
                 modifiers = parseModifiers $ innerFlags ic
             return $ InnerType modifiers inner
        
        -- (ClassDecl _ id tys super ifaces body) `withClassModifiers` ms = ClassDecl ms id tys super ifaces body
        -- (InterfaceDecl _ id tys ifaces body) `withInterfaceModifiers` ms = InterfaceDecl ms id tys ifaces body

        getOuter classType ics = innerOuter =<< find (\ ic -> innerInner ic == classType) ics

-- WIP:
--        
-- type DeclMap = [(ClassType, ImportedType)]
-- -- | Parse a bunch of binary .class files
-- --
-- -- Because inner class declarations are actually described in separate .class files, 
-- -- this function needs all relevant .class files. Inner classes in the result are 
-- -- represented as proper 'MemberDecl's.
-- parseClasses :: [ByteString] -> [(ClassType, TypeDecl)]
-- parseClasses streams = mapMaybe toTypeDecl declMap
--   where declMap = map run streams
--         run stream = runGet (getClass_ $ Just declMap) stream

-- toTypeDecl :: (ClassType, ImportedType) -> Maybe (ClassType, TypeDecl)
-- toTypeDecl (classType, (Toplevel decl)) = Just (classType, decl)
-- toTypeDecl _ = Nothing

-- -- | Parse all classes in a .jar file, and return a list of 'CompilationUnit's corresponding to non-inner classes.
-- parseJar :: FilePath -> IO [CompilationUnit]
-- parseJar jarPath = undefined
