{-# LANGUAGE DoRec #-}
module Main where

import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.ClassFile
import Data.Binary.Get  
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad (replicateM, forM_)
import Data.Maybe (mapMaybe)
import System.Path
import Codec.Archive.LibZip

test = 
  do streams <- mapM BL.readFile files
     return $ parseClasses streams
  where
    -- files = ["test/Foo.class", 
    --          "test/Foo$Bar.class",
    --          "test/Foo$Bar$Quux.class",
    --          "test/Foo$IBaz.class",
    --          "test/Foo$IBaz$Blargh.class",
    --          "test/Foo$1.class"
    --          ]
    files = ["/tmp/jar/java/lang/Short.class",
             "/tmp/jar/java/lang/Short$ShortCache.class"]
    
main = 
  do decls <- testJar
     forM_ decls $ \(ClassType parts, decl) ->
       do print $ pretty $ CompilationUnit (Just $ PackageDecl $ Name $ map fst $ init parts) [] [decl]

testZipList = 
  do withArchive [CheckConsFlag] jarPath $ do
       files <- filter isClassfile <$> fileNames []
       return files
  where
    jarPath = "/usr/lib/jvm/java-6-sun-1.6.0.22/jre/lib/rt.jar"
    isClassfile path = snd (splitExt path) == ".class"

testJar =
  do withArchive [CheckConsFlag] jarPath $ do
       classfiles <- filter isClassfile <$> fileNames []
       streams <- mapM (fileContents []) classfiles
       return $ parseClasses (map BL.pack streams)
  where 
    files = ["java/lang/Class.class"]
    jarPath = "/usr/lib/jvm/java-6-sun-1.6.0.22/jre/lib/rt.jar"                      
    isClassfile path = snd (splitExt path) == ".class"
