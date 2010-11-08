{-# LANGUAGE DoRec #-}
module Main where

import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.ClassFile
import Data.Binary.Get  
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

test = 
  do   
    streams <- mapM BL.readFile files
    return $ parseClasses streams
  where
    files = ["test/Foo.class", 
             "test/Foo$Bar.class",
             "test/Foo$Bar$Quux.class",
             "test/Foo$IBaz.class",
             "test/Foo$IBaz$Blargh.class",
             "test/Foo$1.class"
             ]
    
main = 
  do decls <- test     
     mapM_ print $ map (pretty . snd) decls
