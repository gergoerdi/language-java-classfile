{-# LANGUAGE DoRec #-}
module Main where

import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.ClassFile
import Data.Binary.Get  
import qualified Data.ByteString.Lazy as BL
import Control.Applicative

test = 
  do   
    file <- BL.readFile f
    return $ runGet getClass file
  where
    f = "test/Foo.class"
  
main = print =<< pretty <$> test
