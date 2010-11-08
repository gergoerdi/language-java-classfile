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
    
    let decls = map (runGet (getClass $ Just decls)) streams
    return decls
  where
    files = ["test/Foo.class", "test/Foo$Bar.class"]
    
main = 
  do decls <- test     
     let decls' = mapMaybe (filterTopLevel . snd) decls
     mapM_ print $ map pretty decls'
  where filterTopLevel (ImportedTopLevel decl) = Just decl
        filterTopLevel _ = Nothing
