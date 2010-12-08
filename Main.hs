{-# LANGUAGE DoRec #-}
module Main where

import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.ClassFile
import Data.Binary.Get  
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad (replicateM, forM, forM_, mapM, mapM_)
import Data.Maybe (mapMaybe)
import System.Path
import Codec.Archive.LibZip
import System.IO.HVFS

import Control.DeepSeq
import Debug.Trace

main = do 
  decls <- testJar
  mapM_ print decls
  -- forM_ decls $ \(ClassType parts, decl) ->
  --   do print $ pretty $ CompilationUnit (Just $ PackageDecl $ Name $ map fst $ init parts) [] [decl]

isClassfile path = snd (splitExt path) == ".class"

testFiles = do 
  files <- filter isClassfile <$> recurseDir SystemFS classRoot
  forM files $ \file -> do    
    stream <- BL.readFile file
    let result = runGet getClass stream
    show result `deepseq` return result        
  where
    classRoot = "/tmp/jar"
    
testJar = do 
  withArchive [CheckConsFlag] jarPath $ do
    classfiles <- filter isClassfile <$> fileNames []
    forM classfiles $ \classfile -> do
      stream <- BL.pack <$> fileContents [] classfile
      let result = runGet getClass stream
      return result
      -- show result `deepseq` return result
         -- lift $ print result
         -- result `deepseq` return result
  where 
    -- jarPath = "/usr/lib/jvm/java-6-sun-1.6.0.22/jre/lib/rt.jar"                      
    jarPath = "/home/cactus/pkg/logisim-generic-2.5.1.jar"
    -- jarPath = "/tmp/foo.jar"
