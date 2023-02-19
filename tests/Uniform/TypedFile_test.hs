{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS -Wall #-}
-- {-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -w #-}

module Uniform.TypedFile_test   where

import Test.Framework

import           Uniform.Error
import           Uniform.FileIOalgebra (Handle)
import           Uniform.Filenames
import           Uniform.FileStrings
--import           Uniform.FileIO (EpochTime, getFileModificationTime)
import           Uniform.FileStatus
import           Uniform.Strings  
import qualified Data.ByteString.Lazy   as L

import Uniform.TypedFile


textLinesFile = makeTyped (Extension "txt")  ::TypedFile5 Text [Text] -- was [Text] ()
dir1 = makeAbsDir "/home/frank/"
file1 = makeRelFile "aaa"
ct = unlines' ["eins", "zwei"] :: Text
test_write = do
    r <- runErr $ write5 dir1 file1 textLinesFile ct
    assertEqual (Right () ) r

test_read = do
    r <- runErr $ read5 dir1 file1 textLinesFile
    assertEqual (Right ct ) r

-- data CompressedByteString
-- a gzip compressed bytestring -- 
gzippedTriples = TypedFile5 {tpext5 = Extension "triples.gzip"} 
                :: TypedFile5 L.ByteString [Text]

ct2 = ["eins", "zwei"]
test_gz4txt = do 
    r <- runErr $ write8 (dir1 </> file2) gzippedTriples  ct2
    assertEqual (Right ()) r 

file2 = makeRelFile "b2"

test_gz4back = do 
    r <- runErr $ read8 (dir1 </> file2) gzippedTriples  
    assertEqual (Right ct2) r 

instance TypedFiles7 L.ByteString  [Text]    where
    unwrap7 =  compress . b2bl . t2b . showT
    wrap7 = read . t2s . bb2t . bl2b . decompress  
    -- - | the a is the base type
    -- -- which is written on file, b is the type for input and output
    -- class FileHandles a => TypedFiles7 a b where
    --     wrap7 :: a -> b
    --     unwrap7 :: b -> a
    
-- issues with extension - should not include leading '.' 
-- but path operations require it
test_extension :: IO ()
test_extension = assertEqual (Extension "triples.gzip")
                            (tpext5 gzippedTriples) 

test_fileFormed = assertEqual ("b2.txt")
                    (toFilePath $ file2 <.> (Extension "txt"))
test_fileFormed2 = assertEqual ("b2.triples.gzip")
                (toFilePath $ file2 <.> (tpext5 gzippedTriples))

instance TypedFiles7 Text [Text] where  -- creates sequence of lines
  wrap7 t = lines' t  
  unwrap7 t = unlines' t 

file3 =   (dir1 </> makeRelFile "c3")
ct3 = ["some text for file c3"] ::[Text]
test_write8txt = do 
        r <- runErr $ write8 (file3) textLinesFile ct3
        assertEqual (Right ()) r

test_rename8 = do 
        r <- runErr $ renameToBak8 file3 textLinesFile 
        assertEqual (Right ()) r