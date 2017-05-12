{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS -Wall #-}
-- {-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -w #-}

module Uniform.TypedFile (
        module Uniform.TypedFile
--        , textFile, textsFile
        -- , module Uniform.FileIO
)  where

import Test.Framework

--import           Uniform.Error
--import           Uniform.FileIOalgebra
import           Uniform.Filenames
import           Uniform.FileStrings

--import           Uniform.Strings hiding ((</>))


data TypedFile5 a b = TypedFile5 { tpext5 :: Extension}


class TypedFiles5 a b where
-- | reads or writes  a structured file with the specified parsers or writer
-- the first parameter is the type of file, the second an arbitrary differentiation
-- to allow two file types with different extension and read
  mkTypedFile5 :: TypedFile5 a b
  -- using a dummy argument for type specification is possible,
  -- but does not seem a good idea
  write5 :: Path ar Dir -> Path Rel File -> TypedFile5 a b -> a -> ErrIO ()
  -- write a file, directory is created if not exist
  -- file, if exist, is replaced
  read5 :: Path ar Dir -> Path Rel File -> TypedFile5 a b ->   ErrIO a


instance TypedFiles5 [Text] () where
    -- file contains a list of lines (text)
    mkTypedFile5  = TypedFile5 { tpext5 = Extension "txt"}
    write5 fp fn tp  ct = do
        dirx <- ensureDir fp
        let fn2 = fn <.> tpext5 tp -- :: Path ar File
        writeFile2 (fp </> fn2 ) (unlines' ct)
--      writeFile2 (fp </> (fn <.> (tpext tp) )) . unlines'
    read5 fp fn tp   = do
        let fn2 = fn <.> (tpext5 tp)
        fmap lines' $ readFile2 (fp </> fn2)
--    $ readFile2 (fp </> (fn <.> (tpext tp) ))

textLinesFile = mkTypedFile5  ::TypedFile5 [Text] ()
dir1 = makeAbsDir "/home/frank/"
file1 = makeRelFile "aaa"
ct = ["eins", "zwei"]
test_write = do
    r <- runErr $ write5 dir1 file1 textLinesFile ct
    assertEqual (Right () ) r

test_read = do
    r <- runErr $ read5 dir1 file1 textLinesFile
    assertEqual (Right ct ) r

--type TPparser a = Path ar df -> ErrIO a
--type TPwriter a = Path ar df -> a -> ErrIO ()

--data TypedFile a = TypedFile { tpext :: LegalExtension}

--sqCharFile = mkTypedFile :: TypedFile Text
--
--textFile = mkTypedFile :: TypedFile Text
--textsFile = mkTypedFile :: TypedFile [Text]

--class TypedFiles a where
---- | reads or writes  a structured file with the specified parsers or writer
--  mkTypedFile :: TypedFile a
--  -- using a dummy argument for type specification is possible,
--  -- but does not seem a good idea
--  write4 :: Path ar df -> Path ar df -> TypedFile a -> a -> ErrIO ()
--  -- write a file, directory is created if not exist
--  -- file, if exist, is replaced
--  read4 :: Path ar df -> Path ar df -> TypedFile a ->   ErrIO a


--instance TypedFiles Text where
--    -- files of a single text stream
--    mkTypedFile  = TypedFile { tpext = e
--                    -- , parserF = tparser
--                -- , writerF = twriter
--            }
--            where e = mkExtension lpX "txt"
--    write4 fp fn tp   = writeFileOrCreate (combineFilepath fp fn (tpext tp))
----                (fp </> (fn <.> (tpext tp) )) a
--    read4 fp fn tp   = readFile2 $ combineFilepath fp fn (tpext tp)
----                (fp </> (fn <.> (tpext tp) ))
--
--sqLinesFile = mkTypedFile :: TypedFile [Text]
