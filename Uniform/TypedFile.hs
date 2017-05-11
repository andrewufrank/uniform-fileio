--{-# OPTIONS_GHC -F -pgmF htfpp #-}
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


import           Uniform.Error
import           Uniform.FileIOalgebra
import           Uniform.Filenames
import           Uniform.FileStrings

import           Uniform.Strings

type TPparser a = LegalPathname -> ErrIO a
type TPwriter a = LegalPathname -> a -> ErrIO ()

data TypedFile a = TypedFile { tpext :: LegalExtension}

data TypedFile5 a b = TypedFile5 { tpext5 :: LegalExtension}

sqCharFile = mkTypedFile :: TypedFile Text

textFile = mkTypedFile :: TypedFile Text
textsFile = mkTypedFile :: TypedFile [Text]

class TypedFiles a where
-- | reads or writes  a structured file with the specified parers or writer
  mkTypedFile :: TypedFile a
  -- using a dummy argument for type specification is possible,
  -- but does not seem a good idea
  write4 :: LegalPathname -> LegalFilename -> TypedFile a -> a -> ErrIO ()
  -- write a file, directory is created if not exist
  -- file, if exist, is replaced
  read4 :: LegalPathname -> LegalFilename -> TypedFile a ->   ErrIO a

class TypedFiles5 a b where
-- | reads or writes  a structured file with the specified parers or writer
-- the first parameter is the type of file, the second an arbitrary differentiation
-- to allow two file types with different extension and read
  mkTypedFile5 :: TypedFile5 a b
  -- using a dummy argument for type specification is possible,
  -- but does not seem a good idea
  write5 :: LegalPathname -> LegalFilename -> TypedFile5 a b -> a -> ErrIO ()
  -- write a file, directory is created if not exist
  -- file, if exist, is replaced
  read5 :: LegalPathname -> LegalFilename -> TypedFile5 a b ->   ErrIO a

instance TypedFiles Text where
    -- files of a single text stream
    mkTypedFile  = TypedFile { tpext = e
                    -- , parserF = tparser
                -- , writerF = twriter
            }
            where e = mkExtension lpX "txt"
    write4 fp fn tp   = writeFileOrCreate (combineFilepath fp fn (tpext tp))
--                (fp </> (fn <.> (tpext tp) )) a
    read4 fp fn tp   = readFile2 $ combineFilepath fp fn (tpext tp)
--                (fp </> (fn <.> (tpext tp) ))

sqLinesFile = mkTypedFile :: TypedFile [Text]

instance TypedFiles [Text] where
    -- files of a single text stream
    mkTypedFile  = TypedFile { tpext = e
                    -- , parserF = tparser
                -- , writerF = twriter
            }
            where e = mkExtension lpX "txt"
    write4 fp fn tp  = writeFileOrCreate (combineFilepath fp fn (tpext tp)) . unlines'
--      writeFile2 (fp </> (fn <.> (tpext tp) )) . unlines'
    read4 fp fn tp   = fmap lines' $ readFile2 (combineFilepath fp fn (tpext tp))
--    $ readFile2 (fp </> (fn <.> (tpext tp) ))
