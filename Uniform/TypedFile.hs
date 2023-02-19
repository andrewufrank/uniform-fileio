----------------------------------------------------------------------
--
-- Module      :  uniform.TypedFile
-- Copyright   :  andrew u frank -
--
-- mapping data structures to files typed with an extension
-- write and read quasi type-checked
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Uniform.TypedFile
  ( module Uniform.TypedFile,
    GZip.compress,
    GZip.decompress,
    EpochTime,
  )
where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as L
import qualified Path.IO (ensureDir)
import Uniform.FileIOalgebra (Handle)
import Uniform.FileStatus (EpochTime)
import Uniform.FileStrings
import Uniform.Filenames as FN (Path)
import Uniform.Strings 
import Uniform.Error

data TypedFile5 a b = TypedFile5 {tpext5 :: Extension}

rdfGraphDebug = False

-- | reads or writes  a structured file with the specified parsers or writer
-- the first parameter is the type of file, it is the type of the
-- input data and the returned data
-- the second an arbitrary differentiation
-- to allow two file types with different extension and read
-- the b can be () if no differentiation is desired
class
  (FileHandles a) =>
  TypedFiles5 a b
  where
  append5 f = errorT ["TypedFiles - no implementation for append5", showT f]
  read5 f = errorT ["TypedFiles - no implementation for read5", showT f]
  read6 f = errorT ["TypedFiles - no implementation for read6", showT f]
  append6 f = errorT ["TypedFiles - no implementation for append6", showT f]
  openHandle6 f = errorT ["TypedFiles - no implementation for openHandle6", showT f]
  writeHandle6 f = errorT ["TypedFiles - no implementation for writeHandle6", showT f]
  closeHandle6 f = errorT ["TypedFiles - no implementation for closeHandle6", showT f]

  write5 :: FN.Path Abs Dir -> Path Rel File -> TypedFile5 a b -> a -> ErrIO ()
  -- write a file, directory is created if not exist
  -- file, if exist, is replaced
  write5 fp fn tp ct = do
    dirx <- Path.IO.ensureDir (unPath fp)
    write6 (fp </> fn) tp ct

  append5 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> a -> ErrIO ()
  read5 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> ErrIO a

  write6 :: Path Abs File -> TypedFile5 a b -> a -> ErrIO ()
  -- write a file, directory is created if not exist
  -- file, if exist, is replaced
  write6 fp tp queryText = do
    --        when rdfGraphDebug $
    putIOwords ["write6", showT fp]
    --        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
    let fn2 = setExtension (tpext5 tp) fp
    createDirIfMissing' (getParentDir fp) -- add everywhere?
    when rdfGraphDebug $
      putIOwords
        [ "sparql Turtle createDIrIfMissing' ",
          showT (getParentDir fp)
        ]
    hand <- openFile2handle fn2 WriteMode
    --        when rdfGraphDebug $ putIOwords ["write6", showT fn2]

    write2handle hand queryText -- changed for Text not []
    closeFile2 hand

  openHandle6 :: Path Abs File -> TypedFile5 a b -> ErrIO Handle

  -- | create the file and open the handle
  -- should attache ".tmp" to extension and when closing
  -- rename to correct filename - > transaction completed
  writeHandle6 :: Handle -> TypedFile5 a b -> a -> ErrIO ()

  closeHandle6 :: Path Abs File -> TypedFile5 a b -> Handle -> ErrIO ()

  -- | close the handle - with transaction
  append6 :: Path Abs File -> TypedFile5 a b -> a -> ErrIO ()

  -- append to the file, with the same methods as in write6
  read6 :: Path Abs File -> TypedFile5 a b -> ErrIO a

  exist6 :: Path Abs File -> TypedFile5 a b -> ErrIO Bool
  -- ^ check whether file exist
  exist6 fp tp = do
    let fn2 = setExtension (tpext5 tp) fp :: Path Abs File
    doesFileExist' fn2

  modificationTime6 :: Path Abs File -> TypedFile5 a b -> ErrIO EpochTime
  modificationTime6 fp tp = do
    let fn2 = setExtension (tpext5 tp) fp :: Path Abs File
    t :: EpochTime <- getFileModificationTime fn2
    return t

  isTyped :: Path Abs File -> TypedFile5 a b -> Bool
  -- ^ check if a given file is of the right type (extenions, not mime type)
  isTyped fp tp = getExtension fp == typedExtension tp

  typedExtension :: TypedFile5 a b -> Extension
  -- ^ get the extension back
  typedExtension tp = tpext5 tp

  makeTyped :: Extension -> TypedFile5 a b
  -- make a typed file type, needs type specification!
  makeTyped ext = TypedFile5 {tpext5 = ext}

instance TypedFiles5 Text b where
  -- file contains a list of lines (text)
  write5 fp fn tp ct = do
    dirx <- Path.IO.ensureDir (unPath fp)
    let fn2 = fn <.> tpext5 tp -- :: Path ar File
    writeFile2 (fp </> fn2) ct

  --      writeFile2 (fp </> (fn <.> (tpext tp) )) . unlines'
  append5 fp fn tp ct = do
    dirx <- Path.IO.ensureDir (unPath fp)
    let fn2 = fn <.> tpext5 tp -- :: Path ar File
    appendFile2 (fp </> fn2) ct
  read5 fp fn tp = do
    let fn2 = fn <.> tpext5 tp
    readFile2 $ fp </> fn2

  append6 fn tp ct = do
    let fn2 = setExtension (tpext5 tp) fn
    appendFile2 fn2 ct
  write6 fn tp ct = do
    let fn2 = setExtension (tpext5 tp) fn
    hand <- openFile2handle fn2 WriteMode

    write2handle hand ct

    closeFile2 hand

  exist6 fn tp = do
    let fn2 = setExtension (tpext5 tp) fn
    doesFileExist' fn2

  read6 fn tp = do
    let fn2 = setExtension (tpext5 tp) fn
    readFile2 fn2

instance TypedFiles5 [Text] b where
  -- file contains a list of lines (text)
  --    mkTypedFile5  = TypedFile5 { tpext5 = Extension "txt"}
  write5 fp fn tp ct = do
    dirx <- Path.IO.ensureDir (unPath fp)
    let fn2 = fn <.> tpext5 tp -- :: Path ar File
    writeFile2 (fp </> fn2) (unlines' ct)

  append5 fp fn tp ct = do
    dirx <- Path.IO.ensureDir (unPath fp)
    let fn2 = fn <.> tpext5 tp -- :: Path ar File
    appendFile2 (fp </> fn2) (unlines' ct)
  read5 fp fn tp = do
    let fn2 = fn <.> tpext5 tp
    fmap lines' . readFile2 $ fp </> fn2

  append6 fn tp ct = do
    let fn2 = setExtension (tpext5 tp) fn
    appendFile2 fn2 (unlines' ct)
  write6 fn tp ct = do
    let fn2 = setExtension (tpext5 tp) fn
    hand <- openFile2handle fn2 WriteMode

    write2handle hand (unlines' ct)

    closeFile2 hand

  exist6 fn tp = do
    let fn2 = setExtension (tpext5 tp) fn
    doesFileExist' fn2

  read6 fn tp = do
    let fn2 = setExtension (tpext5 tp) fn
    fmap lines' . readFile2 $ fn2

data GZip

-- | files with full triples stored as zip
instance TypedFiles5 LazyByteString GZip where
  append6 fp tp jsonld = do
    when rdfGraphDebug $ putIOwords ["triples append6", showT fp]
    let fn2 = setExtension (tpext5 tp) fp

    appendFile2 fn2 (GZip.compress jsonld)

  openHandle6 fp tp = do
    when rdfGraphDebug $ putIOwords ["openHandle6 jsonld"]
    let ext = unExtension (tpext5 tp)
    let tmpext = Extension (ext <.> "tmp")
    let fn2 = setExtension tmpext fp
    when rdfGraphDebug $ putIOwords ["openHandle6 jsonld", showT fn2]

    createDirIfMissing' (getParentDir fn2) -- add everywhere?
    hand <- openFile2handle fn2 WriteMode
    -- should create or truncate the file, but not when the dir not exist
    --https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html#g:5
    when rdfGraphDebug $ putIOwords ["openHandle6 jsonld", showT fn2]
    return hand

  closeHandle6 fp tp hand = do
    --        when rdfGraphDebug $
    when rdfGraphDebug $ putIOwords ["closeHandle6 jsonld"]
    let ext = unExtension (tpext5 tp)
    let tmpext = Extension (ext <.> "tmp")
    closeFile2 hand
    let fn2 = setExtension tmpext fp
    let fn1 = setExtension (tpext5 tp) fp
    renameOneFile fn2 fn1
    when rdfGraphDebug $ putIOwords ["closeHandle6 jsonld", showT fn2]
    return ()

  writeHandle6 hand tp jsonld = do
    r <- write2handle hand (GZip.compress jsonld)
    return r

  read6 fp tp = error "read for jsonld is not easy and not required"

-- | the a is the base type
-- which is written on file, b is the type for input and output
class FileHandles a => TypedFiles7 a b where
  wrap7 :: a -> b
  unwrap7 :: b -> a

class FileHandles a => TypedFiles7a a b where
  -- | the 7 have two arguments for path and file
  read7 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> ErrIO b

  write7 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> b -> ErrIO ()

  -- | the 8 versions have a single argument for path and file
  read8 :: Path Abs File -> TypedFile5 a b -> ErrIO b

  write8 :: Path Abs File -> TypedFile5 a b -> b -> ErrIO ()


  renameToBak8 :: Path Abs File -> TypedFile5 a b -> ErrIO ()
  -- ^ the createDir if missingis implied in the write

instance (TypedFiles7 Text b) => TypedFiles7a Text b where
  -- an instance for all what has text or bytestring  as underlying rep
  write7 fp fn tp ct = do
    write8 (fp </> fn) tp ct

  read7 fp fn tp = do
    read8 (fp </> fn) tp

  write8 fp tp ct = do
    let fn2 = fp <.> tpext5 tp -- :: Path ar File
    let parent = getParentDir fn2
    createDirIfMissing' parent

    writeFile2 fn2 (unwrap7 ct :: Text)

  read8 fp tp = do
    let fp2 = fp <.> tpext5 tp
    ares :: Text <- readFile2 fp2
    return . wrap7 $ ares

  renameToBak8 fp tp = do 
    let fp2 = fp <.> tpext5 tp 
    let fpBak = fp <.> (Extension "bak")
    renameOneFile fp2 fpBak 
    return ()

instance (TypedFiles7 L.ByteString b) => TypedFiles7a L.ByteString b where
  -- an instance for all what has text or bytestring  as underlying rep
  write7 fp fn tp ct = do
    write8 (fp </> fn) tp ct

  read7 fp fn tp = do
    read8 (fp </> fn) tp

  write8 fp tp ct = do
    let fn2 = fp <.> tpext5 tp -- :: Path ar File
    let parent = getParentDir fn2
    createDirIfMissing' parent
    writeFile2 fn2 (unwrap7 ct :: L.ByteString)

  read8 fp tp = do
    let fp2 = fp <.> tpext5 tp
    ares :: L.ByteString <- readFile2 fp2
    return . wrap7 $ ares
