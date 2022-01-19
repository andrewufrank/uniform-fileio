-------------------------------------------------------------------
--
-- Module      :  Filenames
-- Copyright   :  andrew u frank -
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | the operations on filenames and extensions
--  uses the Path library, but wraps it in Path (to construct a read)
-- is a class except for the make
module Uniform.Filenames
  ( module Uniform.Filenames,
    module Uniform.Error,
    -- module Path.Internal,
    -- module Uniform.PathShowCase,
    Abs,
    Rel,
    File,
    Dir,
    Path,
    -- Path.Path(..),
    toFilePath,
  )
where

-- for Generics
-- import Path 
--   ( Abs,
--     Dir,
--     File,
--     Path,
--     Rel,
--     toFilePath,
--     Path
--   )
import Path hiding ((</>), addExtension)
import qualified Path 
-- import  Path.Internal (Path(..))
import qualified Path.IO as PathIO
import qualified System.FilePath as S
import Uniform.Error(ErrIO, callIO)
-- import Uniform.Zero(Zeros(..))
import Uniform.Strings
-- (Text, fromJustNote, t2s)
import Uniform.PathShowCase ()

takeBaseName' :: FilePath -> FilePath
takeBaseName' = S.takeBaseName

homeDir :: Path Abs Dir
homeDir = makeAbsDir "/home/frank/" :: Path Abs Dir

homeDir2 :: ErrIO (Path Abs Dir)
homeDir2 = callIO PathIO.getHomeDir :: ErrIO (Path Abs Dir)

-- replace homeDir with homeDir2 - is user independent but requires IO
currentDir :: ErrIO (Path Abs Dir)
currentDir = callIO PathIO.getCurrentDir

setCurrentDir :: Path Abs Dir -> ErrIO ()
setCurrentDir path = PathIO.setCurrentDir (unPath path)

stripProperPrefix' :: Path b Dir -> Path b t -> ErrIO (Path Rel t)
stripProperPrefix' dir fn = Path.stripProperPrefix (unPath dir) (unPath fn)

stripProperPrefixMaybe :: Path b Dir -> Path b t -> Maybe (Path Rel t)
stripProperPrefixMaybe dir fn = Path.stripProperPrefix (unPath dir) (unPath fn)

unPath :: a -> a
unPath = id

    

makeRelFile :: FilePath -> Path Rel File
makeRelDir :: FilePath -> Path Rel Dir
makeAbsFile :: FilePath -> Path Abs File
makeAbsDir :: FilePath -> Path Abs Dir
makeRelFile fn = fromJustNote ("makeRelFile " ++ fn) $ Path.parseRelFile fn

makeRelDir fn = fromJustNote ("makeRelDir " ++ fn) $ Path.parseRelDir fn

makeAbsFile fn = fromJustNote ("makeAbsFile " ++ fn) $ Path.parseAbsFile fn

makeAbsDir fn = fromJustNote ("makeAbsDir " ++ fn) $ Path.parseAbsDir fn

makeRelFileT :: Text -> Path Rel File
makeRelDirT :: Text -> Path Rel Dir
makeAbsFileT :: Text -> Path Abs File
makeAbsDirT :: Text -> Path Abs Dir
makeRelFileT = makeRelFile . t2s

makeRelDirT = makeRelDir . t2s

makeAbsFileT = makeAbsFile . t2s

makeAbsDirT = makeAbsDir . t2s

toShortFilePath :: Path df ar -> FilePath
---- ^ get the filepath, but without the trailing separator
--    , necessary for systemcalls
toShortFilePath = S.dropTrailingPathSeparator . toFilePath

instance Zeros (Path Abs Dir) where
  zero = makeAbsDir "/"

instance Zeros (Path Abs File) where
  zero = makeAbsFile "/zero"

instance Zeros (Path Rel Dir) where
  zero = makeRelDir "./"

instance Zeros (Path Rel File) where
  zero = makeRelFile "zero"

newtype Extension = Extension FilePath deriving (Show, Read, Eq, Ord)

unExtension :: Extension -> FilePath
unExtension (Extension e) = e

makeExtension :: FilePath -> Extension
makeExtension = Extension

-- extension does not include a leading "."
-- would need a makeExtension in IO to catch errors here
makeExtensionT :: Text -> Extension
makeExtensionT = Extension . t2s

class Filenames fp fr where
  getFileName :: fp -> fr

class Filenames3 fp file where
  type FileResultT fp file

  -- add a filepath to a absolute dir and givev an absolte file
  --
  (</>), addFileName :: fp -> file -> FileResultT fp file
  -- fails, if file is empty  does not add anything if file is empty
  (</>) = addFileName

class Filenames5 dir fil res where
  stripPrefix :: dir -> fil -> Maybe res
  -- ^ strip the

instance Filenames5 (Path b Dir) (Path b t) (Path Rel t) where
  stripPrefix d f = Path.stripProperPrefix (unPath d) (unPath f)

class Filenames4 fp file where
  type FileResultT4 fp file

  -- add a filepath to a absolute dir and givev an absolte dir
  --
  addDir :: fp -> file -> FileResultT4 fp file

class Filenames1 fp where
  -- instantiate only for filepath TODO do for path
  getImmediateParentDir :: fp -> FilePath
  -- ^ gets the name of the dir immediately above

  getParentDir :: fp -> FilePath
  -- ^ the parent dir of file

  getNakedFileName :: fp -> FilePath
  -- ^ filename without extension

  getNakedDir :: fp -> FilePath
  -- ^ get the last dir

instance Filenames FilePath FilePath where
  getFileName = snd . S.splitFileName

instance Filenames3 FilePath FilePath where
  type FileResultT FilePath FilePath = FilePath
  addFileName = S.combine

instance Filenames (Path ar File) (Path Rel File) where
  getFileName = Path.filename . unPath

instance Filenames3 (Path b Dir) FilePath where
  type FileResultT (Path b Dir) FilePath = (Path b File)
  addFileName p d =
    if null' d
      then error ("addFileName with empty file" ++ d)
      else (Path.</>) (unPath p) (unPath d2)
    where
      d2 = makeRelFile d :: Path Rel File

instance Filenames4 FilePath FilePath where
  type FileResultT4 FilePath FilePath = FilePath
  addDir p d = if null' d then p else p </> d

instance Filenames4 (Path b Dir) FilePath where
  type FileResultT4 (Path b Dir) FilePath = (Path b Dir)
  addDir p d =
    if null' d
      then p
      else p </> d2
    where
      d2 = makeRelDir d :: Path Rel Dir

instance Filenames4 (Path b Dir) (Path Rel t) where
  type FileResultT4 (Path b Dir) (Path Rel t) = (Path b t)
  addDir p d = (Path.</>) (unPath p) (unPath d)

instance Filenames3 (Path b Dir) (Path Rel t) where
  type FileResultT (Path b Dir) (Path Rel t) = (Path b t)
  addFileName p d = (Path.</>) (unPath p) (unPath d)

instance Filenames1 (Path ar File) where
  getNakedFileName = getNakedFileName . toFilePath
  getImmediateParentDir = getImmediateParentDir . toFilePath
  getParentDir = getParentDir . toFilePath
  getNakedDir = error "getNakedDir for Filenamse1 Path ar File) not existing"

instance Filenames1 (Path ar Dir) where
  getNakedFileName = error "getNakedFileName not from Dir"
  getImmediateParentDir = getImmediateParentDir . toFilePath
  getParentDir = getParentDir . toFilePath
  getNakedDir = getNakedDir . toFilePath

instance Filenames1 FilePath where
  getNakedFileName = removeExtension . getFileName
  getImmediateParentDir = (!! 1) . reverse . S.splitDirectories
  getParentDir = S.takeDirectory
  getNakedDir = (!! 0) . reverse . S.splitDirectories

class (Eq (ExtensionType fp)) => Extensions fp where
  -- extension do not include a leading '.'
  type ExtensionType fp
  getExtension :: fp -> ExtensionType fp
  removeExtension :: fp -> fp
  addExtension :: ExtensionType fp -> fp -> fp
--   setFileExtension :: ExtensionType fp -> fp -> fp 
-- to avoid name clash with Path

  -- must not have an extension before
  (<.>) :: fp -> ExtensionType fp -> fp -- eror when not legal?
  (<.>) f e = addExtension e f
  setExtension :: ExtensionType fp -> fp -> fp
  setExtension ext   = addExtension ext . removeExtension 
  hasExtension :: ExtensionType fp -> fp -> Bool
  hasExtension e = (e ==) . getExtension

  prop_add_has :: ExtensionType fp -> fp -> Bool
  prop_add_has e f = hasExtension e (addExtension e f)
  prop_add_add_has :: ExtensionType fp -> ExtensionType fp -> fp -> Bool
  prop_add_add_has e1 e2 f =
    hasExtension
      e1
      (setExtension e1 . setExtension e2 $ f)
  prop_set_get :: ExtensionType fp -> fp -> Bool
  prop_set_get e f = ((e ==) . getExtension) (setExtension e f)

instance Extensions FilePath where
  type ExtensionType FilePath = FilePath

  getExtension = removeChar '.' . snd . S.splitExtension
  addExtension e fp = fp S.<.> e
  removeExtension = fst . S.splitExtension
--   setExtension e = addExtension e . removeExtension
  

--    hasExtension e = (e ==) . getExtension

instance Extensions (Path ar File) where
  type ExtensionType (Path ar File) = Extension

  getExtension f = Extension e
    where
      -- definition of extension in path is with leading '.'
      -- multiple extensions are gradually built and removed
      -- split gives only the last
      -- add allows only one to add
      -- empty extensions throw error

      e = getExtension . toFilePath $ f

  setExtension e f =
    fromJustNote "setExtension" $ Path.setFileExtension (unExtension e) f

  addExtension = setExtension
  removeExtension = setExtension (Extension "")
