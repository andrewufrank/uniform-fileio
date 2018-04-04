-----------------------------------------------------------------------------
--
-- Module      :  Filenames
-- Copyright   :  andrew u frank -
--
-- | the operations on filenames and extensions
--  uses the Path library, but wraps it in Path (to construct a read)
-- is a class except for the make


-----------------------------------------------------------------------------
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
        , DeriveGeneric
        -- , DeriveAnyClass 
         #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.Filenames  (
         module Uniform.Filenames
         , module Uniform.Error
         , Abs, Rel, File, Dir
             ) where
import           Uniform.Error hiding ((</>), (<.>))
import Uniform.Zero -- for Generics

import qualified Path.IO as PathIO
import qualified Path  as Path
import Path (Abs, Rel, File, Dir)

import  qualified         System.FilePath       as S -- prefered
import  qualified         System.FilePath.Posix       as S -- prefered
import qualified Data.List.Split          as Sp
--import  qualified         Filesystem.Path       as F -- prefered
-- not usable, has a different definition of FilePath


homeDir =  makeAbsDir "/home/frank/":: Path Abs Dir
homeDir2 = fmap Path $ callIO $ PathIO.getHomeDir  ::ErrIO (Path Abs Dir)
-- replace homeDir with homeDir2 - is user independent but requires IO

newtype Path b t = Path (Path.Path b t)
-- in Path: newtype   Path b t = Path FilePath
-- should this be used
  deriving (Ord, Eq, Generic)
  -- read and show is defined separately
  -- unclear what zero should be ?

instance Zeros (Path Abs File) where  -- required for NTdescriptor
  zero = makeAbsFile ""
instance Zeros (Path Rel File) where  -- required for NTdescriptor
  zero = makeRelFile ""

unPath (Path s) = s
toFilePath = Path.toFilePath . unPath


instance Show (Path b t) where
  show = show . Path.toFilePath . unPath

makeRelFile :: FilePath -> Path Rel File
makeRelDir :: FilePath -> Path Rel Dir
makeAbsFile :: FilePath -> Path Abs File
makeAbsDir :: FilePath -> Path Abs Dir

makeRelFile fn = Path . fromJustNote ("makeRelFile " ++ fn)
                        $ Path.parseRelFile fn
makeRelDir fn = Path . fromJustNote ("makeRelDir " ++ fn)
                        $ Path.parseRelDir fn
makeAbsFile fn = Path . fromJustNote ("makeAbsFile " ++ fn)
                        $ Path.parseAbsFile fn
makeAbsDir fn = Path . fromJustNote ("makeAbsDir " ++ fn)
                        $ Path.parseAbsDir fn

toShortFilePath :: Path df ar -> FilePath
---- ^ get the filepath, but without the trailing separator
--    , necessary for systemcalls
toShortFilePath = S.dropTrailingPathSeparator . Path.toFilePath . unPath

instance IsString (Path Abs File) where
    fromString = read . show
instance IsString (Path Abs Dir) where
    fromString = read
instance IsString (Path Rel File) where
    fromString = read
instance IsString (Path Rel Dir) where
    fromString = read

instance Read (Path Abs Dir) where
        readsPrec i r =   maybe []  (\res -> [(Path res, rem1)] )
                $ Path.parseAbsDir x
                where  [(x ::String , rem1)] = readsPrec i r
instance Read (Path Abs File) where
        readsPrec i r =  maybe []  (\res -> [(Path res, rem1)] )
                $ Path.parseAbsFile x
                where  [(x ::String , rem1)] = readsPrec i r
--                       mres = parseAbsFile x :: Maybe (Path Abs File)

instance Read (Path Rel Dir) where
        readsPrec i r =  maybe []  (\res -> [(Path res, rem1)] )
                $ Path.parseRelDir x
                where  [(x ::String , rem1)] = readsPrec i r
instance Read (Path Rel File) where
        readsPrec i r =  maybe []  (\res -> [(Path res, rem1)] )
                $ Path.parseRelFile x
                where  [(x ::String , rem1)] = readsPrec i r


instance CharChains2 (Path a d) String where show'  = show
instance CharChains2 (Path a d) Text where show'  = s2t . show

newtype Extension = Extension FilePath deriving (Show, Read, Eq, Ord)
unExtension (Extension e) = e

makeExtension = Extension
-- would need a makeExtension in IO to catch errors here

class Filenames fp fr where
    getFileName :: fp -> fr
class Filenames3 fp file  where
    type FileResultT fp file
    -- add a filepath to a absolute dir and givev an absolte file
    --
    (</>), addFileName :: fp -> file -> FileResultT fp file
    -- fails, if file is empty  does not add anything if file is empty
    (</>) = addFileName

class Filenames4 fp file  where
    type FileResultT4 fp file
    -- add a filepath to a absolute dir and givev an absolte dir
    --
    addDir  :: fp -> file -> FileResultT4 fp file

class Filenames1 fp where
    -- instantiate only for filepath
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
instance Filenames3 FilePath FilePath  where
    type FileResultT FilePath FilePath = FilePath
    addFileName  = S.combine

instance Filenames (Path ar File) (Path Rel File) where
    getFileName = Path . Path.filename . unPath

instance Filenames3 (Path b Dir) FilePath  where
    type FileResultT (Path b Dir) FilePath = (Path b File)
    addFileName p  d =
                Path $ if null' d
                    then error ("addFileName with empty file" ++ d)
                    else (Path.</>) (unPath p) (unPath d2)
        where
            d2 = makeRelFile d :: Path Rel File


instance Filenames4 FilePath FilePath  where
    type FileResultT4 FilePath FilePath = FilePath
    addDir p  d =  if null' d then p else (p </>) d

instance Filenames4 (Path b Dir) FilePath  where
    type FileResultT4 (Path b Dir) FilePath = (Path b Dir)
    addDir p  d =  Path $ if null' d then (unPath p)
                                    else (Path.</>) (unPath p) (unPath d2)
        where
            d2 = makeRelDir d :: Path Rel Dir

instance Filenames3 (Path b Dir) (Path Rel t)  where
    type FileResultT (Path b Dir) (Path Rel t) = (Path b t)
    addFileName p  d =  Path $ (Path.</>) (unPath p) (unPath d)

instance Filenames1 (Path ar File)   where
    getNakedFileName =   getNakedFileName . toFilePath
    getImmediateParentDir = getImmediateParentDir . toFilePath
    getParentDir =  getParentDir . toFilePath
    getNakedDir = error
        "getNakedDir for Filenamse1 Path ar File) not existing"

instance Filenames1 (Path ar Dir) where
    getNakedDir = getNakedDir . toFilePath

instance Filenames1 FilePath   where
    getNakedFileName =   removeExtension . getFileName
    getImmediateParentDir = (!! 1) . reverse . S.splitDirectories
    getParentDir = S.takeDirectory
    getNakedDir = (!! 0) . reverse . S.splitDirectories


class (Eq (ExtensionType fp)) => Extensions fp where
    type ExtensionType fp
    getExtension :: fp -> ExtensionType fp
    removeExtension :: fp -> fp
    addExtension :: ExtensionType fp -> fp -> fp
    -- must not have an extension before
    (<.>) :: fp -> ExtensionType fp -> fp  -- eror when not legal?
    (<.>) f e =  addExtension e f
    setExtension :: ExtensionType fp -> fp -> fp
    hasExtension :: ExtensionType fp -> fp -> Bool
    hasExtension e = (e ==) . getExtension

    prop_add_has :: ExtensionType fp -> fp -> Bool
    prop_add_has e f =  (hasExtension e) (addExtension e f)
    prop_add_add_has :: ExtensionType fp -> ExtensionType fp -> fp -> Bool
    prop_add_add_has e1 e2 f = (hasExtension e1)
               ( (setExtension e1) . setExtension e2 $ f)
    prop_set_get :: ExtensionType fp -> fp -> Bool
    prop_set_get e f =  ((e==) . getExtension)  (setExtension e f)

instance Extensions FilePath  where
    type ExtensionType FilePath = FilePath

    getExtension = removeChar '.' . snd . S.splitExtension
    addExtension e fp =  fp S.<.> e
    removeExtension  = fst . S.splitExtension
    setExtension e  = addExtension e . removeExtension
--    hasExtension e = (e ==) . getExtension

instance Extensions (Path ar File) where
    type ExtensionType (Path ar File) = Extension

    getExtension f = Extension . removeChar '.'
                    . Path.fileExtension . unPath $ f
    setExtension e f = Path $ fromJustNote "setExtension"
                    $ Path.setFileExtension
                    (unExtension e) (unPath f)
    addExtension   =  setExtension
    removeExtension   =  setExtension (Extension "")
--    hasExtension e f = (e==). getExtension

