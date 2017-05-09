-----------------------------------------------------------------------------
--
-- Module      :  FilenamesAlgebra
-- Copyright   :  andrew u frank -
--
-- | the operations on filenames and extensions
-- instances for LegalFilenames (in Uniform.Filenames)
-- and FilePath

-- does not work unless i use a phantom first argument
-- change to use only the operations here outside
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FilenamesAlgebra (

         Filepathes (..)
         , L.LegalPathname, lpX
         , FilePath, fpX
         , L.LegalExtension
         , L.LegalFilename
         , t2fp
        --  , module Uniformm.Filenames
         , module Uniform.Error
         , module Uniform.Zero
          , htf_thisModulesTests
             ) where
--
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import           Uniform.Error
import           Uniform.Strings       (s2t, showT, t2s, removeChar)
import           Uniform.Zero
import Safe
import qualified          System.Posix.FilePath as P
    -- for FilePath ~ String
import  qualified         System.FilePath       as S
    -- for bytestring
import qualified          Uniform.Filenames  as L
--                (           LegalPathname, unLegalPathname, makeLegalPath
--                         , LegalExtension, unLegalExtension, makeLegalExtension
--                         , LegalFilename , unLegalFilename, makeLegalFilename
--                         , isHiddenS, splitFilepathS
--                         , unLegalExtension
--                         , combine
--            )
import Test.Framework
--import Test.Invariant
--

fpX = undef "FilePath phantom" :: FilePath
ftX = undef "Filename Text phantom" :: Text
lpX  = undef "LegalPath phantom" :: L.LegalPathname

class Filepathes fp    where
-- ^ a class for operations on filepathes and
    type FN fp  -- the filename proper
    type Ext fp  -- the extension

    -- the mkXX interface call error when illegal input
    -- the option to correct is .. TODO
    mkFilepath :: fp -> Text -> fp
    mkFilename :: fp -> Text -> FN fp
    mkExtension :: fp -> Text -> Ext fp
    -- inverses:
    filepath2text :: fp -> fp -> Text
    -- ^ returns the full path, with filename and extension
    -- phantom is not required, but added for uniformity
    -- not just the dir
    filename2text :: fp -> FN fp -> Text
    extension2text :: fp -> Ext fp -> Text

    takeDir :: fp -> fp
    takeDir = fst3 . splitFilepath
    takeFilename :: fp -> FN fp
    takeExtension :: fp -> Ext fp
    takeExtension  = thd3 . splitFilepath
    -- take only the last extension (not all)
    hasExtension :: Text -> fp -> Bool
    hasExtension e fp = (e==) . extension2text fp . takeExtension $ fp


    splitFilepath :: fp -> (fp, FN fp, Ext fp)
    combineFilepath :: fp -> FN fp -> Ext fp  -> fp

    splitDirectories :: fp -> [FN fp]
    --
    addFn, (</>) :: fp -> FN fp -> fp
    (</>) = addFn
    addExt :: fp -> FN fp -> Ext fp -> FN fp
    -- cannot be inline, because phantom is required
    isHidden :: fp -> Bool

test_hasExtension = do
    let f :: FilePath = mkFilepath fpX "a/b/c.e"
    assertBool (hasExtension "e" f)
test_extension = do
    let f :: FilePath = mkFilepath fpX "a/b/c.e"
    let res = extension2text f  . takeExtension $ f
    assertEqual "e" res

instance Filepathes FilePath where -- is a synonym for String?
    type FN FilePath = FilePath
    type Ext FilePath = FilePath
    mkFilepath _ = t2s
    mkFilename _ = t2s
    mkExtension _ = t2s

    filepath2text _ = s2t
    filename2text _ = s2t
    extension2text _ = s2t

    splitFilepath fp = (fp1, fn2, ext1 )
            where
                (fp1,fn1) = S.splitFileName fp   -- inverse combine
                (fn2, ext1) = second (removeChar '.') $ S.splitExtension fp
    combineFilepath fp fn e =  addFn fp (addExt fpX fn e)
    splitDirectories = map t2s .  L.splitDirectoriesOS . s2t
    addFn = S.combine
    addExt _ = S.addExtension
    isHidden = L.isHiddenS

instance Filepathes Text where -- is a synonym for String?
    type FN Text = Text
    type Ext Text = Text
    mkFilepath _ = id
    mkFilename _ = id
    mkExtension _ = id

    filepath2text _ = id
    filename2text _ = id
    extension2text _ = id

    splitFilepath fpa = (mkFilepath ftX . fp2t $ fp2
                            , mkFilename ftX . fp2t $ fn2
                            , mkExtension  ftX . fp2t $ ext2  )
            where
                (fp2, fn2, ext2) = splitFilepath . t2fp . filepath2text ftX $ fpa :: (FilePath, FilePath, FilePath)
    combineFilepath fp fn e =  addFn fp (addExt ftX fn e)
    splitDirectories =    splitDirectories
    addFn f e = fp2t ((t2fp f) </> (t2fp e))
    addExt _ f e = mkFilename ftX . s2t  $
                        addExt fpX (t2s . filename2text ftX $ f)  (t2s . extension2text ftX $ e)
    isHidden = isHidden . t2fp

fp2t :: FilePath -> Text
fp2t = s2t

t2fp :: Text -> FilePath
t2fp = t2s

instance Filepathes L.LegalPathname where
    type FN L.LegalPathname = L.LegalFilename
    type Ext L.LegalPathname = L.LegalExtension

    mkFilepath _ a = fromJustNoteT ["mkFilepath", "illegal input", showT a]
                . L.makeLegalPath $ a
    mkFilename _ a = fromJustNoteT ["mkFilename", "illegal input", showT a]
                . L.makeLegalFilename $ a
    mkExtension _ a = fromJustNoteT ["mkExtension", "illegal input", showT a]
                . L.makeLegalExtension $ a

    filepath2text _ = L.unLegalPathname
    filename2text _ = L.unLegalFilename
    extension2text _ = L.unLegalExtension

    splitFilepath fpa = (mkFilepath lpX . fp2t $ fp2 :: L.LegalPathname
                            , mkFilename lpX . fp2t $ fn2 :: L.LegalFilename
                            , mkExtension  lpX . fp2t $ ext2 :: L.LegalExtension)
            where
                (fp2, fn2, ext2) = splitFilepath . t2fp . filepath2text lpX $ fpa :: (FilePath, FilePath, FilePath)
    combineFilepath fp fn e =  addFn fp (addExt lpX fn e)
    splitDirectories =  map (mkFilename lpX) . splitDirectories . filepath2text lpX
    addFn = L.combine
    addExt _ f e = mkFilename lpX . s2t  $
                        addExt fpX (t2s . filename2text lpX $ f)  (t2s . extension2text lpX $ e)
    isHidden = L.isHiddenS
