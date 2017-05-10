-----------------------------------------------------------------------------
--
-- Module      :  FilenamesAlgebra
-- Copyright   :  andrew u frank -
--
-- | the operations on filenames and extensions
-- instances for  FilePath

-- does not work unless i use a phantom first argument

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

module Uniform.FilenamesFilePath (

         Filepathes (..)
          , htf_thisModulesTests
             ) where
---- using uniform:
import Uniform.FilenamesAlgebra
--import           Uniform.Error
import           Uniform.Strings
--import           Uniform.Zero
--import Safe
--import qualified          System.Posix.FilePath as P
--    -- for FilePath ~ String
import  qualified         System.FilePath       as S
--    -- for bytestring
----            )
import Test.Framework
--import Test.Invariant
--

fpX = undef "FilePath phantom" :: FilePath
--ftX = undef "Filename Text phantom" :: Text
--lpX  = undef "LegalPath phantom" :: L.LegalPathname



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
    splitDirectories = S.splitDirectories
    addFn = S.combine
    addExt _ = S.addExtension
    isHidden = isPrefixOf' "."

--instance Filepathes Text where -- is a synonym for String?
--    type FN Text = Text
--    type Ext Text = Text
--    mkFilepath _ = id
--    mkFilename _ = id
--    mkExtension _ = id
--
--    filepath2text _ = id
--    filename2text _ = id
--    extension2text _ = id
--
--    splitFilepath fpa = (mkFilepath ftX . fp2t $ fp2
--                            , mkFilename ftX . fp2t $ fn2
--                            , mkExtension  ftX . fp2t $ ext2  )
--            where
--                (fp2, fn2, ext2) = splitFilepath . t2fp . filepath2text ftX $ fpa :: (FilePath, FilePath, FilePath)
--    combineFilepath fp fn e =  addFn fp (addExt ftX fn e)
--    splitDirectories =    splitDirectories
--    addFn f e = fp2t ((t2fp f) </> (t2fp e))
--    addExt _ f e = mkFilename ftX . s2t  $
--                        addExt fpX (t2s . filename2text ftX $ f)  (t2s . extension2text ftX $ e)
--    isHidden = isHidden . t2fp
--
--fp2t :: FilePath -> Text
--fp2t = s2t
--
--t2fp :: Text -> FilePath
--t2fp = t2s
--
--instance Filepathes L.LegalPathname where
--    type FN L.LegalPathname = L.LegalFilename
--    type Ext L.LegalPathname = L.LegalExtension
--
--    mkFilepath _ a = fromJustNoteT ["mkFilepath", "illegal input", showT a]
--                . L.makeLegalPath $ a
--    mkFilename _ a = fromJustNoteT ["mkFilename", "illegal input", showT a]
--                . L.makeLegalFilename $ a
--    mkExtension _ a = fromJustNoteT ["mkExtension", "illegal input", showT a]
--                . L.makeLegalExtension $ a
--
--    filepath2text _ = L.unLegalPathname
--    filename2text _ = L.unLegalFilename
--    extension2text _ = L.unLegalExtension
--
--    splitFilepath fpa = (mkFilepath lpX . fp2t $ fp2 :: L.LegalPathname
--                            , mkFilename lpX . fp2t $ fn2 :: L.LegalFilename
--                            , mkExtension  lpX . fp2t $ ext2 :: L.LegalExtension)
--            where
--                (fp2, fn2, ext2) = splitFilepath . t2fp . filepath2text lpX $ fpa :: (FilePath, FilePath, FilePath)
--    combineFilepath fp fn e =  addFn fp (addExt lpX fn e)
--    splitDirectories =  map (mkFilename lpX) . splitDirectories . filepath2text lpX
--    addFn = L.combine
--    addExt _ f e = mkFilename lpX . s2t  $
--                        addExt fpX (t2s . filename2text lpX $ f)  (t2s . extension2text lpX $ e)
--    isHidden = L.isHiddenS
