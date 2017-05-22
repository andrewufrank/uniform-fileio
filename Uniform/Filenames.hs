-----------------------------------------------------------------------------
--
-- Module      :  Filenames
-- Copyright   :  andrew u frank -
--
-- | the operations on filenames and extensions
--  uses the Path library
-- is a class except for the make


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

module Uniform.Filenames  (
         module Uniform.Filenames
         , module Path
         , module Uniform.Error
         , module Uniform.Strings
--          , htf_thisModulesTests
             ) where
--
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import           Uniform.Error
import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error
import Path   hiding ( (</>) ) -- should I hide the quasi quoters?
import qualified Path   ((</>))
--import qualified          System.Posix.FilePath as P
import Path.IO
import  qualified         System.FilePath       as S -- prefered
import  qualified         System.FilePath.Posix       as S -- prefered

import Test.Framework
import Test.Invariant


makeRelFile :: FilePath -> Path Rel File
makeRelDir :: FilePath -> Path Rel Dir
makeAbsFile :: FilePath -> Path Abs File
makeAbsDir :: FilePath -> Path Abs Dir

makeRelFile fn = fromJustNote ("makeRelFile " ++ fn) $ parseRelFile fn
makeRelDir fn = fromJustNote ("makeRelDir " ++ fn) $ parseRelDir fn
makeAbsFile fn = fromJustNote ("makeAbsFile " ++ fn) $ parseAbsFile fn
makeAbsDir fn = fromJustNote ("makeAbsDir " ++ fn) $ parseAbsDir fn

toShortFilePath :: Path df ar -> FilePath
---- ^ get the filepath, but without the trailing separator, necessary for systemcalls
toShortFilePath = S.dropTrailingPathSeparator . Path.toFilePath

instance CharChains2 (Path a d) String where show'  = show
instance CharChains2 (Path a d) Text where show'  = s2t . show

newtype Extension = Extension FilePath deriving (Show, Read, Eq, Ord)
unExtension (Extension e) = e

class Filenames fp fr where
    getFileName :: fp -> fr
class Filenames3 fp file  where
    type FileResultT fp file
    -- add a filepath to a absolute dir and givev an absolte file
    (</>)  :: fp -> file -> FileResultT fp file

class Filenames1 fp where
    -- instantiate only for filepath
    getImmediateParentDir :: fp -> FilePath
    -- ^ the parent dir of file
    getNakedFileName :: fp -> FilePath
    -- ^ filename without extension

instance Filenames FilePath FilePath where
    getFileName = snd . S.splitFileName
instance Filenames3 FilePath FilePath  where
    type FileResultT FilePath FilePath = FilePath
    p </>  d  = S.combine p d

instance Filenames (Path ar File) (Path Rel File) where
    getFileName = filename

instance Filenames3 (Path b Dir) FilePath  where
    type FileResultT (Path b Dir) FilePath = (Path b File)
    p </> d =  (Path.</>) p d2
        where
            d2 = makeRelFile d :: Path Rel File
instance Filenames3 (Path b Dir) (Path Rel t)  where
    type FileResultT (Path b Dir) (Path Rel t) = (Path b t)
    p </> d =  (Path.</>) p d

instance Filenames1 (Path ar File)   where
    getNakedFileName =   getNakedFileName . toFilePath
    getImmediateParentDir = getImmediateParentDir . toFilePath

instance Filenames1 FilePath   where
    getNakedFileName =   removeExtension . getFileName
    getImmediateParentDir = (!! 1) . reverse . S.splitDirectories

testname = "/home/frank/dir1/file.ext" :: FilePath
test_immediateParent = assertEqual "dir1" (getImmediateParentDir testname)
test_nakedFilename = assertEqual "file" (getNakedFileName testname)

testname2 = makeAbsFile testname

test_immediateParent2 = assertEqual "dir1" (getImmediateParentDir testname2)
test_nakedFilename2 = assertEqual "file" (getNakedFileName testname2)

class (Eq (ExtensionType fp)) => Extensions fp where
    type ExtensionType fp
    getExtension :: fp -> ExtensionType fp
    removeExtension :: fp -> fp
    addExtension :: ExtensionType fp -> fp -> Maybe fp
    -- must not have an extension before
    (<.>) :: fp -> ExtensionType fp -> fp  -- eror when not legal?
    (<.>) f e = fromJustNote "addExtension was not legal" $ addExtension e f
    setExtension :: ExtensionType fp -> fp -> Maybe fp
    hasExtension :: ExtensionType fp -> fp -> Bool
    hasExtension e = (e ==) . getExtension

    prop_add_has :: ExtensionType fp -> fp -> Bool
    prop_add_has e f = maybe True (hasExtension e) (addExtension e f)
    prop_add_add_has :: ExtensionType fp -> ExtensionType fp -> fp -> Bool
    prop_add_add_has e1 e2 f =  maybe True (hasExtension e1)
               (maybe Nothing (setExtension e1) . setExtension e2 $ f)
    prop_set_get :: ExtensionType fp -> fp -> Bool
    prop_set_get e f = maybe True ((e==) . getExtension) (setExtension e f)

instance Extensions FilePath  where
    type ExtensionType FilePath = FilePath

    getExtension = removeChar '.' . snd . S.splitExtension
    addExtension e fp = Just $ fp S.<.> e
    removeExtension  = fst . S.splitExtension
    setExtension e  = addExtension e . removeExtension
--    hasExtension e = (e ==) . getExtension

instance Extensions (Path ar File) where
    type ExtensionType (Path ar File) = Extension

    getExtension f = Extension . removeChar '.' . Path.fileExtension $ f
    setExtension e = Path.setFileExtension (unExtension e)
    addExtension e f  = Path.setFileExtension (unExtension e) f
    removeExtension   = fromJustNote "removeExtension" . setExtension (Extension "")
--    hasExtension e f = (e==). getExtension

--instance AnyPath FilePath where
--    makeAbsolute t =  do
--            let f = (makeAbsFile "testesss")
--            return $ AbsPath (makeAbsFile "testesss")
------------------tests

-- rigerous filepath testing is difficult, as many inputs are not leading to leagal path
f1 = "afile" :: FilePath
f0 = "" :: FilePath  -- not legal?
f2 = "afile.ext" :: FilePath
f3 = "/somedir/more/afile.ext"  :: FilePath
test_emptyExt = assertEqual "" (getExtension f1)
test_emptyExt0 = assertEqual "" (getExtension f0)
test_getExt = assertEqual "ext" (getExtension f2)
test_hasExt = assertBool $  hasExtension "ext" f2
test_hasExt2 = assertBool $  hasExtension "ext" f3
test_addExt = assertEqual (Just f2) $  addExtension "ext" f1
test_removeExt = assertEqual f1 (removeExtension f2)
test_setExt = assertEqual (Just "afile.txt") (setExtension "txt" f2)

--prop_add_has_FP :: FilePath -> FilePath -> Bool
--prop_add_has_FP e f = if (isInfixOf' "." e) then True else prop_add_has e f
--prop_add_add_has_FP :: FilePath ->FilePath ->FilePath -> Bool
--prop_add_add_has_FP  =  prop_add_add_has
--prop_set_get_FP :: FilePath -> FilePath ->  Bool
--prop_set_get_FP  = prop_set_get

g1 = makeRelFile "afile"
--g0 = ""  -- not legal?
g2 = makeRelFile "afile.ext"
g3 = makeAbsFile "/somedir/more/afile.ext"
g4 = makeAbsFile "/somedir/more/afile.txt"
e1 = (Extension "ext")
test_emptyExt_P = assertEqual (Extension "") (getExtension g1)
--test_emptyExt0 = assertEqual "" (getExtension f0)
test_getExt_P = assertEqual e1 (getExtension g2)
test_hasExt_P = assertBool $  hasExtension e1 g2
test_hasExt2_P = assertBool $  hasExtension e1 g2
test_addExt_P = assertEqual (Just g2) $  addExtension e1 g1
test_removeExt_P = assertEqual g1 (removeExtension g2)
test_setExt_P = assertEqual (Just g4) (setExtension (Extension "txt") g3)

--instance Arbitrary (Path Rel File) where
--    arbitrary = do
--        s :: String <- arbitrary
--        let mf = Path.parseRelFile s
--        case mf of
--            Just f -> return f
--            Nothing -> arbitrary
--instance Arbitrary Extension where
--    arbitrary = do
--        e0 :: String <- arbitrary
--        if legalExtension e0 then return (Extension e0)  else arbitrary

--prop_set_get_P :: Extension -> Path Rel File -> Bool
--prop_set_get_P e f = prop_set_get e f




--legalName :: String -> Bool
--legalName f = not ( null' f
--            || ("\n" `isInfixOf'` (toString f) )
--            || ("\t" `isInfixOf'` (toString f) )
--            || (" " `isPostfixOf'` (toString f) )
--            || (" " `isPrefixOf'` (toString f) )
--            )
--
--legalExtension f = null' f
--    || (legalName f
--        && not ("." `isInfixOf'` (toString f) )
--        && not ("/" `isInfixOf'` (toString f) )
--        )




--class Filepathes fp    where
---- ^ a class for operations on filepathes and
--    type Ext fp  -- the extension
--
--    -- the mkXX interface call error when illegal input
--    -- the option to correct is .. TODO
----    mkFilepath :: fp -> Text -> fp
----    mkFilename :: fp -> Text -> FN fp
----    mkExtension :: fp -> Text -> Ext fp
--    -- inverses:
--    filepath2text ::   fp -> Text
--    -- ^ returns the full path, with filename and extension
--    -- phantom is not required, but added for uniformity
--    -- not just the dir
--    filename2text :: fp   -> Text
----    extension2text :: fp -> Ext fp -> Text
--
--    takeDir :: fp -> fp
--    takeDir = fst3 . splitFilepath
--    takeFilename :: fp -> FN fp
--    takeExtension :: fp -> Ext fp
--    takeExtension  = thd3 . splitFilepath
--    -- take only the last extension (not all)
--    hasExtension :: Text -> fp -> Bool
--    hasExtension e fp = (e==) . extension2text fp . takeExtension $ fp
--
--
--    splitFilepath :: fp -> (fp, FN fp, Ext fp)
--    combineFilepath :: fp -> FN fp -> Ext fp  -> fp
--
--    splitDirectories :: fp -> [FN fp]
--    --
--    addFn, (</>) :: fp -> FN fp -> fp
--    (</>) = addFn
--    addExt :: fp -> FN fp -> Ext fp -> FN fp
--    -- cannot be inline, because phantom is required
--    isHidden :: fp -> Bool
--
--test_hasExtension = do
--    let f :: FilePath = mkFilepath fpX "a/b/c.e"
--    assertBool (hasExtension "e" f)
--test_extension = do
--    let f :: FilePath = mkFilepath fpX "a/b/c.e"
--    let res = extension2text f  . takeExtension $ f
--    assertEqual "e" res
--
--instance Filepathes FilePath where -- is a synonym for String?
--    type FN FilePath = FilePath
--    type Ext FilePath = FilePath
--    mkFilepath _ = t2s
--    mkFilename _ = t2s
--    mkExtension _ = t2s
--
--    filepath2text _ = s2t
--    filename2text _ = s2t
--    extension2text _ = s2t
--
--    splitFilepath fp = (fp1, fn2, ext1 )
--            where
--                (fp1,fn1) = S.splitFileName fp   -- inverse combine
--                (fn2, ext1) = second (removeChar '.') $ S.splitExtension fp
--    combineFilepath fp fn e =  addFn fp (addExt fpX fn e)
--    splitDirectories = map t2s .  L.splitDirectoriesOS . s2t
--    addFn = S.combine
--    addExt _ = S.addExtension
--    isHidden = L.isHiddenS
--
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
