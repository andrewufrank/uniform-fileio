-----------------------------------------------------------------------------
--
-- Module      :  SVN.FileIO
-- Copyright   :  andrew u frank -
--
-- | handling of filenames to check initially for legality in a central place
-- assume that all filenames which exist on the OS are legal!
-- all functions are total (yield Maybe)

-- TODO  -- too complicated to use

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# OPTIONS -w #-}


module Uniform.Filenames (
    LegalFilename (..), unLegalFilename, makeLegalFilename
    , LegalPathname (..),  unLegalPathname, unL, makeLegalPath
    , LegalExtension (..), makeLegalExtension, makeLegalExtensionx
    , unLegalExtension
    , attachToFileName
    -- , _mkExtSS
            , CV2paths (..)
--          , mkFilenameS
          , CV2legal (..)
--          , convert2legalDir
         , (<.>)
         , (</>) -- attention: combination (a </> (b <.>c)) needs ()
          , splitFilenameExtensionS
          , takeFilenameS, takeExtensionS
          , testExtensionS, replaceExtensionS
          , dirs2path
          , getFilepathS, getFilenameS
          , Hiddings(..)
          , FPtype
          , FNstrings (..)
          , htf_thisModulesTests

            ) where

--import qualified Data.Text as T
--import qualified System.Posix as P
--import qualified System.Directory as S

-- using uniform:
import           Uniform.Error
import           Uniform.Strings hiding ((<.>), (</>))
import           Uniform.Zero
--import Data.StringUtilities

import           Data.Maybe

import           Test.Framework
import           Test.Invariant

--import           Control.Arrow   (first, second)
import qualified System.FilePath as OS

type FPtype = Text
-- String -- change to Text requires overloading the OS functions

makeLegalExtensionx = error "sdfa"
unL :: LegalPathname -> FilePath
unL = t2s . unLegalPathname

makeLegalPath :: (Monad m) =>  FPtype -> m LegalPathname
-- ^ make a LegalPathname - no IO, just reporting error!
makeLegalPath fp = do
    let f1 = dirs2path fp
    case f1 of
        Just fp2 -> return fp2
        Nothing  -> fail . unwords $ ["makeLegalPath", show fp, "not legal"]

makeLegalFilename :: (Monad m) =>  FPtype -> m LegalFilename
-- ^ make a LegalPathname - no IO, just reporting error!
makeLegalFilename fp = do
    let f1 = cv2legal fp
    case f1 of
        Just fp2 -> return fp2
        Nothing  -> fail . unwords $ ["makeLegalFilename", show fp, "not legal"]

makeLegalExtension :: (Monad m) =>  FPtype -> m LegalExtension
-- ^ make a LegalPathname - no IO, just reporting error!
makeLegalExtension fp = do
    let f1 = cv2legal fp
    case f1 of
        Just fp2 -> return fp2
        Nothing  -> fail . unwords $ ["makeLegalExtension", show fp, "not legal"]

attachToFileName :: LegalFilename -> Text -> LegalFilename
attachToFileName fn txt = fromJustNoteT ["attachToFileName", showT fn, txt]
        . makeLegalFilename $ (unLegalFilename fn <> txt)

instance CharChains2 LegalPathname Text where
    show' = unLegalPathname

joinPathOS :: [Text] -> Text
joinPathOS  = s2t . OS.joinPath . map t2s

splitDirectoriesOS :: Text -> [Text]
splitDirectoriesOS = map s2t . OS.splitDirectories . t2s

addExtensionOS :: Text -> Text -> Text
addExtensionOS f e = s2t $ OS.addExtension (t2s f) (t2s e)

splitExtensionOS :: Text -> (Text, Text)
splitExtensionOS = second (removeChar '.') . pair s2t . OS.splitExtension . t2s

test_splitExtensionOS =
    assertEqual ("a", "b") (splitExtensionOS "a.b")

--joinPathOS :: Text -> Text -> Text
--joinPathOS f e = s2t . OS.joinPath (t2s f) (t2s e)

-- | checking legal char in filenames and paths strings
-- what else should be not permitted?
fpLegalName :: FPtype -> Bool
fpLegalName f = not ( null' f
            || ("\n" `isInfixOf'` (toString f) )
            || ("\t" `isInfixOf'` (toString f) )
            || (" " `isPostfixOf'` (toString f) )
            || (" " `isPrefixOf'` (toString f) )
            )

fpLegalExtension f = null' f
    || (fpLegalName f
        && not ("." `isInfixOf'` (toString f) )
        && not ("/" `isInfixOf'` (toString f) )
        )

fpLegalFilename f = fpLegalName f
--    && not ("." `isInfixOf'` (toString f) )
    && not ("/" `isInfixOf'` (toString f) )

fpLegalPathname f = (f == "/")
    || ( fpLegalName f)

test_Legal1 = assertBool $ not (fpLegalName "home ")
test_Legal2 = assertBool $ not  (fpLegalName " home")
test_Legal3 = assertEqual False (fpLegalName "home.ext")
test_Legal4 = assertEqual False (fpLegalName "homen\ndir")
test_Legal5 = assertEqual False (fpLegalName "home\ttext ")

test_legalPath1 = assertBool
        (fpLegalPathname "home.s")
test_legalPath2 = assertBool
        (fpLegalPathname ".cabal")

newtype LegalName  = LegalName Text deriving (Show, Eq, Ord)
-- building block to construct rest
-- not containing tab cr lf, not starting or ending in blank

newtype LegalExtension  = LegalExtension Text deriving (Show, Eq, Ord)
-- a filename or an extenion, not including .
-- used as type of extension (in results)
-- not null, not contain . or /
newtype LegalFilename  = LegalFilename Text deriving (Show, Eq, Ord)
-- the filename proper, can contain , but not being null
newtype LegalPathname  = LegalPathname Text deriving (Show, Eq, Ord)
-- the representatino of all filenames (including directory names)
-- can contain . and / as correctly formed
-- can be empty (if path part) or empty extension

-- how to deal with adding an extension when one already present? eg. xx.txt.pdf
-- just add to filename

-- possibly need a single Root value extra?

instance Arbitrary LegalName where
    arbitrary = do
        s :: FPtype <- arbitrary
        let t = fpLegalName s
        if t then return (LegalName s) else arbitrary


unLegalName (LegalName s) = s
unLegalExtension (LegalExtension s) = s
unLegalFilename (LegalFilename s) = s
unLegalPathname (LegalPathname s) = s

---- | check a string for legality (internal)
--convert2legal  :: FPtype -> Maybe (LegalName)
--convert2legal s = if fpLegalName s then Just (LegalName s) else Nothing

class CV2legal f where
    cv2legal :: FPtype -> Maybe f
class CV2paths f where
    cv2path ::  f -> LegalPathname
    -- ^ a LegalName is also a pathname (pathname would include root)

instance CV2legal LegalExtension where
    cv2legal a = if fpLegalExtension a then Just . LegalExtension $ a else Nothing
instance CV2legal LegalFilename where
    cv2legal a = if fpLegalFilename a then Just . LegalFilename $ a else Nothing
instance CV2legal LegalPathname where
    cv2legal a = if fpLegalPathname a then Just . LegalPathname $ a else Nothing



instance CV2paths LegalFilename where
    cv2path = LegalPathname . unLegalFilename
instance CV2paths LegalName where
    cv2path = LegalPathname . unLegalName
instance CV2paths LegalPathname where
    cv2path = id

--class ShowFilenames f where
-- showFN :: f -> Text
--
--instance ShowFilenames (

-- | combine two legal into a filename
joinFilenameExtensionS :: LegalFilename -> LegalExtension -> LegalFilename
joinFilenameExtensionS (LegalFilename f) (LegalExtension e) =
                LegalFilename . s2t $ OS.addExtension (t2s f) (t2s e)

-- | split in filename and extension
-- extension without "."
splitFilenameExtensionS :: LegalFilename -> (LegalFilename, LegalExtension)
splitFilenameExtensionS (LegalFilename s) = (LegalFilename n, LegalExtension e2)
    where   (n,e) = splitExtensionOS s
            e2 = if null' e then e
                else fromJustNote "extension not starting with ." . stripPrefix' "." $ e


-- | get extension (no ".")
takeExtensionS :: LegalFilename ->  LegalExtension
takeExtensionS  = snd . splitFilenameExtensionS

-- | get filename
takeFilenameS :: LegalFilename -> LegalFilename
takeFilenameS = fst . splitFilenameExtensionS

replaceExtensionS :: LegalFilename ->  LegalExtension ->  LegalFilename
-- ^ replace the extension by a given string (not including .)
replaceExtensionS f e =  joinFilenameExtensionS fx e
    where fx =   takeFilenameS $ f

-- | make or test a filename or path for being hidden
class Hiddings f where
    mkHiddenS :: f -> f
    isHiddenS :: f -> Bool

instance Hiddings LegalFilename where
    mkHiddenS = LegalFilename . ("." <>) . unLegalFilename
    isHiddenS = isPrefixOf' "." . unLegalFilename

instance Hiddings FilePath where
    mkHiddenS =   ("." ++)
    isHiddenS = isPrefixOf' "."

instance Hiddings LegalPathname where
    mkHiddenS = LegalPathname . ("." <>) . unLegalPathname
    isHiddenS = isHiddenS . getFilenameS

--mkFilename :: String -> Maybe (LegalFilename)
--mkFilename s =
---- | make a filename from a string

---- | make a filename from filename and extension (can be empty)
---- no "." for extension
-- only for testing
_mkFilenameS :: FPtype -> FPtype -> Maybe LegalFilename
_mkFilenameS f e = if (fpLegalExtension e ) && fpLegalFilename f  then
         Just . LegalFilename $  addExtensionOS f e  else Nothing
         -- empty extension should be permitted

testExtensionS ::  FPtype -> LegalFilename   -> Bool
-- ^ has a given exension (without .) empty string for empty extension or "."
testExtensionS s f = (unLegalExtension . takeExtensionS $ f) == s


--(<.>) :: String -> String -> Maybe LegalFilename
--f <.> e =  -- fromJustNote ("file " ++ toString f ++ " addExtension "++ toString e ++ "probably not legal")
----             $   mkFilenameS f e

class FNstrings a b where
    (<.>) :: a -> b  -> a
    (</>), combine :: a  -> b -> a
    (</>) = combine

instance FNstrings LegalFilename LegalExtension where
        f <.> e = LegalFilename  $
                addExtensionOS (unLegalFilename f) (unLegalExtension e)


instance FNstrings LegalExtension LegalExtension where
    f <.> e = LegalExtension $
            addExtensionOS (unLegalExtension f) (unLegalExtension e)
--instance  (CV2paths b) => FNstrings LegalPathname b where
--    p </> f = joinPathS [p, f]

instance    FNstrings LegalPathname LegalFilename where
    -- add a filename to a pathname
    combine p  f = joinPathS [p, cv2path f]
instance    FNstrings LegalPathname LegalPathname where
    -- add a filename to a pathname
    combine p  f = joinPathS [p,  f]

--instance FNstrings String String where
--    p </> f = p <> "/" <> f
--    f <.> e = f <> "." <> e

instance FNstrings Text Text where
    p </> f = p <> "/" <> f
    f <.> e = f <> "." <> e

--(<.>) :: LegalFilename -> LegalExtension  ->  LegalFilename
--f <.> e = LegalFilename $ OS.addExtension (unLegalFilename f) (unLegalExtension e)

--(</>) :: (CV2paths f) => LegalPathname  -> f -> LegalPathname
--p </> f = joinPathS [p, cv2path f]

_mkFileSS :: FPtype -> LegalFilename
_mkFileSS = fromJustNote "mkFileSS" . cv2legal

_mkExtSS :: FPtype -> LegalExtension
_mkExtSS = fromJustNote "mkExtSS" . cv2legal

prop_extension2 ::  FPtype -> FPtype  -> Bool
    -- ^ test that split and add extension are inverse
prop_extension2 f s =
        (maybe True  (((LegalFilename f, LegalExtension s) ==) . splitFilenameExtensionS)
                 (_mkFilenameS f s)
                 )

prop_hidden :: LegalName -> Bool
prop_hidden = isHiddenS . mkHiddenS . LegalFilename . unLegalName

test_hidden1 = assertEqual False (isHiddenS  $ LegalFilename  "aaa.b")
test_hidden2 = assertEqual True (isHiddenS  $ LegalFilename  ".conf.xt")
test_hidden3 = assertBool (isHiddenS  $ LegalFilename  "abb/.conf.xt")
test_hidden4 = assertBool (isHiddenS  $ LegalFilename  "abb/.conf/")

test_ext2a = assertEqual (Just (LegalFilename "a.b")) (_mkFilenameS "a" "b")
test_ext2b = assertEqual (LegalFilename "a", LegalExtension "b")
                (splitFilenameExtensionS . LegalFilename $ "a.b")

------------- for directories

-- | split filepath with no trailing /
splitFilepathS :: LegalPathname  -> [LegalFilename]
splitFilepathS = map LegalFilename . splitDirectoriesOS . unLegalPathname

-- | join Paths (add os specific separator)
joinPathS :: (CV2paths f) => [f] ->  LegalPathname
    -- joint the path elements, empty gives root
joinPathS = LegalPathname . joinPathOS . map unLegalPathname . map cv2path


-- | get the path
getFilepathS :: LegalPathname -> LegalPathname
getFilepathS = joinPathS . reverse . tail . reverse . splitFilepathS

-- | get the filename
getFilenameS :: LegalPathname -> LegalFilename
getFilenameS f = case length fps of
            0         -> error "getFilename on empty input"
            1         -> LegalFilename . unLegalPathname $ f   -- no path, is legal
            otherwise -> headNote "should not occur werww2" . reverse $ fps
    where   fps =  splitFilepathS f

-- | combine path and filename
combineS  :: LegalPathname -> LegalFilename -> LegalPathname
    -- combine filepath filename, checked
combineS p1 p2 = joinPathS [LegalFilename . unLegalPathname $ p1, p2]
--LegalFilename $ OS.combine (unLegalFilename p1) (unLegalFilename p2)

--(</>) :: String -> String -> Maybe LegalFilename
--p </> f = fromJustNote ("make path "++ toString p ++ "and" ++ toString f ++ "probably not legal")
--            $  joinPathS (concatM
--    where
--        lp = convert2legal p
--        lf = convert2legal f


--fromJustNote ("make path "++ toString p ++ "and" ++ toString f ++ "probably not legal")
--            $  joinPathS (concatM
--    where
--        lp = convert2legal p
--        lf = convert2legal f


prop_joins_splits :: [LegalName] -> Bool
prop_joins_splits fs = (fsx ==) . splitFilepathS . joinPathS $ fsx
    where fsx =  map (LegalFilename . unLegalName) $ fs

prop_join_split :: [LegalName] -> Bool
    -- ^ test that split and combine are inverse
prop_join_split [] = True
prop_join_split fns = fnx == (pn `combineS` fn)
           where
                    fnx = joinPathS .  map (LegalFilename . unLegalName)  $ fns
                    pn = getFilepathS fnx
                    fn = getFilenameS fnx

test_getFilename1 = assertEqual (LegalFilename "a.x") (getFilenameS (LegalPathname "a.x"))
test_getFilename2 = assertEqual (LegalFilename "a.x") (getFilenameS (LegalPathname "b/a.x"))
test_getFilename3 = assertEqual (LegalFilename "a.x") (getFilenameS (LegalPathname "/c/b/a.x"))

test_getFilepath1 = assertEqual (LegalPathname "") (getFilepathS (LegalPathname "a.x"))
test_getFilepath2 = assertEqual (LegalPathname "b") (getFilepathS (LegalPathname "b/a.x"))
test_getFilepath3 = assertEqual (LegalPathname "/c/b") (getFilepathS (LegalPathname "/c/b/a.x"))

dirs2path :: FPtype -> Maybe LegalPathname
dirs2path fp1 = if (all isJust fp3)
        then Just . joinPathS . map fromJust $ fp3 else Nothing

        where
            fp2 =   OS.splitDirectories  . t2s $ fp1
            fp3 = map (cv2legal . s2t) fp2 ::[Maybe LegalPathname]



--convert2legalDir f = if fpLegalPathname f
--    then Just . LegalPathname $ f
--    else Nothing


test_convert2legal = assertEqual (Just . LegalPathname $ "/home/frank/.cabal/bin")
         (dirs2path "/home/frank/.cabal/bin")

test_splitDir = assertEqual (["/", "home", "frank", ".cabal", "bin"])
         (OS.splitDirectories "/home/frank/.cabal/bin/")


test_make_incorrectFilename =  do
    f <- runErr $ makeLegalFilename  "name/and.more"
    assertEqual (Left "") f



--    -- | test if file is hidden (prefix .)
--    -- could require OS specific ops
--    isHidden2 :: f -> Bool
----    isHidden2  = isHidden2 . getFilepath
--
    -- make relattive if ever used--
--
---- | operations on files and directories
---- the doesXX do not produce any exceptiosn
-- is polymorph either in LegalFilename or in RawFilePath (i.e. bytestring )



----------old
--- | operatiosn to combine filepath with strings or similar
--class (FilePathes0 f
--    ) => FilePathes2 f  where
--    {-# MINIMAL addExtension3, splitExtension  #-}
--
----    makeFP :: FPstring f -> f
----    -- drop the following operations? (especially the later ones)
--
--    addExtension3 :: f ->  f ->  f
--    -- ^ add the string as extension
--    -- filename must not be null
--    -- extension must not contain \ nor . nor tab nor newline
--    -- what else is not desirable?
----    addExtension3 f e =
----    addExtension2 f e = if fpLegalString e && fpLegalString f  then
----         Just $ addExtension0 f e else Nothing
----
----    addExtension0 :: f ->  f -> f
----    -- the underlying operation of the OS
--
--    splitExtension :: f -> (f,  f)
--    -- ^ split the filename and the extension
--
--    takeExtension :: f ->  f
--    --        , without leading "." -- empty for empty extension
----                not like Hackage Posix!
--    takeExtension  = snd . splitExtension
--
--    replaceExtension2 :: f ->  f -> Maybe f
--    -- ^ replace the extension by a given string (not including .)
--    replaceExtension2 f e =  (\fx -> addExtension2 fx e) (removeExtension f)
--
--    testExtension ::  f -> f -> Bool
--    -- ^ has a given exension (without .) empty string for empty extension or "."
--    testExtension s f = (takeExtension f) == s
--
--    removeExtension :: f -> f
--    removeExtension = fst . splitExtension
