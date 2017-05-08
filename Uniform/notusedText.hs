------------------------------------------------------------------------------
--
-- Module      :  FileIO.Text
--
-- | the instance for text
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , TypeFamilies
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}


module FileIO.Text (
            FilePath
            , module FileIO.FileIO
    , htf_thisModulesTests

            ) where
-- using uniform
import Uniform.Error
import Data.Strings
import FileIO.FileIO

import FileIO.Strings () -- only instances imported
import Test.Framework
import Test.Invariant

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Text as T
import  Data.Text (Text)
import qualified Data.Text.IO as T
import qualified System.Directory as S
import qualified System.FilePath as OS
--       (addExtension, makeRelative, FilePath, combine, splitPath,
--        takeDirectory, replaceExtension, takeExtension)
import Data.List (isPrefixOf)
--import Control.Monad (unless)
import Control.Arrow (first, second)


instance () => FilePathes0 Text  where
instance () => FilePathes2 Text  where

--    takeExtension1 = OS.takeExtension
--    takeExtension2 = tailSafe . takeExtension1
--    splitExtension = OS.splitExtension
    splitExtension = first s2t . second s2t . splitExtension . t2s
--    replaceExtension = OS.replaceExtension
    addExtension0 f e = s2t $ addExtension0 (t2s f) (t2s e)

----prop_FP_fp :: FilePath -> String -> Bool
----prop_FP_fp = prop_extension1
prop_FP_fp2 :: Text -> Text -> Bool
prop_FP_fp2 = prop_extension2
--
--test_fp1 = assertEqual "a." (addExtension0 "a"  ".")
--test_fp3 = assertEqual "a.a" (addExtension0 "a"  ".a")
--        -- extension must not be . nor start with .
--test_fp2 = assertEqual "a./b" (addExtension0 "a"   "/b")
--test_fp4 = assertEqual "a.a" (addExtension0 "a"   "a")
--test_fp5 = assertEqual "a" (addExtension0 "a"  "")
--    -- empty extension does not require dot
--test_fp5a = assertEqual "a.a." (addExtension0 "a"  "a.")
--test_fp5b = assertEqual "a./" (addExtension0 "a"   "/")
--test_fp6 = assertEqual ("a","a")
--                 (splitExtension2 . addExtension0 "a" $ "a")
--test_x :: IO ()
--test_x = assertEqual True ("/" `isPrefixOf'` ( "/"))
--test_x2 = assertEqual True ("." `isInfixOf'` ( "a."))


instance FilePathes Text where
    combine0  a b =   s2t $ OS.combine (t2s a) (t2s b)
    joinPath0 as = s2t $ OS.joinPath . map t2s $ as

    makeRelative a b = s2t $ OS.makeRelative (t2s a) (t2s b)
    isHidden2 a   =   isPrefixOf "."  (t2s a)
    splitFileName0 a = first s2t . second s2t  $  OS.splitFileName (t2s a)
--    splitDirFile a =  map s2t $ OS.splitPath (t2s a)
    splitDirFiles2 a = map s2t $ OS.splitDirectories (t2s a)

    getFilepath = t2s
--    takeLastDirName  "" = ""
--    takeLastDirName   = s2t . takeLastDirName . t2s

---- how to force text types here?
--test_p1 = assertEqual ("a/b" ) ("a" </> "b" )
--test_s1 = assertEqual ("a"::Text, "b"::Text) (splitFileName2 ("a/b"::Text))
--
--test_ps1 = assertEqual (Just "a/b/c") (joinPath2 ["a", "b", "c"])
--test_ps2 = assertEqual ["a", "b", "c"] (splitDirFiles2 "a/b/c")

prop_FP_js :: Text -> Text -> Bool
prop_FP_js = prop_join_split

prop_FP_jss :: [Text] -> Bool
prop_FP_jss = prop_joins_splits


--instance () => FilePathes2 Text  where
--    type ExtensionF Text = Text
----    addTo f s = f </>  s
----    addExt f s = f <.> s
----    makeFP = id
--
--    takeExtension1 = s2t . OS.takeExtension . t2s
----    takeExtension2 = s2t . takeExtension2 . t2s
--    splitExtension = cross (s2t, s2t) . OS.splitExtension . t2s
--    splitExtension2 = second (fromJustNote "spliExtension2 not . 354dsd" . stripPrefix' (toText ".")) . splitExtension
----    replaceExtension f s  = s2t $  OS.replaceExtension (t2s f) (t2s s)
--    addExtension0 f s  =  s2t $ OS.addExtension (t2s f) (t2s s)

instance FileOps T.Text where
-- would need to parameters? returns usually a string - fixed in class!
    doesDirectoryExist = doesDirectoryExist . t2s
    doesFileExist = doesFileExist . t2s
    doesFileOrDirExist = doesFileOrDirExist . t2s
    createDir = createDir . t2s
    createDirIfMissing = createDirIfMissing . t2s
    copyFile old new = copyFile (t2s old) (t2s new)
    renameFile old new = renameFile (t2s old) (t2s new)
    deleteFile = deleteFile . t2s
    deleteDirRecursive = deleteDirRecursive . t2s
    getDirCont =   getDirCont . t2s         -- string return
    getDirContentNonHidden =  getDirContentNonHidden . t2s  -- string
    getMD5 = getMD5 . t2s
--    getAppConfigDirectory = fmap s2t . getAppConfigDirectory
    renameDirectory = error "not implemented renameDirectory"
    getAppConfigDirectory = error "not implemented  getAppConfigDirectory"




instance FileOps2 FilePath T.Text where

    readFile2 fp = callIO $  T.readFile fp

    writeFile2  fp st = callIO $  T.writeFile fp st

instance FileOps2 T.Text T.Text where

    readFile2 fp = callIO $  T.readFile . t2s $ fp

    writeFile2  fp st = callIO $  T.writeFile (t2s fp) st



