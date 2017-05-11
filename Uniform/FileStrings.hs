------------------------------------------------------------------------------
--
-- Module      :  FileIO.Strings
--
-- | the instance for strings (was in 0.1.1)
-- filenames are Path
-- should only export the instances
-- removed -- file content can be lazy bytestring
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS -w #-}

module Uniform.FileStrings (htf_thisModulesTests
            , module Uniform.Filenames   -- exports Path
            , module Uniform.FileIOalgebra


--    , readFileT, writeFileT

            ) where

-- using uniform
--import Uniform.Error
import           Uniform.FileIOalgebra
--import           Uniform.FilenamesAlgebra
import           Uniform.Filenames -- (makeLegalPath)
import           Uniform.FileStatus
import           Uniform.Strings hiding ((<.>), (</>))

import           Test.Framework
import           Test.Invariant

import qualified Path as P
import qualified Path.IO as P

-- what is further required?
import qualified System.IO as SIO
import System.Posix (FileMode)


import qualified Data.ByteString       as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy  as L
import           Data.Digest.Pure.MD5  (md5)
--import Data.Hash.MD5 (md5s)
import qualified Data.Text.IO          as T
import Data.Maybe (catMaybes)

import qualified System.Directory      as S
import qualified System.FilePath       as OS
--       (addExtension, makeRelative, FilePath, combine, splitPath,
--        takeDirectory, replaceExtension, takeExtension)
import qualified System.Posix          as P
-- for fileAccess
import           Control.Arrow         (second)
import           Data.List             (isPrefixOf)
import Data.Either (isLeft)
import Control.Exception (catch, SomeException)
import Control.DeepSeq (($!!), force)

closeFile2 :: Handle -> ErrIO ()
-- close a handle, does not need a filepath
closeFile2 handle = callIO $ SIO.hClose handle

instance DirOps FilePath where
    doesDirExist = callIO . S.doesDirectoryExist
    renameDir old new = do
        putIOwords ["renamed start"]
        testSource <-  doesDirExist old
        testTarget <-  doesDirExist new
        if testTarget then throwErrorT
--                signalf  TargetExist
                        [showT new]
          else if not testSource then throwErrorT
--                    signalf  SourceExist
                    [showT old]
                else do
                     callIO $ putStrLn "renamed"
                     r <- callIO $ S.renameDirectory (old) (new)
                     return $ unwordsT
                        [ "renamed dir from ", showT old
                            , " to " , showT  new]

instance FileOps FilePath  where
    doesFileExist   = callIO . S.doesFileExist


--    createDir fp = do
--        t <- doesFileOrDirExist fp
--        if not t then  callIO $ S.createDirectory   fp
--            else throwErrorT
--                ["File or Dir exists", showT fp]


--    createDirIfMissing = callIO . S.createDirectoryIfMissing True
--    copyFile old new = do
--        t <- doesFileExist old
--        t2 <- doesFileExist new
--        if t && (not t2) then do
--            let dir = takeDir new
--            direxist <- doesDirExist   dir
--            unless direxist $ do
--                createDirIfMissing  dir
--            callIO $ S.copyFile (old) ( new)
--
--                else if t then  throwErrorT
--                    ["copyFile source not exist", showT old]
--                    -- signalf   SourceNotExist
--                            else throwErrorT  [
--                            "copyFile target exist", showT new]
--                            --   signalf  TargetExist
--    renameFile old new = do
--        t <- doesFileExist old
--        t2 <- doesFileExist new
--        if t && (not t2)
--            then do
--                let dir = takeDir new
--                direxist <- doesDirExist  dir
--                unless direxist $ do
--                    createDirIfMissing  dir
--                callIO $ S.renameFile ( old) ( new)
--
--            else if not t
--                then  throwErrorT
----                signalf   SourceNotExist
--                            ["renameFile source not exist", showT old]
--                else throwErrorT
----                            signalf  TargetExist
--                                 ["renameFile target exist", showT new]


    getMD5 fn = do
--            putIOwords ["getMD5 in FileStrings.hs", showT fn]
            status <- getSymbolicLinkStatus fn
--            let status = fromJustNote "getMD5 xx33" $ mstatus
            let regular = isRegularFile status
            readable <- getFileAccess fn (True, False, False)
--            putIOwords ["getMD5 in FileStrings.hs before if"]
            if regular && readable then callIO $ do
    --                    putIOwords ["getMD5 in FileStrings.hs file 1"]
                        filedata :: L.ByteString <- L.readFile fn  -- fails for some special files eg. /proc
    --                    putIOwords ["getMD5 in FileStrings.hs file 2"]
                        let res = showT $ md5  filedata
    --                    putIOwords ["getMD5 in FileStrings.hs file 3"]
                        return $!! (Just res)
--                   `catch` \(e::SomeException) -> do
--                            putIOwords ["caught with catch in getmd5 ", showT e]
--                            return Nothing

                else throwErrorT $ ["getMD5 error file not readable" , showT fn]

        `catchError` \e -> do
            putIOwords ["getMD5 in FileStrings.hs", showT fn, showT e]  -- reached
            throwErrorT $ ["getMD5 error for" , showT fn]

--    getDirCont fn  = do
----        putIOwords ["getDirCont", show f]
--        testDir <- doesDirExist fn
--        readExec <- getFileAccess fn (True, False, True)
--        if testDir && readExec then
--            do
--               r <- callIO . S.listDirectory $ fn
--               let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
--               let r3 = map (fn </>) r2
----               putIOwords ["FileStrigs - getDirCont", showT fn, "files: ", unwordsT . map showT $ r]
--               return r3
--          else
--                throwErrorT
--                    ["getDirCont not exist or not readable"
--                    , showT fn, showT testDir, showT readExec]

--    getDirContentNonHidden fp = do
----        putIOwords ["getDirContentNonHidden", unL fp]
--        r <- getDirCont fp
--        let r2 = filter (not . isHidden) r
----        r2 <- callIO $ S.listDirectory (unL fp)
----          would be possible but filter here is simpler
----        let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
----        let r2 = filter (not . isPrefixOf "." ) r
----        putIOwords ["nonhidden files", show r2]
--        return r2

    deleteFile f = do
         putIOwords ["delete file ", showT f]
         callIO . S.removeFile   $ f

    deleteDirRecursive f =
        do
            t <- doesDirExist f
            when t $ do
                callIO . S.removeDirectoryRecursive  $  f

                putIOwords ["deleted", showT f]
            return ()


    getAppConfigDirectory = error "not implemented" -- do
--        let prefsfileDir = ".config"
--        homeDir <- callIO $ S.getHomeDirectory
--        return  (homeDir </> prefsfileDir)

--    getSymbolicLinkStatus :: fp ->   ErrIO (Maybe P.FileStatus)
    getSymbolicLinkStatus fp = do
        --    putIOwords ["fileio getSymbolicLinkStatus", fp]
            st <- callIO $ P.getSymbolicLinkStatus fp
        --    putIOwords ["fileio getSymbolicLinkStatus done", fp]
            return  $ st
--          `catchError` (\s -> do
--                    putIOwords ["fileio getSymbolicLinkStatus not found", showT fp]
--                    return Nothing)

    -- ^ get status if exist (else Nothing)
    --   is the status of the link, does not follow the link

    getFileAccess fp (r,w,e) = callIO $
         P.fileAccess fp r w  e
--              `catchError` \e -> do
--                     putIOwords ["getFileAccess error", showT fp, s2t $ show e]
--                     return False

        -- fails if broken symbolic link?

    checkSymbolicLink fp =  callIO $ S.pathIsSymbolicLink ( fp)


    openFile fp mode = callIO $ SIO.openFile fp mode
--    closeFile _ handle = callIO $ SIO.hClose handle

--unL = t2s . filepath2text lpX
--mkL = mkFilepath lpX . s2t

instance DirOps (P.Path ar Dir)  where
    doesDirExist =  P.doesDirExist
    createDir  = P.createDir
--        do
--        t <- doesFileOrDirExist fp
--        if not t then  callIO $ S.createDirectory . unL $ fp
--            else throwErrorT
--                ["File or Dir exists", showT fp]


    createDirIfMissing = P.createDirIfMissing True

instance FileOps (P.Path ar File)  where
    doesFileExist   =  doesFileExist

    getMD5 fp = getMD5 (unL fp)
-- use listDir which separats result in dir and file list and does not include . and ..
--    getDirCont fn  = do
--          r1 <- getDirCont  . unL $ fn
--          let r2 = map mkL r1
--          return r2
--
--    getDirContentNonHidden fp = do
----        putIOwords ["getDirContentNonHidden", unL fp]
--        r <- getDirCont . unL $ fp
--        let r2 = filter (not . isHidden) r
----        r2 <- callIO $ S.listDirectory (unL fp)
----          would be possible but filter here is simpler
----        let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
----        let r2 = filter (not . isPrefixOf "." ) r
----        putIOwords ["nonhidden files", show r2]
--        let r3 = (map (makeLegalPath . s2t) r2)
--        return (catMaybes r3)

    openFile fp mode =  openFile (unL fp) mode
--    closeFile fp handle = closeFile (unL fp) handle

    checkSymbolicLink fp =   callIO $ S.pathIsSymbolicLink (unL fp)

    getFileAccess fp (r,w,e) =
        do
--            putIOwords ["getFileAccess", show fp]
            callIO $
                (do

                    P.fileAccess (unL fp) r w  e
              `catchError` \e -> do
                     putIOwords ["getFileAccess error", showT fp, s2t $ show e]
                     return False )


unL = P.toFilePath


readFileT :: Path ar File  -> ErrIO Text
readFileT fp = callIO .  T.readFile . unL $ fp

writeFileT :: Path ar File  -> Text -> ErrIO ()
writeFileT  fp st = callIO $  T.writeFile (unL fp) st

instance   FileOps2 (Path ar File) String where

    readFile2 fp = callIO $ readFile   (unL fp)
    -- a strict read (does cloes?)

    writeFile2  fp st = callIO $ writeFile   (unL  fp) st


instance   FileOps2 (Path ar File) Text where

    readFile2 fp = readFile2  (unL fp)
    -- a strict read (does cloes?)

    writeFile2  fp st = writeFile2  (unL  fp) st

instance FileOps2 FilePath Text where

    readFile2 fp = callIO $  T.readFile fp

    writeFile2  fp st = callIO $  T.writeFile fp st

instance FileOps2 FilePath L.ByteString where

    readFile2 fp = callIO $  L.readFile fp

    writeFile2  fp st = callIO $  L.writeFile fp st

instance FileOps2 (Path ar File) L.ByteString where

    readFile2 fp = callIO $  L.readFile . unL $ fp

    writeFile2  fp st = callIO $  L.writeFile (unL fp) st

--------------------------test with path

notexisting = makeRelFile "xxxxabcd"

test_catch_error2p = do
    res <- runErr $ do
                            f :: Text <-  readFile2 notexisting
                            return False
                `catchError `
                            \(e::Text) ->  return True
    assertEqual (Right True) res

test_call_IOp = do
    res <- runErr $ do
        f :: String <-   readFile2 notexisting  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "xxxxabcd: openFile: does not exist (No such file or directory)") res

test_call_IO_Lp = do
    res <- runErr $ do
        f :: L.ByteString <-  readFile2 notexisting  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "xxxxabcd: openBinaryFile: does not exist (No such file or directory)") res

--test_call_IO_Corrupt= do
--    res <- runErr $ callIO $ do
--        f :: L.ByteString <-    L.readFile corruptJPG  -- not existing fileAccess
--        putIOwords ["call_IO_Corrupt", showT . L.length $ f]  -- just to enforce strictness
--        return False   -- expect that read fials
--    assertEqual ( Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: \
--        \hGetBufSome: hardware fault (Input/output error)") res

procFile = makeAbsFile "/proc/1/task/1/maps"
test_call_procp = do
    res <- runErr $ do
        f :: Text   <-    readFile2  procFile -- not allowed fileAccess
        return False   -- expect that read fials
    assertBool (isLeft res)
--    assertEqual ( Left "/proc/1/task/1/maps: openBinaryFile: permission denied (Permission denied)") res

test_md5_nonReadablep = do
    res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5 procFile
    putIOwords ["test_md5_nonReadable res", showT res]
    assertEqual (Left "getMD5 error for \"/proc/1/task/1/maps\"") res


------------old test with filepath


test_catch_error2 = do
    res <- runErr $ do
                            f :: Text <-  readFile2 ("xxxabcd" :: FilePath)
                            return False
                `catchError `
                            \(e::Text) ->  return True
    assertEqual (Right True) res

test_call_IO = do
    res <- runErr $ do
        f :: String <-   callIO $ readFile "xxxabcd17"  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "xxxabcd17: openFile: does not exist (No such file or directory)") res

test_call_IO_L = do
    res <- runErr $ do
        f :: L.ByteString <-   callIO $ L.readFile "xxxabcd17"  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "xxxabcd17: openBinaryFile: does not exist (No such file or directory)") res

--test_call_IO_Corrupt= do
--    res <- runErr $ callIO $ do
--        f :: L.ByteString <-    L.readFile corruptJPG  -- not existing fileAccess
--        putIOwords ["call_IO_Corrupt", showT . L.length $ f]  -- just to enforce strictness
--        return False   -- expect that read fials
--    assertEqual ( Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: \
--        \hGetBufSome: hardware fault (Input/output error)") res

test_call_proc = do
    res <- runErr $ do
        f   <-   callIO $ L.readFile "/proc/1/task/1/maps"  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "/proc/1/task/1/maps: openBinaryFile: permission denied (Permission denied)") res

test_md5_nonReadable = do
    res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5 ("/proc/1/task/1/maps" ::FilePath)
    putIOwords ["test_md5_nonReadable res", showT res]
    assertEqual (Left "getMD5 error for \"/proc/1/task/1/maps\"") res

corruptJPG = "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG" ::FilePath

-- readable on santafe but not oporto
--test_md5_nonReadable2 :: IO ()
--test_md5_nonReadable2 = do
--        res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5  corruptJPG
--        putIOwords ["test_md5_nonReadable corrupt jpg file", showT res]
--        -- does not catch the error?
--        assertEqual (Left "getMD5 error for \"/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG\"") res
----   `catch` \(e::SomeException) -> do
----                putIOwords ["caught with catch in test_md5_nonReadable2 ", showT e]
----                return ()

-- not corrupt on santa fe, but on oporto
--test_md5_catch :: IO ()
--test_md5_catch = do
--        res3 :: ErrOrVal ByteString <- runErr $ callIO $ do
--                        res1 :: L.ByteString <-    L.readFile  corruptJPG
--                        let res2 = L.toStrict  res1
--                        return $!! res2
--        assertEqual (Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: hGetBufSome: hardware fault (Input/output error)") res3
----   `catch` \(e::SomeException) -> do
----                putIOwords ["caught with catch in test_md5_catch ", showT e]
----                return ()





