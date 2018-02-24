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
--            , module Path
            , module Path.IO
            , SIO.IOMode (..)  -- from System.IO
            , closeFile2
            , listDir'
            , readFile5
            , TIO.hGetLine, TIO.hPutStr  -- for other implementations of FileHandle

--    , readFileT, writeFileT

            ) where

-- using uniform
--import Uniform.Error
import           Uniform.FileIOalgebra
--import           Uniform.FilenamesAlgebra
import           Uniform.Filenames
import           Uniform.FileStatus
-- import           Uniform.Strings hiding ((<.>), (</>))

import           Test.Framework
import           Test.Invariant

import           Path                   as P
import           Path.IO                as P

import           Path
import           Path.IO

-- what is further required?
import qualified System.IO              as SIO
import           System.Posix           (FileMode)


import qualified Data.ByteString        as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy   as L
import           Data.Digest.Pure.MD5   (md5)
--import Data.Hash.MD5 (md5s)
import           Data.Maybe             (catMaybes)
import qualified Data.Text.IO           as T (readFile, writeFile, appendFile)
import qualified Data.Text.IO           as TIO (hGetLine, hPutStr)

import qualified System.Directory       as D
--import qualified System.Directory      as D
import qualified System.FilePath        as OS
--       (addExtension, makeRelative, FilePath, combine, splitPath,
--        takeDirectory, replaceExtension, takeExtension)
import qualified System.Posix           as P
-- for fileAccess
import           Control.Arrow          (second)
import           Control.DeepSeq        (force, ($!!))
import           Control.Exception      (SomeException, catch)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Either            (isLeft)
import           Data.List              (isPrefixOf)

closeFile2 :: SIO.Handle -> ErrIO ()
-- close a handle, does not need a filepath
closeFile2 handle = callIO $ SIO.hClose handle

instance FileHandles String where
    write2handle h c = callIO $ SIO.hPutStr h c
    readLine4handle h = callIO $ SIO.hGetLine h

instance FileHandles L.ByteString where
    write2handle h c = callIO $ L.hPutStr h c
    readLine4handle h = error "readLine4handle not implemented for lazy bytestring in FileStrings"

instance FileHandles Text where
    write2handle h c = callIO $ TIO.hPutStr h c
    readLine4handle h = callIO $ TIO.hGetLine h

instance FileHandles [Text] where
    write2handle h c = callIO $ TIO.hPutStr h (unlines' c)
    readLine4handle h = do
            res <-  callIO $ TIO.hGetLine h
            return . lines' $ res

--for testing:
readFile5 :: Path ar File -> IO Text
readFile5 = fmap s2t .readFile . toFilePath


listDir' :: (MonadIO m, MonadThrow m)
  => Path b Dir          -- ^ Directory to list
  -> m ([Path Abs Dir], [Path Abs File]) -- ^ Sub-directories and files
listDir' = P.listDir
-- would require a class and an implementation for FilePath

instance FileSystemOps FilePath where
    checkSymbolicLink fp =  callIO $ D.pathIsSymbolicLink ( fp)
    getPermissions' = callIO . D.getPermissions

instance DirOps FilePath where
    doesDirExist' = callIO . D.doesDirectoryExist
    createDirIfMissing' = callIO . D.createDirectoryIfMissing True
    -- creates recusrively up

    createDir' = callIO . D.createDirectory
    renameDir' old new = do
        putIOwords ["renamed start"]
        testSource <-  doesDirExist' old
        testTarget <-  doesDirExist' new
        if testTarget then throwErrorT
--                signalf  TargetExist
                        [showT new]
          else if not testSource then throwErrorT
--                    signalf  SourceExist
                    [showT old]
                else do
                     callIO $ putStrLn "renamed"
                     r <- callIO $ D.renameDirectory (old) (new)
                     return $ unwordsT
                        [ "renamed dir from ", showT old
                            , " to " , showT  new]

instance FileOps FilePath  where
    doesFileExist'   = callIO . D.doesFileExist
--    getPermissions' = callIO . D.getPermissions

--    createDir fp = do
--        t <- doesFileOrDirExist fp
--        if not t then  callIO $ D.createDirectory   fp
--            else throwErrorT
--                ["File or Dir exists", showT fp]


--    createDirIfMissing = callIO . D.createDirectoryIfMissing True
--    copyFile old new = do
--        t <- doesFileExist old
--        t2 <- doesFileExist new
--        if t && (not t2) then do
--            let dir = takeDir new
--            direxist <- doesDirExist   dir
--            unless direxist $ do
--                createDirIfMissing  dir
--            callIO $ D.copyFile (old) ( new)
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
--                callIO $ D.renameFile ( old) ( new)
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
--               r <- callIO . D.listDirectory $ fn
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
----        r2 <- callIO $ D.listDirectory (unL fp)
----          would be possible but filter here is simpler
----        let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
----        let r2 = filter (not . isPrefixOf "." ) r
----        putIOwords ["nonhidden files", show r2]
--        return r2

    deleteFile f = do
         putIOwords ["delete file ", showT f]
         callIO . D.removeFile   $ f

    deleteDirRecursive f =
        do
            t <- doesDirExist' f
            when t $ do
                callIO . D.removeDirectoryRecursive  $  f

                putIOwords ["deleted", showT f]
            return ()


    getAppConfigDirectory = error "not implemented" -- do
--        let prefsfileDir = ".config"
--        homeDir <- callIO $ D.getHomeDirectory
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

--    isFileAbeforeB fpa fpb = do
--        statA :: P.FileStatus <- getFileStatus' fpa
--        statB <- getFileStatus' fpb
--        let
--            timea = getModificationTimeFromStatus statA
--            timeb = getModificationTimeFromStatus statB
--        return $ timea < timeb

    getFileModificationTime  fp = do
        stat :: P.FileStatus <- getFileStatus' fp
        let
            time = getModificationTimeFromStatus stat
        return time



    openFile2handle fp mode = do
            callIO $ SIO.openFile fp mode
--        `catchError` \e -> do
        -- most likely the dir does not exist.
        -- try to create the file?
--    closeFile _ handle = callIO $ SIO.hClose handle

--unL = t2s . filepath2text lpX
--mkL = mkFilepath lpX . s2t

instance FileSystemOps (Path ar df) where
    getPermissions' = P.getPermissions
    checkSymbolicLink  fp =   callIO $ D.pathIsSymbolicLink (unL fp)

instance DirOps (P.Path ar Dir)  where
    doesDirExist' =  P.doesDirExist
--    getDirPermissions = P.getPermissions
    createDir'  = P.createDir
--        do
--        t <- doesFileOrDirExist fp
--        if not t then  callIO $ D.createDirectory . unL $ fp
--            else throwErrorT
--                ["File or Dir exists", showT fp]


    createDirIfMissing' = P.createDirIfMissing True

instance FileOps (P.Path ar File)  where
    doesFileExist'   =  P.doesFileExist
--    getPermissions' = P.getPermissions

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
----        r2 <- callIO $ D.listDirectory (unL fp)
----          would be possible but filter here is simpler
----        let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
----        let r2 = filter (not . isPrefixOf "." ) r
----        putIOwords ["nonhidden files", show r2]
--        let r3 = (map (makeLegalPath . s2t) r2)
--        return (catMaybes r3)

--    isFileAbeforeB fpa fpb = do
--        statA :: P.FileStatus <- getFileStatus fpa
--        statB <- getFileStatus fpb
--        let
--            timea = getModificationTimeFromStatus statA
--            timeb = getModificationTimeFromStatus statB
--        return $ timea < timeb
    getFileModificationTime  fp =  getFileModificationTime (unL fp)


    openFile2handle fp mode =  openFile2handle (unL fp) mode
--    closeFile fp handle = closeFile (unL fp) handle

--    checkSymbolicLink fp =   callIO $ D.pathIsSymbolicLink (unL fp)

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
-- attention - does not create file if not existing

instance   FileOps2 (Path ar File) String where

    readFile2 fp = callIO $ readFile   (unL fp)
    -- a strict read (does cloes?)
    writeFile2  fp st = callIO $ writeFile   (unL  fp) st
    appendFile2  fp st = callIO  $   appendFile  (unL fp) st


instance   FileOps2 (Path ar File) Text where

    readFile2 fp = readFile2 (unL fp)
    -- a strict read (does cloes?)
    writeFile2  fp st = writeFile2 (unL fp) st
    appendFile2  fp st = appendFile2  (unL fp) st

    writeFileOrCreate2 filepath st = do
        let dir = getParentDir filepath
--        let (dir, fn, ext) = splitFilepath filepath
        --        writeFileCreateDir dir fn st
        --                  writeFileCreateDir dirpath filename st = do
--        let fp = dirpath `combine` filename
--        d <- doesDirExist' dir
--        --        f <- doesFileExist fp --
--        unless d $
        createDirIfMissing' dir
        when False $ putIOwords ["writeFileOrCreate2 dir created", showT dir]
        t <- doesDirExist' dir
        when False $ putIOwords ["writeFileOrCreate2 dir test", showT t]
        writeFile2 filepath st
        putIOwords ["writeFileOrCreate2 file written", showT filepath]

instance FileOps2 FilePath Text where

    readFile2 fp = callIO $  T.readFile fp
    writeFile2  fp st = callIO $  T.writeFile fp st
    appendFile2  fp st = callIO  $  T.appendFile  fp st

instance FileOps2 FilePath L.ByteString where

    readFile2 fp = callIO $  L.readFile fp
    writeFile2  fp st = callIO $  L.writeFile fp st
    appendFile2  fp st = callIO  $  L.appendFile  fp st

instance FileOps2 (Path ar File) L.ByteString where

    readFile2 fp = callIO $  L.readFile . unL $ fp
    writeFile2  fp st = callIO $  L.writeFile (unL fp) st
    appendFile2  fp st = callIO  $  L.appendFile  (unL fp) st

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

test_createNewDirFile = do
    let fn = makeAbsFile "/home/frank/test/1.test"
    r <- runErr $ writeFileOrCreate2 fn ("testtext"::Text)
    assertEqual (Right () ) r
--
--test_md5_nonReadablep = do
--    res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5 procFile
--    putIOwords ["test_md5_nonReadable res", showT res]
--    assertEqual (Left "getMD5 error for \"/proc/1/task/1/maps\"") res
--
--
--test_before = do
--    let fna = makeAbsFile "/home/frank/test/a.test"
--    let fnb = makeAbsFile "/home/frank/test/b.test"
--    r <- runErr $ isFileAbeforeB fna fnb
--    assertEqual (Right True ) r


--------------old test with filepath
--
--
--test_catch_error2 = do
--    res <- runErr $ do
--                            f :: Text <-  readFile2 ("xxxabcd" :: FilePath)
--                            return False
--                `catchError `
--                            \(e::Text) ->  return True
--    assertEqual (Right True) res
--
--test_call_IO = do
--    res <- runErr $ do
--        f :: String <-   callIO $ readFile "xxxabcd17"  -- not existing fileAccess
--        return False   -- expect that read fials
--    assertEqual ( Left "xxxabcd17: openFile: does not exist (No such file or directory)") res
--
--test_call_IO_L = do
--    res <- runErr $ do
--        f :: L.ByteString <-   callIO $ L.readFile "xxxabcd17"  -- not existing fileAccess
--        return False   -- expect that read fials
--    assertEqual ( Left "xxxabcd17: openBinaryFile: does not exist (No such file or directory)") res
--
----test_call_IO_Corrupt= do
----    res <- runErr $ callIO $ do
----        f :: L.ByteString <-    L.readFile corruptJPG  -- not existing fileAccess
----        putIOwords ["call_IO_Corrupt", showT . L.length $ f]  -- just to enforce strictness
----        return False   -- expect that read fials
----    assertEqual ( Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: \
----        \hGetBufSome: hardware fault (Input/output error)") res
--
--test_call_proc = do
--    res <- runErr $ do
--        f   <-   callIO $ L.readFile "/proc/1/task/1/maps"  -- not existing fileAccess
--        return False   -- expect that read fials
--    assertEqual ( Left "/proc/1/task/1/maps: openBinaryFile: permission denied (Permission denied)") res
--
--test_md5_nonReadable = do
--    res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5 ("/proc/1/task/1/maps" ::FilePath)
--    putIOwords ["test_md5_nonReadable res", showT res]
--    assertEqual (Left "getMD5 error for \"/proc/1/task/1/maps\"") res
--
--corruptJPG = "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG" ::FilePath
--
----test_fail = assertEqual "Fail intentionally just to insure that tests are run"(""::Text)
---- readable on santafe but not oporto
----test_md5_nonReadable2 :: IO ()
----test_md5_nonReadable2 = do
----        res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5  corruptJPG
----        putIOwords ["test_md5_nonReadable corrupt jpg file", showT res]
----        -- does not catch the error?
----        assertEqual (Left "getMD5 error for \"/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG\"") res
------   `catch` \(e::SomeException) -> do
------                putIOwords ["caught with catch in test_md5_nonReadable2 ", showT e]
------                return ()
--
---- not corrupt on santa fe, but on oporto
----test_md5_catch :: IO ()
----test_md5_catch = do
----        res3 :: ErrOrVal ByteString <- runErr $ callIO $ do
----                        res1 :: L.ByteString <-    L.readFile  corruptJPG
----                        let res2 = L.toStrict  res1
----                        return $!! res2
----        assertEqual (Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: hGetBufSome: hardware fault (Input/output error)") res3
------   `catch` \(e::SomeException) -> do
------                putIOwords ["caught with catch in test_md5_catch ", showT e]
------                return ()
--
--test_symlink :: IO ()
--test_symlink = do
--    let t =   makeAbsFile "/bin/X11/X11"
--    isSymlink1 <- D.pathIsSymbolicLink   (OS.dropTrailingPathSeparator $ toFilePath t)
--    isSymlink2 <- D.pathIsSymbolicLink "/bin/X11/X11" -- (toFilePath t)
--    isSymlink3 <- D.pathIsSymbolicLink "/bin/X11/X11/" -- (toFilePath t)
--    assertEqual  (True, True, False) (isSymlink1, isSymlink2, isSymlink3)
--
--