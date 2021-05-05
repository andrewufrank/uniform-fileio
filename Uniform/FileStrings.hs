---------------------------------------------------------------------
-- Module      :  FileIO.Strings
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FileStrings
  ( module Uniform.Filenames,
    module Uniform.FileIOalgebra,
    SIO.IOMode (..),
    closeFile2,
    listDir',
    TIO.hGetLine,
    TIO.hPutStr,
  )
where

import Control.Arrow (first, second)
import Control.DeepSeq (force, ($!!))
import Control.Exception (SomeException, catch)
import Control.Monad (filterM, when)
import Control.Monad.Catch as Catch
  ( Exception,
    MonadThrow,
    SomeException,
  )
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5 (md5)
import Data.Either (isLeft)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text.IO as T (appendFile, readFile, writeFile)
import qualified Data.Text.IO as TIO (hGetLine, hPutStr)
import qualified Path
import qualified Path.IO as PathIO
import qualified System.Directory as D
import qualified System.FilePath as OS
import qualified System.IO as SIO
import System.Posix (FileMode)
import qualified System.Posix as Posix
import Uniform.FileIOalgebra
import Uniform.FileStatus
import Uniform.Filenames
import Uniform.Filenames as FN (toFilePath)
import Uniform.Strings -- (Text)
import Uniform.Error


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
    res <- callIO $ TIO.hGetLine h
    return . lines' $ res

listDir' ::
  (MonadIO m, MonadThrow m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Abs Dir], [Path Abs File])
listDir' p = do
  abList :: ([Path.Path Abs Dir], [Path.Path Abs File]) <-
    PathIO.listDir . unPath $ p
  let abPathList = abList
  return abPathList

instance FileSystemOps FilePath where
  checkSymbolicLink fp = callIO $ D.pathIsSymbolicLink fp
  getPermissions' = callIO . D.getPermissions

instance DirOps FilePath where
  doesDirExist' = callIO . D.doesDirectoryExist
  createDirIfMissing' = callIO . D.createDirectoryIfMissing True

  createDir' = callIO . D.createDirectory
  renameDir' old new = do
    putIOwords ["renamed start"]
    testSource <- doesDirExist' old
    testTarget <- doesDirExist' new
    if testTarget
      then
        throwErrorT
          [showT new]
      else
        if not testSource
          then
            throwErrorT
              [showT old]
          else do
            callIO $ putStrLn "renamed"
            r <- callIO $ D.renameDirectory old new
            return ()

  getDirectoryDirs' dir = filterM f =<< getDirCont dir
    where
      f = doesDirExist'
  getDirectoryDirsNonHidden' dir = filterM f =<< getDirContNonHidden dir
    where
      f = doesDirExist'

  deleteDirRecursive f =
    do
      t <- doesDirExist' f
      when t $ do
        callIO . D.removeDirectoryRecursive $ f

        putIOwords ["deleted", showT f]

instance FileOps FilePath where
  doesFileExist' = callIO . D.doesFileExist

  copyOneFile old new = do
    -- source must exist, target must not exist
    t <- doesFileExist' old
    t2 <- doesFileExist' new
    if t && not t2
      then do
        let dir = getParentDir new -- was takeDir
        direxist <- doesDirExist' dir
        unless direxist $
          createDirIfMissing' dir
        callIO $ D.copyFile old new
      else
        if not t
          then
            throwErrorT
              ["copyFile source not exist", showT old]
          else
            if t2
              then
                throwErrorT
                  ["copyFile target exist", showT new]
              else throwErrorT ["copyOneFile", "other error"]
  copyOneFileOver old new = do
    -- may overwrite existing target
    t <- doesFileExist' old
    if t
      then do
        let dir = getParentDir new -- was takeDir
        direxist <- doesDirExist' dir
        unless direxist $
          createDirIfMissing' dir
        callIO $ D.copyFile old new
      else -- not t - not existing source
        throwErrorT ["copyFileOver source not exist", showT old]

  getMD5 fn =
    do
      status <- getSymbolicLinkStatus fn
      let regular = isRegularFile status
      readable <- getFileAccess fn (True, False, False)
      if regular && readable
        then callIO $ do
          filedata :: L.ByteString <- L.readFile fn
          let res = showT $ md5 filedata
          return $!! (Just res)
        else throwErrorT ["getMD5 error file not readable", showT fn]
      `catchError` \e -> do
        putIOwords ["getMD5 in FileStrings.hs", showT fn, showT e]

        throwErrorT ["getMD5 error for", showT fn]

  getDirCont fn = getDirContAll True fn

  getDirContNonHidden fp = do
    r <- getDirContAll False fp
    return r

  deleteFile f = do
    callIO . D.removeFile $ f

  getAppConfigDirectory = error "not implemented" -- do

  getSymbolicLinkStatus fp = do
    st <- callIO $ Posix.getSymbolicLinkStatus fp
    return st

  getFileAccess fp (r, w, e) =
    callIO $
      Posix.fileAccess fp r w e

  getFileModificationTime fp = do
    stat :: Posix.FileStatus <- getFileStatus' fp
    let time = getModificationTimeFromStatus stat
    return time

  openFile2handle fp mode =
    callIO $ SIO.openFile fp mode

getDirContAll hiddenFlag fn = do
  -- the hiddenFlag must be true to include them
  testDir <- doesDirExist' fn
  readExec <- getFileAccess fn (True, False, True)
  if testDir && readExec
    then do
      r <- callIO . D.listDirectory $ fn
      let r2 = filter (\file' -> file' /= "." && file' /= "..") r
      let r3 =
            if hiddenFlag
              then r2
              else filter (not . isPrefixOf ".") r2
      let r4 = map (fn </>) r3
      return r4
    else
      throwErrorT
        [ "getDirCont not exist or not readable",
          showT fn,
          showT testDir,
          showT readExec
        ]

instance FileSystemOps (Path ar df) where
  getPermissions' = PathIO.getPermissions . unPath
  checkSymbolicLink fp = callIO $ D.pathIsSymbolicLink (unL fp)

instance DirOps (Path Abs Dir) where
  doesDirExist' = PathIO.doesDirExist

  createDir' = PathIO.createDir . unPath

  renameDir' old new =
    -- :: fp -> fp ->  ErrIO Text
    PathIO.renameDir (unPath old) (unPath new)

  getDirectoryDirs' dir = do
    res <- filterM f =<< getDirCont (toFilePath dir)
    return . map makeAbsDir $ res
    where
      f = doesDirExist'
  getDirectoryDirsNonHidden' dir = do
    res <- filterM f =<< getDirContNonHidden (toFilePath dir)
    return . map makeAbsDir $ res
    where
      f = doesDirExist'

  createDirIfMissing' = PathIO.createDirIfMissing True . unPath

  copyDirRecursive old new = PathIO.copyDirRecur (unPath old) (unPath new)

  deleteDirRecursive f = deleteDirRecursive (unL f)

instance DirOps (Path Rel Dir) where
  doesDirExist' = PathIO.doesDirExist

  createDir' = PathIO.createDir . unPath

  renameDir' old new =
    PathIO.renameDir (unPath old) (unPath new)

  getDirectoryDirs' dir = do
    res <- filterM f =<< getDirCont (toFilePath dir)
    return . map makeRelDir $ res
    where
      f = doesDirExist'

  getDirectoryDirsNonHidden' dir = do
    res <- filterM f =<< getDirContNonHidden (toFilePath dir)
    return . map makeRelDir $ res
    where
      f = doesDirExist'

  createDirIfMissing' = PathIO.createDirIfMissing True . unPath

  copyDirRecursive old new = PathIO.copyDirRecur (unPath old) (unPath new)

  deleteDirRecursive f = deleteDirRecursive (unL f)

instance (Show (Path ar File)) => FileOps (Path ar File) where
  doesFileExist' = PathIO.doesFileExist . unPath

  copyOneFile old new = copyOneFile (unL old) (unL new)
  copyOneFileOver old new = copyOneFileOver (unL old) (unL new)
  renameOneFile old new =
    PathIO.renameFile (unPath old) (unPath new)

  deleteFile f = deleteFile (unL f)

  getMD5 fp = getMD5 (unL fp)

  getDirCont fp =
    error "getDirCont cannot be implemented for Path"
  getDirContNonHidden fp =
    error "getDirContentNonHidden cannot be implemented for Path"

  getFileModificationTime fp = getFileModificationTime (unL fp)

  openFile2handle fp mode = openFile2handle (unL fp) mode

  getFileAccess fp (r, w, e) =
    callIO
      ( do
          Posix.fileAccess (unL fp) r w e
          `catchError` \e -> do
            putIOwords ["getFileAccess error", showT fp, s2t $ show e]
            return False
      )

unL = FN.toFilePath

readFileT :: Path ar File -> ErrIO Text
readFileT fp = callIO . T.readFile . unL $ fp

writeFileT :: Path ar File -> Text -> ErrIO ()
writeFileT fp st = callIO $ T.writeFile (unL fp) st

-- attention - does not create file if not existing

instance (Show (Path ar File)) => FileOps2 (Path ar File) String where
  readFile2 fp = callIO $ readFile (unL fp)

  -- a strict read (does cloes?)
  writeFile2 fp st = callIO $ writeFile (unL fp) st
  appendFile2 fp st = callIO $ appendFile (unL fp) st

instance (Show (Path ar File)) => FileOps2 (Path ar File) Text where
  readFile2 fp = readFile2 (unL fp)

  writeFile2 fp st = writeFile2 (unL fp) st
  appendFile2 fp st = appendFile2 (unL fp) st

  writeFileOrCreate2 filepath st = do
    let dir = getParentDir filepath

    createDirIfMissing' dir
    when False $ putIOwords ["writeFileOrCreate2 dir created", showT dir]
    t <- doesDirExist' dir
    when False $ putIOwords ["writeFileOrCreate2 dir test", showT t]
    writeFile2 filepath st
    when False $ putIOwords ["writeFileOrCreate2 file written", showT filepath]

instance FileOps2 FilePath Text where
  readFile2 fp = callIO $ T.readFile fp
  writeFile2 fp st = callIO $ T.writeFile fp st
  appendFile2 fp st = callIO $ T.appendFile fp st

instance FileOps2 FilePath L.ByteString where
  readFile2 fp = callIO $ L.readFile fp
  writeFile2 fp st = callIO $ L.writeFile fp st
  appendFile2 fp st = callIO $ L.appendFile fp st

instance (Show (Path ar File)) => FileOps2 (Path ar File) L.ByteString where
  readFile2 fp = callIO $ L.readFile . unL $ fp
  writeFile2 fp st = callIO $ L.writeFile (unL fp) st
  appendFile2 fp st = callIO $ L.appendFile (unL fp) st

instance FileOps2a FilePath FilePath where
  getDirContentFiles dir =
    filterM doesFileExist'
      =<< getDirCont dir

  getDirContentNonHiddenFiles dir =
    filterM doesFileExist'
      =<< getDirContNonHidden dir

instance FileOps2a (Path Abs Dir) (Path Abs File) where
  getDirContentFiles dir = do
    res <- getDirContentFiles (toFilePath dir)
    return (map makeAbsFile res)
  getDirContentNonHiddenFiles dir = do
    res <- getDirContentNonHiddenFiles (toFilePath dir)
    return (map makeAbsFile res)

instance FileOps2a (Path Rel Dir) (Path Rel File) where
  getDirContentFiles dir = do
    res <- getDirContentFiles (toFilePath dir)
    return (map makeRelFile res)
  getDirContentNonHiddenFiles dir = do
    res <- getDirContentNonHiddenFiles (toFilePath dir)
    return (map makeRelFile res)
