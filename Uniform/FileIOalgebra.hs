----------------------------------------------------------------------
--
-- Module      :  Uniform.FileIO
-- Copyright   :  andrew u frank -
--
-- reduced oct 2016 to small set
-- separate the operations from the OS and the operations which check for
-- undesirable characters in the filename and extension
-- approach: addExtension checks for bad extension characters
-- checks for filenames in the instances which use legalPathname
--
----------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Uniform.FileIOalgebra
  ( module Uniform.FileIOalgebra,
    module Uniform.Error,
    module Uniform.Zero,
    module System.IO,
  )
where

import qualified System.Directory as D
import System.IO (Handle, IOMode (..))
import Uniform.Error
import Uniform.FileStatus (EpochTime, FileStatus)
import Uniform.Time (UTCTime, epochTime2UTCTime)
import Uniform.Zero

class FileHandles t where
  write2handle :: Handle -> t -> ErrIO ()

  -- write a string or text to a handle
  readLine4handle :: Handle -> ErrIO t

-- read a lline from a handle - used?

--class ListDir d f where
--    listDir' :: d -> ErrIO ([d],[f])
class FileSystemOps fp where
  getPermissions' :: fp -> ErrIO D.Permissions

  checkSymbolicLink :: fp -> ErrIO Bool
  -- ^ check if the fp points to a symbolic link
  -- better use isSimbolicLink (from FileStatus)

class DirOps fp where
  doesDirExist' :: fp -> ErrIO Bool
  createDir' :: fp -> ErrIO ()

  -- | write in a dir a new file with content
  --    getDirPermissions  :: fp -> ErrIO D.Permissions
  createDirIfMissing' :: fp -> ErrIO ()

  -- | creates the directory, if missing, recursive for path
  -- noop if dir exist
  renameDir' :: fp -> fp -> ErrIO ()
  -- ^ rename directory old to new
  -- signals: getFileStatus: does not exist (No such file or directory)

  getDirectoryDirs' :: fp -> ErrIO [fp]

  -- get the directories (but not . and ..)
  -- getDirectoryDirs' dir = filterM f =<< getDirCont  dir
  --     where f  =  doesDirExist'
  getDirectoryDirsNonHidden' :: fp -> ErrIO [fp]

  copyDirRecursive :: fp -> fp -> ErrIO ()

  -- | copy the directory content recursively, does not follow symlink
  -- implemented only for Path n Dir, not FilePath
  deleteDirRecursive :: fp -> ErrIO ()
  -- ^ delete a directory (even non empty), no error if not existing

class (Show fp) => FileOps fp where
  doesFileExist' :: fp -> ErrIO Bool

  copyOneFile :: fp -> fp -> ErrIO ()
  -- ^ copy a file from old to new
  -- source must exist, target must NOT exist

  copyOneFileOver :: fp -> fp -> ErrIO ()
  -- ^ copy a file from old to new
  -- source must exist, target may exist

  renameOneFile :: fp -> fp -> ErrIO ()
  -- ^ rename a file from old to new

  deleteFile :: fp -> ErrIO ()

  --    assertDirNotExist :: fp -> ErrIO ()
  --    -- ^ delete a directory (even non empty), if exist

  -- | get the directory content - if not existing Nothing, if empty Just []
  -- not returning the special entries   . and ..
  -- filenames completed with the filename calling
  -- check access and readable
  -- returns for filepath always an absolute path
  -- for Path Rel gives Path Rel results
  getDirCont :: fp -> ErrIO [fp] --  (Maybe [String])

  -- | get the directory content - if not existing Nothing, if empty Just []
  -- not returning any hidden files
  -- alphabetic search to assure that equal directories have equal conten
  -- independent of file system internal structure
  -- filenames completed with calling fp
  -- only for filepath!
  getDirContNonHidden :: fp -> ErrIO [fp]

  getMD5 :: fp -> ErrIO (Maybe Text)

  -- get MD5, but why Text  -- TODO

  getAppConfigDirectory :: ErrIO fp
  -- ^ find the .config directory path

  getSymbolicLinkStatus :: fp -> ErrIO FileStatus
  -- ^ get status if exist (else Nothing)
  --   is the status of the link, does not follow the link

  getFileAccess :: fp -> (Bool, Bool, Bool) -> ErrIO Bool
  -- ^ check the read, write and execute permission on file
  -- dir get content needs execute,

  getFileModificationTime :: fp -> ErrIO EpochTime
  -- ^ get the modification time  (replaces isFileAbeforeB)

  getFileModificationUTCTime :: fp -> ErrIO UTCTime
  -- ^ get the modification time  in UTCTIme
  getFileModificationUTCTime = fmap epochTime2UTCTime . getFileModificationTime

  -- operations on handle

  openFile2handle :: fp -> IOMode -> ErrIO Handle

-- | operations on dir to produce file
class (Show fd, Show ff) => FileOps2a fd ff where
  getDirContentFiles :: fd -> ErrIO [ff]

  getDirContentNonHiddenFiles :: fd -> ErrIO [ff]

---- | the operations on files with content
class
  (Show fp) =>
  FileOps2 fp fc
  where
  --
  writeFile2 :: fp -> fc -> ErrIO ()

  -- write file if dir exist
  readFile2 :: fp -> ErrIO fc

  -- read file
  appendFile2 :: fp -> fc -> ErrIO ()

  writeFileOrCreate2 :: fp -> fc -> ErrIO ()

  --

  readFileOrZero2 :: (FileOps fp, Zeros fc) => fp -> ErrIO fc

  -- | reads file, if not present, returns zero
  readFileOrZero2 fp = do
    f <- doesFileExist' fp
    if f
      then readFile2 fp
      else return zero
