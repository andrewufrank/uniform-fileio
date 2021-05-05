----------------------------------------------------------------------
--
-- Module      :  uniform-FileIO
-- Copyright   :  andrew u frank -
--
----------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | the routines to take apart the file status
module Uniform.FileStatus
  ( getFileStatus,
    isDirectory,
    isRegularFile,
    getFileStatus',
    isSymbolicLink,
    getModificationTimeFromStatus,
    getFileSize,
    P.EpochTime,
    P.FileStatus,
  )
where

import qualified System.Directory as S
import qualified System.Posix as P
import Uniform.Error 
-- (ErrIO, callIO, putIOwords, showT)
import Uniform.Filenames (Path, toShortFilePath)
import Uniform.Strings (putIOwords, showT)

unL :: Path df ar -> FilePath
unL = toShortFilePath

--getFileStatus :: Path df ra -> ErrIO P.FileStatus
-- getFileStatus :: (Control.Monad.Error.Class.MonadError m,
--  Control.Monad.IO.Class.MonadIO m,
--  Control.Monad.Error.Class.ErrorType m ~ Data.Text.Internal.Text)
--     => Path df ar -> m P.FileStatus
getFileStatus fp = callIO $ P.getFileStatus . unL $ fp

getFileStatus' :: FilePath -> ErrIO P.FileStatus
getFileStatus' fp = callIO $ P.getFileStatus fp

isRegularFile :: P.FileStatus -> Bool
isRegularFile = P.isRegularFile

isDirectory :: P.FileStatus -> Bool
isDirectory = P.isDirectory

isSymbolicLink :: P.FileStatus -> Bool
isSymbolicLink = P.isSymbolicLink

getModificationTimeFromStatus :: P.FileStatus -> P.EpochTime
getModificationTimeFromStatus = P.modificationTime

getFileSize = P.fileSize

createSymbolicLink :: Show (Path df ra) => Path df ra -> Path df ra -> ErrIO ()
createSymbolicLink fn tn = do
  putIOwords ["createSymbolidLink", showT fn, "to", showT tn]
  callIO $ P.createSymbolicLink (unL fn) (unL tn)

renameLink :: Path df ra -> Path df ra -> ErrIO ()
renameLink old new = callIO $ P.rename (unL old) (unL new)
