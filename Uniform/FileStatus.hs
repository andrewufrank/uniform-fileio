-----------------------------------------------------------------------------
--
-- Module      :  uniform-FileIO
-- Copyright   :  andrew u frank -
--
-- | the routines to take apart the file status
-----------------------------------------------------------------------------
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , TypeFamilies
    , OverloadedStrings

    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# OPTIONS -w #-}

module Uniform.FileStatus (
--          FileOps (..)
--         ,  FileOps2  (..)
--         , FileOps3 (..)
--        , FileOpsSignals (..)
--        , FilePathes (..)
--        , FilePathes2 (..)
        getFileStatus, isDirectory, isRegularFile
        , getFileStatus'
        , isSymbolicLink
        , getSymbolicLinkStatusFP
--        , createSymbolicLink, renameLink
--        , doesExist
    ,getModificationTime  -- TODO is this correct export?
    , getFileSize
--        , getSymbolicLinkStatusX
                   ) where

--import qualified Data.Text as T
import qualified System.Posix as P
import qualified System.Directory as S
----import Basics
import Uniform.Error
import Uniform.Zero
import Uniform.Strings
import Uniform.Filenames

-- new approach

getSymbolicLinkStatusFP :: FilePath  -> ErrIO ( P.FileStatus)
-- ^ get status if exist (else Nothing)
--   is the status of the link, does not follow the link
--
getSymbolicLinkStatusFP  fp = do
----    putIOwords ["fileio getSymbolicLinkStatus", fp]
    st <- callIO $ P.getSymbolicLinkStatus fp
----    putIOwords ["fileio getSymbolicLinkStatus done", fp]
    return   st
--  `catchError` (\s -> do
--            putIOwords ["fileio getSymbolicLinkStatus not found", showT fp]
--            return Nothing)
--  where fp = unL lfp

getFileStatus :: LegalPathname -> ErrIO P.FileStatus
getFileStatus fp = callIO $ P.getFileStatus . unL $ fp

getFileStatus' :: FilePath  -> ErrIO P.FileStatus
getFileStatus' fp = callIO $ P.getFileStatus   fp


isRegularFile :: P.FileStatus -> Bool
isDirectory :: P.FileStatus -> Bool
isSymbolicLink :: P.FileStatus -> Bool
isDirectory = P.isDirectory
isRegularFile = P.isRegularFile
isSymbolicLink = P.isSymbolicLink
getModificationTime = P.modificationTime
getFileSize = P.fileSize

createSymbolicLink :: LegalPathname -> LegalPathname -> ErrIO ()
createSymbolicLink fn tn = do
    putIOwords ["createSymbolidLink", showT fn , "to", showT tn]
    callIO $ P.createSymbolicLink (unL fn) (unL tn)


renameLink :: LegalPathname -> LegalPathname -> ErrIO ()
renameLink old new = callIO $ P.rename (unL old) (unL new)
-- should check that this is a link and existing etc.

doesExist :: LegalPathname -> ErrIO Bool
-- ^ test if dir, file or link exist
doesExist lfp = liftIO $ do

    f <- S.doesFileExist fp
    d <- S.doesDirectoryExist fp
    s <- S.pathIsSymbolicLink fp
    return (f || d || s)
  where fp = unL lfp

instance CharChains2 FilePath Text where
    show' = s2t

----from fay
---- | Join for Maybe.
--joinMaybe :: Maybe (Maybe a) -> Maybe a
--joinMaybe (Just (Just x)) = Just x
--joinMaybe _ = Nothing



--



