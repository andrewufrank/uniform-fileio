-----------------------------------------------------------------------------
--
-- Module      :  SVN.FileIO
-- Copyright   :  andrew u frank -
--
-- | the basic file io - translated for the Either or ErrorT signaling style
--  there are better (higher performance methods - replace while retaining conditions

-- reduced oct 2016 to small set
-- separate the operations from the OS and the operations which check for
-- undesirable characters in the filename and extension
-- approach: addExtension checks for bad extension characters
-- checks for filenames in the instances which use legalPathname
--
-- naming consistentl dir (not directory)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FileIOalgebra (
         FileOps (..)
         , FileOps2 (..)
        , Handle , IOMode (..)

         , module Uniform.Error
         , module Uniform.Zero

--         , LegalPathname, unLegalPathname
--         , LegalFilename(..)
--         , LegalExtension (..)
--         , CV2legal (..)
--         , CV2paths (..)
----         , (</>), (<.>)
--         , FNstrings (..)
--         , Hiddings(..)
--         , getFilepathS, dirs2path, makeLegalPath
--         , P.FileStatus
--         , FPtype
--         , pipedDo, pipedDoIO
--            , getRecursiveContents
--    , htf_thisModulesTests

            ) where

--import qualified Data.Text as T
import qualified System.Posix  as P (FileStatus)
--import qualified System.Directory as S

---- using uniform:
import           Uniform.Error
import           Uniform.Zero
--import           Uniform.FilenamesAlgebra
import System.IO (Handle, IOMode (..) )
--import System.Posix (FileMode)

--import Uniform.Strings
--
--import Uniform.Filenames
--import Uniform.Piped
--
--import Test.Framework
--import Test.Invariant
--
--import Control.Arrow (first, second)
--import qualified System.FilePath as OS

---- | operations on files and directories
---- the doesXX do not produce any exceptiosn
-- is polymorph either in LegalFilename or in RawFilePath (i.e. bytestring )

class FileOps fp   where
    doesDirExist :: fp -> ErrIO Bool
    doesFileExist :: fp -> ErrIO Bool
    doesFileOrDirExist :: fp -> ErrIO Bool
    doesFileOrDirExist fp = do
        d <- doesDirExist fp
        f <- doesFileExist fp
        return   (d || f)

    createDir :: fp -> ErrIO ()
    -- | write in a dir a new file with content

    createDirIfMissing :: fp ->  ErrIO ()
    -- | creates the directory, if missing
    -- noop if dir exist

    copyFile :: fp -> fp ->  ErrIO ()
    -- ^ copy a file from old to new
    renameFile :: fp -> fp ->  ErrIO ()
    -- ^ rename a file from old to new
    renameDir :: fp -> fp ->  ErrIO Text
    -- ^ rename directory old to new
    -- signals: getFileStatus: does not exist (No such file or directory)

    deleteFile :: fp -> ErrIO ()
    deleteDirRecursive :: fp -> ErrIO ()
    -- ^ delete a directory (even non empty), no error if not existing

--    assertDirNotExist :: fp -> ErrIO ()
--    -- ^ delete a directory (even non empty), if exist

    -- | get the directory content - if not existing Nothing, if empty Just []
    -- not returning the special entries   . and ..
        -- filenames completed with the filename calling
        -- check access and readable
    getDirCont :: fp ->  ErrIO [fp] --  (Maybe [String])
    -- | get the directory content - if not existing Nothing, if empty Just []
    -- not returning any hidden files
    -- alphabetic search to assure that equal directories have equal conten
    -- independent of file system internal structure
    -- filenames completed with calling fp
    getDirContentNonHidden :: fp ->  ErrIO [fp]

    getMD5 :: fp -> ErrIO (Maybe Text)
    -- get MD5, but why Text  -- TODO

    getAppConfigDirectory :: ErrIO fp
    -- ^ find the .config directory path

    getSymbolicLinkStatus :: fp ->   ErrIO ( P.FileStatus)
    -- ^ get status if exist (else Nothing)
    --   is the status of the link, does not follow the link

    getFileAccess :: fp -> (Bool, Bool, Bool) -> ErrIO Bool
    -- ^ check the read, write and execute permission on file
    -- dir get content needs execute,

    checkSymbolicLink :: fp -> ErrIO Bool
    -- ^ check if the fp points to a symbolic link
    -- better use isSimbolicLink (from FileStatus)


-- operations on handle

    openFile :: fp -> IOMode -> ErrIO Handle
--    closeFile :: fp ->  Handle -> ErrIO ()
    -- the filepath is used only as phantom ...
        -- use closeFile2 without fp


-- read and write file in some monad
--class   FileOps3 fp fc m where
--
--    readFile3 :: fp -> m fc
--    writeFile3 :: fp -> fc -> m ()

--
--
---- | the operations on files with content
--
class (FileOps fp, Filepathes fp, Show fp) =>
    FileOps2 fp fc where
--
    writeFile2 :: fp -> fc -> ErrIO ()
    -- write file if dir exist
    readFile2 :: fp -> ErrIO fc
    -- read file
    appendFile2 :: fp -> fc -> ErrIO ()
    -- append to a file




--    writeNewFileInDir :: fp -> fp -> fc ->  ErrIO ()
--    -- | create file in existing dir
--
--    writeFileCreateDir :: fp -> fp -> fc ->  ErrIO ()
--    -- | create file in existing dir
--
    writeFileOrCreate :: fp -> fc -> ErrIO ()
   -- | write or create a file
   -- if create dir if not exist (recursively for path)
    writeFileOrCreate filepath st = do
        let (dir, fn, ext) = splitFilepath filepath
        --        writeFileCreateDir dir fn st
        --                  writeFileCreateDir dirpath filename st = do
--        let fp = dirpath `combine` filename
        d <- doesDirExist dir
        --        f <- doesFileExist fp --
        unless d $
                createDirIfMissing dir
        writeFile2 filepath st

--

    readFileOrZero2 :: (FileOps fp, Zeros fc) => fp -> ErrIO fc
    -- | reads file, if not present, returns zero
    readFileOrZero2 fp = do
        f <- doesFileExist fp
        if f
            then readFile2 fp
            else return zero
--
--

--    writeNewFileInDir dirpath filename st =  -- writeNewFileInDir (b2s dirpath) (b2s filename) (b2s st)
--        do
--            let fp = dirpath `combine` filename
--            d <- doesDirectoryExist dirpath
--            f <- doesFileExist fp -- filename
--            if not d  then throwError . unwords $
----            signalf  DirNotExist
--                    ["writeNewFileInDir"
--                        , show dirpath, "not existing"]
--                        else if f then throwError . unwords $
----                            signalf  TargetExist
--                            ["writeNewFileInDir"
--                        , show filename, "exist"]
--                else  writeFile2 fp st
--
--    writeFileCreateDir dirpath filename st = do
--        let fp = dirpath `combine` filename
--        d <- doesDirectoryExist dirpath
--        f <- doesFileExist fp --
--        unless d $
--                createDirIfMissing dirpath
--        writeFile2 fp st
--
----
--
--
---- | the signals for fileops
--data FileOpsSignals = TargetExist   -- ^ the file or dir exists
--        | DirNotExist
--        | DirEmpty
--        | SourceExist
--        | SourceNotExist
--        | SystemIOerror
--        | SomeReadError
--        deriving (Show, Eq)

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
