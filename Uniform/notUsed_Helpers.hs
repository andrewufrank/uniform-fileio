-----------------------------------------------------------------------------
--
-- Module      :  SVN.FileIO
-- Copyright   :  andrew u frank -
--
-- | the basic file io - translated for the Either or ErrorT signaling style
--  there are better (higher performance methods - replace while retaining conditions

-- reduced oct 2016 to small set
-----------------------------------------------------------------------------
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

module FileIO.FileIO (
          FileOps (..)
         ,  FileOps2  (..)
         , FileOps3 (..)
        , FileOpsSignals (..)
        , FilePathes (..)
        , FilePathes2 (..)
--        , getFileStatus, isDirectory, isRegularFile
--        , isSymbolicLink, getSymbolicLinkStatus
--        , createSymbolicLink, renameLink
--        , doesExist
    , getModificationTime  -- TODO is this correct export?
            ) where

--import qualified Data.Text as T
--import qualified System.Posix as P
--import qualified System.Directory as S
----import Basics
--import Uniform.Error
--import Uniform.Zero
--import Data.Strings

-- new approach

getSymbolicLinkStatus :: FilePath -> ErrIO (Maybe P.FileStatus)
-- ^ get status if exist (else Nothing)

getSymbolicLinkStatus fp = do
    putIOwords ["fileio getSymbolicLinkStatus", fp]
    st <- liftIO $ P.getSymbolicLinkStatus fp
    putIOwords ["fileio getSymbolicLinkStatus done", fp]
    return . Just $ st
  `catchError` (\s -> do
            putIOwords ["fileio getSymbolicLinkStatus not found", fp]
            return Nothing)


-- | operatiosn to combine filepath with strings or similar
class (Strings f, Strings (ExtensionF f)
    , Eq (ExtensionF f), Eq f, FilePathes f
    ) => FilePathes2 f  where
    type ExtensionF f

--    makeFP :: FPstring f -> f
--    -- drop the following operations? (especially the later ones)
    {-# MINIMAL splitExtension2, addExtension0  #-}
    addExtension2 :: f -> ExtensionF f -> Maybe f
    -- ^ add the string as extension
    -- filename must not be null
    -- extension must not contain \ nor . nor tab nor newline
    -- what else is not desirable?
    addExtension2 f e = if  null' f ||null' e
            || ("." `isInfixOf'` (toString e) )
            || (".\n" `isInfixOf'` (toString e) )
            || (".\t" `isInfixOf'` (toString e) )
            || ("/" `isInfixOf'` (toString e) ) then Nothing
        else Just $ addExtension0 f e

    addExtension0 :: f -> ExtensionF f -> f
    -- the underlying operation of the OS

    splitExtension2 :: f -> (f, ExtensionF f)
    -- ^ split the filename and the extension
    -- from OS

--    addExtension, (<.>) :: f -> ExtensionF f -> f
--    -- ^ add the string as extension
--    addExtension f e =
--            fromJustNote (unwords ["error in addExtension2"
--                        , toString f, toString e]) . addExtension2 f $ e
--    takeExtension1 :: f -> ExtensionF f
--    -- ^ get the extension, including the leading "."
    takeExtension2 :: f -> ExtensionF f
    --        , without leading "." -- empty for empty extension
--                not like Hackage Posix!
    takeExtension2  = snd . splitExtension2
--    splitExtension :: f -> (f,ExtensionF f)
--    -- ^ split the filename and the extension, include leading dot
----    splitExtension2 = second (fromJustNote "spliExtension2 not . 354dsd" . stripPrefix' ".") . splitExtension

    replaceExtension2 :: f -> ExtensionF f -> Maybe f
    -- ^ replace the extension by a given string (not including .)
    replaceExtension2 f e =  (\fx -> addExtension2 fx e) (removeExtension f)

    testExtension :: ExtensionF f -> f -> Bool
    -- ^ has a given exension (without .) empty string for empty extension or "."
    testExtension s f = (takeExtension2 f) == s

    removeExtension :: f -> f
    removeExtension = fst . splitExtension2


    (<.>) :: f -> ExtensionF f -> f
    f <.> e = fromJustNote ("addExtension "++ toString e ++ "probably not legal")
            . addExtension2 f $ e

--    prop_extension1 :: (Strings f, Strings (ExtensionF f)) =>  f -> ExtensionF f -> Bool
--    prop_extension1 f s =
--        if null' f || ("." `isPrefixOf'` (toString s) )
--            || ("/" `isPrefixOf'` (toString s) )        then True
--          else
--            (takeExtension2 ( addExtension f s) == s)

    prop_extension2 :: (Strings f, Strings (ExtensionF f))
                         =>  f -> ExtensionF f -> Bool
    -- ^ test that split and add extension are inverse
    prop_extension2 f s =
--        if null' f || null' s
--            || ("." `isInfixOf'` (toString s) )
--            || ("/" `isInfixOf'` (toString s) )
--             then True
--          else
            (maybe True  (((f,s) ==) . splitExtension2)
                     (addExtension2 f s)
                     )

--x :: f -> s -> f -> Bool
--x f s f2 = ((f,s) ==) . splitExtension2 $ f2

---- | operatiosn to combine filepath with strings or similar
--class (Eq s, FilePathes f) => FilePathes2 f s where
--    makeFP :: s -> f
--    -- drop the following operations? (especially the later ones)
--
--    takeExtension :: f -> s
--    -- ^ get the extension
--    takeExtension2 :: f -> s
--    --        , without leading "." -- empty for empty extension
----                not like Hackage Posix!
--    splitExtension :: f -> (s,s)
--    -- ^ split the filename and the extension
--
--    replaceExtension :: f -> s -> f
--    -- ^ replace the extension by a given string (not including .)
--    addExtension, (<.>) :: f -> s -> f
--    -- ^ add the string as extension
--    testExtension :: s -> f -> Bool
--    -- ^ has a given exension (without .) empty string for empty extension or "."
--    testExtension s f = (takeExtension2 f) == s
--
--
--    (<.>) = addExtension



-- | the operations on filepath - pure, no signals
class FilePathes f where
    combine, (</>) :: f -> f -> f
    -- ^ combine two filepath in the OS specific way (svn uses posix)
    -- use combine when OS specific is necessary,
    -- the </> is changed to operate independent of OS
    (</>) = combine

    joinPath :: [f] -> f
    -- joint the path elements

    -- | split a fn into directory and fn (inverse of combing
    splitFileName :: f -> (f,f)

    -- | split directories witht he OS specific way (with / at end)
    splitDirFile :: f  -> [f]
    -- | split directories with no trailing /
    splitDirectories :: f  -> [f]

    -- | get last name (usually filename)
    takeLastDirName:: f -> f

    -- | make a name relative to a root (which is first arg)
    makeRelative :: f -> f -> f


    -- | test if file is hidden (prefix .)
    -- could require OS specific ops
    isHidden2 :: f -> Bool
--    isHidden2  = isHidden2 . getFilepath

    -- | undo a packaged fp
    getFilepath :: f -> FilePath
    -- | make a specific fp

--    removeExtension :: f -> f

-- | operations on files and directories
-- the doesXX do not produce any exceptiosn
class FileOps fp   where
    doesDirectoryExist :: fp -> ErrIO Bool
    doesFileExist :: fp -> ErrIO Bool
    doesFileOrDirExist :: fp -> ErrIO Bool

    createDir :: fp -> ErrIO ()
    -- | write in a dir a new file with content

    createDirIfMissing :: fp ->  ErrIO ()
    -- | creates the directory, if missing

    copyFile :: fp -> fp ->  ErrIO ()
    -- ^ copy a file from old to new
    renameFile :: fp -> fp ->  ErrIO ()
    -- ^ rename a file from old to new
    renameDirectory :: fp -> fp ->  ErrIO String
    -- ^ rename directory old to new
    -- signals: getFileStatus: does not exist (No such file or directory)

    deleteFile :: fp -> ErrIO ()
    deleteDirRecursive :: fp -> ErrIO String
    -- ^ delete a directory (even non empty)
--    -- | get the directory content - if not existing Nothing, if empty Just []
    getDirCont :: fp ->  ErrIO [String] --  (Maybe [String])
    getDirContentNonHidden :: fp ->  ErrIO [String]
    getMD5 :: fp -> ErrIO String

    getAppConfigDirectory :: ErrIO fp
    -- ^ find the .config directory path

-- | the operations on files with content

class (FileOps fp, FilePathes fp, Show fp) => FileOps2 fp fc where

    writeNewFileInDir :: fp -> fp -> fc ->  ErrIO ()
    -- | create file in existing dir

    writeFileCreateDir :: fp -> fp -> fc ->  ErrIO ()
    -- | create file in existing dir

    writeFileOrCreate :: fp -> fc -> ErrIO ()
    -- | write or create a file

    readFile2 :: fp -> ErrIO fc
    readFileOrZero2 :: Zeros fc => fp -> ErrIO fc
    -- | reads file, if not present, returns zero
    readFileOrZero2 fp = do
        f <- doesFileExist fp
        if f
            then readFile2 fp
            else return zero


    writeFile2 :: fp -> fc -> ErrIO ()

    writeNewFileInDir dirpath filename st =  -- writeNewFileInDir (b2s dirpath) (b2s filename) (b2s st)
        do
            let fp = dirpath `combine` filename
            d <- doesDirectoryExist dirpath
            f <- doesFileExist fp -- filename
            if not d  then throwError . unwords $
--            signalf  DirNotExist
                    ["writeNewFileInDir"
                        , show dirpath, "not existing"]
                        else if f then throwError . unwords $
--                            signalf  TargetExist
                            ["writeNewFileInDir"
                        , show filename, "exist"]
                else  writeFile2 fp st

    writeFileCreateDir dirpath filename st = do
        let fp = dirpath `combine` filename
        d <- doesDirectoryExist dirpath
        f <- doesFileExist fp --
        unless d $
                createDirIfMissing dirpath
        writeFile2 fp st

    writeFileOrCreate filepath st = do
        let (dir, fn) = splitFileName filepath
        writeFileCreateDir dir fn st

class   FileOps3 fp fc m where

    readFile3 :: fp -> m fc
    writeFile3 :: fp -> fc -> m ()

-- | the signals for fileops
data FileOpsSignals = TargetExist   -- ^ the file or dir exists
        | DirNotExist
        | DirEmpty
        | SourceExist
        | SourceNotExist
        | SystemIOerror
        | SomeReadError
        deriving (Show, Eq)



getFileStatus :: FilePath -> ErrIO P.FileStatus
isRegularFile :: P.FileStatus -> Bool
isDirectory :: P.FileStatus -> Bool
isSymbolicLink :: P.FileStatus -> Bool
getFileStatus fp = liftIO $ P.getFileStatus fp
isDirectory = P.isDirectory
isRegularFile = P.isRegularFile
isSymbolicLink = P.isSymbolicLink
getModificationTime = P.getModificationTime

createSymbolicLink :: FilePath -> FilePath -> ErrIO ()
createSymbolicLink fn tn = do
    putIOwords ["createSymbolidLink", fn , "to", tn]
    callIO $ P.createSymbolicLink fn tn

renameLink old new = P.rename old new
-- should check that this is a link and existing etc.

doesExist :: FilePath -> ErrIO Bool
-- ^ test if dir, file or link exist
doesExist fp = liftIO $ do
    f <- S.doesFileExist fp
    d <- S.doesDirectoryExist fp
    s <- S.isSymbolicLink fp
    return (f || d || s)


--from fay
-- | Join for Maybe.
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just x)) = Just x
joinMaybe _ = Nothing

