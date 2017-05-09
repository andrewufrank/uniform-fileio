-----------------------------------------------------------------------------
--
-- Module      :  piped
-- Copyright   :  andrew u frank -
--
-- | the recursive access to many files not blocking
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
    , FlexibleInstances
    , FlexibleContexts
--    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
--    , TypeFamilies
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module  Uniform.Piped (pipedDo, pipedDoIO
            , getRecursiveContents
    , htf_thisModulesTests

            ) where

import Pipes
import qualified Pipes.Prelude as P
import Control.Monad (forM_)

--import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
--import System.FilePath ((</>))
--import System.IO (openFile, IOMode (..), hClose)

---- using uniform:
import Uniform.Error
--import Uniform.Zero
import Uniform.Strings hiding ((<.>), (</>))
--
--import FileIO.Filenames
--import Uniform.FileIO
import Uniform.FileStrings
import Uniform.Filenames
--
import Test.Framework
--import Test.Invariant
--

getRecursiveContents :: LegalPathname -> Producer LegalPathname  ErrIO ()
getRecursiveContents topPath = do

  lift $ putIOwords ["getRecursiveContents", showT topPath]
  properNames <- lift $ getDirContentNonHidden topPath
  -- lift into Producer (ie. proxy)
--  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isLink <- lift $ checkSymbolicLink path
    if isLink then return ()
        else do
            isDirectory <- lift $ doesDirExist path
            isReadExecutable <- lift $ getFileAccess path (True, False, True)
            if isDirectory && isReadExecutable
              then   getRecursiveContents path
              else  do
                    isReadable <- lift $ getFileAccess path (True, False, False)
                    lift $ putIOwords ["getRecursiveContents isReadable"
                                    , showT isReadable, showT path]
                    if isReadable
                            then yield path
                            else return ()

-- examples how to use...

pipedDo :: LegalPathname -> (LegalPathname -> Text) -> ErrIO ()
pipedDo path transf =  do

  runEffect $
    getRecursiveContents path
    >-> P.map (t2s . transf)
    >-> P.stdoutLn

testDir = fromJustNote "testdir" $ makeLegalPath "/home/frank/Workspace8/uniform-fileio/testDirFileIO"
test_getRec = do
    res <- runErr $ pipedDo testDir (showT)
    assertEqual (Right ()) res
    -- check manually




pipedDoIO :: LegalPathname -> LegalPathname -> (LegalPathname -> ErrIO Text) -> ErrIO ()
-- | write to the first filename the operation applied to the dir tree in the second
-- first path must not be non-readable dir or
pipedDoIO file path transf =  do
  hand <-   openFile file WriteMode
  runEffect $
    getRecursiveContents path
    >-> P.mapM (fmap t2s . transf)
--    >-> P.stdoutLn
    >-> P.toHandle hand
  closeFile2 hand


