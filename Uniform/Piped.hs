----------------------------------------------------------------------
--
-- Module      :  piped
-- Copyright   :  andrew u frank -
--
---------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | the recursive access to many files not blocking
module Uniform.Piped
  ( getRecursiveContents,
    --    , pipeMap, pipeStdoutLn
    pipedDoIO,
    pipedDoIOwithFilter
  )
where

import Data.List (sort)
import qualified Path.IO (readable, searchable)
import Pipes ((>->))
import qualified Pipes as Pipe
import qualified Pipes.Prelude as PipePrelude
import Uniform.Error
--   ( ErrIO,
--     ErrorT,
--     Text,
--     putIOwords,
--     showT,
--     t2s,
--     when,
--   )
import Uniform.Strings 
import Uniform.FileStrings

getRecursiveContents :: -- (Path Abs File-> Pipe.Proxy Pipe.X () () String (ErrorT Text IO) ())
  Path Abs Dir ->
  Pipe.Proxy Pipe.X () () (Path Abs File) (ErrorT Text IO) ()
getRecursiveContents fp = do
  --    putIOwords ["recurseDir start", showT fp]
  perm <- Pipe.lift $ getPermissions' fp
  if not (Path.IO.readable perm && Path.IO.searchable perm)
    then Pipe.lift $ putIOwords ["recurseDir not readable or not searchable", showT fp]
    else do
      symLink <- Pipe.lift $ checkSymbolicLink fp -- callIO $ xisSymbolicLink fp
      if symLink
        then Pipe.lift $ putIOwords ["recurseDir symlink", showT fp]
        else do
          (dirs, files) <- Pipe.lift $ listDir' fp
          when False $ do
            Pipe.lift $ putIOwords ["recurseDir files\n", showT files]
            Pipe.lift $ putIOwords ["recurseDir directories\n", showT dirs]

          Prelude.mapM_ Pipe.yield (sort files)
          --                                (Path.IO.sort (map unPath files))
          Prelude.mapM_ getRecursiveContents (sort dirs)
          --                            (Path.IO.sort (map unPath dirs))
          return () --    where processOneFile fp = Pipe.yield fp

--
---- examples how to use...
--
--pipedDo :: LegalPathname -> (LegalPathname -> Text) -> ErrIO ()
--pipedDo path transf =  do
--
--  runEffect $
--    getRecursiveContents path
--    >-> P.map (t2s . transf)
--    >-> P.stdoutLn
--
--testDir = fromJustNote "testdir" $ makeLegalPath "/home/frank/Workspace8/uniform-fileio/testDirFileIO"
--test_getRec = do
--    res <- runErr $ pipedDo testDir (showT)
--    assertEqual (Right ()) res
--    -- check manually
--
--
--
--

-- | a convenient function to go through a directory and
-- recursively apply a function to each
pipedDoIO :: Path Abs File -> Path Abs Dir -> (Path Abs File -> Text) -> ErrIO ()
pipedDoIO file path transf = do
        -- pipedDoIOwithFilter file path ?? transf
  hand <- openFile2handle file WriteMode
  Pipe.runEffect $
    getRecursiveContents path
      >-> PipePrelude.map (t2s . transf) -- some IO type left?
      >-> PipePrelude.toHandle hand
  closeFile2 hand
  return ()

-- a convenient function to go through a directory and
-- recursively apply a function to each file or directory
-- filters for extension md
pipedDoIOwithFilter :: Path Abs File -> Path Abs Dir -> Extension -> (Path Abs File -> ErrIO String) -> ErrIO ()
pipedDoIOwithFilter file path ext opex = do
  hand <- openFile2handle file WriteMode
  Pipe.runEffect $
    getRecursiveContents path
      >-> PipePrelude.filter (hasExtension ext)
      >-> PipePrelude.mapM opex
      >-> PipePrelude.toHandle hand
  closeFile2 hand
  return ()