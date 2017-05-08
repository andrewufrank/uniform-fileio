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
-- no checks for the filename?
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FileIO (
         FileOps (..)
         , FileOps2 (..)
         , closeFile2
         , Filepathes (..)
         , lpX, fpX
         , t2fp  -- convert text to filepath, is t2s
         , attachToFileName
--         , CV2legal (..)
--         , CV2paths (..)
--        --  , (</>), (<.>)
--         , FNstrings (..)
--         , Hiddings(..)
----         , getFilepathS, dirs2path, makeLegalPath
--        --  , module Uniform.Filenames  -- export only these functions
        , LegalPathname (..)
--        , unLegalPathname, unL, makeLegalPath
        , LegalFilename
--            (..), unLegalFilename, makeLegalFilename
        , LegalExtension
            --(..), makeLegalExtension
--        , attachToFileName
--        , getFilenameS
--        , FPtype   -- should not be used
--        -- , FPtype
--         , module Uniform.FileStatus
--         , P.FileStatus
         , module Uniform.FileStrings
         , module Uniform.Error
         , module Uniform.TypedFile

--         , getModificationTime
--         , P.FileStatus
         , pipedDo, pipedDoIO
         , getRecursiveContents
         , htf_thisModulesTests
         ,legalpathnameConstructor
                -- LegalPathname is not available as a constructor

            ) where

--import qualified Data.Text as T
import qualified System.Posix          as P (FileStatus)
--import qualified System.Directory as S
--import System.IO (Handle, IOMode (..))

-- using uniform:
import           Uniform.Error
import           Uniform.FileIOalgebra
import           Uniform.FilenamesAlgebra
import Uniform.Filenames -- must import the constructors for the test strategy to work
import           Uniform.FileStatus
import           Uniform.FileStrings
import           Uniform.Piped
import           Uniform.Strings
import           Uniform.TypedFile
import           Uniform.Zero

import           Test.Framework
import           Test.Invariant

import           Control.Arrow         (first, second)
import qualified System.FilePath       as OS

---- | operations on files and directories
---- the doesXX do not produce any exceptiosn
-- is polymorph either in LegalFilename or in RawFilePath (i.e. bytestring )

legalpathnameConstructor  :: Text -> LegalPathname
legalpathnameConstructor = LegalPathname
