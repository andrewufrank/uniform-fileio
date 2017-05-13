-----------------------------------------------------------------------------
--
-- Module      :  FileIO
-- Copyright   :  andrew u frank -
--
-- | the basic file io - translated for the Either or ErrorT signaling style
--  there are better (higher performance methods - replace while retaining conditions

-- uses the Path and Path.IO framework
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
        module Uniform.Filenames
         , module Uniform.Error
         , module Uniform.Strings
         , module Uniform.FileStatus
         , module Uniform.FileIOalgebra
         , module Uniform.TypedFile
         , module Uniform.FileStrings
         , module Uniform.Piped
--         FileOps (..)
--         , FileOps2 (..)
--         , closeFile2
--         , Filepathes (..), unL
--         , lpX, fpX
--         , t2fp  -- convert text to filepath, is t2s
--         , attachToFileName
----         , CV2legal (..)
----         , CV2paths (..)
----        --  , (</>), (<.>)
----         , FNstrings (..)
----         , Hiddings(..)
------         , getFilepathS, dirs2path, makeLegalPath
----        --  , module Uniform.Filenames  -- export only these functions
--        , LegalPathname (..)
----        , unLegalPathname, unL, makeLegalPath
--        , LegalFilename
----            (..), unLegalFilename, makeLegalFilename
--        , LegalExtension
--            --(..), makeLegalExtension
----        , attachToFileName
----        , getFilenameS
----        , FPtype   -- should not be used
----        -- , FPtype
----         , module Uniform.FileStatus
----         , P.FileStatus
--         , module Uniform.FileStrings
--         , module Uniform.TypedFile

--         , getModificationTime
--         , P.FileStatus
--         , pipedDo, pipedDoIO
--         , getRecursiveContents
         , htf_thisModulesTests
--         ,legalpathnameConstructor
                -- LegalPathname is not available as a constructor

            ) where

--import qualified Data.Text as T
import qualified System.Posix          as P (FileStatus)
--import qualified System.Directory as S
--import System.IO (Handle, IOMode (..))

-- using uniform:
import           Uniform.Error
import           Uniform.FileIOalgebra
import           Uniform.Filenames
import           Uniform.FileStatus
import           Uniform.FileStrings
import           Uniform.Piped
import           Uniform.Strings hiding ((</>), (<.>))
import           Uniform.TypedFile

import           Uniform.Zero

import           Test.Framework
import           Test.Invariant


instance CharChains2 FilePath Text where
    show' = s2t


--import           Control.Arrow         (first, second)
--import qualified System.FilePath       as OS

---- | operations on files and directories
---- the doesXX do not produce any exceptiosn
-- is polymorph either in LegalFilename or in RawFilePath (i.e. bytestring )

--legalpathnameConstructor  :: Text -> LegalPathname
--legalpathnameConstructor = LegalPathname
