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
        --  , module Uniform.Error
        --  , module Uniform.Strings
         , module Uniform.FileStatus
         , module Uniform.FileIOalgebra
         , module Uniform.TypedFile
         , module Uniform.FileStrings
         , module Uniform.Piped
--         , Handle, IOMode (..)
--        , EpochTime
            ) where

--import qualified Data.Text as T
import qualified System.Posix          as P (FileStatus)
--import qualified System.Directory as S
--import System.IO (Handle, IOMode (..))

-- using uniform:
-- import           Uniform.Error
import           Uniform.FileIOalgebra hiding ((<.>), (</>))
import           Uniform.Filenames
import           Uniform.FileStatus
import           Uniform.FileStrings
import           Uniform.Piped
-- import           Uniform.Strings hiding ((</>), (<.>))
import           Uniform.TypedFile

import           Uniform.Zero

import           Test.Framework
import           Test.Invariant


--instance CharChains2 FilePath Text where
--    show' = s2t
-- now in stringUtilities



--import           Control.Arrow         (first, second)
--import qualified System.FilePath       as OS

---- | operations on files and directories
---- the doesXX do not produce any exceptiosn
-- is polymorph either in LegalFilename or in RawFilePath (i.e. bytestring )

--legalpathnameConstructor  :: Text -> LegalPathname
--legalpathnameConstructor = LegalPathname
