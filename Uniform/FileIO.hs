----------------------------------------------------------------------
--
-- Module      :  FileIO
-- Copyright   :  andrew u frank -
--
-- | the basic file io - translated for the Either or ErrorT signaling style
-- uses the Path and Path.IO framework

-- this is the general export

----------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- {-# LANGUAGE DeriveAnyClass          #-}


module Uniform.FileIO (
        module Uniform.Filenames
         , module Uniform.FileStatus
         , module Uniform.FileIOalgebra
         , module Uniform.TypedFile
         , module Uniform.FileStrings
         , module Uniform.Piped
         , module Uniform.PathShowCase
         , Path.IO.getAppUserDataDir
         , Path.IO.doesFileExist  --works in IO, not ErrIO
            ) where

import           Uniform.FileIOalgebra -- hiding ((<.>), (</>))
import           Uniform.Filenames
import           Uniform.FileStatus
import           Uniform.FileStrings
import           Uniform.Piped
import           Uniform.TypedFile
import Uniform.PathShowCase()
import qualified Path.IO (makeAbsolute, getAppUserDataDir, doesFileExist)

-- import UniformBase

data Aby40 = Aby40 Int (Path Abs Dir)  deriving (Eq, Ord, Show, Read)