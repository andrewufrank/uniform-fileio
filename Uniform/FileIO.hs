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

module Uniform.FileIO (
        module Uniform.Filenames
         , module Uniform.FileStatus
         , module Uniform.FileIOalgebra
         , module Uniform.TypedFile
         , module Uniform.FileStrings
         , module Uniform.Piped
         , Path.IO.getAppUserDataDir
         , Path.IO.doesFileExist  --works in IO, not ErrIO
            ) where

import           Uniform.FileIOalgebra -- hiding ((<.>), (</>))
import           Uniform.Filenames
import           Uniform.FileStatus
import           Uniform.FileStrings
import           Uniform.Piped
import           Uniform.TypedFile
import qualified Path.IO (makeAbsolute, getAppUserDataDir, doesFileExist)
