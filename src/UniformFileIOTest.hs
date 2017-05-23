-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import            Test.Framework
-- import    {-@ HTF_TESTS @-}        Uniform.FileIO
import     {-@ HTF_TESTS @-}       Uniform.FileStrings
----import {-@ HTF_TESTS @-} FileIO.Text
----import {-@ HTF_TESTS @-} FileIO.ByteString
----import {-@ HTF_TESTS @-} TestingFileIO
import     {-@ HTF_TESTS @-}       Uniform.Filenames
import    {-@ HTF_TESTS @-}        Uniform.FileStatus
import     {-@ HTF_TESTS @-}       Uniform.Piped
import    {-@ HTF_TESTS @-}        Uniform.TypedFile

import           Uniform.Strings

--import TestingFileIO

test_fileio = assertBool False

main :: IO ()
main = do

    putIOwords ["HTF LayoutTest.hs:\n posTest"]
--    htfMainWithArgs ["--quiet"] htf_importedTests
    htfMain   htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    runTest test_fileio
    return ()
