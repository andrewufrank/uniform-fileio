-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import Test.Framework
import {-@ HTF_TESTS @-} Uniform.FileIO
import {-@ HTF_TESTS @-} Uniform.FileStrings
--import {-@ HTF_TESTS @-} FileIO.Text
--import {-@ HTF_TESTS @-} FileIO.ByteString
--import {-@ HTF_TESTS @-} TestingFileIO
import Uniform.Strings
--import Uniform.Error

--import TestingFileIO

test_fileio = assertBool False

main :: IO ()
main = do

    putIOwords ["HTF LayoutTest.hs:\n posTest"]
--    htfMainWithArgs ["--quiet"] htf_importedTests
    htfMain   htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    runTest test_fileio

--    r <- runErr fileioTest2
--    v1 ::[Bool] <- case r of
--        Left msg -> do
--                putIOwords ["fileioTest returned Left :", msg]
--                return [False]
--        Right v -> return v
--
--    let res =  v1
--    putIOwords ["result", unwords . map show' $ res]
    return ()






