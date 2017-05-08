-----------------------------------------------------------------------------
--
-- Module      :    tests for typedFiles
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}

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

import           Uniform.Strings
import Uniform.FileIO
-- import           Test.Framework
-- import           Uniform.TypedFile


main :: IO ()
main = do
    putIOwords ["HTF TypeFilesTest.hs:\n main"]
    -- r <- htfMainWithArgs ["--quiet"] htf_importedTests
    r <- runErr $ test_filewriteText
    putIOwords ["HTF end TypeFilesTest.hs:\n main", showT r]
    case r of
            Left msg -> errorT   ["test failed (main)" , msg]
            Right _  -> return ()
    -- return ()


test_filewriteText ::  ErrIO Bool
test_filewriteText = do
    putIOwords ["test_filewriteText"]
    fp <- makeLegalPath "/home/frank/Workspace8/testfolderForFileIO"
    fn <- makeLegalFilename "textInOne"
    let txtFile = mkTypedFile :: TypedFile Text
    let cont = "abc"
    write4 fp fn txtFile cont
    r <- read4 fp fn txtFile
    let res =  r == cont
    putIOwords ["test_filewrite txt read", r , showT res]

    putIOwords ["test_filewriteLines"]
    fnl <- makeLegalFilename "textInLines"
    let textFile = mkTypedFile :: TypedFile [Text]
    let ls = ["abc","def","ghi"]
    write4 fp fnl textFile ls
    ls2 <- read4 fp fnl textFile
    let res2 =  ls == ls2
    putIOwords ["test_filewrite text read", showT ls , showT ls2 ]
    if (res && res2) then return True else throwErrorT ["the read text was not the same"]
    -- return False
