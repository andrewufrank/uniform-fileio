-----------------------------------------------------------------------------
--
-- Module      :   tests
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

module TestingFileIO  (htf_thisModulesTests

            )   where


--import System.Exit
import Uniform.FileIO
import Uniform.FileStrings

--import Uniform.Strings
--import Uniform.Piped
--import Uniform.FileStatus

import Uniform.Strings hiding ((</>))
import Uniform.Error
import Test.Framework
--import {-@ HTF_TESTS @-} Uniform.Error
import Data.List

test_testingFileIO :: IO ()  -- runs automatically by HTF
test_testingFileIO = do
    r <- fileioTest
    assertEqual  [[True, True, True,
                        True, True, True, True, True]
                     , [True, True]
                        ] r

fileioTest  ::   IO [[Bool]]
fileioTest = do
    r <- runErr fileioTest2
    v1 <- case r of
        Left msg -> do
                putIOwords ["fileioTest returned Left :", msg]
                return [[False]]
        Right v -> return v
    putIOwords ["end fileioTest", unwordsT . map concatT  $ map (map showT) v1]

    r <- runErr fileioTestPipe
    v2<- case r of
        Left msg -> do
                putIOwords ["fileioTestPipe returned Left :", msg]
                return ("Error returned " <> msg)
        Right v -> return v
    putIOwords ["end fileioTestPipe",   v2]

    return v1

txtExt = fromJustNote "extension txt" $ cv2legal "txt" :: LegalExtension
nullExt = fromJustNote "extension null" $ cv2legal "" :: LegalExtension
hiddenExt = fromJustNote "extension hidden" $ cv2legal "hidden" :: LegalExtension

a1fn = fromJustNote "a1" $ cv2legal "a1" :: LegalFilename
a2fn = fromJustNote "a2" $ cv2legal "a2" :: LegalFilename
a3fn = fromJustNote "a3" $ cv2legal "a3" :: LegalFilename
a4fn = fromJustNote "a4" $ cv2legal "a4" :: LegalFilename

subb = fromJustNote "subb" $ cv2legal "sub.d" :: LegalPathname
subf = fromJustNote "sufb" $ cv2legal "xxxsuf.d" :: LegalPathname
subnew = fromJustNote "subnew" $ cv2legal "subnew" :: LegalPathname
hiddensubb = (subb </>) $ mkHiddenS
            (fromJustNote "subb" $ cv2legal "hiddensub.d" :: LegalPathname)

a1fne = a1fn <.> txtExt
a2fne = (a2fn <.> nullExt)
a3fne = a3fn
a4fne = (mkHiddenS $  a4fn <.> hiddenExt)

constructTestDir :: LegalPathname -> ErrIO ()
constructTestDir pathx = do
    deleteDirRecursive pathx
    createDirIfMissing pathx


    writeFileT (pathx </> a1fne)  "a1.txt"
    writeFileT (pathx </> a2fne )  "a2.txt"
    writeFileT (pathx </> a3fne) "a3"
    writeFileT (pathx </> a4fne) ".a4.hidden"


    let path2 = pathx </> subb
    createDirIfMissing path2

    writeFileT (path2 </> a1fne )  "a1.txt"
    writeFileT (path2 </> (a2fne  ) )  "a2.txt"
    writeFileT (path2 </> a3fne) "a3"
    writeFileT (path2 </> a4fne) ".a4.hidden"

    let path3 = pathx </> hiddensubb

    createDirIfMissing path3

    writeFileT (path3 </> a1fne)  "a1.txt"
    writeFileT (path3 </> a2fne )  "a2.txt"
    writeFileT (path3 </> a3fne) "a3"
    writeFileT (path3 </> a4fne) ".a4.hidden"

    return ()


testDirDelete :: LegalPathname -> ErrIO [Bool]
testDirDelete pathx = do
    a1 <- mustReturnTrueB "exist base dir" $ doesDirExist pathx
    a2 <- mustReturnTrueB "exist sub dir" $ doesDirExist (pathx </> subb)
    a3 <- mustReturnFalseB "not exist sub dir"
                                    $ doesDirExist (pathx </> subf)

    b1 <- mustReturnTrueB "exist file" $ doesFileExist (pathx </> subb </> a3fn)
    let px2 =  ((pathx </> subf </> a3fn))
--    putIOwords ["not existing path", show px2]
    b2 <- mustReturnFalseB ("not exist sub dir" )
                    $ doesFileExist px2

    let pathNew = (pathx </> subnew)
    b3 <- mustSucceedM "delete non existing" $ deleteDirRecursive pathNew
    b4 <- mustReturnFalseB "new dir exists" $ doesDirExist pathNew
    createDir pathNew
    b5 <- mustReturnTrueB "new dir exists" $ doesDirExist pathNew

    let r = [a1, a2, a3, b1, b2,b3, b4, b5]
    putIOwords ["result testDirDelete", unwordsT $ map showT r]
    return r

testDirContent :: LegalPathname -> ErrIO [Bool]
testDirContent pathx = do
    let contentX =  map cv2path [a1fne, a2fne, a3fne, a4fne ]
                ++ [subb, subnew]
    let cx2 = sort $ map (pathx </>)   contentX

    c1 <- mustReturnValueMB "retrieve all"  cx2
                $ fmap sort (getDirCont  pathx)
    -- testing
    unless c1 $ do
        s <- fmap sort $ getDirCont pathx
        putIOwords ["\ngetDirCont", unwordsT . map showT  $ s]
        putIOwords ["\nexpected", unwordsT . map showT  $ cx2]

    -- non-hidden:

    let contentNH =  map cv2path [a1fne, a2fne, a3fne ]
                ++ [subb, subnew]
    let cnh = sort $ map (pathx </>)   contentNH

    c2 <- mustReturnValueMB "retrieve all"  cnh
                $ fmap sort (getDirContentNonHidden  pathx)
    -- testing
    unless c2 $ do
        snh <- fmap sort $ getDirContentNonHidden pathx


        putIOwords ["\ngetDirContnh\n", unwordsT . map showT  $ snh]
        putIOwords ["\nexpectedNH\n", unwordsT . map showT  $ cnh]

    let r = [c1, c2]
    putIOwords ["\nresult testDirContent", unwordsT $ map showT  r]
    return r

testRename :: LegalPathname -> ErrIO [Bool]
testRename pathx = do


    let r = [True]
    putIOwords ["result testDirContent", unwordsT $ map showT r]
    return r

fileioTest2 ::   ErrIO [[Bool]]
-- examples how to use
fileioTest2 = do
    putIOwords ["\nstart fileioTest"]
    tp2 <- makeLegalPath "/home/frank/testFileIO"
    constructTestDir tp2
    putIOwords ["constructed" ]
    x2 <- testDirDelete tp2
    putIOwords ["testDirDelete done" ]
    x3 <- testDirContent tp2
    putIOwords ["testDirContent done"]


    putIOwords ["end fileioTest", unwordsT. map showT $ x2]
    return [x2, x3]

fileioTestPipe ::   ErrIO Text
-- examples how to use
fileioTestPipe = do
    putIOwords ["output from pipedDo"]
    fp2 <- makeLegalPath "/home/frank/Scratch/dirList_fileio.piped_test"
    tp2 <- makeLegalPath "//home/frank/Workspace8/testfolderForFileIO"
--    let f1 = dirs2path "/home/frank/Scratch/dirList_fileio.piped_test"
--    let f2 = fromJustNote "input f1 is not ok" f1
--    let tp1 = dirs2path "//home/frank/Workspace8/testfolderForFileIO"
--    let tp2 = fromJustNote "input tp2is not ok" tp1
    pipedDo tp2 (\p -> unwordsT ["fileioTestPipe: Filename is ", unLegalPathname $ p])
    putIOwords ["start list with md5"]
    pipedDoIO fp2 tp2 mkFN_md5
    putIOwords ["end"]
    return "OK"

mkFN_md5:: LegalPathname -> ErrIO Text
mkFN_md5 pn = do
-- needs check for link and other non regular files
--    mstatus <- getSymbolicLinkStatus pn
--    let status = fromJustNote "mkFN_md5 xx33" $ mstatus
--    let regular = isRegularFile status
----    let link = isSymbolicLink status
--    if regular then
--        do
            mmd <- getMD5 pn
            case mmd of
                Just md -> do
                        return . unwordsT $ [unLegalPathname pn, md]
                Nothing -> return "NOT REGULAR FILE"


--        else fail "is not regular file"


