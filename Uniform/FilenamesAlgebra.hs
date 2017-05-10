-----------------------------------------------------------------------------
--
-- Module      :  FilenamesAlgebra
-- Copyright   :  andrew u frank -
--
-- | the operations on filenames and extensions
-- properties but test need instantiation

-- does not work unless i use a phantom first argument
-- change to use only the operations here outside
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FilenamesAlgebra (

         Filepathes (..)
         , module Uniform.Error
         , module Uniform.Zero
         , module Uniform.Strings
--          , htf_thisModulesTests
             ) where
import           Uniform.Error
import           Uniform.Strings       (s2t, showT, t2s, removeChar)
import           Uniform.Zero

--import Test.Framework


class Filepathes fp    where
-- ^ a class for operations on filepathes and
    type FN fp  -- the filename proper
    type Ext fp  -- the extension

    -- the mkXX interface call error when illegal input
    -- the option to correct is .. TODO
    mkFilepath :: fp -> Text -> fp
    -- ^ make a filepath of the type of the phantom
    mkFilename :: fp -> Text -> FN fp
    -- ^ make a filename of the type of the phantom
    mkExtension :: fp -> Text -> Ext fp
    -- ^ make an extensio of the type of the phantom
    -- inverses:
    filepath2text :: fp -> fp -> Text
    -- ^ returns the full path, with filename and extension
    -- phantom is not required, but added for uniformity
    -- not just the dir
    filename2text :: fp -> FN fp -> Text
    -- ^   filename as text
    extension2text :: fp -> Ext fp -> Text
    -- ^   extension as text

    prop_filename :: fp -> Bool
--    prop_filename ph = inverse (mkFilepath ph) (filepath2text ph)

    takeDir :: fp -> fp
    takeDir = fst3 . splitFilepath
    takeFilename :: fp -> FN fp
    takeExtension :: fp -> Ext fp
    takeExtension  = thd3 . splitFilepath
    -- take only the last extension (not all)
    hasExtension :: Text -> fp -> Bool
    hasExtension e fp = (e==) . extension2text fp . takeExtension $ fp


    splitFilepath :: fp -> (fp, FN fp, Ext fp)
    combineFilepath :: fp -> FN fp -> Ext fp  -> fp

    splitDirectories :: fp -> [FN fp]
    --
    addFn, (</>) :: fp -> FN fp -> fp
    (</>) = addFn
    addExt :: fp -> FN fp -> Ext fp -> FN fp
    -- cannot be inline, because phantom is required
    isHidden :: fp -> Bool

