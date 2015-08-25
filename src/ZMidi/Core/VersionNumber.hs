{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.VersionNumber
-- Copyright   :  (c) Stephen Tetley 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Version number
--
--------------------------------------------------------------------------------

module ZMidi.Core.VersionNumber
  ( 
    zmidi_core_version

  ) where

-- | Version number
--
-- > (0,8,1)
--
zmidi_core_version :: (Int,Int,Int)
zmidi_core_version = (0,8,1)
