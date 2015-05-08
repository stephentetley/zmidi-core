{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core
-- Copyright   :  (c) Stephen Tetley 2010-2013
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Common interface to @ZMidi.Core@.
--
-- This is a /shim/ module re-exporting types and functions from
-- the exposed ZMidi-Core modules. Just import this module to use 
-- ZMidi-Core. 
--
--------------------------------------------------------------------------------


module ZMidi.Core
  (

    module ZMidi.Core.Canonical
  , module ZMidi.Core.Datatypes
  , module ZMidi.Core.Pretty
  , module ZMidi.Core.Pretty.Ascii
  , module ZMidi.Core.Pretty.Csv
  , module ZMidi.Core.ReadFile
  , module ZMidi.Core.VersionNumber
  , module ZMidi.Core.WriteFile
      
  ) where

import ZMidi.Core.Canonical
import ZMidi.Core.Datatypes
import ZMidi.Core.Pretty
import ZMidi.Core.Pretty.Ascii
import ZMidi.Core.Pretty.Csv
import ZMidi.Core.ReadFile
import ZMidi.Core.VersionNumber
import ZMidi.Core.WriteFile
