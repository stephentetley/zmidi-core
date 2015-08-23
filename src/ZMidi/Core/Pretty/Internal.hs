{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Pretty.Internal
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Helper functions to pretty print MIDI as text.
--
-- The functionality is unstable and may change between 
-- releases however it is still exposed as it may be useful
-- for writing a custom pretty printer. 
--
--------------------------------------------------------------------------------


module ZMidi.Core.Pretty.Internal
  (

    midi_header_columns
  , tellMidiHeader

  , byteList
  , safeString
  , textType

  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.SimpleFormat


import Data.Char
#ifndef MIN_VERSION_GLASGOW_HASKELL
import Data.Monoid
#endif
import Data.Word

-- | Column specs for Header - Header is printed as simple 
-- name-value pairs (2 columns).
--
midi_header_columns :: ColumnSpecs 
midi_header_columns = ColumnSpecs '|' [ PadR 21, PadR 38 ] 

-- | Log the MidiHeader in the Table monad (cf. Writer).
--
tellMidiHeader :: MidiHeader -> Table ()
tellMidiHeader (MidiHeader fmt tcount td) = 
    localColumns midi_header_columns $ do
      tellRow $ \_ _ -> [ text "MIDI Format",      ppFormat fmt ]
      tellRow $ \_ _ -> [ text "Number of tracks", integral tcount ]
      tellRow $ \_ _ -> [ text "Time Division",    ppTimeDivision td ]


-- | Decode MidiFormat.
--
ppFormat :: MidiFormat -> WString
ppFormat MF0  = text "Type 0 MIDI File"
ppFormat MF1  = text "Type 1 MIDI File"
ppFormat MF2  = text "Type 2 MIDI File"

-- | Decode TimeDivision.
--
ppTimeDivision :: MidiTimeDivision -> WString
ppTimeDivision (FPS i)   = text "FPS"   <+> integral i
ppTimeDivision (TPB i)   = text "Ticks" <+> integral i


--------------------------------------------------------------------------------
-- Helpers


-- | Print a short byte-list as Hex. Byte-lists longer than 
-- 10 chars are printed as ellipses.
--
byteList :: (Show a, Integral a) => a -> [Word8] -> WString
byteList n ws | n < 10    = integral n <+> mconcat (map hex2 ws)
              | otherwise = integral n <+> repeatChar 10 '.'


-- | Decode Text Type
--
textType :: MidiTextType -> String
textType GENERIC_TEXT         =  "generic-text" 
textType COPYRIGHT_NOTICE     =  "copyright-notice"  
textType SEQUENCE_NAME        =  "sequence-name"
textType INSTRUMENT_NAME      =  "instrument-name"
textType LYRICS               =  "lyrics"
textType MARKER               =  "marker"
textType CUE_POINT            =  "cue-point"
  

-- | Make a string safe for stdout.
-- 
-- This is a temporary hack - characters above ASCII 163
-- cause an (invalid character) error when written to stdout 
-- on Windows (Cygwin).
--
safeString :: String -> String
safeString = map (f . ord) 
  where
    f i | i < 164   = chr i
        | otherwise = '#'

