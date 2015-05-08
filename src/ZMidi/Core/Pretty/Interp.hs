{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Pretty.Interp
-- Copyright   :  (c) Stephen Tetley 2013
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


module ZMidi.Core.Pretty.Interp
  (

    ScaleMap
  , scale_map

  , majorScaleName
  , minorScaleName

  , midiScaleName

  , simpleNoteName

  , timeSignature
  , timeSignatureName

  ) where


import ZMidi.Core.Datatypes

import qualified Data.IntMap as IM
import Data.Ratio


-- | Representation of scales mapping the number of accidentals 
-- to (major,minor) key names.
--
type ScaleMap = IM.IntMap (String,String)

-- | Populated ScaleMap.
--
-- Positive numbers are number of sharps
-- Negative numbers are number of flats.
--
scale_map :: ScaleMap
scale_map = IM.fromList $ 
    [ (0,  ("C",  "A" ))
    , (1,  ("G",  "E" ))
    , (2,  ("D",  "B" ))
    , (3,  ("A",  "F#"))
    , (4,  ("E",  "C#"))
    , (5,  ("B",  "G#"))
    , (6,  ("F#", "D#"))
    , (7,  ("C#", "A#"))
    , (-1, ("F",  "D" ))
    , (-2, ("Bb", "G" ))
    , (-3, ("Eb", "C" ))
    , (-4, ("Ab", "F" ))
    , (-5, ("Db", "Bb"))
    , (-6, ("Gb", "Eb"))
    , (-7, ("Cb", "Ab"))
    ] 

-- | Decode major scale name.
--
majorScaleName :: Int -> Maybe String
majorScaleName n = fmap (\(ss,_) -> ss ++ " major") $ IM.lookup n scale_map

   
-- | Decode minor scale name.
--
minorScaleName :: Int -> Maybe String
minorScaleName n = fmap (\(_,ss) -> ss ++ " minor") $ IM.lookup n scale_map


-- | Decode scale name.
--
midiScaleName :: MidiScaleType -> Int -> Maybe String
midiScaleName (MAJOR) n = majorScaleName n
midiScaleName (MINOR) n = minorScaleName n
midiScaleName _       _ = Nothing



-- | Decode simple note name.
--
-- Follows the example of the book /Beyond MIDI/ - there is 
-- no enharmonic spelling, all black key notes are named as 
-- their respective sharp note.
-- 
simpleNoteName :: Int -> String
simpleNoteName n = 
    let (o,l) = n `divMod` 12 
        ove   = show $ o - 1
    in pch l ++ ove
  where
    pch  0 = "C"
    pch  1 = "C#"
    pch  2 = "D"
    pch  3 = "D#"
    pch  4 = "E"
    pch  5 = "F"
    pch  6 = "F#"
    pch  7 = "G"
    pch  8 = "G#"
    pch  9 = "A"
    pch 10 = "A#"
    pch 11 = "B"
    pch  _ = "ERROR - simpleNoteName - impossible "


-- | Decode a time signature.
-- 
-- Returned as (numerator, denoimator) pair.
--               
timeSignature :: Int -> Int -> (Int,Int)
timeSignature nn dd = (nn, d1)
  where
    ddi :: Integer
    ddi = fromIntegral dd
    d1  :: Int
    d1  = fromIntegral $ denominator $ (2::Rational) ^^ (negate ddi)


-- | Decode a time signature - and print.
--
timeSignatureName :: Int -> Int -> String
timeSignatureName nn dd = 
    let (ni,di) = timeSignature nn dd in show ni ++ "/" ++ show di