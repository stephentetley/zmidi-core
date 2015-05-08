{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Canonical
-- Copyright   :  (c) Stephen Tetley 2012-2013
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (at least generalized newtype deriving)
--
-- Convert a MidiFile into \"canonical\" form - i.e. expand 
-- any use of Running Status and translate Running Status high, 
-- NoteOn channel velocity 0 events to NoteOff events.
--
--------------------------------------------------------------------------------


module ZMidi.Core.Canonical
  (

    canonical

  ) where

import ZMidi.Core.Datatypes


-- | Convert an MidiFile into \"canonical\" form where any 
-- abbreviation introduced by Running Status is expanded.
--
-- Note - even with Running Status on the syntax tree is almost 
-- canonical (some expansion takes place in the Parser), so this
-- translation is quite simplistic.
--
canonical :: MidiFile -> MidiFile
canonical (MidiFile hdr trks) = MidiFile hdr (map transTrack trks)

transTrack :: MidiTrack -> MidiTrack
transTrack (MidiTrack msgs) = MidiTrack $ map transMsg msgs

transMsg :: MidiMessage -> MidiMessage
transMsg (dt,e) = (dt, transEvent e)

transEvent :: MidiEvent -> MidiEvent
transEvent (VoiceEvent rs e) = VoiceEvent RS_OFF (transVoiceEvent rs e)
transEvent evt               = evt


transVoiceEvent :: MidiRunningStatus -> MidiVoiceEvent -> MidiVoiceEvent
transVoiceEvent RS_ON (NoteOn ch pch 0) = NoteOff ch pch 0
transVoiceEvent _     evt               = evt





