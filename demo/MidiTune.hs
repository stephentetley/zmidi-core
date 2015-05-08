{-# OPTIONS -Wall #-}

--
-- Write a MIDI \"tune\".
--
-- The MIDI AST should be considered too low level to work with 
-- directly...
--

module Main where

import ZMidi.Core

import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeMidi "./out/midi_tune.mid" midi_tune01
    putStrLn "Wrote file: ./out/midi_tune.mid"
    

midi_tune01 :: MidiFile
midi_tune01 = MidiFile
    { mf_header = MidiHeader { hdr_format    = MF1
                             , num_tracks    = 2
                             , time_division = TPB 480
                             }
    , mf_tracks = [ meta_track, sound_track ]
    }
  where
    meta_track  = MidiTrack [ (0, MetaEvent $ TextEvent SEQUENCE_NAME "Track 0")
                            , (0, MetaEvent $ EndOfTrack) 
                            ]

    sound_track = MidiTrack [ (0, MetaEvent $ TextEvent SEQUENCE_NAME "Track 1")
                            , (0, MetaEvent $ SetTempo 500000)
                            , (0,   VoiceEvent RS_OFF $ NoteOn  0 60 127)
                            , (480, VoiceEvent RS_OFF $ NoteOff 0 60 15)
                            , (0,   VoiceEvent RS_OFF $ NoteOn  0 62 127)
                            , (480, VoiceEvent RS_OFF $ NoteOff 0 62 15)
                            , (0,   VoiceEvent RS_OFF $ NoteOn  0 64 127)
                            , (480, VoiceEvent RS_OFF $ NoteOff 0 64 15)
                            , (0,   VoiceEvent RS_OFF $ NoteOn  0 66 127)
                            , (480, VoiceEvent RS_OFF $ NoteOff 0 66 15)
                            , (0, MetaEvent $ EndOfTrack) 
                            ]


