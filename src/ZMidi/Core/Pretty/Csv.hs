{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Pretty.Csv
-- Copyright   :  (c) Stephen Tetley 2013
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Pretty print MIDI to CVS format based on @midicsv@.
--
--------------------------------------------------------------------------------


module ZMidi.Core.Pretty.Csv
  (

    putCsv
  , printCsv

  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.SimpleFormat
import ZMidi.Core.Pretty.Internal


import Data.Monoid


-- | Print the MIDI file to stdout (CSV format).
--
-- One event is printed per line, so the output may be large.
--
putCsv :: MidiFile -> IO ()
putCsv = mapM_ putStrLn . printCsv



-- | Print the MIDI file to a list of Strings (CSV format).
--
-- Results are returned as a list of String to avoid extraneous
-- concatenation.
--
printCsv :: MidiFile -> [String]
printCsv (MidiFile hdr tracks) = execTable body_columns $ do 
    tellHeader hdr
    mapM_ tellTrack tracks




--------------------------------------------------------------------------------

infixr 6 <%>

-- | Concatenate with a comma-space.
--
(<%>) :: WString -> WString -> WString
a <%> b = a <> char ',' <+> b


-- | Tell MidiHeader.
--
tellHeader :: MidiHeader -> Table ()
tellHeader (MidiHeader fmt ntrks tdiv) = 
    tellFree $ \track_num acctime -> 
        int track_num <%> integral acctime   <%> text "Header" 
                      <%> int (fromEnum fmt) <%> integral ntrks 
                      <%> division tdiv
  where
    division (FPS n) = integral n
    division (TPB n) = integral n



-- | Tell a track.
--
tellTrack :: MidiTrack -> Table ()
tellTrack (MidiTrack xs) = nextTrack >> mapM_ message xs
    

-- | Body is free-text (not tabular) so specify and arbitrary 
-- long column.
--
body_columns :: ColumnSpecs
body_columns = 
    ColumnSpecs ' ' [ PadR 60 ]


-- | Log a message.
--
message :: MidiMessage -> Table ()
message (delta,evt) = do
    incrDelta $ fromIntegral delta
    tellFree $ \track_num acctime -> 
        int track_num <%> integral acctime <%> ppEvent evt


-- | Decode an Event.
--
ppEvent :: MidiEvent -> WString
ppEvent (MidiEventOther _)    = text "Unrecognized_event_other"
ppEvent (VoiceEvent _ e)      = ppVoiceEvent e
ppEvent (SysExEvent e)        = ppSysExEvent e
ppEvent (SysCommonEvent e)    = ppSysCommonEvent e
ppEvent (SysRealTimeEvent e)  = ppSysRealTimeEvent e
ppEvent (MetaEvent e)         = ppMetaEvent e

-- | Decode a VoiceEvent.
--
ppVoiceEvent :: MidiVoiceEvent -> WString
ppVoiceEvent (Controller c n v)     = 
    text "Control_c" <%> integral c <%> integral n <%> integral v

ppVoiceEvent (ProgramChange c n)    =
    text "Program_c" <%> integral c <%> integral n

ppVoiceEvent (NoteOff c n v)         =
    text "Note_off_c" <%> integral c <%> integral n <%> integral v

ppVoiceEvent (NoteOn c n v)          =
    text "Note_on_c" <%> integral c <%> integral n <%> integral v

ppVoiceEvent (NoteAftertouch c n v)  =
    text "Poly_aftertouch_c" <%> integral c <%> integral n <%> integral v

ppVoiceEvent (ChanAftertouch c v)    =
    text "Channel_aftertouch_c" <%> integral c <%> integral v

ppVoiceEvent (PitchBend c v)         = 
    text "Pitch_bend_c" <%> integral c <%> integral v



-- | Decode a SysEx Event.
--
ppSysExEvent :: MidiSysExEvent -> WString
ppSysExEvent (SysExSingle n _) = 
    text "System_exclusive" <%> integral n <%> text "..."

ppSysExEvent (SysExCont n _ _)  =
    text "System_exclusive_continuation" <%> integral n <%> text "..."

ppSysExEvent (SysExEscape n _)  =
    text "System_exclusive_escape" <%> integral n <%> text "..."


-- | Decode a SysCommon Event.
--
ppSysCommonEvent :: MidiSysCommonEvent -> WString
ppSysCommonEvent (QuarterFrame sb)       = 
    text "Quarter_frame_s" <%> integral sb

ppSysCommonEvent (SongPosPointer a b)    =
    text "Song_position_pointer_s" <%> integral a <%> integral b
           
ppSysCommonEvent (SongSelect w)          =
    text "Song_select_s" <%> integral w

ppSysCommonEvent (UndefinedF4)           = 
    text "Undefined_s" <%> text "0xF4"

ppSysCommonEvent (UndefinedF5)           = 
    text "Undefined_s" <%> text "0xF5"

ppSysCommonEvent (TuneRequest)           = 
    text "Tune_request_s"

ppSysCommonEvent (EOX)                   = 
    text "EOX_s"



-- | Decode a SysRealTime Event.
--
ppSysRealTimeEvent :: MidiSysRealTimeEvent -> WString
ppSysRealTimeEvent (TimingClock)       = text "Timing_clock_s"
ppSysRealTimeEvent (UndefinedF9)       = text "Undefined_s" <%> text "0xF9"
ppSysRealTimeEvent (StartSequence)     = text "Start_sequence_s" 
ppSysRealTimeEvent (ContinueSequence)  = text "Continue_sequence_s"
ppSysRealTimeEvent (StopSequence)      = text "Stop_sequence_s"
ppSysRealTimeEvent (UndefinedFD)       = text "Undefined_s" <%> text "0xFD"
ppSysRealTimeEvent (ActiveSensing)     = text "Active_sensing_s"
ppSysRealTimeEvent (SystemReset)       = text "System_reset_s"

-- | Decode a Meta Event.
--
ppMetaEvent :: MidiMetaEvent -> WString
ppMetaEvent (TextEvent ty s)          = ppTextEvent ty s
ppMetaEvent (SequenceNumber w)        = text "Sequence_number" <%> integral w
ppMetaEvent (ChannelPrefix ch)        = text "Channel_prefix" <%> integral ch
ppMetaEvent (MidiPort w)              = text "MIDI_port" <%> integral w
ppMetaEvent (EndOfTrack)              = text "End_track"
ppMetaEvent (SetTempo w)              = text "Tempo" <%> integral w
ppMetaEvent (SMPTEOffset h m s f sf)  = 
    text "SMTPE_offset" <%> integral h <%> integral m <%> integral s
                        <%> integral f <%> integral sf

ppMetaEvent (TimeSignature n d m t)   = 
    text "Time_signature" <%> integral n <%> integral d 
                          <%> integral m <%> integral t

ppMetaEvent (KeySignature n sc)       = 
    let scale = case sc of { MAJOR -> "major"
                           ; MINOR -> "minor" 
                           ; _ -> "unrecognized" }
    in text "Key_signature" <%> integral n <%> text scale

ppMetaEvent (SSME n _)                = 
    text "Sequencer_specific" <%> integral n <%> text "..."

ppMetaEvent (MetaOther ty len _)      = 
    text "Unknown_meta_event" <%> integral ty <%> integral len <%> text "..."


-- | Avoid pernicious ASCII characters e.g. the copyright symbol.
--
safetext :: String -> WString
safetext = text . safeString

-- | Decode MIDI text.
--
ppTextEvent :: MidiTextType -> String -> WString
ppTextEvent GENERIC_TEXT      s = text "Text_t" <%> safetext s
ppTextEvent COPYRIGHT_NOTICE  s = text "Copyright_t" <%> safetext s
ppTextEvent SEQUENCE_NAME     s = text "Sequence_name_t" <%> safetext s
ppTextEvent INSTRUMENT_NAME   s = text "Instrument_name_t" <%> safetext s
ppTextEvent LYRICS            s = text "Lyric_t" <%> safetext s
ppTextEvent MARKER            s = text "marker" <%> safetext s
ppTextEvent CUE_POINT         s = text "cue-point" <%> safetext s
