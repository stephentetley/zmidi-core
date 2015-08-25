{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Pretty
-- Copyright   :  (c) Stephen Tetley 2010-2013
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Pretty print the MIDI representation.
-- 
-- The output format is lossy - the content of Meta and SysEx 
-- events may be abbreviated. This makes the format unsuitable 
-- as a text representation of MIDI, however it can enable
-- quick /disassembly/ of MIDI files in order to see the note
-- events.
--
--------------------------------------------------------------------------------


module ZMidi.Core.Pretty
  (

    putMidi
  , printMidi
  , printMidiHeader
  , printMidiTrack

  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.SimpleFormat
import ZMidi.Core.Pretty.Internal
import ZMidi.Core.Pretty.Interp

#ifndef MIN_VERSION_GLASGOW_HASKELL
import Data.Monoid
#endif

-- | Print the MIDI file to stdout.
--
-- One event is printed per line, so the output may be large.
--
putMidi :: MidiFile -> IO ()
putMidi = mapM_ putStrLn . printMidi


-- | Print the MIDI file to a list of Strings.
--
-- Results are returned as a list of String to avoid extraneous
-- concatenation.
--
printMidi :: MidiFile -> [String]
printMidi (MidiFile hdr tracks) = execTable body_columns $ do 
    tellBreak
    tellMidiHeader hdr
    mapM_ (\t -> tellBreak >> tellTrack t) $ tracks


-- | Print the MIDI header.
--
-- Results are returned as a list of String to avoid extraneous
-- concatenation.
-- 
printMidiHeader :: MidiFile -> [String]
printMidiHeader (MidiFile hdr _) = execTable body_columns $ tellMidiHeader hdr


-- | Print a track.
--
-- Results are returned as a list of String to avoid extraneous
-- concatenation.
--
printMidiTrack :: Int -> MidiFile -> [String]
printMidiTrack track_num (MidiFile _ trks) = 
    case findTrack track_num trks of
      Just trk -> execTable body_columns $ tellArbTrack track_num trk
      Nothing  -> ["*** ERROR *** cannot find track " ++ show track_num ]
  where
    -- Numbering starts from 1..
    findTrack 1 (t:_)           = Just t
    findTrack n (_:ts) | n > 1  = findTrack (n-1) ts
    findTrack _ _               = Nothing



--------------------------------------------------------------------------------
-- 

-- | Helper for printing arbitrary tracks.
--
tellArbTrack :: Int -> MidiTrack -> Table ()
tellArbTrack track_num (MidiTrack xs) = arbTrack track_num >> mapM_ message xs


-- | Increment the track number (in the monad) and write a track.
--
tellTrack :: MidiTrack -> Table ()
tellTrack (MidiTrack xs) = nextTrack >> mapM_ message xs

    

-- | Column specs for the track tables.
--
body_columns :: ColumnSpecs
body_columns = 
    ColumnSpecs '|' [ PadR 3, PadL 14, PadL 11, PadR 23, PadR 23 ]


-- | Log a message using the internal Writer of the Table monad.
--
message :: MidiMessage -> Table ()
message (delta,evt) = do 
    incrDelta $ fromIntegral delta
    tellRow $ \track_num acctime -> [ integral track_num
                                    , integral acctime
                                    , integral delta
                                    , descEvent evt
                                    , valEvent evt ]
    

--------------------------------------------------------------------------------
-- Description column

-- | Get description text.
--
descEvent :: MidiEvent -> WString
descEvent (MidiEventOther e)    = descDataOther e
descEvent (VoiceEvent rs e)     = descVoiceEvent rs e
descEvent (SysExEvent e)        = descSysExEvent e
descEvent (SysCommonEvent e)    = descSysCommonEvent e
descEvent (SysRealTimeEvent e)  = descSysRealTimeEvent e
descEvent (MetaEvent e)         = descMetaEvent e


-- | Get description text.
--
descDataOther :: MidiDataOther -> WString
descDataOther (MidiDataOther {})  = text "midi data other"


-- | Get description text.
--
descVoiceEvent :: MidiRunningStatus -> MidiVoiceEvent -> WString
descVoiceEvent _  (Controller {})     = text "controller"
descVoiceEvent _  (ProgramChange {})  = text "program-change"
descVoiceEvent _  (NoteOff {})        = text "note-off"  
descVoiceEvent rs (NoteOn _ _ v)      
    | rs == RS_ON && v == 0               = text "note-on *RS,V0*"
    | otherwise                           = text "note-on"


-- | Get description text.
--
descVoiceEvent _  (NoteAftertouch {}) = text "note-aftertouch"
descVoiceEvent _  (ChanAftertouch {}) = text "channel-aftertouch"
descVoiceEvent _  (PitchBend {})      = text "pitch-bend"


-- | Get description text.
--
descSysExEvent :: MidiSysExEvent -> WString
descSysExEvent (SysExSingle {}) = text "sys-ex"
descSysExEvent (SysExCont {})   = text "sys-ex cont"
descSysExEvent (SysExEscape {}) = text "sys-ex F7"


-- | Get description text.
--
descSysCommonEvent :: MidiSysCommonEvent -> WString
descSysCommonEvent (QuarterFrame {})    = text "time-code-1/4-frame"
descSysCommonEvent (SongPosPointer {})  = text "song position ptr"
descSysCommonEvent (SongSelect {})      = text "song-select"
descSysCommonEvent (UndefinedF4)        = text "undefined 0xF4"
descSysCommonEvent (UndefinedF5)        = text "undefined 0xF5"
descSysCommonEvent (TuneRequest)        = text "tune-request"
descSysCommonEvent (EOX)                = text "end-of-sys-ex"


-- | Get description text.
--
descSysRealTimeEvent :: MidiSysRealTimeEvent -> WString
descSysRealTimeEvent (TimingClock)      = text "sysRT timing-clock"
descSysRealTimeEvent (UndefinedF9)      = text "sysRT 0xF9"
descSysRealTimeEvent (StartSequence)    = text "sysRT start seq."
descSysRealTimeEvent (ContinueSequence) = text "sysRT continue"
descSysRealTimeEvent (StopSequence)     = text "sysRT stop seq."
descSysRealTimeEvent (UndefinedFD)      = text "sysRT 0xFD"
descSysRealTimeEvent (ActiveSensing)    = text "sysRT active sensing"
descSysRealTimeEvent (SystemReset)      = text "sysRT reset"


-- | Get description text.
--
descMetaEvent :: MidiMetaEvent -> WString
descMetaEvent (TextEvent ty _)        = text $ textType ty
descMetaEvent (SequenceNumber {})     = text "sequence-number"
descMetaEvent (ChannelPrefix {})      = text "channel-prefix"
descMetaEvent (MidiPort {})           = text "midi-port"
descMetaEvent (EndOfTrack)            = text "end-of-track"
descMetaEvent (SetTempo {})           = text "set-tempo"
descMetaEvent (SMPTEOffset {})        = text "smpte-offest"
descMetaEvent (TimeSignature {})      = text "time-signature"
descMetaEvent (KeySignature {})       = text "key-signature"
descMetaEvent (SSME {})               = text "sequencer-specific"
descMetaEvent (MetaOther {})          = text "meta-other"


--------------------------------------------------------------------------------
-- Value column

-- | Get formatted value.
--
valEvent :: MidiEvent -> WString
valEvent (MidiEventOther e)    = valDataOther e
valEvent (VoiceEvent _ e)      = valVoiceEvent e
valEvent (SysExEvent e)        = valSysExEvent e
valEvent (SysCommonEvent e)    = valSysCommonEvent e
valEvent (SysRealTimeEvent e)  = valSysRealTimeEvent e
valEvent (MetaEvent e)         = valMetaEvent e


-- | Get formatted value.
--
valDataOther :: MidiDataOther -> WString
valDataOther (MidiDataOther n)   = hex2 n


-- | Get formatted value.
--
valVoiceEvent :: MidiVoiceEvent -> WString
valVoiceEvent (Controller c n v)      = hex2 c <+> hex2 n <+> hex2 v
valVoiceEvent (ProgramChange c n)     = hex2 c <+> hex2 n
valVoiceEvent (NoteOff c n v)         = hex2 c <+> hex2 n <+> hex2 v
valVoiceEvent (NoteOn c n v)          = hex2 c <+> hex2 n <+> hex2 v
valVoiceEvent (NoteAftertouch c n v)  = hex2 c <+> hex2 n <+> hex2 v
valVoiceEvent (ChanAftertouch c v)    = hex2 c <+> hex2 v
valVoiceEvent (PitchBend c v)         = hex2 c <+> hex4 v


-- | Get formatted value.
--
valSysExEvent :: MidiSysExEvent -> WString
valSysExEvent (SysExSingle n ws) = byteList n ws
valSysExEvent (SysExCont n ws _) = byteList n ws
valSysExEvent (SysExEscape n ws) = byteList n ws


-- | Get formatted value.
--
valSysCommonEvent :: MidiSysCommonEvent -> WString
valSysCommonEvent (QuarterFrame sb)       = hex2 sb
valSysCommonEvent (SongPosPointer a b)    = hex2 a <+> hex2 b
valSysCommonEvent (SongSelect w)          = hex2 w
valSysCommonEvent (UndefinedF4)           = mempty
valSysCommonEvent (UndefinedF5)           = mempty
valSysCommonEvent (TuneRequest)           = mempty
valSysCommonEvent (EOX)                   = mempty


-- | Get formatted value.
--
valSysRealTimeEvent :: MidiSysRealTimeEvent -> WString
valSysRealTimeEvent _                   = mempty


-- | Get formatted value.
--
valMetaEvent :: MidiMetaEvent -> WString
valMetaEvent (TextEvent _ s)           = text $ safeString s
valMetaEvent (SequenceNumber w)        = hex4 w
valMetaEvent (ChannelPrefix ch)        = hex2 ch
valMetaEvent (MidiPort w)              = hex2 w
valMetaEvent (EndOfTrack)              = mempty
valMetaEvent (SetTempo w)              = integral w
valMetaEvent (SMPTEOffset h m s f sf)  = mconcat $ map hex2 [h,m,s,f,sf]
valMetaEvent (TimeSignature n d m t)   = 
      let tsig = text $ timeSignatureName (fromIntegral n) (fromIntegral d)
      in tsig <+> hex2 m <+> hex2 t

valMetaEvent (KeySignature n sc)       = 
      maybe (text "unrecognized") text $ midiScaleName sc (fromIntegral n)

valMetaEvent (SSME n ws)               = byteList n ws
valMetaEvent (MetaOther ty len ws)     = hex2 ty <+> byteList len ws

