{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Pretty.Ascii
-- Copyright   :  (c) Stephen Tetley 2013-2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Pretty print MIDI in format based on the /ASCII format/
-- in the book /Beyond MIDI/.
-- 
--------------------------------------------------------------------------------


module ZMidi.Core.Pretty.Ascii
  (

    putAscii
  , printAscii

  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.SimpleFormat
import ZMidi.Core.Pretty.Internal
import ZMidi.Core.Pretty.Interp

#ifndef MIN_VERSION_GLASGOW_HASKELL
import Data.Monoid
#endif
import Data.Word


-- | Print the MIDI file to stdout (ASCII format based on 
-- output in the book Beyond MIDI).
--
-- One event is printed per line, so the output may be large.
--
putAscii :: MidiFile -> IO ()
putAscii = mapM_ putStrLn . printAscii


-- | Print the MIDI file to a list of Strings (ASCII format based
-- on output in the book Beyond MIDI).
--
-- Results are returned as a list of String to avoid extraneous
-- concatenation.
--
printAscii :: MidiFile -> [String]
printAscii (MidiFile hdr tracks) = execTable body_columns $ do 
    tellBreak
    tellMidiHeader hdr
    tellBreak
    mapM_ (\t -> columnHeaders >> tellBreak >> tellTrack t >> tellBlank ) 
          tracks




--------------------------------------------------------------------------------


-- | Increment the track number (in the monad) and write a track.
--
tellTrack :: MidiTrack -> Table ()
tellTrack (MidiTrack xs) = nextTrack >> mapM_ message xs
    

-- | Column specs for the track tables.
--
body_columns :: ColumnSpecs
body_columns = 
    ColumnSpecs ' ' [ PadR 3    -- track no
                    , PadR 15   -- event type
                    , PadR 7    -- note name
                    , PadL 7    -- key no
                    , PadL 11   -- delta time
                    , PadL 13   -- elapsed time
                    , PadL 9    -- dynamic level (velocity)  
                    ]
-- Note 
-- Numbers generally look better PadL.


-- | Log a message using the internal Writer of the Table monad.
--
message :: MidiMessage -> Table ()
message (delta,evt) = do 
    incrDelta $ fromIntegral delta
    tellRow $ \track_num acctime -> [ integral track_num
                                    , descEvent evt
                                    , eventNoteName evt
                                    , eventKeyNumber evt
                                    , integral delta
                                    , integral acctime
                                    , eventVelocity evt
                                    ]

-- | Log column headers.
--
columnHeaders :: Table ()
columnHeaders = tellRow $ \_ _ -> 
    map text [ "Trk"
             , "Event Type"
             , "Note" 
             , "Key no"
             , "Delta Time"
             , "Elapsed Time"
             , "Velocity"  
             ]

--------------------------------------------------------------------------------
-- Event type


-- | Get the event type (description).
--
descEvent :: MidiEvent -> WString
descEvent (VoiceEvent rs e)     = descVoiceEvent rs e
descEvent (MetaEvent e)         = descMetaEvent e
descEvent _                     = mempty


-- | Get the event type (description).
--
descVoiceEvent :: MidiRunningStatus -> MidiVoiceEvent -> WString
descVoiceEvent _  (Controller {})     = text "Controller"
descVoiceEvent _  (ProgramChange {})  = text "Program change"
descVoiceEvent _  (NoteOff {})        = text "Note off"  
descVoiceEvent rs (NoteOn _ _ v)      
    | rs == RS_ON && v == 0           = text "Note on*"
    | otherwise                       = text "Note on"

descVoiceEvent _  (NoteAftertouch {}) = text "Note aftertouch"
descVoiceEvent _  (ChanAftertouch {}) = text "Channel aftertouch"
descVoiceEvent _  (PitchBend {})      = text "Pitch bend"


-- | Get the event type (description).
--
descMetaEvent :: MidiMetaEvent -> WString
descMetaEvent (TextEvent ty _)        = text $ textType ty
descMetaEvent (SequenceNumber {})     = text "Sequence number"
descMetaEvent (ChannelPrefix {})      = text "Channel prefix"
descMetaEvent (MidiPort {})           = text "Midi port"
descMetaEvent (EndOfTrack)            = text "End of track"
descMetaEvent (SetTempo {})           = text "Set tempo"
descMetaEvent (SMPTEOffset {})        = text "SMTPE offest"
descMetaEvent (TimeSignature {})      = text "Time signature"
descMetaEvent (KeySignature {})       = text "Key signature"
descMetaEvent (SSME {})               = text "SSME"
descMetaEvent (MetaOther {})          = text "Meta other"


--------------------------------------------------------------------------------
-- Note Name

-- | Get the symbolic note name.
--
eventNoteName :: MidiEvent -> WString
eventNoteName (VoiceEvent _ e)     = voiceEventNoteName e
eventNoteName _                    = mempty

-- | Helper.
--
noteName :: Word8 -> WString
noteName = text . simpleNoteName . fromIntegral

-- | Get the symbolic note name.
--
voiceEventNoteName :: MidiVoiceEvent -> WString
voiceEventNoteName (Controller _ n _)      = noteName n
voiceEventNoteName (ProgramChange _ n)     = noteName n
voiceEventNoteName (NoteOff _ n _)         = noteName n
voiceEventNoteName (NoteOn _ n _)          = noteName n
voiceEventNoteName (NoteAftertouch _ n _)  = noteName n
voiceEventNoteName (ChanAftertouch _ _)    = mempty
voiceEventNoteName (PitchBend _ _)         = mempty


--------------------------------------------------------------------------------
-- Key Number

-- | Get the key number (aka MIDI pitch).
--
eventKeyNumber :: MidiEvent -> WString
eventKeyNumber (VoiceEvent _ e)     = voiceEventKeyNumber e
eventKeyNumber _                    = mempty

-- | Get the key number (aka MIDI pitch).
--
voiceEventKeyNumber :: MidiVoiceEvent -> WString
voiceEventKeyNumber (Controller _ n _)      = integral n
voiceEventKeyNumber (ProgramChange _ n)     = integral n 
voiceEventKeyNumber (NoteOff _ n _)         = integral n 
voiceEventKeyNumber (NoteOn _ n _)          = integral n
voiceEventKeyNumber (NoteAftertouch _ n _)  = integral n
voiceEventKeyNumber (ChanAftertouch _ _)    = mempty
voiceEventKeyNumber (PitchBend _ _)         = mempty

--------------------------------------------------------------------------------
-- Velocity

-- | Get the velocity.
--
eventVelocity :: MidiEvent -> WString
eventVelocity (VoiceEvent _ e)     = voiceEventVelocity e
eventVelocity _                    = mempty


-- | Get the velocity.
--
voiceEventVelocity :: MidiVoiceEvent -> WString
voiceEventVelocity (Controller _ _ v)      = integral v
voiceEventVelocity (ProgramChange _ _)     = mempty
voiceEventVelocity (NoteOff _ _ v)         = integral v 
voiceEventVelocity (NoteOn _ _ v)          = integral v
voiceEventVelocity (NoteAftertouch _ _ v)  = integral v
voiceEventVelocity (ChanAftertouch _ v)    = integral v
voiceEventVelocity (PitchBend _ v)         = integral v
