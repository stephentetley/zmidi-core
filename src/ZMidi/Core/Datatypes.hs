{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010-2013
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (at least generalized newtype deriving)
--
-- Concrete syntax tree for MIDI files.
--
-- Values are sometimes not interpreted. This means that the
-- the data types do not fully represent the sematics of MIDI 
-- events, but all the data is either stored within the data type 
-- or synthesizeable. Hence, @ readFile >>= writeFile @ will 
-- produce an identical binary \[1\]. 
--
-- \[1\] Or it should, failure indicates a bug...
--
--------------------------------------------------------------------------------


module ZMidi.Core.Datatypes 
  (
  -- * MidiFile syntax.
    DeltaTime
  , TagByte

  , MidiFile(..)
  , MidiHeader(..)  
  , MidiTrack(..)
  , MidiFormat(..)
  , MidiRunningStatus(..)
  , MidiMessage
  , MidiEvent(..)

  , MidiDataOther(..)
  , MidiVoiceEvent(..)
  , MidiSysExEvent(..)
  , MidiSysExContPacket(..)
  , MidiSysCommonEvent(..)
  , MidiSysRealTimeEvent(..)
  , MidiMetaEvent(..)
  , MidiTimeDivision(..)
  , MidiTextType(..)
  , MidiScaleType(..)
    
  ) where

import Data.Int
import Data.Word



-- | All time values in a MIDI track are represented as a \delta\ 
-- from the previous event rather than an absolute time. 
--
-- DeltaTime is a newtype wrapper over Word32, note that in MIDI 
-- files it is represented as a @varlen@ to potentially save 
-- space that would otherwise require a four byte number. 
--
newtype DeltaTime = DeltaTime { getDeltaTime :: Word32 }
  deriving (Enum,Eq,Ord,Num,Integral,Real)

instance Show DeltaTime where
  showsPrec p = showsPrec p . getDeltaTime



-- | TagByte is an alias to 'Word8'.
--
type TagByte = Word8


-- | 'MidiFile' : @ header * tracks @
--
data MidiFile = MidiFile 
      { mf_header         :: MidiHeader
      , mf_tracks         :: [MidiTrack]
      }
  deriving (Eq,Show)

-- | 'Header' : @ format * num_tracks * time_division @ 
--
-- 'TimeDivision' is often 384 or 480 ticks per beat.
--
-- The header is the start of a MIDI file, it is indicated by the 
-- 4 character marker @MThd@.   
--
data MidiHeader = MidiHeader 
      { hdr_format        :: MidiFormat
      , num_tracks        :: Word16
      , time_division     :: MidiTimeDivision
      }
  deriving (Eq,Show)

-- | 'Track' : @ [message] @
--
-- In MIDI files, the start of a track is indicated by the 4 
-- character marker @MTrk@.  
--
newtype MidiTrack = MidiTrack { getTrackMessages :: [MidiMessage] }
  deriving (Eq,Show)

-- | The file format - in a MIDI file this is a big-endian 
-- word16 with 0,1 or 2 being the only valid values. 
--
data MidiFormat 
    -- | Format 0 file - single multi-channel track.
    = MF0 
    -- | Format 1 file - 1 or more tracks, played simultaneously.
    | MF1
    -- | Format 2 file - 1 or more independent tracks.
    | MF2
  deriving (Eq, Enum, Show) 

-- | Default unit of time in the MIDI file.
--
data MidiTimeDivision 
    -- | Frames-per-second.
    --
    = FPS Word16
    
    -- | Ticks-per-beat, i.e. the number of units for a quarter 
    -- note.
    --
    | TPB Word16    
  deriving (Eq,Show)
                                             
-- | Enumeration of the text meta event types.
--
data MidiTextType 
    = GENERIC_TEXT 
    | COPYRIGHT_NOTICE 
    | SEQUENCE_NAME 
    | INSTRUMENT_NAME
    | LYRICS 
    | MARKER 
    | CUE_POINT 
  deriving (Eq,Enum,Ord,Show) 


-- | MIDI messages are pairs of 'DeltaTime' and 'Event' wrapped in 
-- a newtype. 
--
-- Sequential messages with delta time 0 are played 
-- simultaneously.  
--
type MidiMessage = (DeltaTime, MidiEvent)


     
-- | Running Status.
--
-- MIDI allows a compact representation of voice events where
-- consecutive events (same event, same channel) only need to
-- include the first event-channel byte - subsequent events 
-- only send payload until the next event or channel change.
--
-- Including @MidiRunningStatus@ in the data representation is 
-- important for ZMidi as an aim is to allow round-tripping
-- of exisiting MIDI files. However it makes MIDI generation
-- more complicated (there is more scope to generate bad 
-- output) - if you are only generating MIDI it is wise to always 
-- set @MidiRunningStatus@ to @RS_OFF@.
-- 
data MidiRunningStatus = RS_ON | RS_OFF
  deriving (Enum,Eq,Ord,Show)


-- | Recognised event types - some types ('MidiEventOther' and 
-- 'SysEx') are not interpreted.
--
data MidiEvent 
    -- | An unrecognized event. This event is not expected in 
    -- well formed MIDI, but the parser may insert it - if it 
    -- encounters ill-formed data.
    --
    = MidiEventOther   MidiDataOther

    -- | Voice event (e.g @note-on@, @note-off@) are relayed to 
    -- specific channels. 
    -- 
    -- Note - they are tagged with Running Status, this is 
    -- pertinent to parsing MIDI where a input stream may use 
    -- running status to save space. If you are generating MIDI
    -- use RunningStatus with caution and ensure that consecutive
    -- events are all of the same sort.
    --
    | VoiceEvent        MidiRunningStatus MidiVoiceEvent


    -- | SysEx - system exclusive event. Usually synthesizer 
    -- specific, not interpreted.
    --
    | SysExEvent        MidiSysExEvent


    -- | SysCommon - system common event.
    --
    | SysCommonEvent    MidiSysCommonEvent


    -- | SysRealTime - system realtime event.
    --
    | SysRealTimeEvent  MidiSysRealTimeEvent


    -- | Meta event - interpreted (e.g. @end-of-track@, 
    -- @set-tempo@).
    --
    | MetaEvent         MidiMetaEvent


  deriving (Eq,Ord,Show)


-- | Data events are events with tags from 0x00 to 0x7F. 
-- 
-- Data events have no payload - they are represented only by the
-- tag byte.  
--
newtype MidiDataOther = MidiDataOther { getMidiDataOther :: TagByte }
  deriving (Eq,Ord,Show)


-- | Voice events control the output of the synthesizer.
--
-- Note - change in v0.5.0 - the constructors have been reordered
-- so the Ord instance matches the order of the /tag/ bytes. Any 
-- code that relied on sorting MIDI events is likely to need 
-- reworking.
--
-- In serialized MIDI data the top 4 bits of the first byte of the 
-- Voice Event are a tag, the bottom 4 bits are the channel 
-- number. ZMidi stores the channel number with a Word8 though 
-- values should be limited to the range 0-15.
--
data MidiVoiceEvent 
    -- | Note off.
    -- 
    -- > 80 to 8F (0 to F is channel number) * note * velocity
    -- 
    -- Turn off a sounding note.
    --
    = NoteOff             Word8 Word8 Word8   

    -- | Note on.
    --
    -- > 90 to 9F (0 to F is channel number) * note * velocity
    -- 
    -- Start playing a note.
    --
    | NoteOn              Word8 Word8 Word8

  
    -- | Polyphonic key pressure.
    --
    -- > A0 to AF (0 to F is channel number) * note * pressure_value
    -- 
    -- Change of pressure applied to the synthesizer key. 
    --
    | NoteAftertouch      Word8 Word8 Word8     


    -- | Set a controller.
    --
    -- > B0 to BF (0 to F is channel number) * controller_number * value 
    -- 
    -- Controller change, e.g. by a footswitch.
    --
    | Controller          Word8 Word8 Word8


    -- | Set the program.
    -- 
    -- > C0 to CF (0 to F is channel number) * program_number 
    --
    -- Change the instrument 
    -- playing on the specified channel. For playback on 
    -- computers (rather than synthesizers) the program numbers
    -- will correspond to the /General MIDI/ instrument numbers.
    --
    | ProgramChange       Word8 Word8


    -- | Channel pressure.
    --
    -- > D0 to DF (0 to F is channel number) * pressure_value
    -- 
    | ChanAftertouch      Word8 Word8


    -- | Pitch bend 
    --
    -- > E0 to EF (0 to F is channel number) * value
    -- 
    -- Change the pitch of a sounding note. Often used to 
    -- approximate microtonal tunings.
    -- 
    -- NOTE - currently value is uninterpreted.
    --
    | PitchBend           Word8 Word16


  deriving (Eq,Ord,Show)

-- | \SysEx\ - system exclusive event. 
--
data MidiSysExEvent
    
    -- | Single SysEx event.
    -- 
    -- > F0 * length * data
    -- 
    -- An uninterpreted sys-ex event.
    --
    = SysExSingle Word32 [Word8]

    -- | SysEx sent as (non-standard) multiple continuation 
    -- packets.
    --
    -- > F0 * length * packet1 ... [SysExContPacket]
    --
    | SysExCont Word32 [Word8] [MidiSysExContPacket]

    -- | Escape sequence of one-or-more SysEx events.
    --
    -- > F7 * length * data
    --
    | SysExEscape Word32 [Word8]
  deriving (Eq,Ord,Show)

-- | Continuation packet for a (non-standard) multi-part SysEx 
-- event.
--
-- Apprently this format is use by Casio. 
--
data MidiSysExContPacket = MidiSysExContPacket DeltaTime Word32 [Word8]
  deriving (Eq,Ord,Show)


-- | System common event.
--
-- Common information for all channels in a system. 
--
-- These events may not be pertinent to MIDI files generated on a 
-- computer (as opposed to MIDI generated by a synthesizer or 
-- sequencer).
--
data MidiSysCommonEvent
    -- | Time code quarter frame.
    -- 
    -- > F1 * payload
    -- 
    -- Note the payload is really a byte split into two 4-bit 
    -- values, however here it is uninterpreted.
    --
    = QuarterFrame      Word8
    

    -- | Song position pointer.
    --
    -- > F2 * lsb * msb
    --
    | SongPosPointer    Word8       Word8


    -- | Song number.
    -- 
    -- > F3 * song_number
    --
    -- Song number should be in the range 0..127.
    --
    | SongSelect        Word8


    -- | Undefined system common event.
    -- 
    -- > F4
    --
    | UndefinedF4

    -- | Undefined system common event.
    -- 
    -- > F5
    --
    | UndefinedF5

    -- | Tune request.
    -- 
    -- > F6
    -- 
    -- Tune request message for analogue synthesizers.
    --
    | TuneRequest
    

    -- | End-of-system-exclusive message.
    -- 
    -- > F7
    -- 
    | EOX
  deriving (Eq,Ord,Show)


-- | System real-time event.
--
-- These events may not be pertinent to MIDI files generated on a 
-- computer (as opposed to MIDI generated by a synthesizer or 
-- sequencer).
--
data MidiSysRealTimeEvent
    -- | Timing signal.
    -- 
    -- > F8 
    --      
    = TimingClock
    
    -- | Undefined real time event.
    -- 
    -- > F9
    --
    --
    | UndefinedF9

    
    -- | Start playing a sequence.
    -- 
    -- > FA
    -- 
    | StartSequence

    
    -- | Continue playing a stopped sequence.
    -- 
    -- > FB
    --
    | ContinueSequence


    -- | Stop playing a sequence.
    -- 
    -- > FC
    --
    | StopSequence


    -- | Undefined real time event.
    -- 
    -- > FD
    --
    --
    | UndefinedFD

    -- | Active sensing
    -- 
    -- > FE
    -- 
    -- Synchronization pulse...
    -- 
    | ActiveSensing


    -- | Reset to power-up status.
    -- 
    -- > FF
    --
    | SystemReset
  deriving (Eq,Ord,Show)






-- | Meta event 
-- 
-- In Format 1 files general events (e.g. text events) should
-- only appear in track 1. Certain events (e.g. end-of-track) 
-- can appear in any track where necessary. 
--
data MidiMetaEvent

    -- | Text / copywright etc.
    -- 
    -- > FF * text_type * contents
    -- 
    -- Free text field (e.g. copyright statement). The contents 
    -- can notionally be any length.
    --
    = TextEvent MidiTextType String

    -- | Sequence Number 
    -- 
    -- > FF 00 02 * value
    -- 
    -- Format 1 files - only track 1 should have a sequence 
    -- number. 
    --
    -- Format 2 files - a sequence number should identify each 
    -- track.
    --  
    -- The sequence number event should occur at the start of a 
    -- track, before any non-zero time events.
    --
    | SequenceNumber Word16

    -- | Channel prefix 
    -- 
    -- > FF 20 01 * channel
    -- 
    -- Relay all meta and sys-ex events to the given channel.
    --
    | ChannelPrefix Word8

    -- | Midi port
    -- 
    -- > FF 21 01 * port
    -- 
    -- Midi port number - used to workaround 16 channel limit...
    -- 
    | MidiPort Word8

    -- | End-of-track event. 
    --
    -- > FF 2F 00
    --
    | EndOfTrack

    -- | Set tempo
    --
    -- > FF 51 03 * microseconds_per_quarter_note
    --
    | SetTempo Word32

    -- | SMPTE offest
    -- 
    -- > FF 54 05 * hour * minute * second * frac * subfrac
    -- 
    -- The SMPTE time when a track should start. This event 
    -- should occur at the start of a track, before any non-zero 
    -- time events.
    --
    | SMPTEOffset Word8 Word8 Word8 Word8 Word8
    
    -- | Time signature 
    -- 
    -- > FF 58 04 * numerator * denominator * metro * num_32nd_notes
    --
    | TimeSignature Word8 Word8 Word8 Word8
    
    -- | Key signature 
    --
    -- > FF 59 02 * key_type * scale_type
    --
    -- @key_type@ is the number of sharps (postive numbers) or 
    -- flats (negative numbers), e.g. (-1) is 1 flat.
    --
    -- @scale_type@ indicates major or minor.  
    --
    | KeySignature Int8 MidiScaleType
    
    -- | SSME 
    -- 
    -- > FF 7F * length * data
    -- 
    -- Sequencer specific meta-event - uninterpreted.
    --
    | SSME Word32 [Word8]

    -- | Unrecognized Meta Event
    --
    -- > FF * type * length * data 
    --
    | MetaOther Word8 Word32 [Word8]

  deriving (Eq,Ord,Show)


-- | Scale type - @major@ or @minor@ or @SCALE_OTHER@.  
--
-- @SCALE_OTHER@ represents a parse error.
-- 
data MidiScaleType = MAJOR | MINOR | SCALE_OTHER Word8
  deriving (Eq,Ord,Show)


