{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.ReadFile
-- Copyright   :  (c) Stephen Tetley 2010-2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- A top down (Parsec style) MIDI file parser. 
--
-- For valid input, the parser should parse without error 
-- (i.e all cases of event types are fully enumerated). 
-- Malformed input (syntactically bad events, or truncated data) 
-- will cause fatal parse errors.
-- 
-- Note - the parser returns a /literal/ result if the input 
-- uses Running Status, i.e, - the answer matches the input - 
-- where running status uses a NoteOn event with velocity 0 to 
-- stand for a NoteOff, the parser likewise returns a Note-On. 
-- Use the @ZMidi.Core.Canonical@ to translate the input to 
-- canonical form where note-offs are encoded directly with 
-- NoteOff.
--
--------------------------------------------------------------------------------

module ZMidi.Core.ReadFile 
  (
  -- * Read a MIDI file
    readMidi

  -- * Auxiallary types
  , ParseErr(..)        -- exported from internal file ParserMonad
  , Pos
  , ErrMsg
  
  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.ExtraTypes
import ZMidi.Core.Internal.ParserMonad


#ifndef MIN_VERSION_GLASGOW_HASKELL
import Control.Applicative
#endif
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy   as L
import Data.Word

-- | Read a well formed MIDI file. 
-- 
readMidi :: FilePath -> IO (Either ParseErr MidiFile)
readMidi filename =
    liftM (runParser `flip` midiFile) (L.readFile filename)

--------------------------------------------------------------------------------
-- 

    
midiFile :: ParserM MidiFile  
midiFile = do
    hdr   <- header
    let i  = trackCount hdr
    trks  <- count i track
    return $ MidiFile hdr trks   
  where
    trackCount :: MidiHeader -> Int 
    trackCount (MidiHeader _ n _) = fromIntegral n

header :: ParserM MidiHeader  
header = 
    MidiHeader <$> (assertString "MThd" *> assertWord32 (6::Int) *> fileFormat)
               <*> word16be
               <*> timeDivision



track :: ParserM MidiTrack
track = liftM MidiTrack (trackHeader >>= messages)

trackHeader :: ParserM Word32
trackHeader = assertString "MTrk" >> word32be



messages :: Word32 -> ParserM [MidiMessage]
messages i = boundRepeat (fromIntegral i) message


message :: ParserM MidiMessage
message = (,) <$> deltaTime <*> event

deltaTime :: ParserM DeltaTime
deltaTime = "delta time" <??> fmap fromIntegral getVarlen

-- | Parse an event - for valid input this function should parse
-- without error (i.e all cases of event types are fully 
-- enumerated). 
--
-- Malformed input (syntactically bad events, or truncated data) 
-- can cause fatal parse errors.
--
event :: ParserM MidiEvent
event = peek >>= step
  where
    -- 00..7f  -- /data/
    step n
      | n == 0xFF  = MetaEvent         <$> (dropW8 *> (word8 >>= metaEvent))
      | n >= 0xF8  = SysRealTimeEvent  <$> (dropW8 *> sysRealTimeEvent n)
      | n == 0xF7  = SysExEvent        <$> (dropW8 *> sysExEscape)
      | n >= 0xF1  = SysCommonEvent    <$> (dropW8 *> sysCommonEvent n)
      | n == 0xF0  = SysExEvent        <$> (dropW8 *> sysExEvent)
      | n >= 0x80  = VoiceEvent RS_OFF <$> (dropW8 *> voiceEvent (splitByte n))
      | otherwise  = getRunningEvent >>= runningStatus


   
-- | Input is a contiguous sequence 0x80 to 0xE0 (considering 
-- just the top half-byte) so any match failure indicates this 
-- parser is called with an invalid argument - either the 
-- splitByte function or the event parser is wrong.
--
voiceEvent :: SplitByte -> ParserM MidiVoiceEvent
voiceEvent (SB 0x80 ch)  = 
    setRunningEvent (RS_NOTE_OFF ch)    >> noteOff ch

voiceEvent (SB 0x90 ch)  = 
    setRunningEvent (RS_NOTE_ON ch)     >> noteOn ch

voiceEvent (SB 0xA0 ch)  = 
    setRunningEvent (RS_NOTE_AFT ch)    >> noteAftertouch ch

voiceEvent (SB 0xB0 ch)  = 
    setRunningEvent (RS_CONTROL ch)     >> controller ch

voiceEvent (SB 0xC0 ch)  = 
    setRunningEvent (RS_PROG_CHANGE ch) >> programChange ch

voiceEvent (SB 0xD0 ch)  = 
    setRunningEvent (RS_CHAN_AFT ch)    >> chanAftertouch ch

voiceEvent (SB 0xE0 ch)  =
    setRunningEvent (RS_PCH_BEND ch)    >> pitchBend ch

-- This is an /impossible/ match - to get here either @splitByte@ 
-- or the pattern match of @step@ in @event@ would be wrong.
voiceEvent (SB z   _ )  = impossibleMatch $ "voiceEvent " ++ hexStr z 



noteOff :: Word8 -> ParserM MidiVoiceEvent
noteOff ch = 
    "note-off"          <??> (NoteOff ch)        <$> word8 <*> word8

noteOn :: Word8 -> ParserM MidiVoiceEvent
noteOn ch = 
    "note-on"           <??> (NoteOn ch)         <$> word8 <*> word8

noteAftertouch :: Word8 -> ParserM MidiVoiceEvent
noteAftertouch ch = 
    "note aftertouch"   <??> (NoteAftertouch ch) <$> word8 <*> word8

controller :: Word8 -> ParserM MidiVoiceEvent    
controller ch = 
    "controller"        <??> (Controller ch)     <$> word8 <*> word8

programChange :: Word8 -> ParserM MidiVoiceEvent
programChange ch = 
    "program change"    <??> (ProgramChange ch)  <$> word8 

chanAftertouch :: Word8 -> ParserM MidiVoiceEvent
chanAftertouch ch = 
    "chan aftertouch"   <??> (ChanAftertouch ch) <$> word8 

pitchBend :: Word8 -> ParserM MidiVoiceEvent
pitchBend ch = 
    "pitch bend"        <??> (PitchBend ch)      <$> word16be





runningStatus :: RS_VoiceEvent -> ParserM MidiEvent
runningStatus (RS_NOTE_OFF ch)        = VoiceEvent RS_ON <$> noteOff ch
runningStatus (RS_NOTE_ON ch)         = VoiceEvent RS_ON <$> noteOn ch
runningStatus (RS_NOTE_AFT ch)        = VoiceEvent RS_ON <$> noteAftertouch ch
runningStatus (RS_CONTROL ch)         = VoiceEvent RS_ON <$> controller ch
runningStatus (RS_PROG_CHANGE ch)     = VoiceEvent RS_ON <$> programChange ch
runningStatus (RS_CHAN_AFT ch)        = VoiceEvent RS_ON <$> chanAftertouch ch
runningStatus (RS_PCH_BEND ch)        = VoiceEvent RS_ON <$> pitchBend ch
runningStatus (RS_STATUS_OFF)         = 
    MidiEventOther . MidiDataOther <$> word8

-- | Input is a contiguous sequence 0xF1 to 0xF7 so any match 
-- failure indicates this parser is called with an invalid 
-- argument (i.e. the event parser is wrong).
--
sysCommonEvent :: Word8 -> ParserM MidiSysCommonEvent
sysCommonEvent 0xF1     = 
    "quarter frame"     <??> QuarterFrame                  <$> word8

sysCommonEvent 0xF2     = 
    "song pos. pointer" <??> SongPosPointer                <$> word8 <*> word8

sysCommonEvent 0xF3     = 
    "song select"       <??> SongSelect                    <$> word8

sysCommonEvent 0xF4     = pure UndefinedF4

sysCommonEvent 0xF5     = pure UndefinedF5

sysCommonEvent 0xF6     = pure TuneRequest

sysCommonEvent 0xF7     = pure EOX

sysCommonEvent tag      = impossibleMatch $ "sysCommonEvent " ++ hexStr tag


-- | Input is a contiguous sequence 0xF8 to 0xFF so any match 
-- failure indicates this parser is called with an invalid 
-- argument (i.e. the event parser is wrong).
--
sysRealTimeEvent :: Word8 -> ParserM MidiSysRealTimeEvent
sysRealTimeEvent 0xF8 = pure TimingClock
sysRealTimeEvent 0xF9 = pure UndefinedF9
sysRealTimeEvent 0xFA = pure StartSequence
sysRealTimeEvent 0xFB = pure ContinueSequence
sysRealTimeEvent 0xFC = pure StopSequence
sysRealTimeEvent 0xFD = pure UndefinedFD
sysRealTimeEvent 0xFE = pure ActiveSensing
sysRealTimeEvent 0xFF = pure SystemReset
sysRealTimeEvent tag  = impossibleMatch $ "sysRealTimeEvent " ++ hexStr tag 


-- | 0xF0 has been parsed, SysEx is uninterpreted.
--
sysExEvent :: ParserM MidiSysExEvent
sysExEvent = "sys-ex" <??> body
  where
    body = getVarlenBytes (,) >>= \(n,xs) -> 
           if isTerminated xs then return $ SysExSingle n xs
                              else sysExContPackets >>= \ks -> 
                                   return $ SysExCont n xs ks

sysExContPackets :: ParserM [MidiSysExContPacket]
sysExContPackets = 
    deltaTime >>= \dt -> getVarlenBytes (,) >>= \(n,xs) -> 
    let ans1 = MidiSysExContPacket dt n xs
    in if isTerminated xs then return [ans1]
                          else sysExContPackets >>= \ks -> 
                               return $ ans1:ks



sysExEscape :: ParserM MidiSysExEvent
sysExEscape = "sys-ex" <??> getVarlenBytes SysExEscape

-- | 0xFF and the "type" byte already parsed, input to this
-- function is the type byte.
--
-- Not all MetaEvents are enumerated - unrecognized ones are
-- delegated to @MetaOther@.
--
metaEvent :: Word8 -> ParserM MidiMetaEvent
metaEvent 0x00          =
    "sequence number"   <??> SequenceNumber <$> (assertWord8 2 *> word16be)

metaEvent 0x01          = "generic text"      <??> textEvent GENERIC_TEXT
metaEvent 0x02          = "copyright notice" <??> textEvent COPYRIGHT_NOTICE
metaEvent 0x03          = "sequence name"     <??> textEvent SEQUENCE_NAME
metaEvent 0x04          = "instrument name"   <??> textEvent INSTRUMENT_NAME
metaEvent 0x05          = "lyrics"            <??> textEvent LYRICS
metaEvent 0x06          = "marker"            <??> textEvent MARKER
metaEvent 0x07          = "cue point"         <??> textEvent CUE_POINT

metaEvent 0x20          =
    "channel prefix"    <??> ChannelPrefix <$> (assertWord8 0x01 *> word8)

metaEvent 0x21          =
    "MIDI port"         <??> MidiPort <$> (assertWord8 0x01 *> word8)

metaEvent 0x2F          =
    "end of track"      <??> EndOfTrack <$ assertWord8 0

metaEvent 0x51          =
    "set tempo"         <??> SetTempo <$> (assertWord8 3     *> word24be)

metaEvent 0x54          =
    "smpte offset"      <??> SMPTEOffset  <$> (assertWord8 5   *> word8)
                                          <*> word8           <*> word8
                                          <*> word8           <*> word8

metaEvent 0x58          =
    "time signature"    <??> TimeSignature  <$> (assertWord8 4   *> word8)
                                            <*> word8           <*> word8
                                            <*> word8

metaEvent 0x59          =
    "key signature"     <??> KeySignature   <$> (assertWord8 2   *> int8)
                                            <*> scale

metaEvent 0x7F          =
    "system specific meta event" <??> getVarlenBytes SSME

metaEvent ty            =
    "meta other"        <??> getVarlenBytes (MetaOther ty)





fileFormat :: ParserM MidiFormat
fileFormat = word16be >>= fn 
  where 
    fn 0 = return MF0
    fn 1 = return MF1
    fn 2 = return MF2
    fn z = fatalError $ 
              "getFormat - unrecognized file format " ++ hexStr z
        
timeDivision :: ParserM MidiTimeDivision
timeDivision = division <$> word16be
  where division i | i `testBit` 15 = FPS (i `clearBit` 15)
                   | otherwise      = TPB i


scale :: ParserM MidiScaleType
scale = word8 >>= fn 
  where
    fn 0 = return MAJOR
    fn 1 = return MINOR
    fn z = return $ SCALE_OTHER z
    
    
textEvent :: MidiTextType -> ParserM MidiMetaEvent
textEvent ty = getVarlenText (\_ ss -> TextEvent ty ss) 

--------------------------------------------------------------------------------
-- helpers


impossibleMatch :: String -> ParserM a
impossibleMatch ss = fatalError $ 
    "impossible match: " ++ ss ++ "\nSeeing this is a error in ReadFile."
 
assertWord8 :: Word8 -> ParserM Word8
assertWord8 i = postCheck word8 (==i) msg
  where 
    msg = "assertWord8 - input did not match " ++ show i
             
assertWord32 :: (Integral a, Show a) => a -> ParserM Word32
assertWord32 i = postCheck word32be ((==i) . fromIntegral) msg
  where
    msg = "assertWord32 - input did not match " ++ show i

assertString :: String -> ParserM String
assertString s = postCheck (text $ length s) (==s) msg
  where
    msg = "assertString - input did not match " ++ s


getVarlenText :: (Word32 -> String -> ans) -> ParserM ans
getVarlenText = gencount getVarlen char8

getVarlenBytes :: (Word32 -> [Word8] -> ans) -> ParserM ans
getVarlenBytes = gencount getVarlen word8


msbHigh :: Word8 -> Bool
msbHigh w = w `testBit` 7

-- msbLow :: Word8 -> Bool
-- msbLow = not . msbHigh

getVarlen :: ParserM Word32
getVarlen = liftM fromVarlen step1
  where
    step1     = word8 >>= \a -> if msbHigh a then step2 a else return (V1 a)
    step2 a   = word8 >>= \b -> if msbHigh b then step3 a b else return (V2 a b)
    step3 a b = word8 >>= \c -> if msbHigh c then do { d <- word8
                                                     ; return (V4 a b c d) }
                                             else return (V3 a b c)  




-- | Apply parse then apply the check, if the check fails report
-- the error message. 
--
postCheck :: ParserM a -> (a -> Bool) -> String -> ParserM a
postCheck p check msg = p >>= \ans -> 
    if check ans then return ans else fatalError msg

isTerminated :: [Word8] -> Bool
isTerminated []     = False
isTerminated [0xF7] = True
isTerminated (_:xs) = isTerminated xs

-- | Read a list of databytes.
-- A stream of databytes is signalled by elements having 
-- continuous low most-significant bits.
--

-- databytesMsbLow :: ParserM [Word8]
-- databytesMsbLow = step
--    where
--      step = cond msbLow >>= \mb -> case mb of 
--               Nothing -> return [] 
--               Just x -> do { xs <- step; return (x:xs) }
                                     
