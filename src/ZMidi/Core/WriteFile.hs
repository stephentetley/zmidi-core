{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.WriteFile
-- Copyright   :  (c) Stephen Tetley 2010-2013
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Write a MIDI file.
--
--------------------------------------------------------------------------------

module ZMidi.Core.WriteFile 
  (
  -- * Write a Midi structure to file
    writeMidi
  ) where

import ZMidi.Core.Datatypes
import ZMidi.Core.Internal.ExtraTypes

import Data.Binary.Put                  -- package: binary

import Control.Applicative
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Int
import Data.Word

import System.IO

-- | Write a MIDI file.
--
writeMidi :: FilePath -> MidiFile -> IO ()
writeMidi filename midi = 
    openBinaryFile filename WriteMode        >>= \hdl -> 
    L.hPut hdl (runPut $ putMidiFile midi)   >>
    hClose hdl                    

putMidiFile :: MidiFile -> PutM ()
putMidiFile (MidiFile hdr trks) = 
    putHeader hdr *> mapM_ putTrack trks
  
putHeader :: MidiHeader -> PutM ()
putHeader (MidiHeader fmt n td) =
    putString "MThd"  *>  putWord32be 6 *> 
    putFormat fmt     *>  putWord16be n *>  putTimeDivision td


putTrack :: MidiTrack -> PutM ()
putTrack (MidiTrack ms) = 
    putString "MTrk" *> (putWord32be $ fromIntegral $ L.length bs)
                     *> putLazyByteString bs
  where 
    bs = runPut (mapM_ putMessage ms) 


putFormat :: MidiFormat -> PutM ()
putFormat MF0 = putWord16be 0
putFormat MF1 = putWord16be 1
putFormat MF2 = putWord16be 2

putTimeDivision :: MidiTimeDivision -> PutM ()
putTimeDivision (FPS n) = putWord16be (n `setBit`   15)
putTimeDivision (TPB n) = putWord16be (n `clearBit` 15)


putDeltaTime :: DeltaTime -> PutM ()
putDeltaTime = putVarlen . fromIntegral

putMessage :: MidiMessage -> PutM () 
putMessage (dt,evt) = putDeltaTime dt *> putEvent evt

putEvent :: MidiEvent -> PutM ()
putEvent (MidiEventOther e)   = putMidiDataOther  e
putEvent (VoiceEvent rs e)    = putVoiceEvent rs e
putEvent (SysExEvent e)       = putSysExEvent e
putEvent (SysCommonEvent e)   = putSysCommonEvent e
putEvent (SysRealTimeEvent e) = putSysRealTimeEvent e
putEvent (MetaEvent e)        = putMetaEvent  e
  

putMidiDataOther :: MidiDataOther -> PutM ()
putMidiDataOther (MidiDataOther n) = putWord8 n


-- | Note - this assumes the output is properly formed where
-- initial events are labelled with RS_OFF and subsequent events 
-- are labelled with RS_ON only when they share the same 
-- constructor and channel.
-- 
putVoiceEvent :: MidiRunningStatus -> MidiVoiceEvent -> PutM ()
putVoiceEvent rs (NoteOff c n v)            = 
    optTagByte rs (0x8 `u4l4` c) *> putWord8 n *> putWord8 v 

putVoiceEvent rs (NoteOn c n v)             = 
    optTagByte rs (0x9 `u4l4` c) *> putWord8 n *> putWord8 v 

putVoiceEvent rs (NoteAftertouch c n v)     = 
    optTagByte rs (0xA `u4l4` c) *> putWord8 n *> putWord8 v

putVoiceEvent rs (Controller c n v)         = 
    optTagByte rs (0xB `u4l4` c) *> putWord8 n *> putWord8 v

putVoiceEvent rs (ProgramChange c n)        = 
    optTagByte rs (0xC `u4l4` c) *> putWord8 n

putVoiceEvent rs (ChanAftertouch c v)       = 
    optTagByte rs (0xD `u4l4` c) *> putWord8 v  

putVoiceEvent rs (PitchBend c v)            = 
    optTagByte rs (0xE `u4l4` c) *> putWord16be v


-- Note - F7 (terminator) should be the last byte in the 
-- payload (ws) for SysExSingle.
-- 
-- It should be the last byte of the last continuation packet
-- for SysExCont.
--
-- The payload for SysExEscape should not be terminated 
-- (with F7).
--
putSysExEvent :: MidiSysExEvent -> PutM ()
putSysExEvent (SysExSingle n ws)    = 
    putWord8 0xF0 *> putVarlen n *> mapM_ putWord8 ws

putSysExEvent (SysExCont n ws ks)   = 
    putWord8 0xF0 *> putVarlen n *> mapM_ putWord8 ws 
                  *> mapM_ putSysExContPacket ks

putSysExEvent (SysExEscape n ws)    = 
    putWord8 0xF7 *> putVarlen n *> mapM_ putWord8 ws


putSysExContPacket :: MidiSysExContPacket -> PutM ()
putSysExContPacket (MidiSysExContPacket dt n ws)  = 
    putDeltaTime dt *> putWord8 0xF7 *> putVarlen n *> mapM_ putWord8 ws
    

putSysCommonEvent :: MidiSysCommonEvent -> PutM ()
putSysCommonEvent (QuarterFrame sb)         = 
    putWord8 0xF1 *> putWord8 sb

putSysCommonEvent (SongPosPointer lsb msb)  = 
    putWord8 0xF2 *> putWord8 lsb *> putWord8 msb

putSysCommonEvent (SongSelect w)            = 
    putWord8 0xF3 *> putWord8 w

putSysCommonEvent (UndefinedF4)             = 
    putWord8 0xF4

putSysCommonEvent (UndefinedF5)             = 
    putWord8 0xF5

putSysCommonEvent TuneRequest               = 
    putWord8 0xF6

putSysCommonEvent (EOX)                     = 
    putWord8 0xF7


putSysRealTimeEvent :: MidiSysRealTimeEvent -> PutM ()
putSysRealTimeEvent (TimingClock)          = putWord8 0xF8
putSysRealTimeEvent (UndefinedF9)          = putWord8 0xF9
putSysRealTimeEvent (StartSequence)        = putWord8 0xFA
putSysRealTimeEvent (ContinueSequence)     = putWord8 0xFB
putSysRealTimeEvent (StopSequence)         = putWord8 0xFC
putSysRealTimeEvent (UndefinedFD)          = putWord8 0xFD
putSysRealTimeEvent (ActiveSensing)        = putWord8 0xFE
putSysRealTimeEvent (SystemReset)          = putWord8 0xFF


putMetaEvent :: MidiMetaEvent -> PutM ()
putMetaEvent (TextEvent ty ss)                = 
    putWord8 0xFF *> putWord8 (texttype ty) 
                  *> putVarlen   (fromIntegral $ length ss) 
                  *> putString ss
  
putMetaEvent (SequenceNumber n)               = 
    putWord8 0xFF *> putWord8 0x00 *> prefixLen 2 (putWord16be n)
  
putMetaEvent (ChannelPrefix ch)               = 
    putWord8 0xFF *> putWord8 0x20 *> putWord8 0x01 *> putWord8 ch

putMetaEvent (MidiPort pn)                     = 
    putWord8 0xFF *> putWord8 0x21 *> putWord8 0x01 *> putWord8 pn
  
putMetaEvent (EndOfTrack)                     = 
    putWord8 0xFF *> putWord8 0x2F *> prefixLen 0 (pure ())
  
putMetaEvent (SetTempo t)                     = 
    putWord8 0xFF *> putWord8 0x51 *> prefixLen 3 (putWord24be t)
  
putMetaEvent (SMPTEOffset hr mn sc fr sfr)    =
    putWord8 0xFF *> putWord8 0x54 *> prefixLen 5 body
 where
    body = putWord8 hr *> putWord8 mn *> putWord8 sc 
                       *> putWord8 fr *> putWord8 sfr
  
putMetaEvent (TimeSignature nmr dnm met nps)  =
    putWord8 0xFF *> putWord8 0x58 *> prefixLen 4 body
  where
    body = putWord8 nmr *> putWord8 dnm *> putWord8 met *> putWord8 nps    
  
putMetaEvent (KeySignature ky sc)             =
    putWord8 0xFF *> putWord8 0x59 *> prefixLen 2 body
  where
    body = putWord8 (wrapint ky) *> putWord8 (wscale sc)

putMetaEvent (SSME i ws)                      =  
    putWord8 0xFF *> putWord8 0x7F *> putVarlen i *> mapM_ putWord8 ws

putMetaEvent (MetaOther ty len bs)             = 
    putWord8 0xff *> putWord8 ty *> putVarlen (fromIntegral len) 
                  *> mapM_ putWord8 bs

    
  



--------------------------------------------------------------------------------
-- Output helpers

optTagByte :: MidiRunningStatus -> Word8 -> PutM ()
optTagByte RS_OFF n = putWord8 n
optTagByte _      _ = return ()


prefixLen :: Word8 -> PutM () -> PutM ()
prefixLen n out = putWord8 n *> out 


infixr 5 `u4l4`

u4l4 :: Word8 -> Word8 -> Word8
a `u4l4` b = (a `shiftL` 4) + b 
  

wrapint :: Int8 -> Word8
wrapint i | i < 0     = fromIntegral $ i' + 256
          | otherwise = fromIntegral i
  where
    i' :: Int
    i' = fromIntegral i  
    
wscale :: MidiScaleType -> Word8
wscale (MAJOR)          = 0x00
wscale (MINOR)          = 0x01
wscale (SCALE_OTHER i)  = i

putWord24be :: Word32 -> PutM ()
putWord24be i = putWord8 c *> putWord8 b *> putWord8 a 
  where 
  (a, r1)   = lowerEight i     
  (b, r2)   = lowerEight r1
  (c, _)    = lowerEight r2
  
    

lowerEight :: (Bits a, Integral a) => a -> (Word8, a)    
lowerEight n = (fromIntegral lower8, remain)
  where
    remain = n `shiftR` 8
    lower8 = n .&. 0xff 
      
putVarlen :: Word32 -> PutM ()
putVarlen = step . toVarlen where
    step (V1 a)          = putWord8 a
    step (V2 a b)        = putWord8 a *> putWord8 b
    step (V3 a b c)      = putWord8 a *> putWord8 b *> putWord8 c
    step (V4 a b c d)    = putWord8 a *> putWord8 b *> putWord8 c *> putWord8 d
    

putString :: String -> PutM ()    
putString s = putLazyByteString (L.pack $ fmap (fromIntegral . ord) s) 




texttype :: MidiTextType -> Word8
texttype GENERIC_TEXT         = 0x01
texttype COPYRIGHT_NOTICE     = 0x02
texttype SEQUENCE_NAME        = 0x03
texttype INSTRUMENT_NAME      = 0x04
texttype LYRICS               = 0x05
texttype MARKER               = 0x06
texttype CUE_POINT            = 0x07

