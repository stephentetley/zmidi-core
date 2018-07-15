{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Internal.ExtraTypes
-- Copyright   :  (c) Stephen Tetley 2010-2018
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  As per dependencies.
--
-- Internal types not exported by the package.
--
--------------------------------------------------------------------------------


module ZMidi.Core.Internal.ExtraTypes
  (

  -- * SplitByte
    SplitByte(..)
  , splitByte
  , joinByte
    
  -- * Varlen
  , Varlen(..)
  , fromVarlen
  , toVarlen

  , hexStr
    
  ) where

import Data.Bits
import Data.Word
import Numeric (showHex)



-- | SplitByte - divide a byte into the upper four and lower 
-- 4 bits.
-- 
-- Note upper4 is not shifted (this is a change in v 0.5.0).
-- 
data SplitByte = SB { upper4 :: !Word8, lower4 :: !Word8 }
  deriving (Eq,Ord,Show)

-- | Split a Word8.
--
splitByte :: Word8 -> SplitByte
splitByte i = SB (i .&. 0xF0) (i .&. 0x0F)


-- | Make a Word8 from a Splitbyte.
--
joinByte :: SplitByte -> Word8
joinByte (SB a b) = (a .&. 0xF0) + (b .&. 0x0F)





--------------------------------------------------------------------------------
-- Helper for varlen
--------------------------------------------------------------------------------

-- | Space efficient representation of length fields.
-- 
-- This data type is not used directly in the syntax tree where
-- it would be cumbersome. But it is used as an intermediate type
-- in the parser and emitter.
--
data Varlen = V1 !Word8
            | V2 !Word8 !Word8
            | V3 !Word8 !Word8 !Word8
            | V4 !Word8 !Word8 !Word8 !Word8
  deriving (Eq,Ord,Show)


up :: Word8 -> Word32
up = fromIntegral . (0x7f .&.)

down :: Word32 -> Word8
down = (0x80 .|.) . fromIntegral

downl :: Word32 -> Word8
downl = (0x7f .&.) . fromIntegral
 
-- | Make a Word32 from a Varlen.
--
fromVarlen :: Varlen -> Word32
fromVarlen (V1 a)       = up a
fromVarlen (V2 a b)     = (left7 $ up a)  + up b
fromVarlen (V3 a b c)   = (left14 $ up a) + (left7  $ up b) + up c
fromVarlen (V4 a b c d) = (left21 $ up a) + (left14 $ up b) 
                        + (left7  $ up c) + up d

left7     :: Word32 -> Word32
left7     = (`shiftL` 7)

left14    :: Word32 -> Word32
left14    = (`shiftL` 14)

left21    :: Word32 -> Word32
left21    = (`shiftL` 21)

right7    :: Word32 -> Word32
right7    = (`shiftR` 7)

right14   :: Word32 -> Word32
right14   = (`shiftR` 14)

right21   :: Word32 -> Word32
right21   = (`shiftR` 21)

-- | Make a Varlen from a Word32.
--
toVarlen :: Word32 -> Varlen
toVarlen i 
    | i < 0x80           = V1 (downl i)
    | i < 0x4000         = V2 (down $ right7 i)  (downl i)
    | i < 0x200000       = V3 (down $ right14 i) (down $ right7  i) (downl i)
    | otherwise          = V4 (down $ right21 i) (down $ right14 i)
                              (down $ right7  i) (downl i)

-- | Show a integral as a hex value with \n @0x@ prefix.
--
hexStr :: (Show a, Integral a) => a -> String
hexStr i = (showString "0x" . showHex i) "" 
