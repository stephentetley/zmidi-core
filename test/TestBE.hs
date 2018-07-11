{-# OPTIONS -Wall #-}

module TestBE where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as L



demo01 = runGet getWord32be $ L.pack [ 0x00, 0x00, 0x5f, 0x8f ]

demo02 = runGet getWord32be $ L.pack [ 0x00, 0x00, 0x5e, 0xde ]
