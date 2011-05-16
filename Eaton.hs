-- | Eaton specific stuff.
module Eaton
  ( esb
  , bootloader
  , jumpToBoot
  ) where

import Data.Bits
import Data.Digest.CRC32
import Data.Word

import PPC
import Utils

-- | Create an image in Eaton's ESB format.
esb :: [Word8] -> [Word8]
esb image = [0xE5, 0x5B, 0xBE, 0xE5, 5, 0, 0, 0] ++ le (fromIntegral $ crc32 block) ++ block
  where
  block :: [Word8]
  block = [1, 0, 0, 0, 3, 0, 0, 0, 128, 0, 0, 0] ++ le (length image) ++ image

-- | Insert appropriate fields for Eaton's bootloader.  Programs must start at address 0x8001c.
bootloader :: [Word8] -> [Word8]
bootloader program' = be (0 - sum block) ++ block
  where
  program = program' ++ case (length program' + 4) `mod` 8 of
    0 -> []
    n -> replicate (8 - n) 0

  sa = 3

  block :: [Word8]
  block = be (length program + 0x8001c)
       ++ assemble (ba 0x8001c)
       ++ [0xea, 0x55, 0xaa, 0x01]
       ++ [0x90, 0x12, 0x00, 0x00]
       ++ [0, 0, 0, 0]
       ++ [130, sa] ++ [0xff, 0xff]
       ++ program

  sum :: [Word8] -> Int
  sum (a : b : c : d : rest) = foldl1 (.|.) [ shiftL (fromIntegral a) n | (a, n) <- zip [a, b, c, d] [24, 16, 8, 0] ] + sum rest
  sum [] = 0
  sum _ = error "words not on 32-bit boundry"

-- | Jump back to the bootloader from the application.
jumpToBoot :: PPC
jumpToBoot = ba 0x8

