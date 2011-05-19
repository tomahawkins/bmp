module Main (main) where

import Data.Bits ()
import qualified Data.ByteString as B

import Eaton
import MTL
import PPC
import Utils

main :: IO ()
main = do
  B.writeFile "test.esb" $ B.pack $ esb $            bootloader $ assemble $ compile test
  writeFile "test.sr" $ srec 0x00080000 0x0008000c $ bootloader $ assemble $ compile test

-- Pointer to CAN A.
canA :: E
canA = 0xfffc0000

-- Pointer to CAN A, Message Buffer 0.
mb0 :: E
mb0 = canA + 0x80

inactive :: E
inactive = 0x08280000

transmit :: E
transmit = 0x0c280000

-- A little MTL program that attempts to send a CAN message.
test :: S
test = Seq
  [ canA + 0x0 := 0x5080000f
  , canA + 0x4 := 0x032e0005    -- Set CTRL for 250K.
  , mb0 := inactive
  , canA + 0x0 := 0x4080000f

  , mb0 := inactive
  , mb0 + 0x4 := 0x00000123  -- Write dummy id.
  , mb0 + 0x8 := 0x01234587  -- Write dummy payload.
  , mb0 + 0xc := 0x89abcdef
  , mb0 := transmit
  , Asm $ assemble $ b 0
  , Asm $ assemble jumpToBoot
  ]

