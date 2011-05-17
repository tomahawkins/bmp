module Main (main) where

import Data.Bits
import qualified Data.ByteString as B

import Eaton
import MTL
import PPC
import Utils

main :: IO ()
main = do
  B.writeFile "test.esb" $ B.pack $ esb $            bootloader $ assemble $ compile test
  writeFile "test.sr" $ srec 0x00080000 0x0008000c $ bootloader $ assemble $ compile test

-- A little MTL program that attempts to send a CAN message.
test :: S
test = Seq
  [ 0xfffc0000 := Deref 0xfffc0000 .|. 0x10000000   -- Set HALT.
  , 0xfffc0004 := 0x032e0005                        -- Set CTRL for 250K.
  , Seq [ Const buffer := 0x08280000 | buffer <- [0xfffc0080, 0xfffc0090 .. 0xfffc0470] ]  -- Set all MBs to transmit.
  , 0xfffc0000 := Deref 0xfffc0000 .&. 0xefffffff .|.0x00020000  -- Clear HALT and set SRX_DIS.

  , 0xfffc0080 := 0x08280000  -- Deref 0xfffc0080 .&. 0xf0ffffff .|. 0x08000000  -- Write code 8 to CS buffer register.
  , 0xfffc0084 := 0x00000123  -- Write dummy id.
  , 0xfffc0088 := 0x01234587  -- Write dummy payload.
  , 0xfffc008c := 0x89abcdef
  , 0xfffc0080 := 0x0c280000  -- Deref 0xfffc0080 .&. 0xf0f0ffff .|. 0x0c080000  -- Write code c and length 8.  XXX What should be SRR and RTR?
  , Asm $ assemble $ b 0
  , Asm $ assemble jumpToBoot
  ]

