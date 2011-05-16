module Main (main) where

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
  [ 0xfffc0080 + 0x0 := 0x08000000  -- Write code 8 to CS buffer register.
  , 0xfffc0080 + 0x4 := 0x00000001  -- Write dummy id.
  , 0xfffc0080 + 0x8 := 0x01234587  -- Write dummy payload.
  , 0xfffc0080 + 0xc := 0x89abcdef
  , 0xfffc0080 + 0x0 := 0x082c0000  -- Write code c to CS register.  XXX What should be SRR and RTR?
  , Asm $ assemble jumpToBoot
  ]

