module Main (main) where

import qualified Data.ByteString as B
import Data.Word

import Eaton
import PPC
import Utils

main :: IO ()
main = do
  B.writeFile "test.esb" $ B.pack $ esb $ bootloader test
  writeFile "test.sr" $ srec 0x00080000 0x0008000c $ bootloader test

test :: [Word8]
test = concat $
  [ set R1 0xfffc0080  -- Base address of CAN A, buffer 0.
  , set R2 0x8
  , stb R2 0x0 R1      -- Write Code = 8.
  , set R2 0x1
  , stw R2 0x4 R1      -- Id = 1.
  , set R2 0x01234567
  , stw R2 0x8 R1
  , set R2 0x89abcdef
  , stw R2 0xc R1
  , set R2 0x28
  , stb R2 0x1 R1
  , set R2 0xc
  , stb R2 0x0 R1
  , jumpToBoot
  ]

