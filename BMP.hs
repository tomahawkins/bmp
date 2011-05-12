module Main (main) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Digest.CRC32
import Data.Word

main :: IO ()
main = B.writeFile "test.esb" $ B.pack $ esb $ bootloaderHeader test

test :: [Word8]
test = jumpToBoot -- b 0

-- | Format ESB image.
esb :: [Word8] -> [Word8]
esb image = [0xE5, 0x5B, 0xBE, 0xE5, 5, 0, 0, 0] ++ le (fromIntegral $ crc32 block) ++ block
  where
  block :: [Word8]
  block = [1, 0, 0, 0, 3, 0, 0, 0, 128, 0, 0, 0] ++ le (length image) ++ image

-- | Adds bootloader header to a program.  Program must start at address 0x8001c.
bootloaderHeader :: [Word8] -> [Word8]
bootloaderHeader program' = be (0 - sum block) ++ block
  where
  program = program' ++ case length program' `mod` 4 of
    1 -> [0, 0, 0]
    2 -> [0, 0]
    3 -> [0]
    _ -> []

  sa = 3

  block :: [Word8]
  block = be (length program + 0x8001c)
       ++ ba 0x8001c
       ++ [0xea, 0x55, 0xaa, 0x01]
       ++ [0x90, 0x12, 0x00, 0x00]
       ++ [0, 0, 0, 0]
       ++ [130, sa] ++ [0xff, 0xff]
       ++ program

  sum :: [Word8] -> Int
  sum (a : b : c : d : rest) = foldl1 (.|.) [ shiftL (fromIntegral a) n | (a, n) <- zip [a, b, c, d] [24, 16, 8, 0] ] + sum rest
  sum [] = 0
  sum _ = error "words not on 32-bit boundry"

le :: Int -> [Word8]
le a = [ fromIntegral $ shiftR a n .&. 0xFF | n <- [0, 8 .. 24] ]

be :: Int -> [Word8]
be a = [ fromIntegral $ shiftR a n .&. 0xFF | n <- [24, 16 .. 0] ]



jumpToBoot :: [Word8]
jumpToBoot = ba 0x8



data R = R0 | R1 | R14 | R15 | R16

reg :: R -> Int
reg a = case a of
  R0  ->  0
  R1  ->  1
  R14 -> 14
  R15 -> 15
  R16 -> 16

nop :: [Word8]
nop = [0x60, 0, 0, 0]

ba :: Int -> [Word8]
ba addr = be $ 0x48000000 .|. addr .&. 0x03fffffc .|. 0x2

b :: Int -> [Word8]
b addr = be $ 0x48000000 .|. addr .&. 0x03fffffc

blr :: [Word8]
blr = [0x4e, 0x80, 0x00, 0x20]

-- Dec CTR, branch on zero.
bdz :: Int -> [Word8]
bdz addr = be $ 0x42400000 .|. addr .&. 0xFFFC

addi :: R -> R -> Int -> [Word8]
addi rt ra si = be $ 0x38000000 .|. shiftL (reg rt) 21 .|. shiftL (reg ra) 16 .|. si .&. 0xFFFF

addis :: R -> R -> Int -> [Word8]
addis rt ra si = be $ 0x3c000000 .|. shiftL (reg rt) 21 .|. shiftL (reg ra) 16 .|. si .&. 0xFFFF

ori :: R -> R -> Int -> [Word8]
ori ra rs ui = be $ 0x60000000 .|. shiftL (reg rs) 21 .|. shiftL (reg ra) 16 .|. ui .&. 0xFFFF

mtctr :: R -> [Word8]
mtctr rs = be $ 0x7c0903a6 .|. shiftL (reg rs) 21

stbux :: R -> R -> R -> [Word8]
stbux rs ra rb = be $ 0x7c0001ee .|. shiftL (reg rs) 21 .|. shiftL (reg ra) 16 .|. shiftL (reg rb) 11

set :: R -> Int -> [Word8]
set rt value | elem (shiftR value 16) [0, -1] = addi rt R0 value
             | otherwise                      = concat [addis rt R0 (shiftR value 16), ori rt rt value]


