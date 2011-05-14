module Main (main) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Digest.CRC32
import Data.Word

main :: IO ()
main = B.writeFile "test.esb" $ B.pack $ esb $ bootloaderHeader test

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
  program = program' ++ case (length program' + 4) `mod` 8 of
    0 -> []
    n -> replicate (8 - n) 0

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



data R
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | R16
  | R17
  | R18
  | R19
  | R20
  | R21
  | R22
  | R23
  | R24
  | R25
  | R26
  | R27
  | R28
  | R29
  | R30
  | R31

reg :: R -> Int
reg a = case a of
  R0  ->  0 
  R1  ->  1 
  R2  ->  2 
  R3  ->  3 
  R4  ->  4 
  R5  ->  5 
  R6  ->  6 
  R7  ->  7 
  R8  ->  8 
  R9  ->  9 
  R10 -> 10
  R11 -> 11
  R12 -> 12
  R13 -> 13
  R14 -> 14
  R15 -> 15
  R16 -> 16
  R17 -> 17
  R18 -> 18
  R19 -> 19
  R20 -> 20
  R21 -> 21
  R22 -> 22
  R23 -> 23
  R24 -> 24
  R25 -> 25
  R26 -> 26
  R27 -> 27
  R28 -> 28
  R29 -> 29
  R30 -> 30
  R31 -> 31


-- Instructions.

nop :: [Word8]
nop = ori R0 R0 0

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
addi rt ra si = dForm 14 rt ra si

addis :: R -> R -> Int -> [Word8]
addis rt ra si = dForm 15 rt ra si

ori :: R -> R -> Int -> [Word8]
ori ra rs ui = dForm 24 rs ra ui

mtctr :: R -> [Word8]
mtctr rs = be $ 0x7c0903a6 .|. shiftL (reg rs) 21

stbux :: R -> R -> R -> [Word8]
stbux rs ra rb = be $ 0x7c0001ee .|. shiftL (reg rs) 21 .|. shiftL (reg ra) 16 .|. shiftL (reg rb) 11

stb :: R -> Int -> R -> [Word8]
stb rs d ra = dForm 38 rs ra d

stw :: R -> Int -> R -> [Word8]
stw rs d ra = dForm 36 rs ra d

lwz :: R -> Int -> R -> [Word8]
lwz rt d ra = dForm 32 rt ra d

dForm :: Int -> R -> R -> Int -> [Word8]
dForm opcode r1 r2 d = be $ shiftL opcode 26 .|. shiftL (reg r1) 21 .|. shiftL (reg r2) 16 .|. d .&. 0xffff

set :: R -> Int -> [Word8]
set rt value | elem (shiftR value 16) [0, -1] = addi rt R0 value
             | otherwise                      = addis rt R0 (shiftR value 16) ++ ori rt rt value


