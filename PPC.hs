-- | PowerPC assembly language and assembler.
module PPC
  (
  -- * Types
    PPC
  , R (..)
  , registers
  -- * Assembler.
  , (+++)
  , assemble
  -- * Instructions
  -- ** Branching
  , ba
  , b
  , blr
  , bdz
  -- ** Arithmetic
  , addi
  , addis
  , add
  , add'
  , addo
  , addo'
  -- * Logical
  , and_
  , and'
  , nor
  , nor'
  , or_
  , or'
  , ori
  -- ** Misc
  , mtctr
  -- ** Memory Access
  , stbux
  , stb
  , stw
  , lwz
  -- * Mnemonics and Macros
  , nop
  , not_
  , set
  , asm
  ) where

import Data.Bits
import Data.Word

import Utils

-- | A PowerPC instruction.
data PPC = PPC [Word8] | PPCSeq PPC PPC

-- | Gluing PPC programs together.
(+++) :: PPC -> PPC -> PPC
a +++ b = PPCSeq a b

-- | Assembles a PPC program into machine code.
assemble :: PPC -> [Word8]
assemble a = case a of
  PPC a -> a
  PPCSeq a b -> assemble a ++ assemble b

-- | Registers.
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
  deriving (Show, Eq)

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

-- | A list of all registers.
registers :: [R]
registers =
  [ R0
  , R1
  , R2
  , R3
  , R4
  , R5
  , R6
  , R7
  , R8
  , R9
  , R10
  , R11
  , R12
  , R13
  , R14
  , R15
  , R16
  , R17
  , R18
  , R19
  , R20
  , R21
  , R22
  , R23
  , R24
  , R25
  , R26
  , R27
  , R28
  , R29
  , R30
  , R31
  ]

dForm :: Int -> R -> R -> Int -> PPC
dForm opcode r1 r2 d = PPC $ be $ shiftL opcode 26 .|. shiftL (reg r1) 21 .|. shiftL (reg r2) 16 .|. d .&. 0xffff

xoForm :: Int -> R -> R -> R -> Bool -> Int -> Bool -> PPC
xoForm opcode rt ra rb oe xopcode rc = PPC $ be $ shiftL opcode 26
                                              .|. shiftL (reg rt) 21
                                              .|. shiftL (reg ra) 16
                                              .|. shiftL (reg rb) 11
                                              .|. (if oe then bit 1 else 0)
                                              .|. shiftL xopcode 1
                                              .|. (if rc then bit 0 else 0)

xForm :: Int -> R -> R -> R -> Int -> Bool -> PPC
xForm opcode rt ra rb xopcode rc = PPC $ be $ shiftL opcode 26
                                          .|. shiftL (reg rt) 21
                                          .|. shiftL (reg ra) 16
                                          .|. shiftL (reg rb) 11
                                          .|. shiftL xopcode 1
                                          .|. (if rc then bit 0 else 0)


-- Instructions.

ba :: Int -> PPC
ba addr = PPC $ be $ 0x48000000 .|. addr .&. 0x03fffffc .|. 0x2

b :: Int -> PPC
b addr = PPC $ be $ 0x48000000 .|. addr .&. 0x03fffffc

blr :: PPC
blr = PPC [0x4e, 0x80, 0x00, 0x20]

-- Dec CTR, branch on zero.
bdz :: Int -> PPC
bdz addr = PPC $ be $ 0x42400000 .|. addr .&. 0xFFFC

addi :: R -> R -> Int -> PPC
addi rt ra si = dForm 14 rt ra si

addis :: R -> R -> Int -> PPC
addis rt ra si = dForm 15 rt ra si

add :: R -> R -> R -> PPC
add rt ra rb = xoForm 31 rt ra rb False 266 False

add' :: R -> R -> R -> PPC
add' rt ra rb = xoForm 31 rt ra rb False 266 True

addo :: R -> R -> R -> PPC
addo rt ra rb = xoForm 31 rt ra rb True 266 False

addo' :: R -> R -> R -> PPC
addo' rt ra rb = xoForm 31 rt ra rb True 266 True

and_ :: R -> R -> R -> PPC
and_ rs ra rb = xForm 31 rs ra rb 28 False

and' :: R -> R -> R -> PPC
and' rs ra rb = xForm 31 rs ra rb 28 True

nor :: R -> R -> R -> PPC
nor rs ra rb = xForm 31 rs ra rb 124 False

nor' :: R -> R -> R -> PPC
nor' rs ra rb = xForm 31 rs ra rb 124 True

or_ :: R -> R -> R -> PPC
or_ rs ra rb = xForm 31 rs ra rb 444 False

or' :: R -> R -> R -> PPC
or' rs ra rb = xForm 31 rs ra rb 444 True

ori :: R -> R -> Int -> PPC
ori ra rs ui = dForm 24 rs ra ui

mtctr :: R -> PPC
mtctr rs = PPC $ be $ 0x7c0903a6 .|. shiftL (reg rs) 21

stbux :: R -> R -> R -> PPC
stbux rs ra rb = PPC $ be $ 0x7c0001ee .|. shiftL (reg rs) 21 .|. shiftL (reg ra) 16 .|. shiftL (reg rb) 11

stb :: R -> Int -> R -> PPC
stb rs d ra = dForm 38 rs ra d

stw :: R -> Int -> R -> PPC
stw rs d ra = dForm 36 rs ra d

lwz :: R -> Int -> R -> PPC
lwz rt d ra = dForm 32 rt ra d


-- Mnemonics.

nop :: PPC
nop = ori R0 R0 0

not_ :: R -> R -> PPC
not_ rt ra = nor rt ra ra

set :: R -> Int -> PPC
set rt value | elem (shiftR value 16) [0, -1] = addi rt R0 value
             | otherwise                      = addis rt R0 (shiftR value 16) +++ ori rt rt value

-- | Custom assembly.
asm :: [Word8] -> PPC
asm a = PPC a


