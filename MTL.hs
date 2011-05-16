-- | Memory Transfer Level (MTL) is a low level, platform independent, assembly language.
module MTL
  ( E (..)
  , S (..)
  , compile
  ) where

import Data.Bits
import Data.List
import Data.Word

import PPC

infixr 0 :=

-- | Expressions.
data E
  = Const Int      -- ^ Constant value.
  | Deref E        -- ^ Dereference a pointer.  Reads value at address.
  | Add E E        -- ^ Addition.
  | And E E        -- ^ Bitwise AND.
  | Or  E E        -- ^ Bitwise OR.
  | Not E          -- ^ Bitwise NOT.
  | Shift E Int    -- ^ Shift left by constant amount.
  deriving (Show, Eq)

instance Num E where
  (+) = Add
  (-) = undefined
  (*) = undefined
  negate a = 0 - a
  abs _ = undefined
  signum _ = undefined
  fromInteger = Const . fromInteger

instance Bits E where
  (.&.) = And
  (.|.) = Or
  complement = Not
  xor a b = (a .&. complement b) .|. (complement a .&. b)
  shift = Shift
  rotate = undefined
  bitSize _ = 32
  isSigned = undefined

-- | Statements.
data S
  = E := E       -- ^ Assigns a value to an address: address := value
  | Seq [S]      -- ^ Sequence of statements.
  | Asm [Word8]  -- ^ Custom assembly.
  deriving (Show, Eq)

-- | Compiles an MTL program to PPC.
compile :: S -> PPC
compile a = set R0 0 +++ compileS a

-- | Compiles a statement.
compileS :: S -> PPC
compileS a = case a of
  Seq a -> foldl1 (+++) $ map compileS a
  Asm a -> asm a
  a := b -> asmA +++ asmB +++ stw rB 0 rA
    where
    (asmA, rA) = compileE regs a
    (asmB, rB) = compileE (delete rA regs) b
    regs = delete R0 registers

-- | Compiles an expression, returning the computation and register of the result.
compileE :: [R] -> E -> (PPC, R)
compileE [] a = error $ "Ran out of registers to compute expression: " ++ show a ++ ".  Yes, this compiler is lame."
compileE regs a = case a of
  Const a -> (set (head regs) a, head regs)

  Deref a -> (asmA +++ lwz rA 0 rA, rA)
    where
    (asmA, rA) = compileE regs a

  Add a b -> (asmA +++ asmB +++ add rA rA rB, rA)
    where
    (asmA, rA) = compileE regs a
    (asmB, rB) = compileE (delete rA regs) b

  And _ _     -> undefined
  Or  _ _     -> undefined
  Not _       -> undefined
  Shift _ _   -> undefined

