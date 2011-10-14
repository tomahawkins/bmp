-- | Compiling MTL to PPC.
module MTLPPC (compile) where

import Data.List

import MTL
import PPC

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

  And a b -> (asmA +++ asmB +++ and_ rA rA rB, rA)
    where
    (asmA, rA) = compileE regs a
    (asmB, rB) = compileE (delete rA regs) b

  Or a b -> (asmA +++ asmB +++ or_ rA rA rB, rA)
    where
    (asmA, rA) = compileE regs a
    (asmB, rB) = compileE (delete rA regs) b

  Not a -> (asmA +++ not_ rA rA, rA)
    where
    (asmA, rA) = compileE regs a

