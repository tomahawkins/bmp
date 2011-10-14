-- | S08 assembly language and assembler.
module S08
  (
  -- * Types
    S08
  , A (..)
  , R (..)
  , reg
  -- * Assembler.
  , (+++)
  , assemble
  -- * Instructions
  , ldhx
  , txs
  -- * Mnemonics and Macros
  , asm
  ) where

import Data.Bits
import Data.Word

-- | An S08 program.
data S08 = S08 [Word8] | S08Seq S08 S08

-- | Sequencing S08 programs together.
(+++) :: S08 -> S08 -> S08
(+++) = S08Seq

-- | Assembles a PPC program into machine code.
assemble :: S08 -> [Word8]
assemble a = case a of
  S08 a -> a
  S08Seq a b -> assemble a ++ assemble b

-- | Custom assembly.
asm :: [Word8] -> S08
asm a = S08 a

-- | Addressing modes.
data A
  = IMM Int  -- ^ Immediate.
  | DIR Int  -- ^ Direct or extended.

op8 :: Word8 -> Int -> S08
op8 opcode operand = S08 [opcode, fromIntegral operand]

op16 :: Word8 -> Int -> S08
op16 opcode operand = S08 [opcode, fromIntegral $ shiftR operand 8, fromIntegral operand]

dir :: Word8 -> Word8 -> Int -> S08
dir dir ext operand
  | operand .&. 0xff00 == 0 = op8  dir operand
  | otherwise               = op16 ext operand

ldhx :: A -> S08
ldhx a = case a of
  IMM a -> op16 0x45 a
  DIR a -> dir  0x55 0x32 a

txs :: S08
txs = S08 [0x94]


-- | Initialize stack pointer to 0x107f (end of RAM).
initSP :: S08
initSP = ldhx (IMM 0x1080) +++ txs





















-- | Registers.
data R
  = PTAD
  | PTADD
  | PTBD
  | PTBDD
  | PTCD
  | PTCDD
  | PTDD
  | PTDDD
  | PTED
  | PTEDD
  | PTFD
  | PTFDD
  | PTGD
  | PTGDD
  | ACMP1SC
  | ACMP2SC
  | ADCSC1
  | ADCSC2
  | ADCRH
  | ADCRL
  | ADCCVH
  | ADCCVL
  | ADCCFG
  | APCTL1
  | APCTL2
  | APCTL3
  | IRQSC
  | TPM1SC
  | TPM1CNTH
  | TPM1CNTL
  | TPM1MODH
  | TPM1MODL
  | TPM1C0SC
  | TPM1C0VH
  | TPM1C0VL
  | TPM1C1SC
  | TPM1C1VH
  | TPM1C1VL
  | TPM1C2SC
  | TPM1C2VH
  | TPM1C2VL
  | TPM1C3SC
  | TPM1C3VH
  | TPM1C3VL
  | TPM1C4SC
  | TPM1C4VH
  | TPM1C4VL
  | TPM1C5SC
  | TPM1C5VH
  | TPM1C5VL
  | SCI1BDH
  | SCI1BDL
  | SCI1C1
  | SCI1C2
  | SCI1S1
  | SCI1S2
  | SCI1C3
  | SCI1D
  | SCI2BDH
  | SCI2BDL
  | SCI2C1
  | SCI2C2
  | SCI2S1
  | SCI2S2
  | SCI2C3
  | SCI2D
  | MCGC1
  | MCGC2
  | MCGTRM
  | MCGSC
  | MCGC3
  | SPIC1
  | SPIC2
  | SPIBR
  | SPIS
  | SPID
  | IICA
  | IICF
  | IICC1
  | IICS
  | IICD
  | IICC2
  | TPM2SC
  | TPM2CNTH
  | TPM2CNTL
  | TPM2MODH
  | TPM2MODL
  | TPM2C0SC
  | TPM2C0VH
  | TPM2C0VL
  | TPM2C1SC
  | TPM2C1VH
  | TPM2C1VL
  | RTCSC
  | RTCCNT
  | RTCMOD
  | SRS
  | SBDFR
  | SOPT1
  | SOPT2
  | SDIDH
  | SDIDL
  | SPMSC1
  | SPMSC2
  | DBGCAH
  | DBGCAL
  | DBGCBH
  | DBGCBL
  | DBGFH
  | DBGFL
  | DBGC
  | DBGT
  | DBGS
  | FCDIV
  | FOPT
  | FCNFG
  | FPROT
  | FSTAT
  | FCMD
  | PTAPE
  | PTASE
  | PTADS
  | PTASC
  | PTAPS
  | PTAES
  | PTBPE
  | PTBSE
  | PTBDS
  | PTBSC
  | PTBPS
  | PTBES
  | PTCPE
  | PTCSE
  | PTCDS
  | PTDPE
  | PTDSE
  | PTDDS
  | PTDSC
  | PTDPS
  | PTDES
  | PTEPE
  | PTESE
  | PTEDS
  | PTFPE
  | PTFSE
  | PTFDS
  | PTGPE
  | PTGSE
  | PTGDS
  | CANCTL0
  | CANCTL1
  | CANBTR0
  | CANBTR1
  | CANRFLG
  | CANRIER
  | CANTFLG
  | CANTIER
  | CANTARQ
  | CANTAAK
  | CANTBSEL
  | CANIDAC
  | CANMISC
  | CANRXERR
  | CANTXERR
  | CANIDAR0
  | CANIDAR1
  | CANIDAR2
  | CANIDAR3
  | CANIDMR0
  | CANIDMR1
  | CANIDMR2
  | CANIDMR3
  | CANIDAR4
  | CANIDAR5
  | CANIDAR6
  | CANIDAR7
  | CANIDMR4
  | CANIDMR5
  | CANIDMR6
  | CANIDMR7
  | CANTTSRH
  | CANTTSRL
  | CANRIDR0
  | CANRIDR1
  | CANRIDR2
  | CANRIDR3
  | CANRDSR0
  | CANRDSR1
  | CANRDSR2
  | CANRDSR3
  | CANRDSR4
  | CANRDSR5
  | CANRDSR6
  | CANRDSR7
  | CANRDLR
  | CANRTSRH
  | CANRTSRL
  | CANTIDR0
  | CANTIDR1
  | CANTIDR2
  | CANTIDR3
  | CANTDSR0
  | CANTDSR1
  | CANTDSR2
  | CANTDSR3
  | CANTDSR4
  | CANTDSR5
  | CANTDSR6
  | CANTDSR7
  | CANTDLR
  | CANTTBPR

-- | Register addresses.
reg :: R -> Int
reg a = case a of
  PTAD      -> 0x0000 
  PTADD     -> 0x0001 
  PTBD      -> 0x0002 
  PTBDD     -> 0x0003 
  PTCD      -> 0x0004 
  PTCDD     -> 0x0005 
  PTDD      -> 0x0006 
  PTDDD     -> 0x0007 
  PTED      -> 0x0008 
  PTEDD     -> 0x0009 
  PTFD      -> 0x000A 
  PTFDD     -> 0x000B 
  PTGD      -> 0x000C 
  PTGDD     -> 0x000D 
  ACMP1SC   -> 0x000E 
  ACMP2SC   -> 0x000F 
  ADCSC1    -> 0x0010 
  ADCSC2    -> 0x0011 
  ADCRH     -> 0x0012 
  ADCRL     -> 0x0013 
  ADCCVH    -> 0x0014 
  ADCCVL    -> 0x0015 
  ADCCFG    -> 0x0016 
  APCTL1    -> 0x0017 
  APCTL2    -> 0x0018 
  APCTL3    -> 0x0019 
  IRQSC     -> 0x001C 
  TPM1SC    -> 0x0020 
  TPM1CNTH  -> 0x0021 
  TPM1CNTL  -> 0x0022 
  TPM1MODH  -> 0x0023 
  TPM1MODL  -> 0x0024 
  TPM1C0SC  -> 0x0025 
  TPM1C0VH  -> 0x0026 
  TPM1C0VL  -> 0x0027 
  TPM1C1SC  -> 0x0028 
  TPM1C1VH  -> 0x0029 
  TPM1C1VL  -> 0x002A 
  TPM1C2SC  -> 0x002B 
  TPM1C2VH  -> 0x002C 
  TPM1C2VL  -> 0x002D 
  TPM1C3SC  -> 0x002E 
  TPM1C3VH  -> 0x002F 
  TPM1C3VL  -> 0x0030 
  TPM1C4SC  -> 0x0031 
  TPM1C4VH  -> 0x0032 
  TPM1C4VL  -> 0x0033 
  TPM1C5SC  -> 0x0034 
  TPM1C5VH  -> 0x0035 
  TPM1C5VL  -> 0x0036 
  SCI1BDH   -> 0x0038 
  SCI1BDL   -> 0x0039 
  SCI1C1    -> 0x003A 
  SCI1C2    -> 0x003B 
  SCI1S1    -> 0x003C 
  SCI1S2    -> 0x003D 
  SCI1C3    -> 0x003E 
  SCI1D     -> 0x003F 
  SCI2BDH   -> 0x0040 
  SCI2BDL   -> 0x0041 
  SCI2C1    -> 0x0042 
  SCI2C2    -> 0x0043 
  SCI2S1    -> 0x0044 
  SCI2S2    -> 0x0045 
  SCI2C3    -> 0x0046 
  SCI2D     -> 0x0047 
  MCGC1     -> 0x0048 
  MCGC2     -> 0x0049 
  MCGTRM    -> 0x004A 
  MCGSC     -> 0x004B 
  MCGC3     -> 0x004C 
  SPIC1     -> 0x0050 
  SPIC2     -> 0x0051 
  SPIBR     -> 0x0052 
  SPIS      -> 0x0053 
  SPID      -> 0x0055 
  IICA      -> 0x0058 
  IICF      -> 0x0059 
  IICC1     -> 0x005A 
  IICS      -> 0x005B 
  IICD      -> 0x005C 
  IICC2     -> 0x005D 
  TPM2SC    -> 0x0060 
  TPM2CNTH  -> 0x0061 
  TPM2CNTL  -> 0x0062 
  TPM2MODH  -> 0x0063 
  TPM2MODL  -> 0x0064 
  TPM2C0SC  -> 0x0065 
  TPM2C0VH  -> 0x0066 
  TPM2C0VL  -> 0x0067 
  TPM2C1SC  -> 0x0068 
  TPM2C1VH  -> 0x0069 
  TPM2C1VL  -> 0x006A 
  RTCSC     -> 0x006C 
  RTCCNT    -> 0x006D 
  RTCMOD    -> 0x006E 
  SRS       -> 0x1800 
  SBDFR     -> 0x1801 
  SOPT1     -> 0x1802 
  SOPT2     -> 0x1803 
  SDIDH     -> 0x1806 
  SDIDL     -> 0x1807 
  SPMSC1    -> 0x1809 
  SPMSC2    -> 0x180A 
  DBGCAH    -> 0x1810 
  DBGCAL    -> 0x1811 
  DBGCBH    -> 0x1812 
  DBGCBL    -> 0x1813 
  DBGFH     -> 0x1814 
  DBGFL     -> 0x1815 
  DBGC      -> 0x1816 
  DBGT      -> 0x1817 
  DBGS      -> 0x1818 
  FCDIV     -> 0x1820 
  FOPT      -> 0x1821 
  FCNFG     -> 0x1823 
  FPROT     -> 0x1824 
  FSTAT     -> 0x1825 
  FCMD      -> 0x1826 
  PTAPE     -> 0x1840 
  PTASE     -> 0x1841 
  PTADS     -> 0x1842 
  PTASC     -> 0x1844 
  PTAPS     -> 0x1845 
  PTAES     -> 0x1846 
  PTBPE     -> 0x1848 
  PTBSE     -> 0x1849 
  PTBDS     -> 0x184A 
  PTBSC     -> 0x184C 
  PTBPS     -> 0x184D 
  PTBES     -> 0x184E 
  PTCPE     -> 0x1850 
  PTCSE     -> 0x1851 
  PTCDS     -> 0x1852 
  PTDPE     -> 0x1858 
  PTDSE     -> 0x1859 
  PTDDS     -> 0x185A 
  PTDSC     -> 0x185C 
  PTDPS     -> 0x185D 
  PTDES     -> 0x185E 
  PTEPE     -> 0x1860 
  PTESE     -> 0x1861 
  PTEDS     -> 0x1862 
  PTFPE     -> 0x1868 
  PTFSE     -> 0x1869 
  PTFDS     -> 0x186A 
  PTGPE     -> 0x1870 
  PTGSE     -> 0x1871 
  PTGDS     -> 0x1872 
  CANCTL0   -> 0x1880 
  CANCTL1   -> 0x1881 
  CANBTR0   -> 0x1882 
  CANBTR1   -> 0x1883 
  CANRFLG   -> 0x1884 
  CANRIER   -> 0x1885 
  CANTFLG   -> 0x1886 
  CANTIER   -> 0x1887 
  CANTARQ   -> 0x1888 
  CANTAAK   -> 0x1889 
  CANTBSEL  -> 0x188A 
  CANIDAC   -> 0x188B 
  CANMISC   -> 0x188D 
  CANRXERR  -> 0x188E 
  CANTXERR  -> 0x188F 
  CANIDAR0  -> 0x1890 
  CANIDAR1  -> 0x1891 
  CANIDAR2  -> 0x1892 
  CANIDAR3  -> 0x1893 
  CANIDMR0  -> 0x1894 
  CANIDMR1  -> 0x1895 
  CANIDMR2  -> 0x1896 
  CANIDMR3  -> 0x1897 
  CANIDAR4  -> 0x1898 
  CANIDAR5  -> 0x1899 
  CANIDAR6  -> 0x189A 
  CANIDAR7  -> 0x189B 
  CANIDMR4  -> 0x189C 
  CANIDMR5  -> 0x189D 
  CANIDMR6  -> 0x189E 
  CANIDMR7  -> 0x189F 
  CANTTSRH  -> 0x18BE 
  CANTTSRL  -> 0x18BF 
  CANRIDR0  -> 0x18A0 
  CANRIDR1  -> 0x18A1 
  CANRIDR2  -> 0x18A2 
  CANRIDR3  -> 0x18A3 
  CANRDSR0  -> 0x18A4 
  CANRDSR1  -> 0x18A5 
  CANRDSR2  -> 0x18A6 
  CANRDSR3  -> 0x18A7 
  CANRDSR4  -> 0x18A8 
  CANRDSR5  -> 0x18A9 
  CANRDSR6  -> 0x18AA 
  CANRDSR7  -> 0x18AB 
  CANRDLR   -> 0x18AC 
  CANRTSRH  -> 0x18AE 
  CANRTSRL  -> 0x18AF 
  CANTIDR0  -> 0x18B0 
  CANTIDR1  -> 0x18B1 
  CANTIDR2  -> 0x18B2 
  CANTIDR3  -> 0x18B3 
  CANTDSR0  -> 0x18B4 
  CANTDSR1  -> 0x18B5 
  CANTDSR2  -> 0x18B6 
  CANTDSR3  -> 0x18B7 
  CANTDSR4  -> 0x18B8 
  CANTDSR5  -> 0x18B9 
  CANTDSR6  -> 0x18BA 
  CANTDSR7  -> 0x18BB 
  CANTDLR   -> 0x18BC 
  CANTTBPR  -> 0x18BD 

