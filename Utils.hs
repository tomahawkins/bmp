-- | Common utilities.
module Utils
  ( be
  , le
  , srec
  ) where

import Data.Bits
import Data.Word
import Text.Printf

-- | S-Record file generation given base address, entry address, and program image.  Disassemble with: powerpc-eabi-objdump -b srec -m powerpc -EB -D test.sr
srec :: Int -> Int -> [Word8] -> String
srec base start image = unlines $ records ++ [record "S5" $ be $ length records, record "S7" $ be start]
  where
  chunks [] = []
  chunks a = take 0x40 a : chunks (drop 0x40 a)

  records :: [String]
  records = [ record "S3" $ be addr ++ chunk | (chunk, addr) <- zip (chunks image) [base, base + 0x40 ..] ]

  record :: String -> [Word8] -> String
  record header payload = header ++ concatMap (printf "%02x") payload2
    where
    payload1 = [fromIntegral $ length payload + 1] ++ payload
    payload2 = payload1 ++ [complement $ sum payload1]

{-
elf :: [Word8] -> [Word8]
elf =
  where
  header =
    [ 0x7f, 0x45, 0x4c, 0x46, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 -- e_ident
    , 0x00, 0x02              -- e_type
    , 0, 20                   -- e_machine
    , 0, 0, 0, 1              -- e_version
    , 0x00, 0x08, 0x00, 0x0c  -- e_entry
    , 0, 0, 0, 0              -- e_phoff
    , 0, 0, 0, 0              -- e_shoff
    , 0, 0, 0, 0              -- e_flags
    , 0, 52                   -- e_ehsize
    , 0, 0                    -- e_phentsize
    , 0, 0                    -- e_phnum
    , 0, 0                    -- e_shentsize
    , 0, 0                    -- e_shnum
    , 0, 0                    -- e_shstrndx
    ]
-}

-- | Encode ints in little endian.
le :: Int -> [Word8]
le a = [ fromIntegral $ shiftR a n .&. 0xFF | n <- [0, 8 .. 24] ]

-- | Encode ints in big endian.
be :: Int -> [Word8]
be a = [ fromIntegral $ shiftR a n .&. 0xFF | n <- [24, 16 .. 0] ]

