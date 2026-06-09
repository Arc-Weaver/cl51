-- NB: NoImplicitPrelude is active from cabal common-options; re-import
-- standard Prelude explicitly so this module can use plain IO/Bits.
module MCS51.TH
    ( loadMcs51Bin
    ) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import qualified Data.ByteString as BS
import Data.Bits  (countLeadingZeros, finiteBitSize, bit)
import Data.Word  (Word8)

import Clash.Prelude (listToVecTH)

-- | Read an assembled MCS-51 flat binary at compile time and splice it in as a
--   @Vec n (BitVector 8)@.
--
--   The file path is relative to the project root (where GHC is invoked).
--   Each byte in the binary becomes one element (MCS-51 code memory is
--   byte-addressed with 8-bit fetch words — no word-swapping needed unlike AVR).
--   The binary is zero-padded with NOPs (@0x00@) to the next power of two,
--   so the resulting @Vec@ size is always a power of two — required for
--   efficient block-RAM addressing.
--
--   Usage:
--     -- In a Clash module:
--     testProgram :: Vec 256 (BitVector 8)
--     testProgram = $(loadMcs51Bin "example/Example/program.bin")
--
--   The Vec size must be annotated (or inferred from context); a mismatch
--   between the annotation and the actual padded file size is a type error.
--
--   GHC will recompile the module whenever @path@ changes because
--   @addDependentFile@ registers it as a dependency.
loadMcs51Bin :: FilePath -> Q Exp
loadMcs51Bin path = do
    addDependentFile path
    content <- runIO (BS.readFile path)
    let ws = padToPow2 (parseBytes (BS.unpack content))
    -- listToVecTH on [Integer] emits polymorphic numeric literals, which
    -- unify with BitVector 8 (or any Num instance) at the call site.
    listToVecTH ws

-- | Convert a raw byte stream to a list of integers.
parseBytes :: [Word8] -> [Integer]
parseBytes = map fromIntegral

-- | Pad a list to the next power of two with zeros (NOP for MCS-51).
padToPow2 :: [Integer] -> [Integer]
padToPow2 [] = [0]
padToPow2 xs =
    let n  = length xs
        n' = nextPow2 n
    in xs ++ replicate (n' - n) 0

-- | Smallest power of two >= n.
nextPow2 :: Int -> Int
nextPow2 n
    | n <= 1    = 1
    | otherwise = bit k
  where
    k = finiteBitSize (0 :: Int) - countLeadingZeros (n - 1)
