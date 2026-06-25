module Tests.MCS51.ISASim where

import Prelude
import Data.Bits (shiftR, (.&.))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import Isacle.ISA (EncodingInfo(..), runCPUDef, ISADef(..))
import Isacle.ISA.Backend.Sim

import MCS51.ISA       (mcs51ISA)
import MCS51.ISA.Types (mcs51CPUDef, MCS51ALU(..))

-- ---------------------------------------------------------------------------
-- Type alias for the MCS-51 simulation monad
-- ---------------------------------------------------------------------------

type M = SimM MCS51ALU 8 8 8 16

-- ---------------------------------------------------------------------------
-- Pre-compute ALU record and instruction table at module load time
-- ---------------------------------------------------------------------------

aluRec :: MCS51ALU
(aluRec, _) = runCPUDef mcs51CPUDef

-- | [(EncodingInfo, body)] for every instruction in mcs51ISA.
--   EncodingInfo is extracted by running each body against a blank state;
--   the 'encoding' call is always the first thing an instruction body does.
instrTable :: [(EncodingInfo, M ())]
instrTable =
    [ (enc body, body) | body <- isaInstrs mcs51ISA ]
  where
    enc body = case ssEncoding (execInstr aluRec 0 body) of
        Just e  -> e
        Nothing -> error "instruction body missing 'encoding' call"

-- ---------------------------------------------------------------------------
-- Opcode-byte decode
--
-- All encodings are now exactly 8 bits (opcode byte only).  Extra operand
-- bytes are fetched via readCode inside the instruction body.
-- ---------------------------------------------------------------------------

decodeOp :: Integer -> Maybe (EncodingInfo, M ())
decodeOp opByte = foldr check Nothing instrTable
  where
    check (enc, body) acc =
        if (opByte .&. encMask enc) == encValue enc
        then Just (enc, body)
        else acc

-- ---------------------------------------------------------------------------
-- Fetch-decode-execute loop
-- ---------------------------------------------------------------------------

-- | Single step: fetch opcode from code memory at PC, decode, run.
--   Instruction bodies write PC explicitly (pcAdvanceN / absJump).
--   Safety net: if the body left PC unchanged (single-byte instruction
--   without an explicit pcAdvance1), advance by 1.
step :: SimState -> SimState
step st =
    let pcNow  = Map.findWithDefault 0 "PC" (scRegs (ssCPU st))
        opByte = IntMap.findWithDefault 0 (fromIntegral pcNow) (ssCodeMem st)
    in case decodeOp opByte of
        Nothing       -> st   -- unknown opcode: halt
        Just (_, body) ->
            let st'     = runInstr aluRec opByte body st
                pcAfter = Map.findWithDefault 0 "PC" (scRegs (ssCPU st'))
            in if pcAfter == pcNow
               -- Safety net: body didn't write PC, advance past the opcode byte
               then st' { ssCPU = (ssCPU st')
                        { scRegs = Map.insert "PC" (pcNow + 1)
                                       (scRegs (ssCPU st')) }}
               else st'

runProg :: [Integer] -> Int -> SimState
runProg code steps = go steps st0
  where
    st0 = emptySim
        { ssCodeMem = IntMap.fromList (zip [0..] code)
        , ssCPU     = SimCPU (Map.fromList [("PC", 0), ("SP", 0x07)])
        }
    go 0 st = st
    go n st = go (n - 1) (step st)

-- ---------------------------------------------------------------------------
-- Read helpers
-- ---------------------------------------------------------------------------

getReg :: SimState -> String -> Integer
getReg st name = Map.findWithDefault 0 name (scRegs (ssCPU st))

getMem :: SimState -> Int -> Integer
getMem st addr = IntMap.findWithDefault 0 addr (ssDataMem st)

-- | Extract a single bit from the PSW register.
-- flagPack assigns MSB-first: CY=7, AC=6, F0=5, RS1=4, RS0=3, OV=2, F1=1, P=0.
getPSW :: SimState -> Int -> Integer
getPSW st bit = (Map.findWithDefault 0 "PSW" (scRegs (ssCPU st)) `shiftR` bit) .&. 1

pswCY, pswAC, pswOV, pswP :: Int
pswCY = 7; pswAC = 6; pswOV = 2; pswP = 0

-- ---------------------------------------------------------------------------
-- Instruction encodings (as Integer byte lists, MSB first)
-- ---------------------------------------------------------------------------

nop :: [Integer]
nop = [0x00]

movAImm :: Integer -> [Integer]
movAImm imm = [0x74, imm]

addAImm :: Integer -> [Integer]
addAImm imm = [0x24, imm]

addARn :: Int -> [Integer]
addARn n = [0x28 + fromIntegral n]

movRnA :: Int -> [Integer]
movRnA n = [0xF8 + fromIntegral n]

movARn :: Int -> [Integer]
movARn n = [0xE8 + fromIntegral n]

incA :: [Integer]
incA = [0x04]

decA :: [Integer]
decA = [0x14]

subbAImm :: Integer -> [Integer]
subbAImm imm = [0x94, imm]

sjmp :: Int -> [Integer]
sjmp rel = [0x80, fromIntegral rel .&. 0xFF]

ljmp :: Int -> [Integer]
ljmp addr = [0x02, fromIntegral (addr `shiftR` 8) .&. 0xFF,
                   fromIntegral addr .&. 0xFF]

lcall :: Int -> [Integer]
lcall addr = [0x12, fromIntegral (addr `shiftR` 8) .&. 0xFF,
                    fromIntegral addr .&. 0xFF]

ret :: [Integer]
ret = [0x22]

jz :: Int -> [Integer]
jz rel = [0x60, fromIntegral rel .&. 0xFF]

jnz :: Int -> [Integer]
jnz rel = [0x70, fromIntegral rel .&. 0xFF]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

-- NOP: PC advances by 1, nothing else changes.
prop_nop :: H.Property
prop_nop = H.withTests 1 . H.property $ do
    let st = runProg [0x00] 1
    getReg st "PC" H.=== 1
    getReg st "A"  H.=== 0

-- MOV A, #imm: loads immediate into A.
prop_mov_a_imm :: H.Property
prop_mov_a_imm = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 0x42) 1
    getReg st "A"  H.=== 0x42
    getReg st "PC" H.=== 2

-- ADD A, #imm: A = A + imm (A starts at 0).
prop_add_a_imm :: H.Property
prop_add_a_imm = H.withTests 1 . H.property $ do
    -- MOV A, #10; ADD A, #32 → A = 42
    let st = runProg (movAImm 10 ++ addAImm 32) 2
    getReg st "A" H.=== 42

-- INC A / DEC A: increment and decrement accumulator.
prop_inc_dec :: H.Property
prop_inc_dec = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 5 ++ incA ++ incA ++ decA) 4
    getReg st "A" H.=== 6

-- SUBB A, #imm with CY=0: A = A - imm.
prop_subb_no_borrow :: H.Property
prop_subb_no_borrow = H.withTests 1 . H.property $ do
    -- MOV A, #10; SUBB A, #3 → A = 7 (CY=0 initially)
    let st = runProg (movAImm 10 ++ subbAImm 3) 2
    getReg st "A" H.=== 7

-- MOV Rn, A / MOV A, Rn: round-trip through IRAM register slot.
prop_mov_rn_roundtrip :: H.Property
prop_mov_rn_roundtrip = H.withTests 1 . H.property $ do
    -- MOV A, #0x55; MOV R3, A; MOV A, #0; MOV A, R3 → A = 0x55
    let code = movAImm 0x55 ++ movRnA 3 ++ movAImm 0 ++ movARn 3
    let st = runProg code 4
    getReg st "A" H.=== 0x55

-- SJMP: forward relative jump skips one instruction.
prop_sjmp_forward :: H.Property
prop_sjmp_forward = H.withTests 1 . H.property $ do
    -- SJMP +2 (skip the next 2-byte MOV); MOV A, #0xFF (skipped); MOV A, #7
    let code = sjmp 2 ++ movAImm 0xFF ++ movAImm 7
    let st = runProg code 2
    getReg st "A" H.=== 7

-- LJMP: absolute jump to a distant address.
prop_ljmp :: H.Property
prop_ljmp = H.withTests 1 . H.property $ do
    -- At 0x00: LJMP 0x10; pad to 0x10: MOV A, #99
    let code = ljmp 0x10
                ++ replicate (0x10 - length (ljmp 0x10)) 0x00
                ++ movAImm 99
    let st = runProg code 2
    getReg st "A"  H.=== 99
    getReg st "PC" H.=== 0x12

-- LCALL + RET: call/return preserves return address and restores stack.
prop_lcall_ret :: H.Property
prop_lcall_ret = H.withTests 1 . H.property $ do
    -- 0x00: LCALL 0x10      (3 bytes, return addr = 0x03)
    -- 0x10: MOV A, #42      (2 bytes)
    -- 0x12: RET             (1 byte)
    let code = lcall 0x10
                ++ replicate (0x10 - length (lcall 0x10)) 0x00
                ++ movAImm 42
                ++ ret
    let st = runProg code 3
    getReg st "A"  H.=== 42
    getReg st "PC" H.=== 0x03   -- returned to instruction after LCALL
    getReg st "SP" H.=== 0x07   -- stack back to initial value

-- JZ: jumps when A == 0, falls through when A /= 0.
prop_jz :: H.Property
prop_jz = H.withTests 1 . H.property $ do
    -- A = 0: JZ +2 (skip); MOV A, #1 (skipped); MOV A, #99
    let codeJumps = movAImm 0 ++ jz 2 ++ movAImm 1 ++ movAImm 99
    let stJ = runProg codeJumps 3
    getReg stJ "A" H.=== 99
    -- A = 5: JZ falls through; MOV A, #1 executes
    let codeFalls = movAImm 5 ++ jz 2 ++ movAImm 1
    let stF = runProg codeFalls 3
    getReg stF "A" H.=== 1

-- Multi-step arithmetic: sum 1..5 using DJNZ-style loop (with SUBB + JNZ).
-- R0 = counter (5→0), A accumulates.
prop_accumulate :: H.Property
prop_accumulate = H.withTests 1 . H.property $ do
    -- 0x00: MOV A, #0         (2) → init accumulator
    -- 0x02: MOV R0, #5... but MOV Rn,#imm is a 2-byte instr: 0x78 #imm
    -- Use: MOV A, #5; MOV R0, A; MOV A, #0
    -- loop (0x07): ADD A, R0 (1); SUBB R0... we don't have SUBB Rn, use:
    --   MOV A, R0; SUBB A, #1; MOV R0, A; MOV A, accum; ADD A, R0... complex.
    -- Simpler: use DEC Rn and JNZ.
    -- Encoding for DEC Rn: "00011rrr" = 0x18+n
    -- JNZ rel: 0x70 rel
    -- Encoding for MOV Rn, #imm: "01111rrr_iiiiiiii" = 0x78+n, imm
    --
    -- Program: count = R0 = 5, sum = R1 = 0
    -- 0x00: MOV R0, #5       (0x78, 0x05)
    -- 0x02: MOV R1, #0       (0x79, 0x00)
    -- loop:
    -- 0x04: MOV A, R1        (0xE9)
    -- 0x05: ADD A, R0        (0x28)
    -- 0x06: MOV R1, A        (0xF9)
    -- 0x07: DEC R0           (0x18)
    -- 0x08: MOV A, R0        (0xE8)
    -- 0x09: JNZ loop (-7 = 0xF9)  (0x70, 0xF9)
    -- 0x0B: MOV A, R1        (0xE9)  ← read result
    let code = [ 0x78, 0x05   -- MOV R0, #5
               , 0x79, 0x00   -- MOV R1, #0
               -- loop @ 0x04:
               , 0xE9         -- MOV A, R1
               , 0x28         -- ADD A, R0
               , 0xF9         -- MOV R1, A
               , 0x18         -- DEC R0
               , 0xE8         -- MOV A, R0
               , 0x70, 0xF9   -- JNZ -7 → back to 0x04
               , 0xE9         -- MOV A, R1  (read result)
               ]
    -- The loop runs 5 times (R0: 5,4,3,2,1,0), summing 5+4+3+2+1 = 15.
    -- After the loop, need one more step to MOV A, R1.
    let st = runProg code 37   -- 2 init + 5 iterations × 6 steps + 1 final read + slack
    getReg st "A" H.=== 15

-- ---------------------------------------------------------------------------
-- Flag tests
-- ---------------------------------------------------------------------------

-- ADD A, #1 where A=127: result=128, OV=1, CY=0
prop_add_overflow :: H.Property
prop_add_overflow = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 127 ++ addAImm 1) 2
    getReg st "A"          H.=== 128
    getPSW st pswOV        H.=== 1
    getPSW st pswCY        H.=== 0

-- ADD A, #200 where A=200: result=144, CY=1, OV=0
prop_add_carry :: H.Property
prop_add_carry = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 200 ++ addAImm 200) 2
    getReg st "A"          H.=== 144
    getPSW st pswCY        H.=== 1
    getPSW st pswOV        H.=== 0

-- ADD A, #0 where A=7: P=1 (three 1-bits → odd parity)
prop_add_parity :: H.Property
prop_add_parity = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 7 ++ addAImm 0) 2
    getReg st "A"          H.=== 7
    getPSW st pswP         H.=== 1

-- ADD A, #0 where A=3: P=0 (two 1-bits → even parity)
prop_add_parity_even :: H.Property
prop_add_parity_even = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 3 ++ addAImm 0) 2
    getReg st "A"          H.=== 3
    getPSW st pswP         H.=== 0

-- SUBB A, #10 where A=3, CY=0: borrow → result=249, CY=1
prop_subb_borrow :: H.Property
prop_subb_borrow = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 3 ++ subbAImm 10) 2
    getReg st "A"          H.=== 249
    getPSW st pswCY        H.=== 1

-- SUBB overflow: A=127, B=128, CY=0 → signed overflow and borrow
prop_subb_overflow :: H.Property
prop_subb_overflow = H.withTests 1 . H.property $ do
    let st = runProg (movAImm 127 ++ subbAImm 128) 2
    getPSW st pswOV        H.=== 1
    getPSW st pswCY        H.=== 1

-- ADDC carry propagation: ADD 200+200=144, CY=1; MOV A,#0; ADDC A,#0 → A=1, CY=0
prop_addc_carry_prop :: H.Property
prop_addc_carry_prop = H.withTests 1 . H.property $ do
    let addcAImm imm = [0x34, imm]
    let code = movAImm 200 ++ addAImm 200 ++ movAImm 0 ++ addcAImm 0
    let st = runProg code 4
    getReg st "A"          H.=== 1
    getPSW st pswCY        H.=== 0

isaSimTests :: TestTree
isaSimTests = $(testGroupGenerator)
