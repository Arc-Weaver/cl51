module Tests.MCS51.Instructions where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import Clash.Prelude (Bit)

import MCS51.Core
import MCS51.InstructionSet
import MCS51.ALU
import MCS51.Exec

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Zero state with ACC pre-loaded.
withAcc :: MCS51Word -> CoreData
withAcc v = zeroState { acc = v }

-- | Zero state with B register pre-loaded.
withB :: MCS51Word -> CoreData
withB v = zeroState { breg = v }

-- | Zero state with ACC and B pre-loaded.
withAB :: MCS51Word -> MCS51Word -> CoreData
withAB a b = zeroState { acc = a, breg = b }

-- | Run a single instruction via mcs51Compute.
exec1 :: Instruction -> CoreData -> CoreData
exec1 instr c = mcs51Compute instr Nothing c

-- ---------------------------------------------------------------------------
-- ADD tests
-- ---------------------------------------------------------------------------

prop_add_imm_basic :: H.Property
prop_add_imm_basic = H.withTests 1 . H.property $ do
    let c = exec1 (AddA_imm 3) (withAcc 5)
    acc c H.=== 8

prop_add_imm_carry :: H.Property
prop_add_imm_carry = H.withTests 1 . H.property $ do
    let c = exec1 (AddA_imm 1) (withAcc 0xFF)
    acc c H.=== 0
    psw_cy (psw c) H.=== (1 :: Bit)

prop_add_imm_no_carry :: H.Property
prop_add_imm_no_carry = H.withTests 1 . H.property $ do
    let c = exec1 (AddA_imm 1) (withAcc 0x7F)
    psw_cy (psw c) H.=== (0 :: Bit)

prop_addc_uses_carry :: H.Property
prop_addc_uses_carry = H.withTests 1 . H.property $ do
    let base  = withAcc 5
        c0    = base { psw = (psw base) { psw_cy = 1 } }
        c1    = exec1 (AddcA_imm 3) c0
    acc c1 H.=== 9   -- 5 + 3 + 1

-- ---------------------------------------------------------------------------
-- SUBB tests
-- ---------------------------------------------------------------------------

prop_subb_basic :: H.Property
prop_subb_basic = H.withTests 1 . H.property $ do
    let c = exec1 (SubbA_imm 3) (withAcc 10)
    acc c H.=== 7
    psw_cy (psw c) H.=== (0 :: Bit)

prop_subb_borrow :: H.Property
prop_subb_borrow = H.withTests 1 . H.property $ do
    let c = exec1 (SubbA_imm 1) (withAcc 0)
    psw_cy (psw c) H.=== (1 :: Bit)

-- ---------------------------------------------------------------------------
-- MUL / DIV tests
-- ---------------------------------------------------------------------------

prop_mul_basic :: H.Property
prop_mul_basic = H.withTests 1 . H.property $ do
    let c = exec1 MulAB (withAB 0x10 0x05)
    acc c  H.=== 0x50   -- 16 * 5 = 80 = 0x50
    breg c H.=== 0      -- high byte = 0

prop_mul_overflow :: H.Property
prop_mul_overflow = H.withTests 1 . H.property $ do
    let c = exec1 MulAB (withAB 0xFF 0xFF)
    -- 255 * 255 = 65025 = 0xFE01
    acc c  H.=== 0x01
    breg c H.=== 0xFE
    psw_ov (psw c) H.=== (1 :: Bit)

prop_div_basic :: H.Property
prop_div_basic = H.withTests 1 . H.property $ do
    let c = exec1 DivAB (withAB 10 3)
    acc c  H.=== 3   -- quotient
    breg c H.=== 1   -- remainder

prop_div_by_zero_sets_ov :: H.Property
prop_div_by_zero_sets_ov = H.withTests 1 . H.property $ do
    let c = exec1 DivAB (withAB 5 0)
    psw_ov (psw c) H.=== (1 :: Bit)

-- ---------------------------------------------------------------------------
-- Logical tests
-- ---------------------------------------------------------------------------

prop_anl_basic :: H.Property
prop_anl_basic = H.withTests 1 . H.property $ do
    let c = exec1 (AnlA_imm 0x0F) (withAcc 0xFF)
    acc c H.=== 0x0F

prop_orl_basic :: H.Property
prop_orl_basic = H.withTests 1 . H.property $ do
    let c = exec1 (OrlA_imm 0x0F) (withAcc 0xF0)
    acc c H.=== 0xFF

prop_xrl_basic :: H.Property
prop_xrl_basic = H.withTests 1 . H.property $ do
    let c = exec1 (XrlA_imm 0xFF) (withAcc 0xAA)
    acc c H.=== 0x55

prop_clr_a :: H.Property
prop_clr_a = H.withTests 1 . H.property $ do
    let c = exec1 ClrA (withAcc 0xFF)
    acc c H.=== 0

prop_cpl_a :: H.Property
prop_cpl_a = H.withTests 1 . H.property $ do
    let c = exec1 CplA (withAcc 0xAA)
    acc c H.=== 0x55

-- ---------------------------------------------------------------------------
-- Rotate tests
-- ---------------------------------------------------------------------------

prop_rl_a :: H.Property
prop_rl_a = H.withTests 1 . H.property $ do
    let c = exec1 RlA (withAcc 0x80)
    acc c H.=== 0x01

prop_rr_a :: H.Property
prop_rr_a = H.withTests 1 . H.property $ do
    let c = exec1 RrA (withAcc 0x01)
    acc c H.=== 0x80

prop_rlc_a :: H.Property
prop_rlc_a = H.withTests 1 . H.property $ do
    let base = withAcc 0x80
        c0   = base { psw = (psw base) { psw_cy = 0 } }
        c1   = exec1 RlcA c0
    acc c1       H.=== 0x00
    psw_cy (psw c1) H.=== (1 :: Bit)

prop_rrc_a :: H.Property
prop_rrc_a = H.withTests 1 . H.property $ do
    let base = withAcc 0x01
        c0   = base { psw = (psw base) { psw_cy = 0 } }
        c1   = exec1 RrcA c0
    acc c1          H.=== 0x00
    psw_cy (psw c1) H.=== (1 :: Bit)

prop_swap_a :: H.Property
prop_swap_a = H.withTests 1 . H.property $ do
    let c = exec1 SwapA (withAcc 0xAB)
    acc c H.=== 0xBA

-- ---------------------------------------------------------------------------
-- INC / DEC tests
-- ---------------------------------------------------------------------------

prop_inc_a :: H.Property
prop_inc_a = H.withTests 1 . H.property $ do
    let c = exec1 IncA (withAcc 0x0F)
    acc c H.=== 0x10

prop_inc_a_wraps :: H.Property
prop_inc_a_wraps = H.withTests 1 . H.property $ do
    let c = exec1 IncA (withAcc 0xFF)
    acc c H.=== 0x00

prop_dec_a :: H.Property
prop_dec_a = H.withTests 1 . H.property $ do
    let c = exec1 DecA (withAcc 0x01)
    acc c H.=== 0x00

prop_inc_dptr :: H.Property
prop_inc_dptr = H.withTests 1 . H.property $ do
    let c0 = setDptr zeroState 0x00FF
        c1 = exec1 IncDptr c0
    getDptr c1 H.=== 0x0100

-- ---------------------------------------------------------------------------
-- MOV / data transfer tests
-- ---------------------------------------------------------------------------

prop_mov_a_imm :: H.Property
prop_mov_a_imm = H.withTests 1 . H.property $ do
    let c = exec1 (MovA_imm 0x42) zeroState
    acc c H.=== 0x42

prop_mov_a_rn :: H.Property
prop_mov_a_rn = H.withTests 1 . H.property $ do
    let c0 = setReg zeroState 2 0xAB
        c1 = exec1 (MovA_rn 2) c0
    acc c1 H.=== 0xAB

prop_mov_rn_a :: H.Property
prop_mov_rn_a = H.withTests 1 . H.property $ do
    let c0 = withAcc 0x77
        c1 = exec1 (MovRn_A 3) c0
    getReg c1 3 H.=== 0x77

prop_mov_dptr :: H.Property
prop_mov_dptr = H.withTests 1 . H.property $ do
    let c = exec1 (MovDptr 0x1234) zeroState
    getDptr c H.=== 0x1234

prop_xch_a_rn :: H.Property
prop_xch_a_rn = H.withTests 1 . H.property $ do
    let c0 = (withAcc 0xAA) { breg = 0 }
        c1 = setReg c0 1 0xBB
        c2 = exec1 (XchA_rn 1) c1
    acc c2    H.=== 0xBB
    getReg c2 1 H.=== 0xAA

-- ---------------------------------------------------------------------------
-- Boolean / bit operation tests
-- ---------------------------------------------------------------------------

prop_clr_c :: H.Property
prop_clr_c = H.withTests 1 . H.property $ do
    let base = zeroState { psw = (psw zeroState) { psw_cy = 1 } }
        c    = exec1 ClrC base
    psw_cy (psw c) H.=== (0 :: Bit)

prop_setb_c :: H.Property
prop_setb_c = H.withTests 1 . H.property $ do
    let c = exec1 SetbC zeroState
    psw_cy (psw c) H.=== (1 :: Bit)

prop_cpl_c :: H.Property
prop_cpl_c = H.withTests 1 . H.property $ do
    let c = exec1 CplC zeroState
    psw_cy (psw c) H.=== (1 :: Bit)

-- ---------------------------------------------------------------------------
-- DA A test
-- ---------------------------------------------------------------------------

-- After BCD add of 0x09 + 0x01 = 0x0A, DA A should adjust to 0x10.
prop_da_a_adjust_low_nibble :: H.Property
prop_da_a_adjust_low_nibble = H.withTests 1 . H.property $ do
    -- Simulating BCD add result: 0x0A in ACC
    let base = withAcc 0x0A
        -- AC = 0 (no carry from bit 3, but low nibble > 9)
        c0   = base { psw = (psw base) { psw_cy = 0, psw_ac = 0 } }
        c1   = exec1 DaA c0
    acc c1 H.=== 0x10

-- ---------------------------------------------------------------------------
-- PUSH / POP stack tests (uses IRAM)
-- ---------------------------------------------------------------------------

prop_push_pop_roundtrip :: H.Property
prop_push_pop_roundtrip = H.withTests 1 . H.property $ do
    -- Set up IRAM address 0x30 with value 0xAB via writeDirect
    let c0 = writeDirect zeroState 0x30 0xAB
    -- Push direct address 0x30
    let c1 = exec1 (PushDir 0x30) c0
    -- SP should have incremented from 0x07 to 0x08
    sp c1 H.=== 0x08
    -- Pop it back into ACC via direct address 0xE0 (ACC SFR)
    let c2 = exec1 (PopDir 0xE0) c1
    acc c2 H.=== 0xAB

-- ---------------------------------------------------------------------------
-- CJNE / DJNZ flag tests (runWithPC tests carry correctness)
-- ---------------------------------------------------------------------------

prop_cjne_sets_carry_when_less :: H.Property
prop_cjne_sets_carry_when_less = H.withTests 1 . H.property $ do
    -- CJNE A, #imm, rel: sets CY if A < imm
    let c = exec1 (CjneA_imm 0x10 0x00) (withAcc 0x05)
    psw_cy (psw c) H.=== (1 :: Bit)

prop_cjne_clears_carry_when_not_less :: H.Property
prop_cjne_clears_carry_when_not_less = H.withTests 1 . H.property $ do
    let c = exec1 (CjneA_imm 0x05 0x00) (withAcc 0x10)
    psw_cy (psw c) H.=== (0 :: Bit)

prop_djnz_rn_decrements :: H.Property
prop_djnz_rn_decrements = H.withTests 1 . H.property $ do
    let c0 = setReg zeroState 0 5
        c1 = exec1 (DjnzRn 0 0x00) c0
    getReg c1 0 H.=== 4

-- ---------------------------------------------------------------------------
-- runLinear integration test
-- ---------------------------------------------------------------------------

-- Load 5, load 3, add → ACC should be 8.
prop_runLinear_add :: H.Property
prop_runLinear_add = H.withTests 1 . H.property $ do
    let prog = [ 0x74, 0x05   -- MOV A, #5
               , 0xF8         -- MOV R0, A
               , 0x74, 0x03   -- MOV A, #3
               , 0x28         -- ADD A, R0
               ]
    let c = runLinear prog zeroState
    acc c H.=== 8

-- MOV A, #5 ; INC A ; INC A → 7
prop_runLinear_inc :: H.Property
prop_runLinear_inc = H.withTests 1 . H.property $ do
    let prog = [ 0x74, 0x05   -- MOV A, #5
               , 0x04          -- INC A
               , 0x04          -- INC A
               ]
    let c = runLinear prog zeroState
    acc c H.=== 7

-- ---------------------------------------------------------------------------
-- runWithPC jump test
-- ---------------------------------------------------------------------------

-- SJMP back to self: after enough execution steps the PC converges.
-- This is trivially safe because runWithPC uses Haskell laziness / stepcount.
--
-- More usefully: verify SJMP forward skips an INC.
--   byte 0: MOV A, #5   (2 bytes)
--   byte 2: SJMP +1     (2 bytes) → skip byte 4
--   byte 4: INC A       (1 byte)  ← SKIPPED
--   byte 5: INC A       (1 byte)  ← executed
prop_runWithPC_sjmp_forward :: H.Property
prop_runWithPC_sjmp_forward = H.withTests 1 . H.property $ do
    let prog = [ 0x74, 0x05   -- byte 0: MOV A, #5
               , 0x80, 0x01   -- byte 2: SJMP +1 → jump to byte 5
               , 0x04          -- byte 4: INC A  (skipped)
               , 0x04          -- byte 5: INC A  (executed)
               ]
    let c = runWithPC prog 6 zeroState
    acc c H.=== 6   -- 5 + 1 (one INC, not two)

instructionTests :: TestTree
instructionTests = $(testGroupGenerator)
