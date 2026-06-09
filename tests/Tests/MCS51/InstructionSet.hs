module Tests.MCS51.InstructionSet where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Prelude (Unsigned)

import MCS51.InstructionSet

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genByte :: H.Gen (Unsigned 8)
genByte = Gen.integral (Range.linear 0 255)

genBit :: H.Gen (Unsigned 8)
genBit = Gen.integral (Range.linear 0 127)   -- bit addresses 0x00-0x7F

genRel :: H.Gen (Unsigned 8)
genRel = Gen.integral (Range.linear 0 255)   -- relative offset (signed, stored as byte)

genRn :: H.Gen (Unsigned 3)
genRn = Gen.integral (Range.linear 0 7)

genRi :: H.Gen (Unsigned 1)
genRi = Gen.integral (Range.linear 0 1)

genAddr11 :: H.Gen (Unsigned 11)
genAddr11 = Gen.integral (Range.linear 0 2047)

genAddr16 :: H.Gen (Unsigned 16)
genAddr16 = Gen.integral (Range.linear 0 65535)

-- ---------------------------------------------------------------------------
-- instrBytes tests
-- ---------------------------------------------------------------------------

-- NOP is 1 byte.
prop_instrBytes_nop_is_1 :: H.Property
prop_instrBytes_nop_is_1 = H.withTests 1 . H.property $
    instrBytes 0x00 H.=== 1

-- LJMP is 3 bytes.
prop_instrBytes_ljmp_is_3 :: H.Property
prop_instrBytes_ljmp_is_3 = H.withTests 1 . H.property $
    instrBytes 0x02 H.=== 3

-- LCALL is 3 bytes.
prop_instrBytes_lcall_is_3 :: H.Property
prop_instrBytes_lcall_is_3 = H.withTests 1 . H.property $
    instrBytes 0x12 H.=== 3

-- SJMP is 2 bytes.
prop_instrBytes_sjmp_is_2 :: H.Property
prop_instrBytes_sjmp_is_2 = H.withTests 1 . H.property $
    instrBytes 0x80 H.=== 2

-- AJMP (opcode & 0x1F == 0x01) is 2 bytes.
prop_instrBytes_ajmp_is_2 :: H.Property
prop_instrBytes_ajmp_is_2 = H.withTests 1 . H.property $
    instrBytes 0x01 H.=== 2

-- ACALL (opcode & 0x1F == 0x11) is 2 bytes.
prop_instrBytes_acall_is_2 :: H.Property
prop_instrBytes_acall_is_2 = H.withTests 1 . H.property $
    instrBytes 0x11 H.=== 2

-- MOV DPTR, #data16 is 3 bytes.
prop_instrBytes_mov_dptr_is_3 :: H.Property
prop_instrBytes_mov_dptr_is_3 = H.withTests 1 . H.property $
    instrBytes 0x90 H.=== 3

-- RET is 1 byte.
prop_instrBytes_ret_is_1 :: H.Property
prop_instrBytes_ret_is_1 = H.withTests 1 . H.property $
    instrBytes 0x22 H.=== 1

-- RETI is 1 byte.
prop_instrBytes_reti_is_1 :: H.Property
prop_instrBytes_reti_is_1 = H.withTests 1 . H.property $
    instrBytes 0x32 H.=== 1

-- ADD A, #imm is 2 bytes.
prop_instrBytes_add_imm_is_2 :: H.Property
prop_instrBytes_add_imm_is_2 = H.withTests 1 . H.property $
    instrBytes 0x24 H.=== 2

-- ORL dir, #imm is 3 bytes.
prop_instrBytes_orl_dir_imm_is_3 :: H.Property
prop_instrBytes_orl_dir_imm_is_3 = H.withTests 1 . H.property $
    instrBytes 0x43 H.=== 3

-- MOV dir, dir is 3 bytes.
prop_instrBytes_mov_dir_dir_is_3 :: H.Property
prop_instrBytes_mov_dir_dir_is_3 = H.withTests 1 . H.property $
    instrBytes 0x85 H.=== 3

-- JBC bit, rel is 3 bytes.
prop_instrBytes_jbc_is_3 :: H.Property
prop_instrBytes_jbc_is_3 = H.withTests 1 . H.property $
    instrBytes 0x10 H.=== 3

-- JB bit, rel is 3 bytes.
prop_instrBytes_jb_is_3 :: H.Property
prop_instrBytes_jb_is_3 = H.withTests 1 . H.property $
    instrBytes 0x20 H.=== 3

-- JNB bit, rel is 3 bytes.
prop_instrBytes_jnb_is_3 :: H.Property
prop_instrBytes_jnb_is_3 = H.withTests 1 . H.property $
    instrBytes 0x30 H.=== 3

-- CJNE A, #imm, rel is 3 bytes.
prop_instrBytes_cjne_a_imm_is_3 :: H.Property
prop_instrBytes_cjne_a_imm_is_3 = H.withTests 1 . H.property $
    instrBytes 0xB4 H.=== 3

-- DJNZ dir, rel is 3 bytes.
prop_instrBytes_djnz_dir_is_3 :: H.Property
prop_instrBytes_djnz_dir_is_3 = H.withTests 1 . H.property $
    instrBytes 0xD5 H.=== 3

-- ---------------------------------------------------------------------------
-- Decode tests
-- ---------------------------------------------------------------------------

-- NOP decodes correctly.
prop_decode_nop :: H.Property
prop_decode_nop = H.withTests 1 . H.property $
    decodeInstruction 0x00 0x00 0x00 H.=== Nop

-- RET decodes correctly.
prop_decode_ret :: H.Property
prop_decode_ret = H.withTests 1 . H.property $
    decodeInstruction 0x22 0x00 0x00 H.=== Ret

-- RETI decodes correctly.
prop_decode_reti :: H.Property
prop_decode_reti = H.withTests 1 . H.property $
    decodeInstruction 0x32 0x00 0x00 H.=== Reti

-- LJMP with target 0x1234 decodes correctly.
prop_decode_ljmp :: H.Property
prop_decode_ljmp = H.withTests 1 . H.property $
    decodeInstruction 0x02 0x12 0x34 H.=== LjmpAddr 0x1234

-- LCALL with target 0xABCD decodes correctly.
prop_decode_lcall :: H.Property
prop_decode_lcall = H.withTests 1 . H.property $
    decodeInstruction 0x12 0xAB 0xCD H.=== LcallAddr 0xABCD

-- SJMP with offset 0x10 decodes correctly.
prop_decode_sjmp :: H.Property
prop_decode_sjmp = H.withTests 1 . H.property $
    decodeInstruction 0x80 0x10 0x00 H.=== SjmpRel 0x10

-- AJMP (first page: opcode 0x01) decodes correctly.
prop_decode_ajmp :: H.Property
prop_decode_ajmp = H.withTests 1 . H.property $
    decodeInstruction 0x01 0x50 0x00 H.=== AjmpAddr 0x050

-- ACALL (first page: opcode 0x11) decodes correctly.
prop_decode_acall :: H.Property
prop_decode_acall = H.withTests 1 . H.property $
    decodeInstruction 0x11 0x50 0x00 H.=== AcallAddr 0x050

-- MOV DPTR, #0x1234 decodes correctly.
prop_decode_mov_dptr :: H.Property
prop_decode_mov_dptr = H.withTests 1 . H.property $
    decodeInstruction 0x90 0x12 0x34 H.=== MovDptr 0x1234

-- ADD A, #0x42 decodes correctly.
prop_decode_add_imm :: H.Property
prop_decode_add_imm = H.withTests 1 . H.property $
    decodeInstruction 0x24 0x42 0x00 H.=== AddA_imm 0x42

-- ADD A, dir decodes correctly.
prop_decode_add_dir :: H.Property
prop_decode_add_dir = H.withTests 1 . H.property $
    decodeInstruction 0x25 0x30 0x00 H.=== AddA_dir 0x30

-- INC A decodes correctly.
prop_decode_inc_a :: H.Property
prop_decode_inc_a = H.withTests 1 . H.property $
    decodeInstruction 0x04 0x00 0x00 H.=== IncA

-- DEC A decodes correctly.
prop_decode_dec_a :: H.Property
prop_decode_dec_a = H.withTests 1 . H.property $
    decodeInstruction 0x14 0x00 0x00 H.=== DecA

-- MUL AB decodes correctly.
prop_decode_mul :: H.Property
prop_decode_mul = H.withTests 1 . H.property $
    decodeInstruction 0xA4 0x00 0x00 H.=== MulAB

-- DIV AB decodes correctly.
prop_decode_div :: H.Property
prop_decode_div = H.withTests 1 . H.property $
    decodeInstruction 0x84 0x00 0x00 H.=== DivAB

-- CLR A decodes correctly.
prop_decode_clr_a :: H.Property
prop_decode_clr_a = H.withTests 1 . H.property $
    decodeInstruction 0xE4 0x00 0x00 H.=== ClrA

-- CPL A decodes correctly.
prop_decode_cpl_a :: H.Property
prop_decode_cpl_a = H.withTests 1 . H.property $
    decodeInstruction 0xF4 0x00 0x00 H.=== CplA

-- RL A decodes correctly.
prop_decode_rl :: H.Property
prop_decode_rl = H.withTests 1 . H.property $
    decodeInstruction 0x23 0x00 0x00 H.=== RlA

-- RR A decodes correctly.
prop_decode_rr :: H.Property
prop_decode_rr = H.withTests 1 . H.property $
    decodeInstruction 0x03 0x00 0x00 H.=== RrA

-- PUSH dir decodes correctly.
prop_decode_push :: H.Property
prop_decode_push = H.withTests 1 . H.property $
    decodeInstruction 0xC0 0x07 0x00 H.=== PushDir 0x07

-- POP dir decodes correctly.
prop_decode_pop :: H.Property
prop_decode_pop = H.withTests 1 . H.property $
    decodeInstruction 0xD0 0x07 0x00 H.=== PopDir 0x07

-- MOVX A, @DPTR decodes correctly.
prop_decode_movx_a_dptr :: H.Property
prop_decode_movx_a_dptr = H.withTests 1 . H.property $
    decodeInstruction 0xE0 0x00 0x00 H.=== MovxA_dptr

-- MOVX @DPTR, A decodes correctly.
prop_decode_movx_dptr_a :: H.Property
prop_decode_movx_dptr_a = H.withTests 1 . H.property $
    decodeInstruction 0xF0 0x00 0x00 H.=== MovxDptr_A

-- JBC bit, rel decodes correctly.
prop_decode_jbc :: H.Property
prop_decode_jbc = H.withTests 1 . H.property $
    decodeInstruction 0x10 0x20 0x05 H.=== JbcBit 0x20 0x05

-- JB bit, rel decodes correctly.
prop_decode_jb :: H.Property
prop_decode_jb = H.withTests 1 . H.property $
    decodeInstruction 0x20 0x20 0x05 H.=== JbBit 0x20 0x05

-- JNB bit, rel decodes correctly.
prop_decode_jnb :: H.Property
prop_decode_jnb = H.withTests 1 . H.property $
    decodeInstruction 0x30 0x20 0x05 H.=== JnbBit 0x20 0x05

-- CJNE A, #imm, rel decodes correctly.
prop_decode_cjne_a_imm :: H.Property
prop_decode_cjne_a_imm = H.withTests 1 . H.property $
    decodeInstruction 0xB4 0x42 0xFE H.=== CjneA_imm 0x42 0xFE

-- DJNZ dir, rel decodes correctly.
prop_decode_djnz_dir :: H.Property
prop_decode_djnz_dir = H.withTests 1 . H.property $
    decodeInstruction 0xD5 0x30 0xFC H.=== DjnzDir 0x30 0xFC

-- DJNZ Rn, rel decodes correctly for R2.
prop_decode_djnz_rn :: H.Property
prop_decode_djnz_rn = H.withTests 1 . H.property $
    decodeInstruction 0xDA 0x10 0x00 H.=== DjnzRn 2 0x10

-- MOV A, Rn decodes correctly for all R0-R7.
prop_decode_mov_a_rn :: H.Property
prop_decode_mov_a_rn = H.withTests 1 . H.property $ do
    n <- H.forAll (Gen.integral (Range.linear 0 7 :: Range.Range (Unsigned 3)))
    let opcode = 0xE8 + fromIntegral n
    decodeInstruction opcode 0x00 0x00 H.=== MovA_rn n

-- MOV Rn, A decodes correctly for all R0-R7.
prop_decode_mov_rn_a :: H.Property
prop_decode_mov_rn_a = H.withTests 1 . H.property $ do
    n <- H.forAll (Gen.integral (Range.linear 0 7 :: Range.Range (Unsigned 3)))
    let opcode = 0xF8 + fromIntegral n
    decodeInstruction opcode 0x00 0x00 H.=== MovRn_A n

-- INC Rn decodes correctly for all R0-R7.
prop_decode_inc_rn :: H.Property
prop_decode_inc_rn = H.withTests 1 . H.property $ do
    n <- H.forAll (Gen.integral (Range.linear 0 7 :: Range.Range (Unsigned 3)))
    let opcode = 0x08 + fromIntegral n
    decodeInstruction opcode 0x00 0x00 H.=== IncRn n

instrTests :: TestTree
instrTests = $(testGroupGenerator)
