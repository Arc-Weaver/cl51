module MCS51.ISA.Branch where

import Prelude hiding (Word)
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice)
import Isacle.ISA
import MCS51.ISA.Types

-- ---------------------------------------------------------------------------
-- Helper: compute absolute target for a PC-relative branch.
--
-- At execute time readReg pcR = address of the current instruction.
-- target = current_PC + instrSize + sign_extend(rel8)
-- ---------------------------------------------------------------------------

relTarget :: MCS51 m
          => CPURegister 16
          -> Integer          -- ^ instruction size in bytes
          -> IExpr 8       -- ^ raw 8-bit relative offset
          -> m (IExpr 16)
relTarget pcR instrSize rel = do
    p   <- readReg pcR
    k   <- signExtendBits rel
    adj <- litC instrSize
    p'  <- aluOp PAdd p adj
    aluOp PAdd p' k

-- ---------------------------------------------------------------------------
-- Unconditional jumps
-- ---------------------------------------------------------------------------

sjmpDef :: MCS51 m => m ()
sjmpDef = do
    mnemonic "SJMP"
    doc      "Short relative jump"
    encoding "10000000"
    pcR <- cpu mcsPC
    rel <- readOp 0
    tgt <- relTarget pcR 2 rel
    absJump pcR tgt

ljmpDef :: MCS51 m => m ()
ljmpDef = do
    mnemonic "LJMP"
    doc      "Long absolute jump to addr16"
    encoding "00000010"
    pcR <- cpu mcsPC
    tgt <- readOp16
    absJump pcR tgt

-- | AJMP addr11 — absolute jump within the 2 KB page of (PC+2).
ajmpDef :: MCS51 m => m ()
ajmpDef = do
    mnemonic "AJMP"
    doc      "Absolute jump within 2 KB page"
    encoding "aaa00001"
    pcR   <- cpu mcsPC
    hi3   <- immediate "aaa"   -- bits [10:8] of the 11-bit offset
    lo8   <- readOp 0          -- bits [7:0]
    -- build off11 as a 16-bit value
    eight <- litC (8 :: Integer)
    hi3e  <- aluOp PShiftL (zeroExtendC (hi3 :: IExpr 3) :: IExpr 16) eight
    off11 <- aluOp POr hi3e (zeroExtend lo8 :: IExpr 16)
    -- target = (PC+2) & 0xF800 | off11
    p     <- readReg pcR
    two   <- litC 2
    pNext <- aluOp PAdd p two
    mask  <- litC 0xF800
    base  <- aluOp PAnd pNext mask
    tgt   <- aluOp POr base off11
    absJump pcR tgt

-- ---------------------------------------------------------------------------
-- Conditional jumps (2-byte, relative)
-- ---------------------------------------------------------------------------

jcDef :: MCS51 m => m ()
jcDef = do
    mnemonic "JC"
    doc      "Jump if carry set"
    encoding "01000000"
    pcR <- cpu mcsPC
    cy  <- getFlag =<< cpuFlag mcsCY
    rel <- readOp 0
    tgt <- relTarget pcR 2 rel
    pcAdvance2
    absJumpIf pcR cy tgt

jncDef :: MCS51 m => m ()
jncDef = do
    mnemonic "JNC"
    doc      "Jump if carry clear"
    encoding "01010000"
    pcR <- cpu mcsPC
    cy  <- getFlag =<< cpuFlag mcsCY
    rel <- readOp 0
    tgt <- relTarget pcR 2 rel
    ncy <- isZero cy
    pcAdvance2
    absJumpIf pcR ncy tgt

jzDef :: MCS51 m => m ()
jzDef = do
    mnemonic "JZ"
    doc      "Jump if A == 0"
    encoding "01100000"
    pcR <- cpu mcsPC
    va  <- readReg =<< cpu mcsA
    rel <- readOp 0
    tgt <- relTarget pcR 2 rel
    z   <- isZero va
    pcAdvance2
    absJumpIf pcR z tgt

jnzDef :: MCS51 m => m ()
jnzDef = do
    mnemonic "JNZ"
    doc      "Jump if A != 0"
    encoding "01110000"
    pcR <- cpu mcsPC
    va  <- readReg =<< cpu mcsA
    rel <- readOp 0
    tgt <- relTarget pcR 2 rel
    nz  <- isZero =<< isZero va
    pcAdvance2
    absJumpIf pcR nz tgt

-- ---------------------------------------------------------------------------
-- DJNZ
-- ---------------------------------------------------------------------------

djnzRnDef :: MCS51 m => m ()
djnzRnDef = do
    mnemonic "DJNZ"
    doc      "Decrement Rn; jump if not zero"
    encoding "11011nnn"
    pcR  <- cpu mcsPC
    n    <- immediate "nnn"
    rel  <- readOp 0
    let addr = n :: IExpr 8
    v    <- readMem addr
    one  <- litC 1
    v'   <- aluOp PSub v one
    writeMem addr v'
    tgt  <- relTarget pcR 2 rel
    nz   <- isZero =<< isZero v'
    pcAdvance2
    absJumpIf pcR nz tgt

djnzDirDef :: MCS51 m => m ()
djnzDirDef = do
    mnemonic "DJNZ"
    doc      "Decrement direct; jump if not zero"
    encoding "11010101"
    pcR  <- cpu mcsPC
    dir  <- readOp 0
    rel  <- readOp 1
    let addr = dir :: IExpr 8
    v    <- readMem addr
    one  <- litC 1
    v'   <- aluOp PSub v one
    writeMem addr v'
    tgt  <- relTarget pcR 3 rel
    nz   <- isZero =<< isZero v'
    pcAdvance3
    absJumpIf pcR nz tgt

-- ---------------------------------------------------------------------------
-- LCALL / ACALL / RET / RETI
--
-- 8051 push convention: SP++, IRAM[SP] = byte.
-- Return address pushed is PC + instruction_size (next instruction address).
-- ---------------------------------------------------------------------------

lcallDef :: MCS51 m => m ()
lcallDef = do
    mnemonic "LCALL"
    doc      "Long call: push return PC, jump to addr16"
    encoding "00010010"
    pcR   <- cpu mcsPC
    spR   <- cpu mcsSP
    tgt   <- readOp16
    -- return address = PC + 3 (past opcode + 2 operand bytes)
    p     <- readReg pcR
    three <- litC 3
    ret   <- aluOp PAdd p (three :: IExpr 16)
    -- push PCL then PCH (8-bit data bus: two separate writes)
    sp    <- readReg spR
    one   <- litC 1
    sp1   <- aluOp PAdd sp one
    writeMem sp1 (truncateB ret)
    eight <- litC 8
    retHi <- aluOp PShiftR ret (eight :: IExpr 16)
    sp2   <- aluOp PAdd sp1 one
    writeMem sp2 (truncateB retHi)
    writeReg spR sp2
    absJump pcR tgt

acallDef :: MCS51 m => m ()
acallDef = do
    mnemonic "ACALL"
    doc      "Absolute call within 2 KB page"
    encoding "aaa10001"
    pcR   <- cpu mcsPC
    spR   <- cpu mcsSP
    hi3   <- immediate "aaa"   -- bits [10:8] of the 11-bit offset
    lo8   <- readOp 0          -- bits [7:0]
    -- return address = PC + 2 (past opcode + 1 operand byte)
    p     <- readReg pcR
    two   <- litC 2
    ret   <- aluOp PAdd p (two :: IExpr 16)
    -- push PCL then PCH
    sp    <- readReg spR
    one   <- litC 1
    sp1   <- aluOp PAdd sp one
    writeMem sp1 (truncateB ret)
    eight <- litC 8
    retHi <- aluOp PShiftR ret (eight :: IExpr 16)
    sp2   <- aluOp PAdd sp1 one
    writeMem sp2 (truncateB retHi)
    writeReg spR sp2
    -- page-relative target: (ret & 0xF800) | (hi3 << 8 | lo8)
    hi3e  <- aluOp PShiftL (zeroExtendC (hi3 :: IExpr 3) :: IExpr 16) eight
    off11 <- aluOp POr hi3e (zeroExtend lo8 :: IExpr 16)
    mask  <- litC 0xF800
    base  <- aluOp PAnd ret mask
    tgt   <- aluOp POr base off11
    absJump pcR tgt

retDef :: MCS51 m => m ()
retDef = do
    mnemonic "RET"
    doc      "Return from subroutine"
    encoding "00100010"
    pcR <- cpu mcsPC
    spR <- cpu mcsSP
    sp  <- readReg spR
    one <- litC 1
    pcH <- readMem sp
    sp1 <- aluOp PSub sp one
    pcL <- readMem sp1
    sp2 <- aluOp PSub sp1 one
    writeReg spR sp2
    let pcH16 = zeroExtend pcH :: IExpr 16
        pcL16 = zeroExtend pcL :: IExpr 16
    eight <- litC 8
    pcHs  <- aluOp PShiftL pcH16 (eight :: IExpr 16)
    tgt   <- aluOp POr pcHs pcL16
    absJump pcR tgt

retiDef :: MCS51 m => m ()
retiDef = do
    mnemonic "RETI"
    doc      "Return from interrupt"
    encoding "00110010"
    pcR <- cpu mcsPC
    spR <- cpu mcsSP
    sp  <- readReg spR
    one <- litC 1
    pcH <- readMem sp
    sp1 <- aluOp PSub sp one
    pcL <- readMem sp1
    sp2 <- aluOp PSub sp1 one
    writeReg spR sp2
    let pcH16 = zeroExtend pcH :: IExpr 16
        pcL16 = zeroExtend pcL :: IExpr 16
    eight <- litC 8
    pcHs  <- aluOp PShiftL pcH16 (eight :: IExpr 16)
    tgt   <- aluOp POr pcHs pcL16
    absJump pcR tgt
