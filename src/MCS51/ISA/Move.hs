module MCS51.ISA.Move where
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (Word)
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Isacle.ISA
import MCS51.ISA.Types

-- ---------------------------------------------------------------------------
-- MOV A, src
-- ---------------------------------------------------------------------------

movARnDef :: MCS51 m => m ()
movARnDef = do
    mnemonic "MOV"
    doc      "Move Rn to A"
    r <- defineInstruction $ regField "11101"
    let n = regAddr r
    v <- readMem (n :: IExpr (Unsigned 8))
    writeField mcsA v

movADirDef :: MCS51 m => m ()
movADirDef = do
    mnemonic "MOV"
    doc      "Move direct byte to A"
    defineInstruction $ fixed "11100101"
    dir <- readOp 0
    v   <- readMem (dir :: IExpr (Unsigned 8))
    writeField mcsA v
    pcAdvance2

movAImmDef :: MCS51 m => m ()
movAImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to A: A = #data"
    defineInstruction $ fixed "01110100"
    imm <- readOp 0
    writeField mcsA (imm :: IExpr (Unsigned 8))
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV dst, A
-- ---------------------------------------------------------------------------

movRnADef :: MCS51 m => m ()
movRnADef = do
    mnemonic "MOV"
    doc      "Move A to Rn"
    r <- defineInstruction $ regField "11111"
    va <- readField mcsA
    let n = regAddr r
    writeMem (n :: IExpr (Unsigned 8)) va

movDirADef :: MCS51 m => m ()
movDirADef = do
    mnemonic "MOV"
    doc      "Move A to direct byte"
    defineInstruction $ fixed "11110101"
    va  <- readField mcsA
    dir <- readOp 0
    writeMem (dir :: IExpr (Unsigned 8)) va
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV Rn, #imm
-- ---------------------------------------------------------------------------

movRnImmDef :: MCS51 m => m ()
movRnImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to Rn"
    r <- defineInstruction $ regField "01111"
    let n = regAddr r
    imm <- readOp 0
    writeMem (n :: IExpr (Unsigned 8)) (imm :: IExpr (Unsigned 8))
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV direct, #imm
-- ---------------------------------------------------------------------------

movDirImmDef :: MCS51 m => m ()
movDirImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to direct byte"
    defineInstruction $ fixed "01110101"
    dir <- readOp 0
    imm <- readOp 1
    writeMem (dir :: IExpr (Unsigned 8)) (imm :: IExpr (Unsigned 8))
    pcAdvance3

-- ---------------------------------------------------------------------------
-- MOV direct, direct  (src in first operand byte, dst in second — 8051 quirk)
-- ---------------------------------------------------------------------------

movDirDirDef :: MCS51 m => m ()
movDirDirDef = do
    mnemonic "MOV"
    doc      "Move direct byte to direct byte: [dst] = [src]"
    defineInstruction $ fixed "10000101"
    src <- readOp 0
    dst <- readOp 1
    v   <- readMem (src :: IExpr (Unsigned 8))
    writeMem (dst :: IExpr (Unsigned 8)) v
    pcAdvance3

-- ---------------------------------------------------------------------------
-- PUSH / POP  (operate on IRAM stack via SP)
--
-- 8051 push: SP++, IRAM[SP] = src
-- 8051 pop:  dst = IRAM[SP], SP--
-- ---------------------------------------------------------------------------

pushDirDef :: MCS51 m => m ()
pushDirDef = do
    mnemonic "PUSH"
    doc      "Push direct byte onto stack"
    defineInstruction $ fixed "11000000"
    dir <- readOp 0
    v   <- readMem (dir :: IExpr (Unsigned 8))
    sp  <- readField mcsSP
    one <- litC 1
    let sp1 = sp + one
    writeField mcsSP sp1
    writeMem sp1 v
    pcAdvance2

popDirDef :: MCS51 m => m ()
popDirDef = do
    mnemonic "POP"
    doc      "Pop stack into direct byte"
    defineInstruction $ fixed "11010000"
    dir <- readOp 0
    sp  <- readField mcsSP
    v   <- readMem sp
    writeMem (dir :: IExpr (Unsigned 8)) v
    one <- litC 1
    writeField mcsSP (sp - one)
    pcAdvance2

-- ---------------------------------------------------------------------------
-- XCH A, src
-- ---------------------------------------------------------------------------

xchARnDef :: MCS51 m => m ()
xchARnDef = do
    mnemonic "XCH"
    doc      "Exchange A with Rn"
    r <- defineInstruction $ regField "11001"
    let n = regAddr r
    let addr = n :: IExpr (Unsigned 8)
    va <- readField mcsA
    vr <- readMem addr
    writeField mcsA vr
    writeMem addr va

xchADirDef :: MCS51 m => m ()
xchADirDef = do
    mnemonic "XCH"
    doc      "Exchange A with direct byte"
    defineInstruction $ fixed "11000101"
    dir <- readOp 0
    let addr = dir :: IExpr (Unsigned 8)
    va  <- readField mcsA
    vd  <- readMem addr
    writeField mcsA vd
    writeMem addr va
    pcAdvance2
