module MCS51.ISA.Move where
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (Word)
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice)
import Isacle.ISA
import MCS51.ISA.Types

-- ---------------------------------------------------------------------------
-- MOV A, src
-- ---------------------------------------------------------------------------

movARnDef :: MCS51 m => m ()
movARnDef = do
    mnemonic "MOV"
    doc      "Move Rn to A"
    encoding "11101rrr"
    n <- immediate "rrr"
    v <- readMem (n :: IExpr 8)
    writeField @"a" v

movADirDef :: MCS51 m => m ()
movADirDef = do
    mnemonic "MOV"
    doc      "Move direct byte to A"
    encoding "11100101"
    dir <- readOp 0
    v   <- readMem (dir :: IExpr 8)
    writeField @"a" v
    pcAdvance2

movAImmDef :: MCS51 m => m ()
movAImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to A: A = #data"
    encoding "01110100"
    imm <- readOp 0
    writeField @"a" (imm :: IExpr 8)
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV dst, A
-- ---------------------------------------------------------------------------

movRnADef :: MCS51 m => m ()
movRnADef = do
    mnemonic "MOV"
    doc      "Move A to Rn"
    encoding "11111rrr"
    va <- readField @"a"
    n  <- immediate "rrr"
    writeMem (n :: IExpr 8) va

movDirADef :: MCS51 m => m ()
movDirADef = do
    mnemonic "MOV"
    doc      "Move A to direct byte"
    encoding "11110101"
    va  <- readField @"a"
    dir <- readOp 0
    writeMem (dir :: IExpr 8) va
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV Rn, #imm
-- ---------------------------------------------------------------------------

movRnImmDef :: MCS51 m => m ()
movRnImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to Rn"
    encoding "01111rrr"
    n   <- immediate "rrr"
    imm <- readOp 0
    writeMem (n :: IExpr 8) (imm :: IExpr 8)
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV direct, #imm
-- ---------------------------------------------------------------------------

movDirImmDef :: MCS51 m => m ()
movDirImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to direct byte"
    encoding "01110101"
    dir <- readOp 0
    imm <- readOp 1
    writeMem (dir :: IExpr 8) (imm :: IExpr 8)
    pcAdvance3

-- ---------------------------------------------------------------------------
-- MOV direct, direct  (src in first operand byte, dst in second — 8051 quirk)
-- ---------------------------------------------------------------------------

movDirDirDef :: MCS51 m => m ()
movDirDirDef = do
    mnemonic "MOV"
    doc      "Move direct byte to direct byte: [dst] = [src]"
    encoding "10000101"
    src <- readOp 0
    dst <- readOp 1
    v   <- readMem (src :: IExpr 8)
    writeMem (dst :: IExpr 8) v
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
    encoding "11000000"
    dir <- readOp 0
    v   <- readMem (dir :: IExpr 8)
    sp  <- readField @"sp"
    one <- litC 1
    sp1 <- aluOp PAdd sp one
    writeField @"sp" sp1
    writeMem sp1 v
    pcAdvance2

popDirDef :: MCS51 m => m ()
popDirDef = do
    mnemonic "POP"
    doc      "Pop stack into direct byte"
    encoding "11010000"
    dir <- readOp 0
    sp  <- readField @"sp"
    v   <- readMem sp
    writeMem (dir :: IExpr 8) v
    one <- litC 1
    writeField @"sp" =<< aluOp PSub sp one
    pcAdvance2

-- ---------------------------------------------------------------------------
-- XCH A, src
-- ---------------------------------------------------------------------------

xchARnDef :: MCS51 m => m ()
xchARnDef = do
    mnemonic "XCH"
    doc      "Exchange A with Rn"
    encoding "11001rrr"
    n  <- immediate "rrr"
    let addr = n :: IExpr 8
    va <- readField @"a"
    vr <- readMem addr
    writeField @"a" vr
    writeMem addr va

xchADirDef :: MCS51 m => m ()
xchADirDef = do
    mnemonic "XCH"
    doc      "Exchange A with direct byte"
    encoding "11000101"
    dir <- readOp 0
    let addr = dir :: IExpr 8
    va  <- readField @"a"
    vd  <- readMem addr
    writeField @"a" vd
    writeMem addr va
    pcAdvance2
