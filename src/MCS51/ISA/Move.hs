module MCS51.ISA.Move where

import Prelude hiding (Word)
import Hdl.Bits
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
    a <- cpu mcsA
    n <- immediate "rrr"
    v <- readMem (n :: Unsigned 8)
    writeReg a v

movADirDef :: MCS51 m => m ()
movADirDef = do
    mnemonic "MOV"
    doc      "Move direct byte to A"
    encoding "11100101"
    a   <- cpu mcsA
    dir <- readOp 0
    v   <- readMem (dir :: Unsigned 8)
    writeReg a v
    pcAdvance2

movAImmDef :: MCS51 m => m ()
movAImmDef = do
    mnemonic "MOV"
    doc      "Move immediate to A: A = #data"
    encoding "01110100"
    a   <- cpu mcsA
    imm <- readOp 0
    writeReg a (imm :: Unsigned 8)
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MOV dst, A
-- ---------------------------------------------------------------------------

movRnADef :: MCS51 m => m ()
movRnADef = do
    mnemonic "MOV"
    doc      "Move A to Rn"
    encoding "11111rrr"
    a  <- cpu mcsA
    va <- readReg a
    n  <- immediate "rrr"
    writeMem (n :: Unsigned 8) va

movDirADef :: MCS51 m => m ()
movDirADef = do
    mnemonic "MOV"
    doc      "Move A to direct byte"
    encoding "11110101"
    a   <- cpu mcsA
    va  <- readReg a
    dir <- readOp 0
    writeMem (dir :: Unsigned 8) va
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
    writeMem (n :: Unsigned 8) (imm :: Unsigned 8)
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
    writeMem (dir :: Unsigned 8) (imm :: Unsigned 8)
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
    v   <- readMem (src :: Unsigned 8)
    writeMem (dst :: Unsigned 8) v
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
    spR <- cpu mcsSP
    dir <- readOp 0
    v   <- readMem (dir :: Unsigned 8)
    sp  <- readReg spR
    one <- litC 1
    sp1 <- aluOp PAdd sp one
    writeReg spR sp1
    writeMem sp1 v
    pcAdvance2

popDirDef :: MCS51 m => m ()
popDirDef = do
    mnemonic "POP"
    doc      "Pop stack into direct byte"
    encoding "11010000"
    spR <- cpu mcsSP
    dir <- readOp 0
    sp  <- readReg spR
    v   <- readMem sp
    writeMem (dir :: Unsigned 8) v
    one <- litC 1
    writeReg spR =<< aluOp PSub sp one
    pcAdvance2

-- ---------------------------------------------------------------------------
-- XCH A, src
-- ---------------------------------------------------------------------------

xchARnDef :: MCS51 m => m ()
xchARnDef = do
    mnemonic "XCH"
    doc      "Exchange A with Rn"
    encoding "11001rrr"
    a  <- cpu mcsA
    n  <- immediate "rrr"
    let addr = n :: Unsigned 8
    va <- readReg a
    vr <- readMem addr
    writeReg a vr
    writeMem addr va

xchADirDef :: MCS51 m => m ()
xchADirDef = do
    mnemonic "XCH"
    doc      "Exchange A with direct byte"
    encoding "11000101"
    a   <- cpu mcsA
    dir <- readOp 0
    let addr = dir :: Unsigned 8
    va  <- readReg a
    vd  <- readMem addr
    writeReg a vd
    writeMem addr va
    pcAdvance2
