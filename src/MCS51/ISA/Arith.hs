module MCS51.ISA.Arith where
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (Word)
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice)
import Isacle.ISA
import MCS51.ISA.Types

-- ---------------------------------------------------------------------------
-- NOP
-- ---------------------------------------------------------------------------

nopDef :: MCS51 m => m ()
nopDef = do
    mnemonic "NOP"
    doc      "No operation"
    encoding "00000000"

-- ---------------------------------------------------------------------------
-- ADD A, src  (no carry in)
-- ---------------------------------------------------------------------------

addARnDef :: MCS51 m => m ()
addARnDef = do
    mnemonic "ADD"
    doc      "Add register to A: A = A + Rn"
    encoding "00101rrr"
    va <- readField mcsA
    n  <- immediate "rrr"
    vr <- readMem (n :: IExpr 8)
    writeField mcsA =<< addArith va vr 0

addADirDef :: MCS51 m => m ()
addADirDef = do
    mnemonic "ADD"
    doc      "Add direct byte to A: A = A + [dir]"
    encoding "00100101"
    va  <- readField mcsA
    dir <- readOp 0
    vd  <- readMem (dir :: IExpr 8)
    writeField mcsA =<< addArith va vd 0
    pcAdvance2

addAImmDef :: MCS51 m => m ()
addAImmDef = do
    mnemonic "ADD"
    doc      "Add immediate to A: A = A + #data"
    encoding "00100100"
    va  <- readField mcsA
    imm <- readOp 0
    writeField mcsA =<< addArith va (imm :: IExpr 8) 0
    pcAdvance2

-- ---------------------------------------------------------------------------
-- ADDC A, src  (add with carry)
-- ---------------------------------------------------------------------------

addcARnDef :: MCS51 m => m ()
addcARnDef = do
    mnemonic "ADDC"
    doc      "Add register to A with carry: A = A + Rn + CY"
    encoding "00111rrr"
    va  <- readField mcsA
    n   <- immediate "rrr"
    vr  <- readMem (n :: IExpr 8)
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< addArith va vr cy

addcAImmDef :: MCS51 m => m ()
addcAImmDef = do
    mnemonic "ADDC"
    doc      "Add immediate to A with carry: A = A + #data + CY"
    encoding "00110100"
    va  <- readField mcsA
    imm <- readOp 0
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< addArith va (imm :: IExpr 8) cy
    pcAdvance2

-- ---------------------------------------------------------------------------
-- SUBB A, src  (subtract with borrow)
-- ---------------------------------------------------------------------------

subbARnDef :: MCS51 m => m ()
subbARnDef = do
    mnemonic "SUBB"
    doc      "Subtract register from A with borrow: A = A - Rn - CY"
    encoding "10011rrr"
    va  <- readField mcsA
    n   <- immediate "rrr"
    vr  <- readMem (n :: IExpr 8)
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< subbArith va vr cy

subbAImmDef :: MCS51 m => m ()
subbAImmDef = do
    mnemonic "SUBB"
    doc      "Subtract immediate from A with borrow: A = A - #data - CY"
    encoding "10010100"
    va  <- readField mcsA
    imm <- readOp 0
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< subbArith va (imm :: IExpr 8) cy
    pcAdvance2

-- ---------------------------------------------------------------------------
-- INC
-- ---------------------------------------------------------------------------

incADef :: MCS51 m => m ()
incADef = do
    mnemonic "INC"
    doc      "Increment A"
    encoding "00000100"
    va <- readField mcsA
    one <- litC 1
    writeField mcsA =<< aluOp PAdd va one

incRnDef :: MCS51 m => m ()
incRnDef = do
    mnemonic "INC"
    doc      "Increment register Rn"
    encoding "00001rrr"
    n  <- immediate "rrr"
    let addr = n :: IExpr 8
    v  <- readMem addr
    one <- litC 1
    writeMem addr =<< aluOp PAdd v one

incDirDef :: MCS51 m => m ()
incDirDef = do
    mnemonic "INC"
    doc      "Increment direct byte"
    encoding "00000101"
    dir <- readOp 0
    let addr = dir :: IExpr 8
    v   <- readMem addr
    one <- litC 1
    writeMem addr =<< aluOp PAdd v one
    pcAdvance2

-- ---------------------------------------------------------------------------
-- DEC
-- ---------------------------------------------------------------------------

decADef :: MCS51 m => m ()
decADef = do
    mnemonic "DEC"
    doc      "Decrement A"
    encoding "00010100"
    va <- readField mcsA
    one <- litC 1
    writeField mcsA =<< aluOp PSub va one

decRnDef :: MCS51 m => m ()
decRnDef = do
    mnemonic "DEC"
    doc      "Decrement register Rn"
    encoding "00011rrr"
    n  <- immediate "rrr"
    let addr = n :: IExpr 8
    v  <- readMem addr
    one <- litC 1
    writeMem addr =<< aluOp PSub v one

decDirDef :: MCS51 m => m ()
decDirDef = do
    mnemonic "DEC"
    doc      "Decrement direct byte"
    encoding "00010101"
    dir <- readOp 0
    let addr = dir :: IExpr 8
    v   <- readMem addr
    one <- litC 1
    writeMem addr =<< aluOp PSub v one
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MUL AB / DIV AB
-- ---------------------------------------------------------------------------

mulABDef :: MCS51 m => m ()
mulABDef = do
    mnemonic "MUL"
    doc      "Multiply A by B: BA = A * B (unsigned 8x8→16)"
    encoding "10100100"
    va <- readField mcsA
    vb <- readField mcsB
    -- 16-bit product: store low byte in A, high byte in B
    prod <- aluOp PMul (zeroExtend va :: IExpr 16) (zeroExtend vb)
    writeField mcsA (truncateB prod)
    eight16 <- litC (8 :: Integer)
    prodHi  <- aluOp PShiftR prod eight16
    writeField mcsB (truncateB prodHi)
    stubFlags

divABDef :: MCS51 m => m ()
divABDef = do
    mnemonic "DIV"
    doc      "Divide A by B: A = quotient, B = remainder"
    encoding "10000100"
    va <- readField mcsA
    vb <- readField mcsB
    -- integer division (PSub approximation — real DIV needs PDiv, stubbed)
    q  <- aluOp PSub va vb   -- placeholder: not real division
    writeField mcsA q
    stubFlags
