module MCS51.ISA.Arith where
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (Word)
import Hdl.Bits hiding (zeroExtend, signExtend, truncateB, bitCoerce, slice, add, mul, shiftL, shiftR, xor, (.&.), (.|.))
import Isacle.ISA
import MCS51.ISA.Types

-- ---------------------------------------------------------------------------
-- NOP
-- ---------------------------------------------------------------------------

nopDef :: MCS51 m => m ()
nopDef = do
    mnemonic "NOP"
    doc      "No operation"
    defineInstruction $ fixed "00000000"

-- ---------------------------------------------------------------------------
-- ADD A, src  (no carry in)
-- ---------------------------------------------------------------------------

addARnDef :: MCS51 m => m ()
addARnDef = do
    mnemonic "ADD"
    doc      "Add register to A: A = A + Rn"
    r <- defineInstruction $ regField "00101"
    va <- readField mcsA
    let n = regAddr r
    vr <- readMem (n :: IExpr (Unsigned 8))
    writeField mcsA =<< addArith va vr 0

addADirDef :: MCS51 m => m ()
addADirDef = do
    mnemonic "ADD"
    doc      "Add direct byte to A: A = A + [dir]"
    defineInstruction $ fixed "00100101"
    va  <- readField mcsA
    dir <- readOp 0
    vd  <- readMem (dir :: IExpr (Unsigned 8))
    writeField mcsA =<< addArith va vd 0
    pcAdvance2

addAImmDef :: MCS51 m => m ()
addAImmDef = do
    mnemonic "ADD"
    doc      "Add immediate to A: A = A + #data"
    defineInstruction $ fixed "00100100"
    va  <- readField mcsA
    imm <- readOp 0
    writeField mcsA =<< addArith va (imm :: IExpr (Unsigned 8)) 0
    pcAdvance2

-- ---------------------------------------------------------------------------
-- ADDC A, src  (add with carry)
-- ---------------------------------------------------------------------------

addcARnDef :: MCS51 m => m ()
addcARnDef = do
    mnemonic "ADDC"
    doc      "Add register to A with carry: A = A + Rn + CY"
    r <- defineInstruction $ regField "00111"
    va  <- readField mcsA
    let n = regAddr r
    vr  <- readMem (n :: IExpr (Unsigned 8))
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< addArith va vr cy

addcAImmDef :: MCS51 m => m ()
addcAImmDef = do
    mnemonic "ADDC"
    doc      "Add immediate to A with carry: A = A + #data + CY"
    defineInstruction $ fixed "00110100"
    va  <- readField mcsA
    imm <- readOp 0
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< addArith va (imm :: IExpr (Unsigned 8)) cy
    pcAdvance2

-- ---------------------------------------------------------------------------
-- SUBB A, src  (subtract with borrow)
-- ---------------------------------------------------------------------------

subbARnDef :: MCS51 m => m ()
subbARnDef = do
    mnemonic "SUBB"
    doc      "Subtract register from A with borrow: A = A - Rn - CY"
    r <- defineInstruction $ regField "10011"
    va  <- readField mcsA
    let n = regAddr r
    vr  <- readMem (n :: IExpr (Unsigned 8))
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< subbArith va vr cy

subbAImmDef :: MCS51 m => m ()
subbAImmDef = do
    mnemonic "SUBB"
    doc      "Subtract immediate from A with borrow: A = A - #data - CY"
    defineInstruction $ fixed "10010100"
    va  <- readField mcsA
    imm <- readOp 0
    cy  <- getFlag =<< cpuFlag mcsCY
    writeField mcsA =<< subbArith va (imm :: IExpr (Unsigned 8)) cy
    pcAdvance2

-- ---------------------------------------------------------------------------
-- INC
-- ---------------------------------------------------------------------------

incADef :: MCS51 m => m ()
incADef = do
    mnemonic "INC"
    doc      "Increment A"
    defineInstruction $ fixed "00000100"
    va <- readField mcsA
    one <- litC 1
    writeField mcsA (va + one)

incRnDef :: MCS51 m => m ()
incRnDef = do
    mnemonic "INC"
    doc      "Increment register Rn"
    r <- defineInstruction $ regField "00001"
    let n = regAddr r
    let addr = n :: IExpr (Unsigned 8)
    v  <- readMem addr
    one <- litC 1
    writeMem addr (v + one)

incDirDef :: MCS51 m => m ()
incDirDef = do
    mnemonic "INC"
    doc      "Increment direct byte"
    defineInstruction $ fixed "00000101"
    dir <- readOp 0
    let addr = dir :: IExpr (Unsigned 8)
    v   <- readMem addr
    one <- litC 1
    writeMem addr (v + one)
    pcAdvance2

-- ---------------------------------------------------------------------------
-- DEC
-- ---------------------------------------------------------------------------

decADef :: MCS51 m => m ()
decADef = do
    mnemonic "DEC"
    doc      "Decrement A"
    defineInstruction $ fixed "00010100"
    va <- readField mcsA
    one <- litC 1
    writeField mcsA (va - one)

decRnDef :: MCS51 m => m ()
decRnDef = do
    mnemonic "DEC"
    doc      "Decrement register Rn"
    r <- defineInstruction $ regField "00011"
    let n = regAddr r
    let addr = n :: IExpr (Unsigned 8)
    v  <- readMem addr
    one <- litC 1
    writeMem addr (v - one)

decDirDef :: MCS51 m => m ()
decDirDef = do
    mnemonic "DEC"
    doc      "Decrement direct byte"
    defineInstruction $ fixed "00010101"
    dir <- readOp 0
    let addr = dir :: IExpr (Unsigned 8)
    v   <- readMem addr
    one <- litC 1
    writeMem addr (v - one)
    pcAdvance2

-- ---------------------------------------------------------------------------
-- MUL AB / DIV AB
-- ---------------------------------------------------------------------------

mulABDef :: MCS51 m => m ()
mulABDef = do
    mnemonic "MUL"
    doc      "Multiply A by B: BA = A * B (unsigned 8x8→16)"
    defineInstruction $ fixed "10100100"
    va <- readField mcsA
    vb <- readField mcsB
    -- 16-bit product: store low byte in A, high byte in B
    let prod = (zeroExtend va :: IExpr (Unsigned 16)) * zeroExtend vb
    writeField mcsA (truncateB prod)
    eight16 <- litC (8 :: Integer)
    let prodHi = shiftR prod eight16
    writeField mcsB (truncateB prodHi)
    stubFlags

divABDef :: MCS51 m => m ()
divABDef = do
    mnemonic "DIV"
    doc      "Divide A by B: A = quotient, B = remainder"
    defineInstruction $ fixed "10000100"
    va <- readField mcsA
    vb <- readField mcsB
    -- integer division (PSub approximation — real DIV needs PDiv, stubbed)
    let q = va - vb   -- placeholder: not real division
    writeField mcsA q
    stubFlags
