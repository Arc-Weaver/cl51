module Tests.MCS51.CPU where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog
import qualified Hedgehog as H

import Clash.Prelude (Bit, Unsigned)

import MCS51.Core  (zeroState, CoreData(..), PSW(..), writeDirect, getReg)
import MCS51.ALU   ()
import MCS51.Exec  (runLinear, runWithPC)
import MCS51.CPU   (CPUState(..), cpuStep, Stage(..))

-- ---------------------------------------------------------------------------
-- Linear ALU test
--
-- A hand-assembled 8051 program to exercise arithmetic and data-transfer
-- instructions.  Each instruction is encoded as raw bytes.
--
-- Program:
--   MOV A, #5        ; 0x74 0x05
--   MOV R0, A        ; 0xF8
--   MOV A, #3        ; 0x74 0x03
--   ADD A, R0        ; 0x28          → ACC = 8
--   MOV R1, A        ; 0xF9
--   MOV A, #0x0A     ; 0x74 0x0A
--   SUBB A, R1       ; 0x99  (CY=0)  → ACC = 2
--   MOV R2, A        ; 0xFA
--   MOV A, #0xFF     ; 0x74 0xFF
--   ANL A, #0x0F     ; 0x54 0x0F     → ACC = 0x0F
--   MOV R3, A        ; 0xFB
--   MOV A, #0xF0     ; 0x74 0xF0
--   ORL A, #0x0F     ; 0x44 0x0F     → ACC = 0xFF
--   MOV R4, A        ; 0xFC
--   MOV A, #0xAA     ; 0x74 0xAA
--   XRL A, #0xFF     ; 0x64 0xFF     → ACC = 0x55
--   MOV R5, A        ; 0xFD
--   MOV A, #0x7F     ; 0x74 0x7F
--   INC A            ; 0x04          → ACC = 0x80
--   MOV R6, A        ; 0xFE
--   MOV A, #0x80     ; 0x74 0x80
--   DEC A            ; 0x14          → ACC = 0x7F
--   MOV R7, A        ; 0xFF
--   NOP              ; 0x00
-- ---------------------------------------------------------------------------

basicProgram :: [Unsigned 8]
basicProgram =
    [ 0x74, 0x05   -- MOV A, #5
    , 0xF8         -- MOV R0, A
    , 0x74, 0x03   -- MOV A, #3
    , 0x28         -- ADD A, R0        → 8
    , 0xF9         -- MOV R1, A
    , 0x74, 0x0A   -- MOV A, #10
    , 0x99         -- SUBB A, R1       → 2  (no borrow)
    , 0xFA         -- MOV R2, A
    , 0x74, 0xFF   -- MOV A, #0xFF
    , 0x54, 0x0F   -- ANL A, #0x0F    → 0x0F
    , 0xFB         -- MOV R3, A
    , 0x74, 0xF0   -- MOV A, #0xF0
    , 0x44, 0x0F   -- ORL A, #0x0F    → 0xFF
    , 0xFC         -- MOV R4, A
    , 0x74, 0xAA   -- MOV A, #0xAA
    , 0x64, 0xFF   -- XRL A, #0xFF    → 0x55
    , 0xFD         -- MOV R5, A
    , 0x74, 0x7F   -- MOV A, #0x7F
    , 0x04         -- INC A           → 0x80
    , 0xFE         -- MOV R6, A
    , 0x74, 0x80   -- MOV A, #0x80
    , 0x14         -- DEC A           → 0x7F
    , 0xFF         -- MOV R7, A
    , 0x00         -- NOP
    ]

prop_basicProgram :: H.Property
prop_basicProgram = H.withTests 1 . H.property $ do
    let final = runLinear basicProgram zeroState
    let r n = getReg final n
    r 0 H.=== 5      -- original MOV R0, A
    r 1 H.=== 8      -- ADD result
    r 2 H.=== 2      -- SUBB result
    r 3 H.=== 0x0F   -- ANL result
    r 4 H.=== 0xFF   -- ORL result
    r 5 H.=== 0x55   -- XRL result
    r 6 H.=== 0x80   -- INC result
    r 7 H.=== 0x7F   -- DEC result

-- ---------------------------------------------------------------------------
-- Jump / branch test
--
-- Program:
--   byte  0: MOV A, #5     ; 0x74 0x05
--   byte  2: MOV R0, A     ; 0xF8
--   byte  3: MOV A, #0     ; 0x74 0x00
--   byte  5: SJMP loop     ; 0x80 0x01  (skip byte 7)
--   byte  7: MOV A, #0xFF  ; 0x74 0xFF (skipped)
-- loop:
--   byte  9: ADD A, R0     ; 0x28
--   byte 10: DJNZ R0, loop ; 0xD8 0xFE (rel=-2 → loop at byte 9)
--   byte 12: NOP           ; 0x00
--
-- Expected: A = 5+4+3+2+1 = 15 = 0x0F; R0 = 0
-- ---------------------------------------------------------------------------

jumpProgram :: [Unsigned 8]
jumpProgram =
    [ 0x74, 0x05   -- byte 0: MOV A, #5
    , 0xF8         -- byte 2: MOV R0, A  → R0=5
    , 0x74, 0x00   -- byte 3: MOV A, #0  → A=0
    , 0x80, 0x01   -- byte 5: SJMP +1    → jump to byte 8 (skip byte 7)
    , 0x74, 0xFF   -- byte 7: MOV A, #0xFF (skipped)
    , 0x28         -- byte 9: ADD A, R0  (loop:)
    , 0xD8, 0xFE   -- byte 10: DJNZ R0, .-2 → rel=0xFE=-2 → target byte 9
    , 0x00         -- byte 12: NOP
    ]

prop_jumpProgram :: H.Property
prop_jumpProgram = H.withTests 1 . H.property $ do
    let prog  = jumpProgram
        final = runWithPC prog (fromIntegral (length prog)) zeroState
    acc final       H.=== 0x0F   -- 5+4+3+2+1 = 15
    getReg final 0  H.=== 0      -- counter decremented to zero

-- ---------------------------------------------------------------------------
-- Interrupt acceptance tests
-- ---------------------------------------------------------------------------

-- | IE.EA is cleared the moment the interrupt is accepted.
prop_interruptAccepted :: H.Property
prop_interruptAccepted = H.withTests 1 . H.property $ do
    let iCore = zeroState { ie = 0x80 }
        s0    = CPUState iCore SFetch1
    let (s1, _) = cpuStep s0 (0x00, 0x00, Just (0x0003 :: Unsigned 16))
    -- IE.EA must be cleared
    (ie (cpuCore s1) `mod` 256 `div` 128) H.=== (0 :: Unsigned 8)

-- | RETI must re-enable IE.EA (bit 7).
prop_retiRestoresEA :: H.Property
prop_retiRestoresEA = H.withTests 1 . H.property $ do
    -- Start in ISR with IE.EA=0; RETI should re-enable it.
    let s0 = CPUState (zeroState { ie = 0x00 }) SFetch1
    -- Fetch RETI (0x32) → startRet True
    let (s1, _) = cpuStep s0 (0x32, 0x00, Nothing)
    cpuStage s1 H.=== SRetRead1 True
    -- Pop hi byte
    let (s2, _) = cpuStep s1 (0x00, 0x00, Nothing)
    -- Pop lo byte → IE.EA restored
    let (s3, _) = cpuStep s2 (0x00, 0x00, Nothing)
    cpuStage s3 H.=== SFetch1
    -- IE.EA (bit 7) should be 1
    (ie (cpuCore s3) `div` 128 `mod` 2) H.=== (1 :: Unsigned 8)

cpuTests :: TestTree
cpuTests = $(testGroupGenerator)
