module MCS51.InstructionSet where

import Clash.Prelude

type Reg     = Unsigned 3   -- R0–R7
type BitAddr = Unsigned 8   -- bit address 0–255
type Relative = Signed 8    -- relative branch offset

-- ---------------------------------------------------------------------------
-- Instruction data type
-- ---------------------------------------------------------------------------

-- | MCS-51 instruction set.
--
--   For AJMP/ACALL the full 11-bit destination is precomputed at decode time
--   and stored as the target 'MCS51Addr' (the upper 5 bits are taken from the
--   address of the following instruction, matching real 8051 behaviour).
data Instruction
    -- ── No operation ──────────────────────────────────────────────────────────
    = Nop
    -- ── Arithmetic ────────────────────────────────────────────────────────────
    | AddA_imm  (Unsigned 8)             -- ADD  A, #data
    | AddA_dir  (Unsigned 8)             -- ADD  A, direct
    | AddA_ri   Reg                      -- ADD  A, @Ri
    | AddA_rn   Reg                      -- ADD  A, Rn
    | AddcA_imm (Unsigned 8)             -- ADDC A, #data
    | AddcA_dir (Unsigned 8)             -- ADDC A, direct
    | AddcA_ri  Reg                      -- ADDC A, @Ri
    | AddcA_rn  Reg                      -- ADDC A, Rn
    | SubbA_imm (Unsigned 8)             -- SUBB A, #data
    | SubbA_dir (Unsigned 8)             -- SUBB A, direct
    | SubbA_ri  Reg                      -- SUBB A, @Ri
    | SubbA_rn  Reg                      -- SUBB A, Rn
    | MulAB                              -- MUL  AB
    | DivAB                              -- DIV  AB
    | DaA                                -- DA   A
    | IncA                               -- INC  A
    | IncDir    (Unsigned 8)             -- INC  direct
    | IncRi     Reg                      -- INC  @Ri
    | IncRn     Reg                      -- INC  Rn
    | IncDptr                            -- INC  DPTR
    | DecA                               -- DEC  A
    | DecDir    (Unsigned 8)             -- DEC  direct
    | DecRi     Reg                      -- DEC  @Ri
    | DecRn     Reg                      -- DEC  Rn
    -- ── Logical ───────────────────────────────────────────────────────────────
    | AnlA_imm  (Unsigned 8)             -- ANL  A, #data
    | AnlA_dir  (Unsigned 8)             -- ANL  A, direct
    | AnlA_ri   Reg                      -- ANL  A, @Ri
    | AnlA_rn   Reg                      -- ANL  A, Rn
    | AnlDir_A  (Unsigned 8)             -- ANL  direct, A
    | AnlDir_imm (Unsigned 8) (Unsigned 8) -- ANL direct, #data
    | OrlA_imm  (Unsigned 8)             -- ORL  A, #data
    | OrlA_dir  (Unsigned 8)             -- ORL  A, direct
    | OrlA_ri   Reg                      -- ORL  A, @Ri
    | OrlA_rn   Reg                      -- ORL  A, Rn
    | OrlDir_A  (Unsigned 8)             -- ORL  direct, A
    | OrlDir_imm (Unsigned 8) (Unsigned 8) -- ORL direct, #data
    | XrlA_imm  (Unsigned 8)             -- XRL  A, #data
    | XrlA_dir  (Unsigned 8)             -- XRL  A, direct
    | XrlA_ri   Reg                      -- XRL  A, @Ri
    | XrlA_rn   Reg                      -- XRL  A, Rn
    | XrlDir_A  (Unsigned 8)             -- XRL  direct, A
    | XrlDir_imm (Unsigned 8) (Unsigned 8) -- XRL direct, #data
    | ClrA                               -- CLR  A
    | CplA                               -- CPL  A
    | RlA                                -- RL   A
    | RlcA                               -- RLC  A
    | RrA                                -- RR   A
    | RrcA                               -- RRC  A
    | SwapA                              -- SWAP A
    -- ── Data transfer ─────────────────────────────────────────────────────────
    | MovA_imm  (Unsigned 8)             -- MOV  A, #data
    | MovA_dir  (Unsigned 8)             -- MOV  A, direct
    | MovA_ri   Reg                      -- MOV  A, @Ri
    | MovA_rn   Reg                      -- MOV  A, Rn
    | MovDir_A  (Unsigned 8)             -- MOV  direct, A
    | MovDir_imm (Unsigned 8) (Unsigned 8) -- MOV direct, #data
    | MovDir_dir (Unsigned 8) (Unsigned 8) -- MOV direct, direct  (src, dst)
    | MovDir_ri  (Unsigned 8) Reg        -- MOV  direct, @Ri
    | MovDir_rn  (Unsigned 8) Reg        -- MOV  direct, Rn
    | MovRi_A   Reg                      -- MOV  @Ri, A
    | MovRi_dir Reg (Unsigned 8)         -- MOV  @Ri, direct
    | MovRi_imm Reg (Unsigned 8)         -- MOV  @Ri, #data
    | MovRn_A   Reg                      -- MOV  Rn, A
    | MovRn_dir Reg (Unsigned 8)         -- MOV  Rn, direct
    | MovRn_imm Reg (Unsigned 8)         -- MOV  Rn, #data
    | MovDptr   (Unsigned 16)            -- MOV  DPTR, #data16
    | MovxA_dptr                         -- MOVX A, @DPTR
    | MovxA_ri  Reg                      -- MOVX A, @Ri
    | MovxDptr_A                         -- MOVX @DPTR, A
    | MovxRi_A  Reg                      -- MOVX @Ri, A
    | MovcA_dptr                         -- MOVC A, @A+DPTR
    | MovcA_pc                           -- MOVC A, @A+PC
    | PushDir   (Unsigned 8)             -- PUSH direct
    | PopDir    (Unsigned 8)             -- POP  direct
    | XchA_dir  (Unsigned 8)             -- XCH  A, direct
    | XchA_ri   Reg                      -- XCH  A, @Ri
    | XchA_rn   Reg                      -- XCH  A, Rn
    | XchdA_ri  Reg                      -- XCHD A, @Ri
    -- ── Boolean / bit operations ──────────────────────────────────────────────
    | ClrC                               -- CLR  C
    | ClrBit    BitAddr                  -- CLR  bit
    | SetbC                              -- SETB C
    | SetbBit   BitAddr                  -- SETB bit
    | CplC                               -- CPL  C
    | CplBit    BitAddr                  -- CPL  bit
    | AnlC_bit  BitAddr                  -- ANL  C, bit
    | AnlC_nbit BitAddr                  -- ANL  C, /bit
    | OrlC_bit  BitAddr                  -- ORL  C, bit
    | OrlC_nbit BitAddr                  -- ORL  C, /bit
    | MovC_bit  BitAddr                  -- MOV  C, bit
    | MovBit_C  BitAddr                  -- MOV  bit, C
    -- ── Jumps ─────────────────────────────────────────────────────────────────
    | AjmpAddr  (Unsigned 16)            -- AJMP addr11 (precomputed target)
    | LjmpAddr  (Unsigned 16)            -- LJMP addr16
    | SjmpRel   Relative                 -- SJMP rel
    | JmpAdptr                           -- JMP  @A+DPTR
    | JzRel     Relative                 -- JZ   rel
    | JnzRel    Relative                 -- JNZ  rel
    | JcRel     Relative                 -- JC   rel
    | JncRel    Relative                 -- JNC  rel
    | JbBit     BitAddr Relative         -- JB   bit, rel
    | JnbBit    BitAddr Relative         -- JNB  bit, rel
    | JbcBit    BitAddr Relative         -- JBC  bit, rel
    | CjneA_imm (Unsigned 8) Relative    -- CJNE A, #data, rel
    | CjneA_dir (Unsigned 8) Relative    -- CJNE A, direct, rel
    | CjneRi_imm Reg (Unsigned 8) Relative -- CJNE @Ri, #data, rel
    | CjneRn_imm Reg (Unsigned 8) Relative -- CJNE Rn, #data, rel
    | DjnzDir   (Unsigned 8) Relative    -- DJNZ direct, rel
    | DjnzRn    Reg Relative             -- DJNZ Rn, rel
    -- ── Calls and returns ─────────────────────────────────────────────────────
    | AcallAddr (Unsigned 16)            -- ACALL addr11 (precomputed target)
    | LcallAddr (Unsigned 16)            -- LCALL addr16
    | Ret                                -- RET
    | Reti                               -- RETI
    deriving (Generic, NFDataX, Show, Eq)

-- ---------------------------------------------------------------------------
-- Instruction byte count (from first opcode byte)
-- ---------------------------------------------------------------------------

-- | Number of bytes this instruction occupies in program memory (1, 2, or 3).
--   Determined solely from the first (opcode) byte.
instrBytes :: Unsigned 8 -> Int
instrBytes op = case op of
    -- 3-byte instructions
    0x02 -> 3   -- LJMP addr16
    0x10 -> 3   -- JBC  bit, rel
    0x12 -> 3   -- LCALL addr16
    0x20 -> 3   -- JB   bit, rel
    0x30 -> 3   -- JNB  bit, rel
    0x43 -> 3   -- ORL  direct, #data
    0x53 -> 3   -- ANL  direct, #data
    0x63 -> 3   -- XRL  direct, #data
    0x75 -> 3   -- MOV  direct, #data
    0x85 -> 3   -- MOV  direct, direct
    0x90 -> 3   -- MOV  DPTR, #data16
    0xB4 -> 3   -- CJNE A, #data, rel
    0xB5 -> 3   -- CJNE A, direct, rel
    0xB6 -> 3   -- CJNE @R0, #data, rel
    0xB7 -> 3   -- CJNE @R1, #data, rel
    0xD5 -> 3   -- DJNZ direct, rel
    _ | op >= 0xB8 && op <= 0xBF -> 3  -- CJNE Rn, #data, rel
    -- 1-byte instructions
    0x00 -> 1   -- NOP
    0x03 -> 1   -- RR A
    0x04 -> 1   -- INC A
    0x13 -> 1   -- RRC A
    0x14 -> 1   -- DEC A
    0x22 -> 1   -- RET
    0x23 -> 1   -- RL A
    0x32 -> 1   -- RETI
    0x33 -> 1   -- RLC A
    0x73 -> 1   -- JMP @A+DPTR
    0x83 -> 1   -- MOVC A, @A+PC
    0x84 -> 1   -- DIV AB
    0x93 -> 1   -- MOVC A, @A+DPTR
    0xA3 -> 1   -- INC DPTR
    0xA4 -> 1   -- MUL AB
    0xC3 -> 1   -- CLR C
    0xC4 -> 1   -- SWAP A
    0xD3 -> 1   -- SETB C
    0xD4 -> 1   -- DA A
    0xE0 -> 1   -- MOVX A, @DPTR
    0xE4 -> 1   -- CLR A
    0xF0 -> 1   -- MOVX @DPTR, A
    0xF4 -> 1   -- CPL A
    _ | op >= 0x06 && op <= 0x07 -> 1  -- INC @Ri
      | op >= 0x08 && op <= 0x0F -> 1  -- INC Rn
      | op >= 0x16 && op <= 0x17 -> 1  -- DEC @Ri
      | op >= 0x18 && op <= 0x1F -> 1  -- DEC Rn
      | op >= 0x26 && op <= 0x27 -> 1  -- ADD A, @Ri
      | op >= 0x28 && op <= 0x2F -> 1  -- ADD A, Rn
      | op >= 0x36 && op <= 0x37 -> 1  -- ADDC A, @Ri
      | op >= 0x38 && op <= 0x3F -> 1  -- ADDC A, Rn
      | op >= 0x46 && op <= 0x47 -> 1  -- ORL A, @Ri
      | op >= 0x48 && op <= 0x4F -> 1  -- ORL A, Rn
      | op >= 0x56 && op <= 0x57 -> 1  -- ANL A, @Ri
      | op >= 0x58 && op <= 0x5F -> 1  -- ANL A, Rn
      | op >= 0x66 && op <= 0x67 -> 1  -- XRL A, @Ri
      | op >= 0x68 && op <= 0x6F -> 1  -- XRL A, Rn
      | op >= 0xC6 && op <= 0xC7 -> 1  -- XCH A, @Ri
      | op >= 0xC8 && op <= 0xCF -> 1  -- XCH A, Rn
      | op >= 0xD6 && op <= 0xD7 -> 1  -- XCHD A, @Ri
      | op >= 0xE2 && op <= 0xE3 -> 1  -- MOVX A, @Ri
      | op >= 0xE6 && op <= 0xE7 -> 1  -- MOV A, @Ri
      | op >= 0xE8 && op <= 0xEF -> 1  -- MOV A, Rn
      | op >= 0xF2 && op <= 0xF3 -> 1  -- MOVX @Ri, A
      | op >= 0xF6 && op <= 0xF7 -> 1  -- MOV @Ri, A
      | op >= 0xF8 && op <= 0xFF -> 1  -- MOV Rn, A
    -- 2-byte instructions (everything else)
    _ -> 2

-- ---------------------------------------------------------------------------
-- Decoder
-- ---------------------------------------------------------------------------

-- | Decode an instruction from three consecutive bytes (b0=opcode, b1, b2).
--   For 1-byte instructions b1 and b2 are ignored.
--   For 2-byte instructions b2 is ignored.
--
--   The @nextPC@ argument is the address of the byte following the instruction;
--   it is used to precompute AJMP/ACALL targets (which reference the 2KB page
--   of the *next* instruction).
decodeInstruction :: Unsigned 8 -> Unsigned 8 -> Unsigned 8 -> Instruction
decodeInstruction b0 b1 b2 = case b0 of
    -- ── NOP ──────────────────────────────────────────────────────────────────
    0x00 -> Nop
    -- ── AJMP page0–page7 (odd 0x01..0xE1, bits [7:5]=page, bits [4:0]=0x01) ─
    _ | isAjmp b0 -> AjmpAddr (ajmpTarget b0 b1)
    -- ── LJMP addr16 ──────────────────────────────────────────────────────────
    0x02 -> LjmpAddr (makeAddr b1 b2)
    -- ── RR A ─────────────────────────────────────────────────────────────────
    0x03 -> RrA
    -- ── INC A ────────────────────────────────────────────────────────────────
    0x04 -> IncA
    -- ── INC direct ───────────────────────────────────────────────────────────
    0x05 -> IncDir b1
    -- ── INC @Ri / INC Rn ─────────────────────────────────────────────────────
    _ | b0 >= 0x06 && b0 <= 0x07 -> IncRi (fromIntegral (b0 .&. 1))
      | b0 >= 0x08 && b0 <= 0x0F -> IncRn (fromIntegral (b0 .&. 7))
    -- ── JBC bit, rel ─────────────────────────────────────────────────────────
    0x10 -> JbcBit b1 (fromIntegral b2)
    -- ── ACALL page0–page7 ────────────────────────────────────────────────────
    _ | isAcall b0 -> AcallAddr (ajmpTarget b0 b1)
    -- ── LCALL addr16 ─────────────────────────────────────────────────────────
    0x12 -> LcallAddr (makeAddr b1 b2)
    -- ── RRC A ────────────────────────────────────────────────────────────────
    0x13 -> RrcA
    -- ── DEC A ────────────────────────────────────────────────────────────────
    0x14 -> DecA
    -- ── DEC direct ───────────────────────────────────────────────────────────
    0x15 -> DecDir b1
    -- ── DEC @Ri / DEC Rn ─────────────────────────────────────────────────────
    _ | b0 >= 0x16 && b0 <= 0x17 -> DecRi (fromIntegral (b0 .&. 1))
      | b0 >= 0x18 && b0 <= 0x1F -> DecRn (fromIntegral (b0 .&. 7))
    -- ── JB bit, rel ──────────────────────────────────────────────────────────
    0x20 -> JbBit b1 (fromIntegral b2)
    -- ── RET ──────────────────────────────────────────────────────────────────
    0x22 -> Ret
    -- ── RL A ─────────────────────────────────────────────────────────────────
    0x23 -> RlA
    -- ── ADD A, #data / ADD A, direct ─────────────────────────────────────────
    0x24 -> AddA_imm b1
    0x25 -> AddA_dir b1
    -- ── ADD A, @Ri / ADD A, Rn ───────────────────────────────────────────────
    _ | b0 >= 0x26 && b0 <= 0x27 -> AddA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0x28 && b0 <= 0x2F -> AddA_rn (fromIntegral (b0 .&. 7))
    -- ── JNB bit, rel ─────────────────────────────────────────────────────────
    0x30 -> JnbBit b1 (fromIntegral b2)
    -- ── RETI ─────────────────────────────────────────────────────────────────
    0x32 -> Reti
    -- ── RLC A ────────────────────────────────────────────────────────────────
    0x33 -> RlcA
    -- ── ADDC A, #data / ADDC A, direct ───────────────────────────────────────
    0x34 -> AddcA_imm b1
    0x35 -> AddcA_dir b1
    -- ── ADDC A, @Ri / ADDC A, Rn ─────────────────────────────────────────────
    _ | b0 >= 0x36 && b0 <= 0x37 -> AddcA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0x38 && b0 <= 0x3F -> AddcA_rn (fromIntegral (b0 .&. 7))
    -- ── JC rel ───────────────────────────────────────────────────────────────
    0x40 -> JcRel (fromIntegral b1)
    -- ── ORL direct, A / ORL direct, #data ────────────────────────────────────
    0x42 -> OrlDir_A b1
    0x43 -> OrlDir_imm b1 b2
    -- ── ORL A, #data / ORL A, direct ─────────────────────────────────────────
    0x44 -> OrlA_imm b1
    0x45 -> OrlA_dir b1
    -- ── ORL A, @Ri / ORL A, Rn ───────────────────────────────────────────────
    _ | b0 >= 0x46 && b0 <= 0x47 -> OrlA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0x48 && b0 <= 0x4F -> OrlA_rn (fromIntegral (b0 .&. 7))
    -- ── JNC rel ──────────────────────────────────────────────────────────────
    0x50 -> JncRel (fromIntegral b1)
    -- ── ANL direct, A / ANL direct, #data ────────────────────────────────────
    0x52 -> AnlDir_A b1
    0x53 -> AnlDir_imm b1 b2
    -- ── ANL A, #data / ANL A, direct ─────────────────────────────────────────
    0x54 -> AnlA_imm b1
    0x55 -> AnlA_dir b1
    -- ── ANL A, @Ri / ANL A, Rn ───────────────────────────────────────────────
    _ | b0 >= 0x56 && b0 <= 0x57 -> AnlA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0x58 && b0 <= 0x5F -> AnlA_rn (fromIntegral (b0 .&. 7))
    -- ── JZ rel ───────────────────────────────────────────────────────────────
    0x60 -> JzRel (fromIntegral b1)
    -- ── XRL direct, A / XRL direct, #data ────────────────────────────────────
    0x62 -> XrlDir_A b1
    0x63 -> XrlDir_imm b1 b2
    -- ── XRL A, #data / XRL A, direct ─────────────────────────────────────────
    0x64 -> XrlA_imm b1
    0x65 -> XrlA_dir b1
    -- ── XRL A, @Ri / XRL A, Rn ───────────────────────────────────────────────
    _ | b0 >= 0x66 && b0 <= 0x67 -> XrlA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0x68 && b0 <= 0x6F -> XrlA_rn (fromIntegral (b0 .&. 7))
    -- ── JNZ rel ──────────────────────────────────────────────────────────────
    0x70 -> JnzRel (fromIntegral b1)
    -- ── ORL C, bit ───────────────────────────────────────────────────────────
    0x72 -> OrlC_bit b1
    -- ── JMP @A+DPTR ──────────────────────────────────────────────────────────
    0x73 -> JmpAdptr
    -- ── MOV A, #data ─────────────────────────────────────────────────────────
    0x74 -> MovA_imm b1
    -- ── MOV direct, #data ────────────────────────────────────────────────────
    0x75 -> MovDir_imm b1 b2
    -- ── MOV @Ri, #data / MOV Rn, #data ───────────────────────────────────────
    _ | b0 >= 0x76 && b0 <= 0x77 -> MovRi_imm (fromIntegral (b0 .&. 1)) b1
      | b0 >= 0x78 && b0 <= 0x7F -> MovRn_imm (fromIntegral (b0 .&. 7)) b1
    -- ── SJMP rel ─────────────────────────────────────────────────────────────
    0x80 -> SjmpRel (fromIntegral b1)
    -- ── ANL C, bit ───────────────────────────────────────────────────────────
    0x82 -> AnlC_bit b1
    -- ── MOVC A, @A+PC ────────────────────────────────────────────────────────
    0x83 -> MovcA_pc
    -- ── DIV AB ───────────────────────────────────────────────────────────────
    0x84 -> DivAB
    -- ── MOV direct, direct ───────────────────────────────────────────────────
    0x85 -> MovDir_dir b1 b2
    -- ── MOV direct, @Ri / MOV direct, Rn ─────────────────────────────────────
    _ | b0 >= 0x86 && b0 <= 0x87 -> MovDir_ri b1 (fromIntegral (b0 .&. 1))
      | b0 >= 0x88 && b0 <= 0x8F -> MovDir_rn b1 (fromIntegral (b0 .&. 7))
    -- ── MOV DPTR, #data16 ────────────────────────────────────────────────────
    0x90 -> MovDptr (makeAddr b1 b2)
    -- ── MOV bit, C ───────────────────────────────────────────────────────────
    0x92 -> MovBit_C b1
    -- ── MOVC A, @A+DPTR ──────────────────────────────────────────────────────
    0x93 -> MovcA_dptr
    -- ── SUBB A, #data / SUBB A, direct ───────────────────────────────────────
    0x94 -> SubbA_imm b1
    0x95 -> SubbA_dir b1
    -- ── SUBB A, @Ri / SUBB A, Rn ─────────────────────────────────────────────
    _ | b0 >= 0x96 && b0 <= 0x97 -> SubbA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0x98 && b0 <= 0x9F -> SubbA_rn (fromIntegral (b0 .&. 7))
    -- ── ORL C, /bit ──────────────────────────────────────────────────────────
    0xA0 -> OrlC_nbit b1
    -- ── MOV C, bit ───────────────────────────────────────────────────────────
    0xA2 -> MovC_bit b1
    -- ── INC DPTR ─────────────────────────────────────────────────────────────
    0xA3 -> IncDptr
    -- ── MUL AB ───────────────────────────────────────────────────────────────
    0xA4 -> MulAB
    -- ── MOV @Ri, direct / MOV Rn, direct ─────────────────────────────────────
    _ | b0 >= 0xA6 && b0 <= 0xA7 -> MovRi_dir (fromIntegral (b0 .&. 1)) b1
      | b0 >= 0xA8 && b0 <= 0xAF -> MovRn_dir (fromIntegral (b0 .&. 7)) b1
    -- ── ANL C, /bit ──────────────────────────────────────────────────────────
    0xB0 -> AnlC_nbit b1
    -- ── CPL bit ──────────────────────────────────────────────────────────────
    0xB2 -> CplBit b1
    -- ── CPL C ────────────────────────────────────────────────────────────────
    0xB3 -> CplC
    -- ── CJNE A, #data, rel / CJNE A, direct, rel ─────────────────────────────
    0xB4 -> CjneA_imm b1 (fromIntegral b2)
    0xB5 -> CjneA_dir b1 (fromIntegral b2)
    -- ── CJNE @Ri, #data, rel ─────────────────────────────────────────────────
    _ | b0 >= 0xB6 && b0 <= 0xB7 ->
            CjneRi_imm (fromIntegral (b0 .&. 1)) b1 (fromIntegral b2)
    -- ── CJNE Rn, #data, rel ──────────────────────────────────────────────────
    _ | b0 >= 0xB8 && b0 <= 0xBF ->
            CjneRn_imm (fromIntegral (b0 .&. 7)) b1 (fromIntegral b2)
    -- ── PUSH direct ──────────────────────────────────────────────────────────
    0xC0 -> PushDir b1
    -- ── CLR bit ──────────────────────────────────────────────────────────────
    0xC2 -> ClrBit b1
    -- ── CLR C ────────────────────────────────────────────────────────────────
    0xC3 -> ClrC
    -- ── SWAP A ───────────────────────────────────────────────────────────────
    0xC4 -> SwapA
    -- ── XCH A, direct ────────────────────────────────────────────────────────
    0xC5 -> XchA_dir b1
    -- ── XCH A, @Ri / XCH A, Rn ───────────────────────────────────────────────
    _ | b0 >= 0xC6 && b0 <= 0xC7 -> XchA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0xC8 && b0 <= 0xCF -> XchA_rn (fromIntegral (b0 .&. 7))
    -- ── POP direct ───────────────────────────────────────────────────────────
    0xD0 -> PopDir b1
    -- ── SETB bit ─────────────────────────────────────────────────────────────
    0xD2 -> SetbBit b1
    -- ── SETB C ───────────────────────────────────────────────────────────────
    0xD3 -> SetbC
    -- ── DA A ─────────────────────────────────────────────────────────────────
    0xD4 -> DaA
    -- ── DJNZ direct, rel ─────────────────────────────────────────────────────
    0xD5 -> DjnzDir b1 (fromIntegral b2)
    -- ── XCHD A, @Ri ──────────────────────────────────────────────────────────
    _ | b0 >= 0xD6 && b0 <= 0xD7 -> XchdA_ri (fromIntegral (b0 .&. 1))
    -- ── DJNZ Rn, rel ─────────────────────────────────────────────────────────
    _ | b0 >= 0xD8 && b0 <= 0xDF -> DjnzRn (fromIntegral (b0 .&. 7)) (fromIntegral b1)
    -- ── MOVX A, @DPTR ────────────────────────────────────────────────────────
    0xE0 -> MovxA_dptr
    -- ── MOVX A, @Ri ──────────────────────────────────────────────────────────
    _ | b0 >= 0xE2 && b0 <= 0xE3 -> MovxA_ri (fromIntegral (b0 .&. 1))
    -- ── CLR A ────────────────────────────────────────────────────────────────
    0xE4 -> ClrA
    -- ── MOV A, direct ────────────────────────────────────────────────────────
    0xE5 -> MovA_dir b1
    -- ── MOV A, @Ri / MOV A, Rn ───────────────────────────────────────────────
    _ | b0 >= 0xE6 && b0 <= 0xE7 -> MovA_ri (fromIntegral (b0 .&. 1))
      | b0 >= 0xE8 && b0 <= 0xEF -> MovA_rn (fromIntegral (b0 .&. 7))
    -- ── MOVX @DPTR, A ────────────────────────────────────────────────────────
    0xF0 -> MovxDptr_A
    -- ── MOVX @Ri, A ──────────────────────────────────────────────────────────
    _ | b0 >= 0xF2 && b0 <= 0xF3 -> MovxRi_A (fromIntegral (b0 .&. 1))
    -- ── CPL A ────────────────────────────────────────────────────────────────
    0xF4 -> CplA
    -- ── MOV direct, A ────────────────────────────────────────────────────────
    0xF5 -> MovDir_A b1
    -- ── MOV @Ri, A / MOV Rn, A ───────────────────────────────────────────────
    _ | b0 >= 0xF6 && b0 <= 0xF7 -> MovRi_A (fromIntegral (b0 .&. 1))
      | b0 >= 0xF8 && b0 <= 0xFF -> MovRn_A (fromIntegral (b0 .&. 7))
    -- ── Fallback ──────────────────────────────────────────────────────────────
    _ -> Nop

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | True if the opcode is an AJMP (bits [2:0] = 001, i.e. opcode & 0x1F = 0x01).
isAjmp :: Unsigned 8 -> Bool
isAjmp op = (op .&. 0x1F) == 0x01

-- | True if the opcode is an ACALL (bits [2:0] = 001, i.e. opcode & 0x1F = 0x11).
isAcall :: Unsigned 8 -> Bool
isAcall op = (op .&. 0x1F) == 0x11

-- | Build the 11-bit AJMP/ACALL destination from opcode and low byte.
--   The upper 3 bits come from opcode[7:5], the lower 8 bits from b1.
--   The full destination is within the 2KB page:
--     target[10:8] = op[7:5]
--     target[7:0]  = b1
--   NOTE: in real hardware the upper 5 bits of (PC+2) are ORed in, but
--   since we store the precomputed target we handle that at the call site.
ajmpTarget :: Unsigned 8 -> Unsigned 8 -> Unsigned 16
ajmpTarget op b1 =
    let page = fromIntegral (op `shiftR` 5) :: Unsigned 16
    in (page `shiftL` 8) .|. fromIntegral b1

-- | Combine two bytes into a 16-bit address (big-endian: hi=b0, lo=b1).
makeAddr :: Unsigned 8 -> Unsigned 8 -> Unsigned 16
makeAddr hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
