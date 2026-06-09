; program.S — GPIO toggle with timer-interrupt demo for mcs51_soc
;
; Assembled with sdcc -mcs51 --asm (or as8051):
;   sdcc -mcs51 --asm program.S -o program.bin
;
; Memory map:
;   0xFF10  GPIO_A PIN  (read-only, XRAM-mapped)
;   0xFF11  GPIO_A DDR  (1 = output)
;   0xFF12  GPIO_A PORT (output latch)
;
; Interrupt vector table (byte addresses):
;   0x0000  RESET  — jump to reset handler (3-byte LJMP)
;   0x0003  INT0   — jump to ISR (wired to the SoC's periodic timer)
;
; Behaviour:
;   Reset: configure all pins as outputs, set PORT_A = 0x55, enable INT0
;          interrupt, then spin in a tight loop waiting for the next interrupt.
;   ISR:   toggle PORT_A between 0x55 and 0xAA on every timer tick.
;
; Registers used:
;   R0  — holds the current PORT_A output value (0x55 or 0xAA).
;          Preserved across interrupt boundaries because the main loop never
;          touches it after initialisation; the ISR updates it in place.
;
; Note: This file is hand-crafted readable assembly.  An actual sdcc/as8051
; build would generate similar binary.  The pre-built program.bin is checked
; in so Setup.hs can fall back to it when an 8051 assembler is unavailable.

    .module program
    .globl reset

;----------------------------------------------------------------------
; Interrupt vector table (at byte address 0x0000)
;----------------------------------------------------------------------
    .area VECTOR (ABS,CODE)
    .org  0x0000
reset_vec:
    ljmp  reset             ; 3 bytes: 0x02 hi lo

    .org  0x0003
int0_vec:
    ljmp  isr               ; 3 bytes: 0x02 hi lo

;----------------------------------------------------------------------
; Reset handler
;----------------------------------------------------------------------
    .area CODE
reset:
    ; Set all GPIO pins as outputs (DDR = 0xFF)
    mov  dptr, #0xFF11      ; DPTR = GPIO DDR address
    mov  a, #0xFF
    movx @dptr, a           ; GPIO_A DDR = 0xFF

    ; Set initial PORT_A = 0x55, save in R0
    mov  dptr, #0xFF12      ; DPTR = GPIO PORT address
    mov  a, #0x55
    mov  r0, a
    movx @dptr, a           ; GPIO_A PORT = 0x55

    ; Enable INT0 interrupt (IE = 0x81: EA + EX0)
    mov  ie, #0x81

loop:
    sjmp loop               ; spin — everything interesting happens in the ISR

;----------------------------------------------------------------------
; ISR: toggle PORT_A between 0x55 and 0xAA
;----------------------------------------------------------------------
isr:
    mov  a, r0
    xrl  a, #0xFF           ; 0x55 → 0xAA, 0xAA → 0x55
    mov  r0, a
    mov  dptr, #0xFF12
    movx @dptr, a           ; GPIO_A PORT = toggled value
    reti
