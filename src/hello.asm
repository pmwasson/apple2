;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------

.include "defines.asm"

PITCH           = $50

.segment "CODE"

.proc main
    jsr     HOME        ; clear screen

    ; display a greeting
    jsr     inline_print
    .byte   "Hello world!",13,"Hit a key to stop tone...",13,0

    ; make a noise
top:
    ldx     #PITCH

    bit     KBDSTRB 
loop:
    lda     KBD 
    bpl     :+ 

    bit     KBDSTRB 
    ; Jump to monitor when done
    jsr     MONZ

:
    dex
    bne     loop 
    bit     SPEAKER 
    jmp     top 

.endproc ; main  

; add utilies
.include "inline_print.asm"
