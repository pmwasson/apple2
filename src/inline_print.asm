;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; inline_print - display following string to COUT
;-----------------------------------------------------------------------------
; Uses stack pointer to find string
;   clobbers A,X,Y
;
; Example:
;   jsr     inline_print
;   .byte   "HELLO WORLD!",0
;   <next instruction>

;.include "defines.asm"

; Zero page usage
stringPtr0      = $60
stringPtr1      = $61

.proc inline_print
    ; Pop return address to find string
    pla
    sta     stringPtr0
    pla
    sta     stringPtr1
    ldy     #0

    ; Print characters until 0 (end-of-string)
printLoop:
    iny
    tya
    pha
    lda     (stringPtr0),y
    beq     printExit
    ora     #$80               ; not inverse/flashing
    jsr     COUT
    pla
    tay
    jmp     printLoop

printExit:
    pla                 ; clean up stack
    ; calculate return address after print string
    clc
    tya
    adc     stringPtr0  ; add low-byte first
    tax                 ; save in X
    lda     stringPtr1  ; carry to high-byte
    adc     #0          
    pha                 ; push return high-byte
    txa
    pha                 ; push return low-byte
    rts                 ; return

.endproc ; print
