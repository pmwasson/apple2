;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------


; Zero page usage
sourcePtr0      := $5A
sourcePtr1      := $5B
destPtr0        := $5C
destPtr1        := $5D
stackPtr0       := $5E
stackPtr1       := $5F


;-----------------------------------------------------------------------------
; Copy - copy memory
;-----------------------------------------------------------------------------
; Inline parameters to specify source start and end and destination
;   clobbers A,X,Y
;
; Example:
;   jsr     memory_copy
;   .word   source_start
;   .word   source_end
;   .word   dest_start
;   <next instruction>

.proc memory_copy
    ; Pop return address to find parameters
    pla
    sta     stackPtr0
    pla
    sta     stackPtr1

    ; source_start
    ldy     #1
    lda     (destPtr0),y
    sta     sourcePtr0
    iny
    lda     (destPtr0),y
    sta     sourcePtr1

    ; source_end
    iny
    lda     (destPtr0),y
    sta     sourceEnd0
    iny
    lda     (destPtr0),y
    sta     sourceEnd1

    ; dest
    iny
    lda     (destPtr0),y
    sta     destPtr0
    iny
    lda     (destPtr0),y
    sta     destPtr1


    ldy     #0


    ; *** copy loop here!


    ; calculate return address

    lda     stackPtr0
    clc
    adc     #6          ; 6 bytes of parameters
    tax                 ; save a copy
    clc
    lda     stackPtr1
    adc     #0
    pha
    txa
    pha
    rts

.endproc ; print
