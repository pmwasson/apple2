;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; medit - Map editor


;-----------------------------------------------------------------------------
; Map Edit
;-----------------------------------------------------------------------------

.proc medit

.proc medit_main
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; display a greeting
    jsr     inline_print
    .byte   "Map editor - ? for help",13,0

    jsr 	HGR
    jsr 	drawBackground

command_loop:

    jsr     inline_print
    .byte   "Command:",0

skip_prompt:
    jsr 	RDKEY

    ; Parse command

    ;------------------
    ; Q = QUIT
    ;------------------
    cmp     #$80 | 'Q'
    bne     :+
    jsr     inline_print
    .byte   "Quit",13,0
    bit     TXTSET
    jmp     MONZ        ; enter monitor

:
    ;------------------
    ; ? = HELP
    ;------------------
    cmp     #$80 + '?'
    bne     :+
    jsr     inline_print
    .byte   "Help (ESC when done)",13,0
    jsr     printHelp
    jmp     command_loop

:
    ;------------------
    ; ESC = Toggle Text
    ;------------------
    cmp     #KEY_ESC
    bne     :+
    ; dont display anything
    lda     TEXTMODE
    bmi     toggle_text_off
    bit     TXTSET    
    jmp     skip_prompt
toggle_text_off:
    bit     TXTCLR    
    jmp     skip_prompt

:
    ;------------------
    ; TAB = switch tool
    ;------------------
    cmp     #$89
    bne     :+
    jsr     inline_print
    .byte   "Switch tool",13,0
    rts

:
    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

.endproc

;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    .byte   " Arrows: Move cursor",13
    .byte   " !:   Dump map",13
    .byte   " ?:   HELP",13
    .byte   " Q:   Quit",13  
    .byte   " Tab: Switch tool",13
    .byte   " Escape: Toggle text/graphics",13
    .byte   0
    rts
.endproc

;-----------------------------------------------------------------------------
; drawBackground
;-----------------------------------------------------------------------------
.proc drawBackground

    ; edit window
    lda     #1
    sta     charTop
    sta     charLeft
    lda 	#16
    sta     charBottom
    lda 	#32
    sta     charRight
    jsr     drawBox

    rts
.endproc

;-----------------------------------------------------------------------------
; drawTile
;-----------------------------------------------------------------------------
.proc drawTile

    ; edit window
    lda     #1
    sta     charTop
    sta     charLeft
    lda 	#16
    sta     charBottom
    lda 	#32
    sta     charRight
    jsr     drawBox

    rts
.endproc

.endproc