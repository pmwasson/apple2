;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; medit - Map editor

;------------------------------------------------
; Zero page usage
;------------------------------------------------

; tilePtr0    :=  $60     ; Tile pointer
; tilePtr1    :=  $61
; screenPtr0  :=  $52     ; Screen pointer
; screenPtr1  :=  $53

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

    lda 	#4
    sta 	mapX
    sta 	mapY
    lda 	#3
    jsr 	drawTile

    jsr 	drawBackground
    jsr 	drawMap

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
    lda 	#18
    sta     charBottom
    lda 	#38
    sta     charRight
    jsr     drawBox

    rts
.endproc

;-----------------------------------------------------------------------------
; drawMap
;-----------------------------------------------------------------------------
.proc drawMap

	lda 	#2
	sta 	mapY

vloop:
	lda 	#2
	sta 	mapX

hloop:

;--- temp junk
	lda 	mapY
	cmp 	mapX
	beq 	:+
	lda 	#0
	adc 	#0
	jmp		skip
:
	lda 	#2
skip:
;----
	jsr 	drawTile

	clc
	lda 	mapX
	adc		#4
	sta 	mapX
	cmp 	#38
	bmi 	hloop

	clc
	lda 	mapY
	adc		#2
	sta 	mapY
	cmp 	#18
	bmi 	vloop

	;
	lda 	#0
	sta 	charX
	sta 	charY
	jsr		drawString
	.byte 	"LOC:00,00",0

    rts
.endproc

;-----------------------------------------------------------------------------
; drawTile
;-----------------------------------------------------------------------------
.proc drawTile
    ; calculate sprite pointer
    sta     temp0           ; Save a copy of A

    ror
    ror
    ror                     ; Multiply by 64
    and 	#$c0
    clc
    adc     tileSheetStart
    sta     tilePtr0

    lda     #0
    adc     tileSheetStart+1
    sta     tilePtr1
    lda     temp0
    lsr
    lsr                     ; Divide by 4

    clc
    adc     tilePtr1
    sta     tilePtr1

    ; calculate screen pointer
    ldx     mapY
    lda     mapX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; Carry not set in loop, so clear outside of loop
    ldx     #$08
drawLoop1:
    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda 	tilePtr0
    adc 	#4
    sta 	tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop1

    ; move to next byte
    lda     screenPtr0
    clc
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    sbc     #$1f        ; subtract 20 if no carry, 19 if carry
    sta     screenPtr1

    clc     ; Carry not set in loop, so clear outside of loop
    ldx     #$08
drawLoop2:
    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda 	tilePtr0
    adc 	#4
    sta 	tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop2

    rts    

; locals
temp0:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; data
;-----------------------------------------------------------------------------

mapX: 	.byte 	0
mapY:	.byte 	0

.align  256

; 32x32 map
map:
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	.byte "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

.endproc