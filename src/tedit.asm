;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; tedit - Tile editor

.include "defines.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

WIDTH_BYTES     =   4
HEIGHT_BYTES    =   16
WIDTH           =   WIDTH_BYTES*7
HEIGHT          =   HEIGHT_BYTES
LENGTH          =   WIDTH_BYTES * HEIGHT_BYTES
PREVIEW_ADRS    =   HGRPAGE1+WIDTH+2
;------------------------------------------------
; Zero page usage
;------------------------------------------------

screenPtr0  :=  $52     ; Screen pointer
screenPtr1  :=  $53
tilePtr0    :=  $54     ; Tile pointer
tilePtr1    :=  $55

;-----------------------------------------------------------------------------
; Main
;-----------------------------------------------------------------------------

.segment "CODE"
.org    $6000

.proc main
    jsr     HGR         ; hi-res mixed mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; Initialize tilePtr
    lda     sheetStart
    sta     tilePtr0
    lda     sheetStart+1
    sta     tilePtr1

    ; Set up screen
    jsr     drawTile
    jsr     drawPreview

    ; Init cursor
    lda     #2
    sta     curX
    sta     curY

version:
    ; display a greeting
    jsr     inline_print
    .byte   "Tile editor v0.1",13,0

command_loop:

    jsr     inline_print
    .byte   "Command:",0

skip_prompt:
    jsr     getInput    ; Wait for a keypress

    ; Parse command

    ;------------------
    ; RIGHT (arrow)
    ;------------------
    cmp     #$95
    bne     :+
    jsr     inline_print
    .byte   "Right ",0
    inc     curX
    lda     #WIDTH
    cmp     curX
    bne     right_good
    lda     #0
    sta     curX
right_good:
    jmp     display_cord
:
    ;------------------
    ; LEFT (arrow)
    ;------------------
    cmp     #$88
    bne     :+
    jsr     inline_print
    .byte   "Left  ",0
    dec     curX
    lda     curX
    bpl     left_good
    lda     #WIDTH-1
    sta     curX
left_good:
    jmp     display_cord
:
    ;------------------
    ; UP (arrow)
    ;------------------
    cmp     #$8B
    bne     :+
    jsr     inline_print
    .byte   "Up    ",0
    dec     curY
    lda     curY
    bpl     up_good
    lda     #HEIGHT-1
    sta     curY
up_good:
    jmp     display_cord
:
    ;------------------
    ; DOWN (arrow)
    ;------------------
    cmp     #$8A
    bne     :+
    jsr     inline_print
    .byte   "Down  ",0
    inc     curY
    lda     #HEIGHT
    cmp     curY
    bne     down_good
    lda     #0
    sta     curY
down_good:
    jmp     display_cord
:
    ;------------------
    ; SP = Toggle Bit
    ;------------------
    cmp     #$80 | ' '
    bne     :+
    jsr     inline_print
    .byte   "Toggle Bit ",0
    jsr     toggleBit
    bne     toggle_bit_on
    jsr     inline_print
    .byte   "off",0
    jmp     display_byte
toggle_bit_on:
    jsr     inline_print
    .byte   "on ",0
    jmp     display_byte
:
    ;------------------
    ; C = Toggle Color
    ;------------------
    cmp     #$80 | 'C'
    bne     :+
    jsr     inline_print
    .byte   "Toggle Color ",0
    jsr     toggleColor
    bne     toggle_color_on
    jsr     inline_print
    .byte   "purple/green",0
    jmp     display_byte
toggle_color_on:
    jsr     inline_print
    .byte   "blue/red    ",0
    jmp     display_byte
:
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
    .byte   "(?) HELP, (Q)uit",13,"  (Arrows) Move, (SP) Toggle Bit,",13,"  Toggle (C)olor",13,0
    jmp     command_loop

:

    ;------------------
    ; ESC = Toggle Text
    ;------------------
    cmp     #$9b
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
    ; D = Dump
    ;------------------
    cmp     #$80 + 'D' 
    bne     :+
    jsr     inline_print
    .byte   "Dump",13,0

    ldx     tilePtr0
    ldy     tilePtr1
    jsr     PRINTXY
    jsr     inline_print
    .byte   ": ",0

    lda     #0
    sta     dump_count
dump_loop:
    lda     #$80 + '$'
    jsr     COUT
    ldy     dump_count
    lda     (tilePtr0),y
    jsr     PRBYTE
    inc     dump_count
    lda     dump_count
    cmp     #LENGTH
    beq     dump_finish
    lda     #$80 + ','
    jsr     COUT
    lda     dump_count
    and     #$7
    bne     dump_loop
    jsr     inline_print
    .byte   13,"      ",0
    jmp     dump_loop

dump_finish:
    jsr     CR
    jmp     command_loop
:

    ;------------------
    ; V = Version
    ;------------------
    cmp     #$80 + 'V' 
    bne     :+
    jsr     inline_print
    .byte   "Version",13,0
    jmp     version
:

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

; jump to after changing coordinates
display_cord:
    jsr     inline_print
    .byte   "X/Y:",0
    lda     curX
    jsr     PRBYTE
    lda     #$80 + ','
    jsr     COUT
    lda     curY
    jsr     PRBYTE
    jsr     inline_print
    .byte   " @ ",0
    jsr     getByteOffset
    jsr     PRBYTE
    lda     #$80 + '.'
    jsr     COUT
    jsr     getBitOffset
    jsr     PRBYTE
    jsr     CR
    jmp     command_loop

; jump to after changing data
display_byte:
    jsr     inline_print
    .byte   ": $",0
    lda     curData
    jsr     PRBYTE
    jsr     CR

    ; update preview
    jsr     drawPreview

    jmp     command_loop

; Local variables

dump_count: .byte   0

.endproc ; main  

;-----------------------------------------------------------------------------
; getInput
;   Blink cursors and wait for keypress
;   Return key in A (upper bit set)
;-----------------------------------------------------------------------------
.proc getInput

cursor_loop:
    ; Strobe keyboard
    bit     KBDSTRB 

    ; Display cursor
    lda     #$FF
    jsr     COUT
    jsr     getColor
    jsr     drawPixel

    ; Wait (on)
    lda     #$FF
    jsr     WAIT

    ; Restore
    lda     #$88        ; backspace
    jsr     COUT
    lda     #$A0        ; space
    jsr     COUT
    lda     #$88        ; backspace
    jsr     COUT
    jsr     getBWColor
    jsr     drawPixel

    ; check for keypress
    lda     KBD 
    bmi     exit

    ; Strobe keyboard
    bit     KBDSTRB 

    ; Wait (off)
    lda     #$FF
    jsr     WAIT

    ; check for keypress
    lda     KBD 
    bpl     cursor_loop

exit:
    bit     KBDSTRB     ; clean up

    rts

.endproc

;-----------------------------------------------------------------------------
; setScreenPtr
;   Based on curX, curY
;-----------------------------------------------------------------------------
.proc setScreenPtr
    ; calculate screen pointer
    clc
    ldx     curY
    lda     curX            ; curX
    adc     lineOffset,x    ; + lineOffset(curY)
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    rts
.endproc

;-----------------------------------------------------------------------------
; DrawPixel
;   A = color (byte copied for each line)
;   Based on curX, curY
;-----------------------------------------------------------------------------
.proc drawPixel
    sta     color
    jsr     setScreenPtr
    ldx     #8
    ldy     #0

pixel_loop:
    lda     color
    sta     (screenPtr0),y
    lda     screenPtr1
    adc     #$04
    sta     screenPtr1
    dex
    bne     pixel_loop
    rts

; local variables
color:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; getByteOffset
;   Return byte offset in A based on curX, curY 
;   WARNING: assumes width of 4, should generalize
;-----------------------------------------------------------------------------
.proc getByteOffset
    lda     curY
    asl
    asl             ; y*4
    sta     tempY
    ldx     #3      ; 3
    lda     curX
    cmp     #7*3
    bpl     addX
    dex             ; 2
    cmp     #7*2
    bpl     addX
    dex             ; 1
    cmp     #7*1
    bpl     addX
    dex
addX:
    txa
    clc
    adc     tempY
    rts

; local variables
tempY:  .byte   0

.endProc

;-----------------------------------------------------------------------------
; getBitOffset
;   Return bit offset in A based on curX 
;-----------------------------------------------------------------------------
.proc getBitOffset
    lda     curX
loop:
    cmp     #7
    bpl     continue
    rts
continue:
    sec
    sbc     #7
    jmp     loop
.endProc

;-----------------------------------------------------------------------------
; getTileByte
;   Return byte based on tilePtr, curX, curY 
;-----------------------------------------------------------------------------
.proc getTileByte
    jsr     getByteOffset
    tay
    lda     (tilePtr0),y
    rts
.endproc

;-----------------------------------------------------------------------------
; getTileBit
;   Return bit in A and Z flag based on tilePtr, curX, curY 
;-----------------------------------------------------------------------------
.proc getTileBit
    jsr     getBitOffset
    sta     tileBit
    jsr     getTileByte
    ldx     tileBit
    beq     exit
bit_loop:
    lsr
    dex
    bne     bit_loop
exit:
    and     #1  ; one-bit and set Z flag
    rts

; local variable
tileBit:    .byte   0

.endproc


;-----------------------------------------------------------------------------
; getBWColor
;-----------------------------------------------------------------------------

.proc getBWColor
    jsr     getTileBit
    bne     bitOne
    lda     #$00
    beq     exit
bitOne:
    lda     #$7f
exit:
    rts
.endproc

;-----------------------------------------------------------------------------
; getColor
; Note - base on position, not value.
; Assumes alignment
;-----------------------------------------------------------------------------

.proc getColor
    jsr     getTileByte
    bmi     bitOne
    lda     #$55
    jmp     exit
bitOne:
    lda     #$D5
exit:
    rts
.endproc

;-----------------------------------------------------------------------------
; drawTile
;   Warning: clobbers curX, curY
;-----------------------------------------------------------------------------

.proc drawTile

    ; init to zero
    lda     #0
    sta     curY
yloop:
    lda     #0
    sta     curX
xloop:
    jsr     getBWColor
    jsr     drawPixel    

    inc     curX
    lda     curX
    cmp     #WIDTH
    bmi     xloop

    inc     curY
    lda     curY
    cmp     #HEIGHT
    bne     yloop
    rts

.endproc

;-----------------------------------------------------------------------------
; drawPreview
;-----------------------------------------------------------------------------

.proc drawPreview

    ; set screenPtr to fixed location
    lda     #<PREVIEW_ADRS
    sta     screenPtr0
    lda     #>PREVIEW_ADRS
    sta     screenPtr1

    lda     tilePtr0    ; save a copy
    pha

    ldx     #HEIGHT_BYTES
drawLoopV:
    ldy     #0
drawLoopH:
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    iny
    cpy     #WIDTH_BYTES
    bne     drawLoopH

    clc
    lda     tilePtr0
    adc     #WIDTH_BYTES
    sta     tilePtr0
    ; assumes spritePtr aligned such that there are no page crossing

    lda     screenPtr1
    adc     #$04
    sta     screenPtr1

    ; check if in next byte
    cmp     #>HGRPAGE2
    bmi     continue

    ; move to next byte
    lda     screenPtr0
    clc
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    sbc     #$1f        ; subtract 20 if no carry, 19 if carry
    sta     screenPtr1

continue:
    dex
    bne     drawLoopV

    ; restore tilePtr
    pla
    sta     tilePtr0

    rts

.endproc


;-----------------------------------------------------------------------------
; toggleBit
;   Flip bit based on tilePtr, curX, curY
;   Byte result in X, bit result on Z flag
;-----------------------------------------------------------------------------
.proc toggleBit
    lda     #1
    sta     shiftedBit
    jsr     getBitOffset
    tax
    beq     continue
    lda     #1
shift_loop:
    asl
    dex
    bne     shift_loop
    sta     shiftedBit
continue:
    jsr     getByteOffset
    tay
    lda     (tilePtr0),y
    eor     shiftedBit
    sta     (tilePtr0),y
    sta     curData
    and     shiftedBit      ; set Z flag for return
    rts

; local variable
shiftedBit: .byte   0

.endproc

;-----------------------------------------------------------------------------
; toggleColor
;   Flip color based on tilePtr, curX, curY 
;   Byte result in X, bit result on Z flag
;-----------------------------------------------------------------------------
.proc toggleColor
    jsr     getByteOffset
    tay
    lda     (tilePtr0),y
    eor     #$80
    sta     (tilePtr0),y
    sta     curData
    and     #$80         ; set Z flag for return
    rts
.endproc

;-----------------------------------------------------------------------------
; Global Variables
;-----------------------------------------------------------------------------

curX:       .byte   0       
curY:       .byte   0       
curData:    .byte   0
sheetStart: .word   exampleStart
sheetEnd:   .word   exampleEnd

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

linePage:
    .byte   >$2000
    .byte   >$2080
    .byte   >$2100
    .byte   >$2180
    .byte   >$2200
    .byte   >$2280
    .byte   >$2300
    .byte   >$2380
    .byte   >$2028
    .byte   >$20A8
    .byte   >$2128
    .byte   >$21A8
    .byte   >$2228
    .byte   >$22A8
    .byte   >$2328
    .byte   >$23A8
    .byte   >$2050
    .byte   >$20D0
    .byte   >$2150
    .byte   >$21D0
    .byte   >$2250
    .byte   >$22D0
    .byte   >$2350
    .byte   >$23D0

lineOffset:
    .byte   <$2000
    .byte   <$2080
    .byte   <$2100
    .byte   <$2180
    .byte   <$2200
    .byte   <$2280
    .byte   <$2300
    .byte   <$2380
    .byte   <$2028
    .byte   <$20A8
    .byte   <$2128
    .byte   <$21A8
    .byte   <$2228
    .byte   <$22A8
    .byte   <$2328
    .byte   <$23A8
    .byte   <$2050
    .byte   <$20D0
    .byte   <$2150
    .byte   <$21D0
    .byte   <$2250
    .byte   <$22D0
    .byte   <$2350
    .byte   <$23D0

; add utilies
.include "inline_print.asm"

; Put example tile last (in case user extends)
; and align

.align  LENGTH
exampleStart:   
    .byte   $00,$00,$00,$00
    .byte   $7f,$7f,$7f,$7f
    .byte   $03,$55,$55,$60
    .byte   $03,$55,$55,$60
    .byte   $03,$55,$55,$60
    .byte   $03,$2a,$2a,$60
    .byte   $03,$2a,$2a,$60
    .byte   $03,$2a,$2a,$60
    .byte   $7f,$7f,$7f,$7f
    .byte   $7f,$7f,$7f,$7f
    .byte   $03,$d5,$d5,$60
    .byte   $03,$d5,$d5,$60
    .byte   $03,$d5,$d5,$60
    .byte   $03,$aa,$aa,$60
    .byte   $03,$aa,$aa,$60
    .byte   $7f,$7f,$7f,$7f
exampleEnd:
