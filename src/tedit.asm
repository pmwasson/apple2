;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; tedit - Tile editor

; .include "defines.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

DIR_LEFT        =   0
DIR_RIGHT       =   1
DIR_UP          =   2
DIR_DOWN        =   3

; REMOVE!
WIDTH_BYTES     =   4
HEIGHT_BYTES    =   16
WIDTH           =   WIDTH_BYTES*7
HEIGHT          =   HEIGHT_BYTES
LENGTH          =   WIDTH_BYTES * HEIGHT_BYTES
PREVIEW_ADRS0   =   HGRPAGE1+$80*1+WIDTH+4
PREVIEW_ADRS1   =   HGRPAGE1+$28+$80*0         +WIDTH+2
PREVIEW_ADRS2   =   HGRPAGE1+$28+$80*0         +WIDTH+2+WIDTH_BYTES
PREVIEW_ADRS3   =   HGRPAGE1+$28+$80*(HEIGHT/8)+WIDTH+2
PREVIEW_ADRS4   =   HGRPAGE1+$28+$80*(HEIGHT/8)+WIDTH+2+WIDTH_BYTES

;------------------------------------------------
; Zero page usage
;------------------------------------------------

tilePtr0    :=  $60     ; Tile pointer
tilePtr1    :=  $61
copyPtr0    :=  $62     ; For copying bytes
copyPtr1    :=  $63

; .segment "CODE"
; .org    $4000

;-----------------------------------------------------------------------------
; Tile Edit
;-----------------------------------------------------------------------------

.proc tedit

.proc tedit_main
    jsr     HGR         ; hi-res mixed mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; Draw background
    jsr     drawBackground

    ; Initialize tilePtr
    lda     sheetStart
    sta     tilePtr0
    lda     sheetStart+1
    sta     tilePtr1

    ; Init cursor
    lda     #0
    sta     curX
    sta     curY

    ; Draw screen
    jsr     refresh

version:
    ; display a greeting
    jsr     inline_print
    .byte   "Tile editor v0.1 - ? for help",13,0

command_loop:

    jsr     inline_print
    .byte   "Tile ",0
    lda     tileIndex
    jsr     PRBYTE
    jsr     inline_print
    .byte   ":",0

skip_prompt:
    jsr     getInput    ; Wait for a keypress

    ; Parse command

    ;------------------
    ; RIGHT (arrow)
    ;------------------
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right ",0
    inc     curX
    lda     width
    cmp     curX
    bne     right_good
    lda     #0
    sta     curX
right_good:
    jmp     finish_move
:
    ;------------------
    ; LEFT (arrow)
    ;------------------
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left  ",0
    dec     curX
    lda     curX
    bpl     left_good
    lda     width_m1
    sta     curX
left_good:
    jmp     finish_move
:
    ;------------------
    ; UP (arrow)
    ;------------------
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up    ",0
    dec     curY
    lda     curY
    bpl     up_good
    lda     #HEIGHT-1
    sta     curY
up_good:
    jmp     finish_move
:
    ;------------------
    ; DOWN (arrow)
    ;------------------
    cmp     #KEY_DOWN
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
    jmp     finish_move
:
    ;------------------
    ; SP = Toggle Bit
    ;------------------
    cmp     #KEY_SPACE
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
    .byte   "Help (ESC when done)",13,0
    jsr     print_help
    jmp     command_loop
:
    ;------------------
    ; F = FILL
    ;------------------
    cmp     #$80 + 'F'
    beq     continue_fill
    jmp     :+              ; too far due to long string
continue_fill:
    jsr     inline_print
    .byte   "Fill - WARNING: Clears tile!",13
    .byte   " Choose color (0/4=black,1=green,",13
    .byte   " 2=purple,3/7=white,5=orange,6=blue,",13
    .byte   " other=cancel):",0
    lda     #$8
    jsr     get_input_number
    bpl     continue_fill2
    jmp     command_loop
continue_fill2:
    tax
    ldy     #0
    lda     fill_color_even,x
    sta     even_fill
    sta     odd_fill
    lda     #1
    cmp     width_bytes
    beq     fill_loop           ; if 1 byte, no even/odd
    lda     fill_color_odd,x
    sta     odd_fill
fill_loop:
    lda     even_fill
    sta     (tilePtr0),y
    iny
    lda     odd_fill
    sta     (tilePtr0),y
    iny
    cpy     #LENGTH
    bne     fill_loop
    jsr     CR
    jsr     refresh
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
    ; D = Dump
    ;------------------
    cmp     #$80 + '!' 
    bne     :+
    bit     TXTSET
    jsr     inline_print
    .byte   "Dump (ESC when done)",13,0

    ldx     tilePtr0
    ldy     tilePtr1
    jsr     PRINTXY
    jsr     inline_print
    .byte   ": ",0

    lda     #0
    sta     dump_count
    jmp     dump_loop
dump_comma:
    lda     #$80 + ','
    jsr     COUT
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
    lda     dump_count
    and     #$7
    bne     dump_comma
    jsr     inline_print
    .byte   13,"      ",0
    jmp     dump_loop

dump_finish:
    jsr     CR
    jmp     command_loop
:
    ;------------------
    ; - = Previous
    ;------------------
    cmp     #$80 | '-'
    bne     :+
    jsr     inline_print
    .byte   "Previous tile: ",0

    lda     tileIndex
    bne     previous_continue1

    ; point to last tile + 1
    lda     tileMax
    sta     tileIndex
    lda     #<example_end
    sta     tilePtr0
    lda     #>example_end
    sta     tilePtr1

previous_continue1:
    dec     tileIndex
    sec
    lda     tilePtr0
    sbc     #LENGTH
    sta     tilePtr0
    bcs     previous_continue2
    dec     tilePtr1
previous_continue2:
    lda     tileIndex
    jsr     PRBYTE
    jsr     CR
    jsr     refresh
    jmp     command_loop
:
    ;------------------
    ; = = Next
    ;------------------
    cmp     #$80 | '='
    bne     :+
    jsr     inline_print
    .byte   "Next tile: ",0

    inc     tileIndex
    clc
    lda     tilePtr0
    adc     #LENGTH
    sta     tilePtr0
    bcc     next_continue1
    inc     tilePtr1
next_continue1:
    lda     tileIndex
    cmp     tileMax
    bne     next_continue2
    lda     #0
    sta     tileIndex
    lda     #<example_start
    sta     tilePtr0
    lda     #>example_start
    sta     tilePtr1
next_continue2:
    lda     tileIndex
    jsr     PRBYTE
    jsr     CR
    jsr     refresh
    jmp     command_loop
:

    ;------------------
    ; R = Rotate
    ;------------------
    cmp     #$80 | 'R'
    bne     rotate_after
    jsr     inline_print
    .byte   "Rotate",13,"  Direction (or cancel):",0
    jsr     get_input_direction
    bmi     rotate_cancel

    cmp     #DIR_UP
    bne     :+
    jsr     rotate_up
    jmp     rotate_done
:

    cmp     #DIR_DOWN
    bne     :+
    jsr     rotate_down
    jmp     rotate_done
:
    cmp     #DIR_LEFT
    bne     :+
    jsr     rotate_left
    jmp     rotate_done
:
    ; must be right
    jsr     rotate_right

rotate_done:
    jsr     refresh

rotate_cancel:
    jmp     command_loop

rotate_after:    

    ;------------------
    ; X = flip bits
    ;------------------
    cmp     #$80 + 'X'
    bne     :+
    jsr     inline_print
    .byte   "Flip pixel bits",13,0
    ldy     #0
flip_bits_loop:
    lda     (tilePtr0),y
    eor     #$7f
    sta     (tilePtr0),y
    iny
    cpy     #LENGTH
    bne     flip_bits_loop
    jsr     refresh
    jmp     command_loop
:

    ;------------------
    ; Z = flip color
    ;------------------
    cmp     #$80 + 'Z'
    bne     :+
    jsr     inline_print
    .byte   "Flip color bits",13,0
    ldy     #0
flip_color_loop:
    lda     (tilePtr0),y
    eor     #$80
    sta     (tilePtr0),y
    iny
    cpy     #LENGTH
    bne     flip_color_loop
    jsr     refresh
    jmp     command_loop
:

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

; jump to after changing coordinates
finish_move:
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

    ; check if open apple (clear) is pressed
    lda     BUTN0 
    bpl     :+
    jsr     clearBit
    jmp     display_byte
:
    ; check if closed apple (set) is pressed
    lda     BUTN1 
    bpl     :+
    jsr     setBit
    jmp     display_byte
:
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

dump_count:     ; share with fill color
even_fill:      .byte   0
odd_fill:       .byte   0

.endproc ; tedit_main  

;-----------------------------------------------------------------------------
; print_help
;-----------------------------------------------------------------------------
.proc print_help
    bit     TXTSET
    jsr     inline_print
    .byte   " Arrows: Move cursor",13
    .byte   " Arrows+open/close-apple: Clear/set bit",13
    .byte   " Space: Toggle bit",13
    .byte   " C: Toggle byte color",13
    .byte   " F: Fill tile color",13
    .byte   " R: Rotate bits in a direction",13
    .byte   " I: *Insert row/col",13
    .byte   " D: *Delete row/col",13
    .byte   " X: Flip all pixel bits",13
    .byte   " Z: Flip all color bits",13
    .byte   " !: Dump bytes",13
    .byte   " L: *Load tile set",13
    .byte   " S: *Save tile set",13
    .byte   " N: *New tile set",13
    .byte   " -: Go to previous tile",13
    .byte   " =: Go to next tile",13
    .byte   " ?: HELP",13
    .byte   " Q: Quit",13  
    .byte   " Escape: Toggle text/graphics",13
    .byte   "   * = unimplemented",13
    .byte   0
    rts
.endproc


;-----------------------------------------------------------------------------
; get_input_number
;   Get input for a number 0..max+1, where A == max+1
;   Display number or cancel and return result in A (-1 for cancel)
;-----------------------------------------------------------------------------
.proc get_input_number
    clc
    adc     #$80 + '0'  ; convert A to ascii number
    sta     max_digit     
    jsr     getInput
    cmp     #$80 + '0'
    bmi     cancel
    cmp     max_digit
    bpl     cancel
    jsr     COUT
    sec
    sbc     #$80 + '0'
    rts
cancel:
    jsr     inline_print
    .byte   "Cancel",13,0
    lda     #$ff
    rts

; local variable
max_digit:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; get_input_direction
;   Pick and diplay 1 of 4 directions or cancel
;-----------------------------------------------------------------------------
.proc get_input_direction
    jsr     getInput
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left ",13,0
    lda     #DIR_LEFT
    rts
:
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right",13,0
    lda     #DIR_RIGHT
    rts
:
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up   ",13,0
    lda     #DIR_UP
    rts
:
    cmp     #KEY_DOWN
    bne     :+
    jsr     inline_print
    .byte   "Down ",13,0
    lda     #DIR_DOWN
    rts
:
    jsr     inline_print
    .byte   "Cancel",13,0
    LDA     #$FF
    rts
.endproc

;-----------------------------------------------------------------------------
; refresh
;   Redraw screen
;-----------------------------------------------------------------------------
.proc refresh
    ; Set up screen
    jsr     drawTile
    jsr     drawPreview
    rts
.endproc

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

    ; FIXME - instead of using wait, use a loop that is 
    ; interrupted if a key is pressed
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
;   Note, offset by 1 for boarder
;-----------------------------------------------------------------------------
.proc setScreenPtr
    ; calculate screen pointer
    clc
    ldx     curY
    inx                     ; y+1
    lda     curX            ; curX
    sec                     ; x+1
    adc     lineOffset,x    ; lineOffset(curY)
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
;-----------------------------------------------------------------------------
.proc getByteOffset
    lda     curY
    sta     tempY
    lda     width_bytes
    cmp     #1
    beq     xoffset     ; if width=1, tempY=y
    asl     tempY
    cmp     #2 
    beq     xoffset     ; if width=2, tempY=y*2
    asl     tempY       ; else        tempY=y*4

xoffset:
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
    lda     #$2a
    jmp     exit
bitOne:
    lda     #$aa
exit:
    rts
.endproc

;-----------------------------------------------------------------------------
; drawTile
;-----------------------------------------------------------------------------

.proc drawTile
    ; save curX/Y
    lda     curX
    pha
    lda     curY
    pha

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
    cmp     width
    bmi     xloop

    inc     curY
    lda     curY
    cmp     #HEIGHT
    bne     yloop

    ; restore curX/Y
    pla
    sta     curY
    pla
    sta     curX
    rts

.endproc

;-----------------------------------------------------------------------------
; drawBackground
;-----------------------------------------------------------------------------
.proc drawBackground

    ; upper-left
    lda     #0
    sta     charX
    lda     #0
    sta     charY
    lda     #boarder_upper_left
    jsr     drawChar

    ; upper-right
    ;---------------------------
    lda     width
    sta     charX
    inc     charX   ; width+1
    ;lda     #0
    ;sta     charY
    lda     #boarder_upper_right
    jsr     drawChar

    ; lower-right
    ;---------------------------
    lda     #HEIGHT+1
    sta     charY
    lda     #boarder_lower_right
    jsr     drawChar

    ; lower-left
    ;---------------------------
    lda     #0
    sta     charX
    lda     #boarder_lower_left
    jsr     drawChar

    ; draw horizontal
    ;---------------------------
    lda     #1
    sta     charX

hloop:
    lda     #0
    sta     charY
    lda     #boarder_horizontal
    jsr     drawChar

    lda     #HEIGHT+1
    sta     charY
    lda     #boarder_horizontal
    jsr     drawChar

    inc     charX
    lda     width
    cmp     charX
    bpl     hloop
 
    ; draw vertical
    ;---------------------------
    lda     #1
    sta     charY

vloop:
    lda     #0
    sta     charX
    lda     #boarder_vertical
    jsr     drawChar

    lda     width
    sta     charX
    inc     charX   ; width+1
    lda     #boarder_vertical
    jsr     drawChar

    inc     charY
    lda     #HEIGHT+1
    cmp     charY
    bne     vloop
    rts

.endproc

;-----------------------------------------------------------------------------
; drawPreview
;-----------------------------------------------------------------------------
.proc drawPreview
    ; set screenPtr to fixed location
    lda     #<PREVIEW_ADRS0
    sta     screenPtr0
    lda     #>PREVIEW_ADRS0
    sta     screenPtr1
    jsr     drawShape

    lda     #<PREVIEW_ADRS1
    sta     screenPtr0
    lda     #>PREVIEW_ADRS1
    sta     screenPtr1
    jsr     drawShape
    lda     #<PREVIEW_ADRS2
    sta     screenPtr0
    lda     #>PREVIEW_ADRS2
    sta     screenPtr1
    jsr     drawShape
    lda     #<PREVIEW_ADRS3
    sta     screenPtr0
    lda     #>PREVIEW_ADRS3
    sta     screenPtr1
    jsr     drawShape
    lda     #<PREVIEW_ADRS4
    sta     screenPtr0
    lda     #>PREVIEW_ADRS4
    sta     screenPtr1
    jsr     drawShape

    rts
.endproc

;-----------------------------------------------------------------------------
; drawShape
;   Uses screenPtr for location and tilePtr for tile
;-----------------------------------------------------------------------------

.proc drawShape
    lda     tilePtr0    ; save a copy
    pha

    ldx     #HEIGHT_BYTES-1
drawLoopV:
    ldy     width_bytes_m1
drawLoopH:
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    dey
    bpl     drawLoopH

    clc
    lda     tilePtr0
    adc     width_bytes
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
    bpl     drawLoopV

    ; restore tilePtr
    pla
    sta     tilePtr0

    rts

.endproc

;-----------------------------------------------------------------------------
; bitMask
;   Get mask of bit based on curX
;-----------------------------------------------------------------------------
.proc bitMask
    jsr     getBitOffset
    tax
    beq     continue
    lda     #1
shift_loop:
    asl
    dex
    bne     shift_loop
    rts
continue:
    lda     #1
    rts
.endproc

;-----------------------------------------------------------------------------
; toggleBit
;   Flip bit based on tilePtr, curX, curY
;   Byte result in curData, bit result on Z flag
;-----------------------------------------------------------------------------
.proc toggleBit
    jsr     bitMask
    sta     shiftedBit
    jsr     getByteOffset
    tay
    lda     (tilePtr0),y
    eor     shiftedBit
    sta     (tilePtr0),y
    sta     curData
    and     shiftedBit      ; set Z flag for return
    rts
.endproc

;-----------------------------------------------------------------------------
; clearBit
;   Clear bit based on tilePtr, curX, curY
;   Byte result in curData
;-----------------------------------------------------------------------------
.proc clearBit
    jsr     bitMask
    eor     #$FF            ; flip mask
    sta     shiftedBit
    jsr     getByteOffset
    tay
    lda     (tilePtr0),y
    and     shiftedBit
    sta     (tilePtr0),y
    sta     curData
    rts
.endproc

;-----------------------------------------------------------------------------
; setBit
;   Set bit based on tilePtr, curX, curY
;   Byte result in curData
;-----------------------------------------------------------------------------
.proc setBit
    jsr     bitMask
    sta     shiftedBit
    jsr     getByteOffset
    tay
    lda     (tilePtr0),y
    ora     shiftedBit
    sta     (tilePtr0),y
    sta     curData
    rts
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
; rotate_up
;-----------------------------------------------------------------------------
.proc rotate_up
    lda     length
    clc
    sbc     width_bytes     ; length-width)bytes-1
    sta     done_value
    sec
    sbc     width_bytes     ; length-width_bytes*2-1
    sta     next_column_value     
    ; copyPtr = tilePtr + WIDTH_BYTES
    lda     tilePtr1
    sta     copyPtr1
    lda     tilePtr0
    clc
    adc     width_bytes
    sta     copyPtr0
    ldy     #0
column_loop:
    ; save first byte in temp    
    lda     (tilePtr0),y
    sta     savedByte
    ldx     #HEIGHT_BYTES-1
    jmp     first_iteration
copy_loop:
    tya
    clc
    adc     width_bytes
    tay
first_iteration:    
    lda     (copyPtr0),y    ; source (dest+4)
    sta     (tilePtr0),y    ; dest
    dex
    bne     copy_loop
    lda     savedByte
    sta     (copyPtr0),y
    ; check if done (read last byte)
    cpy     done_value
    bne     :+
    rts
:
    ; next column
    tya
    sec
    sbc     next_column_value
    tay
    jmp     column_loop

done_value:         .byte   0
next_column_value:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; rotate_down
;-----------------------------------------------------------------------------
.proc rotate_down
    ; copyPtr = tilePtr + WIDTH_BYTES
    lda     tilePtr1
    sta     copyPtr1
    lda     tilePtr0
    clc
    adc     width_bytes
    sta     copyPtr0
    ldy     #LENGTH-WIDTH_BYTES-1
column_loop:
    ; save first byte in temp    
    lda     (copyPtr0),y
    sta     savedByte
    ldx     #HEIGHT_BYTES-1
    jmp     first_iteration
copy_loop:
    tya
    sec
    sbc     width_bytes
    tay
first_iteration:    
    lda     (tilePtr0),y    ; source (dest-4)
    sta     (copyPtr0),y    ; dest
    dex
    bne     copy_loop
    lda     savedByte
    sta     (tilePtr0),y
    ; check if done (read last byte)
    cpy     #0
    bne     :+
    rts
:
    ; next column
    tya
    clc
    adc     #LENGTH-WIDTH_BYTES*2-1
    tay
    jmp     column_loop
.endproc

;-----------------------------------------------------------------------------
; rotate_right
;-----------------------------------------------------------------------------
.proc rotate_right
    ldy     #0
loop:
    lda     #0
    sta     prevSavedBit
    ldx     #WIDTH_BYTES

byte_loop:
    ; save color bit
    lda     (tilePtr0),y
    and     #$80
    sta     savedColor
    ; save bit 6 in bit 0
    lda     (tilePtr0),y
    rol     ; 6 -> 7
    rol     ; 7 -> C
    rol     ; C -> 0
    and     #$1             ; only bit 0
    sta     savedBit
    ; shift bits
    lda     (tilePtr0),y
    asl
    and     #$7e
    ora     savedColor      ; preserve color
    ora     prevSavedBit    ; shift in previous bit
    sta     (tilePtr0),y

    lda     savedBit
    sta     prevSavedBit

    iny
    dex
    bne     byte_loop

    ; set final bit
    sty     savedColor      ; save Y for next row
    tya
    sec
    sbc     #WIDTH_BYTES
    tay
    lda     (tilePtr0),y
    ora     prevSavedBit
    sta     (tilePtr0),y

    ; next row
    ldy     savedColor
    cpy     #LENGTH
    bne     loop

    rts

.endproc

;-----------------------------------------------------------------------------
; rotate_left
;-----------------------------------------------------------------------------
.proc rotate_left
    ldy     #LENGTH-1
loop:
    lda     #0
    sta     prevSavedBit
    ldx     #WIDTH_BYTES

byte_loop:
    ; save color bit
    lda     (tilePtr0),y
    and     #$80
    sta     savedColor
    ; save bit 0 in bit 6
    lda     (tilePtr0),y
    ror     ; 0 -> C
    ror     ; C -> 7
    ror     ; 7 -> 6
    and     #$40            ; only bit 6
    sta     savedBit
    ; shift bits
    lda     (tilePtr0),y
    lsr
    and     #$3f
    ora     savedColor      ; preserve color
    ora     prevSavedBit    ; shift in previous bit
    sta     (tilePtr0),y

    lda     savedBit
    sta     prevSavedBit

    dey
    dex
    bne     byte_loop

    ; set final bit
    sty     savedColor      ; save Y for next row
    tya
    clc
    adc     #WIDTH_BYTES
    tay
    lda     (tilePtr0),y
    ora     prevSavedBit
    sta     (tilePtr0),y

    ; next row
    ldy     savedColor
    bpl     loop

    rts

.endproc

;-----------------------------------------------------------------------------
; Global Variables
;-----------------------------------------------------------------------------

curX:           .byte   0       
curY:           .byte   0       
curData:        .byte   0
tileIndex:      .byte   0
tileMax:        .byte   8
sheetStart:     .word   example_start
sheetEnd:       .word   example_end

; when changes size, all of the following need to be updated
width_bytes:    .byte   4       
width_bytes_m1: .byte   3       ; width_byte - 1       
width:          .byte   28      ; width_bytes * 7
width_m1:       .byte   27      ; width - 1
height:         .byte   16
length:         .byte   64      ; width_bytes * height

; Temporary

savedByte:
savedBit:
shiftedBit:     
temp0:          .byte   0
savedColor:     
temp1:          .byte   0
prevSavedBit:   
temp2:          .byte   0

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------



fill_color_even:
    .byte   $00     ; 0 Black
    .byte   $2A     ; 1 Green   - flip on odd bytes
    .byte   $55     ; 2 Purple  - flip on odd bytes
    .byte   $7F     ; 3 White
    .byte   $80     ; 4 Black
    .byte   $AA     ; 5 Orange  - flip on odd bytes
    .byte   $D5     ; 6 Blue    - flip on odd bytes
    .byte   $FF     ; 7 White

fill_color_odd:
    .byte   $00     ; 0 Black
    .byte   $55     ; 1 Green   - flip on odd bytes
    .byte   $2A     ; 2 Purple  - flip on odd bytes
    .byte   $7F     ; 3 White
    .byte   $80     ; 4 Black
    .byte   $D5     ; 5 Orange  - flip on odd bytes
    .byte   $AA     ; 6 Blue    - flip on odd bytes
    .byte   $FF     ; 7 White





; Put example tile last (in case user extends)
; and align

.align  256

example_start:
water:
    .byte   $F5,$AF,$D5,$AA,$DD,$BA,$D5,$AA
    .byte   $D7,$EA,$D5,$AA,$D5,$AA,$D7,$EA
    .byte   $D5,$AA,$DD,$BA,$D5,$AA,$F5,$AF
    .byte   $D5,$AA,$D5,$AA,$D5,$AA,$D5,$AA
    .byte   $D5,$FA,$D7,$AA,$D5,$AE,$DD,$AA
    .byte   $D5,$AB,$F5,$AA,$F5,$AA,$D5,$AB
    .byte   $DD,$AA,$D5,$AE,$D7,$AA,$D5,$FA
    .byte   $D5,$AA,$D5,$AA,$D5,$AA,$D5,$AA

grass:   
    .byte   $2A,$55,$2A,$55,$2A,$55,$2A,$55
    .byte   $2A,$55,$2A,$55,$2A,$55,$28,$55
    .byte   $2A,$55,$2A,$55,$2A,$55,$2A,$55
    .byte   $2A,$55,$2A,$55,$2A,$55,$2A,$55
    .byte   $2A,$55,$2A,$55,$0A,$55,$2A,$55
    .byte   $2A,$55,$2A,$55,$2A,$55,$2A,$55
    .byte   $2A,$55,$2A,$55,$2A,$55,$2A,$54
    .byte   $2A,$55,$2A,$55,$2A,$55,$2A,$55

bricks:   
    .byte   $EA,$D5,$AE,$D5,$EA,$D5,$AE,$D5
    .byte   $EA,$D5,$AE,$D5,$EA,$D5,$AE,$D5
    .byte   $FF,$FF,$FF,$FF,$AA,$DD,$AA,$F5
    .byte   $AA,$DD,$AA,$F5,$AA,$DD,$AA,$F5
    .byte   $AA,$DD,$AA,$F5,$AA,$DD,$AA,$F5
    .byte   $FF,$FF,$FF,$FF,$AB,$D5,$EA,$D5
    .byte   $AB,$D5,$EA,$D5,$AB,$D5,$EA,$D5
    .byte   $AB,$D5,$EA,$D5,$FF,$FF,$FF,$FF

frog1:
    .byte   $80,$80,$80,$80,$20,$15,$28,$05   
    .byte   $68,$5F,$7A,$17,$78,$7A,$5E,$1E   
    .byte   $58,$68,$16,$1A,$78,$7A,$5E,$1E   
    .byte   $68,$5F,$7A,$17,$28,$55,$2A,$15   
    .byte   $20,$45,$22,$05,$20,$55,$2A,$05   
    .byte   $20,$D4,$AA,$04,$20,$55,$2A,$05   
    .byte   $80,$28,$15,$80,$80,$24,$25,$80   
    .byte   $80,$50,$0A,$80,$80,$14,$28,$80   

frog2: 
    .byte   $00,$80,$80,$00,$20,$15,$28,$05   
    .byte   $28,$55,$2A,$15,$28,$55,$2A,$15   
    .byte   $28,$55,$2A,$15,$08,$45,$22,$11   
    .byte   $28,$50,$0A,$14,$28,$55,$2A,$15   
    .byte   $20,$45,$22,$05,$20,$55,$2A,$05   
    .byte   $20,$01,$00,$05,$00,$55,$2A,$01   
    .byte   $00,$28,$15,$00,$00,$24,$25,$00   
    .byte   $00,$50,$0A,$00,$00,$14,$28,$00   

bridge0:            
    .byte   $80,$80,$80,$80,$8A,$D5,$AA,$D4   
    .byte   $8A,$D5,$AA,$D4,$8A,$D5,$AA,$D4   
    .byte   $80,$80,$80,$80,$D5,$D2,$D4,$AA   
    .byte   $D5,$D2,$D4,$AA,$D5,$D2,$D4,$AA   
    .byte   $D5,$D2,$D4,$AA,$D5,$D2,$D4,$AA   
    .byte   $D5,$D2,$D4,$AA,$D5,$D6,$D6,$AA   
    .byte   $D5,$FA,$D5,$AA,$D5,$AA,$D5,$AA   
    .byte   $D5,$AA,$D5,$AA,$D5,$AA,$D5,$AA   

bridge1:
    .byte   $80,$80,$80,$80,$8A,$D5,$AA,$D4   
    .byte   $8A,$D5,$AA,$D4,$8A,$D5,$AA,$D4   
    .byte   $80,$80,$80,$80,$D5,$D2,$D4,$AA   
    .byte   $D5,$D2,$D4,$AA,$D5,$D2,$D4,$AA   
    .byte   $D5,$D2,$D4,$AA,$D5,$D2,$D4,$AA   
    .byte   $D5,$D6,$D6,$AA,$D5,$FA,$D5,$AA   
    .byte   $D5,$AA,$D5,$AA,$D5,$AA,$D5,$AA   
    .byte   $D5,$AA,$D5,$AA,$D5,$AA,$D5,$AA   

trees:         
    .byte   $2A,$45,$2A,$45,$2A,$11,$2A,$11   
    .byte   $28,$51,$28,$54,$22,$54,$02,$55   
    .byte   $0A,$54,$23,$57,$0E,$55,$0A,$55   
    .byte   $0A,$5D,$0A,$75,$22,$55,$2A,$54   
    .byte   $22,$55,$2A,$54,$22,$57,$2A,$54   
    .byte   $28,$55,$3A,$51,$28,$55,$2A,$01   
    .byte   $28,$05,$2A,$01,$02,$D0,$80,$44   
    .byte   $0A,$D0,$80,$55,$2A,$55,$2A,$55   

example_end:

.endproc ; tedit
