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

;------------------------------------------------
; Zero page usage
;------------------------------------------------

screenPtr0  :=  $52
screenPtr1  :=  $53

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

    ; display a greeting
    jsr     inline_print
    .byte   "Tile editor v0.1",13,0

command_loop:

    jsr     inline_print
    .byte   "Command:",0

    jsr     getInput    ; Wait for a keypress

    ; Parse command

    ;------------------
    ; RIGHT (arrow)
    ;------------------
    cmp     #$95
    bne     :+
    jsr     inline_print
    .byte   "Right X=",0
    inc     curX
    lda     #WIDTH
    cmp     curX
    bne     right_good
    lda     #0
    sta     curX
right_good:
    lda     curX
    jsr     PRBYTE
    jsr     CR
    jmp     command_loop
:
    ;------------------
    ; LEFT (arrow)
    ;------------------
    cmp     #$88
    bne     :+
    jsr     inline_print
    .byte   "Left  X=",0
    dec     curX
    lda     curX
    bpl     left_good
    lda     #WIDTH-1
    sta     curX
left_good:
    lda     curX
    jsr     PRBYTE
    jsr     CR
    jmp     command_loop
:
    ;------------------
    ; UP (arrow)
    ;------------------
    cmp     #$8B
    bne     :+
    jsr     inline_print
    .byte   "Up    Y=",0
    dec     curY
    lda     curY
    bpl     up_good
    lda     #HEIGHT-1
    sta     curY
up_good:
    lda     curY
    jsr     PRBYTE
    jsr     CR
    jmp     command_loop
:
    ;------------------
    ; DOWN (arrow)
    ;------------------
    cmp     #$8A
    bne     :+
    jsr     inline_print
    .byte   "Down  Y=",0
    inc     curY
    lda     #HEIGHT
    cmp     curY
    bne     down_good
    lda     #0
    sta     curY
down_good:
    lda     curY
    jsr     PRBYTE
    jsr     CR
    jmp     command_loop
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
    cmp     #$80 | '?'
    bne     :+
    jsr     inline_print
    .byte   "HELP",13,"(Arrows) Move,(Q)uit,(?)HELP",13,0
    jmp     command_loop

:
    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

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
    lda     #$55
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
    lda     #$00
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

color:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Global Variables
;-----------------------------------------------------------------------------

curX:       .byte   0       
curY:       .byte   0       
pixel:      .byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; max size 4*16 = 64
            .byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

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
