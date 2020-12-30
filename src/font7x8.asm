;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------

; Zero page usage
screenPtr0  :=  $52     ; Screen pointer
screenPtr1  :=  $53
fontPtr0    :=  $54
fontPtr1    :=  $55
stackPtr0   :=  $56
stackPtr1   :=  $57

;-----------------------------------------------------------------------------
; drawChar
;-----------------------------------------------------------------------------
;
;   Draw the character specified in A on the screen at the position pointed 
;   to by charX, charY
;
;-----------------------------------------------------------------------------

.proc drawChar
    and     #$7F

    ; calculate sprite pointer
    sta     temp0           ; Save a copy of A

    asl
    asl
    asl                     ; Multiply by 8
    clc
    adc     #<font7x8
    sta     fontPtr0

    lda     #0
    adc     #>font7x8
    sta     fontPtr1
    lda     temp0
    lsr
    lsr
    lsr
    lsr
    lsr                     ; Divide by 32

    clc
    adc     fontPtr1
    sta     fontPtr1


    ; calculate screen pointer
    ldx     charY
    lda     charX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; Carry not set in loop, so clear outside of loop
    ldx     #$08
drawLoop:
    ldy     #0
    lda     (fontPtr0),y
    sta     (screenPtr0),y

    ; assumes fontPtr aligned such that there are no page crossing
    inc     fontPtr0

    lda     screenPtr1
    adc     #$04
    sta     screenPtr1

    dex
    bne     drawLoop

    rts    

; locals
temp0:  .byte   0

.endproc

.proc drawString
    ; Pop return address to find string
    pla
    sta     stackPtr0
    pla
    sta     stackPtr1
    ldy     #0

    ; Print characters until 0 (end-of-string)
printLoop:
    iny
    bne     :+              ; Allow strings > 255
    inc     stackPtr1
:
    tya
    pha
    lda     (stackPtr0),y
    beq     printExit
    and     #$3f            ; convert ascii
    jsr     drawChar
    inc     charX
    pla
    tay
    jmp     printLoop

printExit:
    pla                 ; clean up stack
    ; calculate return address after print string
    clc
    tya
    adc     stackPtr0  ; add low-byte first
    tax                 ; save in X
    lda     stackPtr1  ; carry to high-byte
    adc     #0          
    pha                 ; push return high-byte
    txa
    pha                 ; push return low-byte
    rts                 ; return

.endproc ; print

.proc   drawBox
    ; upper-left
    ;---------------------------
    lda     charLeft
    sta     charX
    lda     charTop
    sta     charY
    lda     charTL
    jsr     drawChar

    ; upper-right
    ;---------------------------
    lda     charRight
    sta     charX
    lda     charTR
    jsr     drawChar

    ; lower-right
    ;---------------------------
    lda     charBottom
    sta     charY
    lda     charBR
    jsr     drawChar

    ; lower-left
    ;---------------------------
    lda     charLeft
    sta     charX
    lda     charBL
    jsr     drawChar

    ; draw horizontal
    ;---------------------------
    lda     charLeft
    sta     charX
    inc     charX

hloop:
    lda     charTop
    sta     charY
    lda     #boarder_horizontal
    jsr     drawChar

    lda     charBottom
    sta     charY
    lda     #boarder_horizontal
    jsr     drawChar

    inc     charX
    lda     charRight
    cmp     charX
    bne     hloop

    ; draw vertical
    ;---------------------------
    lda     charTop
    sta     charY
    inc     charY

vloop:
    lda     charLeft
    sta     charX
    lda     #boarder_vertical
    jsr     drawChar

    lda     charRight
    sta     charX
    lda     #boarder_vertical
    jsr     drawChar

    inc     charY
    lda     charBottom
    cmp     charY
    bne     vloop

    rts

.endproc

;-----------------------------------------------------------------------------
; globals
;-----------------------------------------------------------------------------

charX:      .byte   0
charY:      .byte   0
charLeft:   .byte   0
charTop:    .byte   0
charRight:  .byte   0
charBottom: .byte   0
charTL:     .byte   boarder_upper_left
charTR:     .byte   boarder_upper_right
charBL:     .byte   boarder_lower_left
charBR:     .byte   boarder_lower_right


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

; 64 character font (no lower case)
; + boarders
.align 8

font7x8:

    ;--------------------------------------
    ; ASSCI shapes
    ;--------------------------------------

    ; @
    .byte   $1E,$33,$33,$3B,$3B,$03,$3E,$00
    ; A
    .byte   $1E,$33,$33,$3F,$33,$33,$33,$00
    ; B
    .byte   $1F,$33,$33,$1F,$33,$33,$1F,$00
    ; C
    .byte   $1E,$33,$03,$03,$03,$33,$1E,$00
    ; D
    .byte   $1F,$33,$33,$33,$33,$33,$1F,$00
    ; E
    .byte   $3E,$06,$06,$1E,$06,$06,$3E,$00
    ; F
    .byte   $3E,$06,$06,$1E,$06,$06,$06,$00
    ; G
    .byte   $1E,$03,$03,$3B,$33,$33,$1E,$00
    ; H
    .byte   $33,$33,$33,$3F,$33,$33,$33,$00
    ; I
    .byte   $1E,$0C,$0C,$0C,$0C,$0C,$1E,$00
    ; J
    .byte   $30,$30,$30,$30,$30,$36,$1C,$00
    ; K
    .byte   $33,$3B,$1F,$0F,$1B,$33,$33,$00
    ; L
    .byte   $06,$06,$06,$06,$06,$06,$3E,$00
    ; M
    .byte   $33,$3F,$3F,$33,$33,$33,$33,$00
    ; N
    .byte   $33,$37,$37,$3F,$3B,$3B,$33,$00
    ; O
    .byte   $1E,$33,$33,$33,$33,$33,$1E,$00
    ; P
    .byte   $1F,$33,$33,$1F,$03,$03,$03,$00
    ; Q
    .byte   $1E,$33,$33,$33,$33,$1B,$3E,$00
    ; R
    .byte   $1F,$33,$33,$1F,$33,$33,$33,$00
    ; S
    .byte   $3C,$06,$06,$1C,$30,$30,$1E,$00
    ; T
    .byte   $3F,$0C,$0C,$0C,$0C,$0C,$0C,$00
    ; U
    .byte   $33,$33,$33,$33,$33,$33,$1E,$00
    ; V
    .byte   $33,$33,$33,$33,$33,$1E,$0C,$00
    ; W
    .byte   $33,$33,$33,$33,$3F,$3F,$33,$00
    ; X
    .byte   $33,$33,$1E,$0C,$1E,$33,$33,$00
    ; Y
    .byte   $33,$33,$33,$1E,$0C,$0C,$0C,$00
    ; Z
    .byte   $3E,$98,$18,$8C,$0C,$86,$3E,$00
    ; [
    .byte   $1E,$06,$06,$06,$06,$06,$1E,$00
    ; \
    .byte   $06,$86,$0C,$8C,$18,$98,$00,$00
    ; ]
    .byte   $1E,$18,$18,$18,$18,$18,$1E,$00
    ; ^
    .byte   $00,$0C,$1E,$33,$00,$00,$00,$00
    ; _
    .byte   $00,$00,$00,$00,$00,$00,$00,$7F
    ; space
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; !
    .byte   $0C,$0C,$0C,$0C,$0C,$00,$0C,$00
    ; "
    .byte   $B3,$33,$00,$00,$00,$00,$00,$00
    ; #
    .byte   $00,$36,$7F,$36,$36,$7F,$36,$00
    ; $
    .byte   $0C,$1E,$03,$1E,$30,$1E,$0C,$00
    ; %
    .byte   $03,$33,$18,$0C,$06,$33,$30,$00
    ; &
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; '
    .byte   $8C,$0C,$00,$00,$00,$00,$00,$00
    ; (
    .byte   $98,$8C,$0C,$0C,$0C,$8C,$98,$00
    ; )
    .byte   $0C,$18,$98,$98,$98,$18,$0C,$00
    ; *
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; +
    .byte   $00,$0C,$0C,$3F,$0C,$0C,$00,$00
    ; ,
    .byte   $00,$00,$00,$00,$8C,$8C,$0C,$00
    ; -
    .byte   $00,$00,$00,$1E,$00,$00,$00,$00
    ; .
    .byte   $00,$00,$00,$00,$00,$0C,$0C,$00
    ; /
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; 0
    .byte   $1E,$33,$33,$33,$33,$33,$1E,$00
    ; 1
    .byte   $0C,$0E,$0C,$0C,$0C,$0C,$1E,$00
    ; 2
    .byte   $1E,$30,$30,$1C,$06,$06,$3E,$00
    ; 3
    .byte   $1E,$30,$30,$1C,$30,$30,$1E,$00
    ; 4
    .byte   $33,$33,$33,$3F,$30,$30,$30,$00
    ; 5
    .byte   $3E,$06,$06,$1E,$30,$30,$1E,$00
    ; 6
    .byte   $1E,$33,$03,$1F,$33,$33,$1E,$00
    ; 7
    .byte   $3F,$98,$18,$8C,$0C,$86,$06,$00
    ; 8
    .byte   $1E,$33,$33,$1E,$33,$33,$1E,$00
    ; 9
    .byte   $1E,$33,$33,$3E,$30,$33,$1E,$00
    ; :
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; ;
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; <
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; =
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; >
    .byte   $00,$00,$00,$00,$00,$00,$00,$00
    ; ?
    .byte   $00,$00,$00,$00,$00,$00,$00,$00


    ;--------------------------------------
    ; Background shapes
    ;--------------------------------------
boarder_start = 64
boarder_upper_left = boarder_start + 0
    .byte   $00,$00,$00,$78,$FC,$3C,$1C,$1C
boarder_horizontal = boarder_start + 1
    .byte   $00,$00,$00,$7F,$7F,$00,$00,$00
boarder_horizontal_r = boarder_start + 2
    .byte   $00,$00,$40,$7F,$7F,$40,$00,$00
boarder_horizontal_l = boarder_start + 3
    .byte   $00,$00,$01,$7F,$7F,$01,$00,$00
boarder_upper_right = boarder_start + 4
    .byte   $00,$00,$00,$0F,$8F,$1E,$1C,$1C
boarder_vertical = boarder_start + 5
    .byte   $1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C
boarder_lower_left = boarder_start + 6
    .byte   $1C,$1C,$3C,$FC,$78,$00,$00,$00
boarder_lower_right = boarder_start + 7
    .byte   $1C,$1C,$1E,$8F,$0F,$00,$00,$00
boarder_t_down = boarder_start + 8
    .byte   $00,$00,$00,$7F,$7F,$9E,$1C,$1C
boarder_t_up = boarder_start + 9
    .byte   $1C,$1C,$9E,$7F,$7F,$00,$00,$00
boarder_t_right = boarder_start + 10    
    .byte   $1C,$1C,$1C,$7C,$7C,$1C,$1C,$1C
boarder_t_left = boarder_start + 11    
    .byte   $1C,$1C,$1C,$1F,$1F,$1C,$1C,$1C
