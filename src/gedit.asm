;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Game Edit     - Collection of tools
;   Tile Edit   - Edit tiles
;   Map Edit    - Edit maps

.include "defines.asm"

; memory map
.segment "CODE"
.org    $4000
tile_address   :=  $6000
map_address    :=  $7000

.proc main

 ;   ; relocated examples to memory
 ;   jsr     memory_copy
 ;   .word   example_tiles
 ;   .word   example_tiles_end
 ;   .word   tile_address

 ;   jsr     memory_clear
 ;   .word   example_tiles_end
 ;   .word   tile_address+$1000

 ;   lda     #<map_address
 ;   sta     sheetStart
 ;   lda     #>map_address
 ;   sta     sheetStart+1

;    jsr     copy
;    .word   example_map
;    .word   example_map_end
;    .word   map_address

;    jsr     memory_clear
;    .word   example_map_end
;    .word   map_address+$1000



    ; Call Tile-Edit
    jsr     tedit
    jsr     medit
    jmp     main
.endproc

; Globals
;-----------------------------------------------------------------------------
; add utilies

.include "inline_print.asm"
.include "font7x8.asm"

; Tile Edit
;-----------------------------------------------------------------------------
.include "tedit.asm"

; Map Edit
;-----------------------------------------------------------------------------
.include "medit.asm"




