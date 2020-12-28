;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Game Edit     - Collection of tools
;   Tile Edit   - Edit tiles
;   Map Edit    - Edit maps

.include "defines.asm"

.segment "CODE"
.org    $4000

.proc main
    ; Call Tile-Edit
    jmp     tedit
.endproc

; Globals
;-----------------------------------------------------------------------------
; add utilies

.include "inline_print.asm"

; Tile Edit
;-----------------------------------------------------------------------------
.include "tedit.asm"




