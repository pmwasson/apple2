;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Predefined memory/ROM locations
;
; Mostly added as needed
; Tried to use standard names if known
;-----------------------------------------------------------------------------

; Grab ca65 defines to start with and then add missing ones
.include "apple2.inc"

; Memory map
HGRPAGE1        = $2000
HGRPAGE2        = $4000

; Soft switches
SPEAKER         = $C030

; ROM routines
HGR             = $F3E2     ; Turn on hi-res mode, page 1 mixed mode, clear    
HGR2            = $F3D8     ; Turn on hi-res mode, page 2, clear
HOME            = $FC58     ; Clear text screen
RDKEY           = $FD0C     ; Read 1 char
COUT            = $FDED     ; Output a character
MON             = $FF65     ; Enter monitor (BRK)
MONZ            = $FF69     ; Enter monitor
