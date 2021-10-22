;+
; Project     : HESSI
;
; Name        : GOES
;
; Purpose     : GOES procedure and function.  Procedure starts GOES GUI interface and
;               optionally returns GOES object.
;               GOES function creates a new GOES object
;
; Category    : synoptic objects
;
; Explanation : Provide a GUI to plot GOES flux, temperature, emission measure,
;               select background time intervals, and write a save file.
;
; Syntax      : IDL> goes  or goes, obj (for procedure)
;               IDL> o = goes()  (for function)
;
; Arguments:  : When called as a procedure:
;               obj - returns GOES object reference
;
; Keywords    : Any keywords to pass into goes object
;
; History     : Written 17-Nov-2005, Kim Tolbert
;
; Contact     : kim.tolbert@gsfc.nasa.gov
;
; Modifications:
;
;-

;---------------------------------------------------------------------------
; This file contains the GOES procedure as well as the GOES function.  The one used
; depends on whether it was called as a procedure or a function.

; GOES procedure starts GOES GUI interface and optionally returns GOES object
; GOES function creates a new GOES object

pro goes, obj, _ref_extra=_extra

obj = ogoes(_extra=_extra)

obj->gui

end

;hmm, this doesn't work yet, need to make it compile the whole thing
;function goes, _extra=_extra
;
;return, obj_new('goes', _extra=extra)
;
;end