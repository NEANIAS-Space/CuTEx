;+
; NAME:
;	REAL
;
; PURPOSE:
;	This function returns the real component of a complex number.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = REAL( Z )
;
; INPUTS:
;	Z:  A scalar or array of type complex.
;
; OUTPUTS:
;	Result:  The real component(s) of Z.
;
; PROCEDURE:
;	This function runs the IDL function FLOAT.  I just like the
;	more obvious call name.
;
; EXAMPLE:
;	Define a complex number.
;	  z = 1. + 2 * im()
;	Extract the real component of z.
;	  result = real( z )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-09.
;-

;***********************************************************************

FUNCTION REAL, Z

;***********************************************************************
;Extract real component

x = float(z)

;***********************************************************************
;The End

return, x
END
