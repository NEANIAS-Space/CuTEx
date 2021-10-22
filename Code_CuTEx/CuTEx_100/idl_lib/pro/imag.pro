;+
; NAME:
;	IMAG
;
; PURPOSE:
;	This function returns the imaginary component of a complex number.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = IMAG( Z )
;
; INPUTS:
;	Z:  A scalar or array of type complex.
;
; OUTPUTS:
;	Result:  The imaginary component(s) of Z.
;
; PROCEDURE:
;	This function runs the IDL function IMAGINARY.  I just like the
;	shorter call name.
;
; EXAMPLE:
;	Define a complex number.
;	  z = 1. + 2 * im()
;	Extract the imaginary component of z.
;	  result = imag( z )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-09.
;-

;***********************************************************************

FUNCTION IMAG, Z

;***********************************************************************
;Extract imaginary component

y = imaginary( z )

;***********************************************************************
;The End

return, y
END
