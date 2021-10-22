;+
; NAME:
;	DIMENSION
;
; PURPOSE:
;	This function returns the dimension of an array.  It returns 0
;	if the input variable is scalar.
;
; CATEGORY:
;	Array
;
; CALLING SEQUENCE:
;	Result = DIMENSION( Inarray )
;
; INPUTS:
;	Inarray:  A scalar or array of any type.
;
; OUTPUTS:
;	Result:  The dimension of Inarray.  Returns 0 if scalar.
;
; PROCEDURE:
;	This function runs the IDL function SIZE.
;
; EXAMPLE:
;	Define a 3*4-element array.
;	  x = findgen(3,4)
;	Calculate the dimension of x.
;	  result = dimension( x )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-07-05.
;-

;***********************************************************************

FUNCTION DIMENSION, Inarray

;***********************************************************************
;Calculate the dimension of Inarray.

outdim = (size(inarray))[0]

;***********************************************************************
;The End

return, outdim
END
