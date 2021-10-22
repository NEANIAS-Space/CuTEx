;+
; NAME:
;	PARTIAL_FACTORIAL
;
; PURPOSE:
;	This function calculates the partial factorial n!/(n-m)!.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = PARTIAL_FACTORIAL( N, M )
;
; INPUTS:
;	N:  A positive integer (see Purpose).
;	M:  A positive integer (see Purpose).
;
; OUTPUT:
;	Result:  The value of N!/(N-M)!.
;
; USES:
;	-
;
; PROCEDURE:
;	This function multiplies all numbers between and including N and N-M+1.
;
; EXAMPLE:
;	Calculate 4!/(4-2)!.
;	  result = partial_factorial( 4, 2 )
;	The result should be 4*3*2*1/(2*1)=12.
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2004-11-22.
;-

;***********************************************************************

FUNCTION PARTIAL_FACTORIAL, $
	N, M

;***********************************************************************
; Variables and Options

; Check that M is greater than 0
if m le 0 then stop

;***********************************************************************
; Calculate Partial Factorial

; Create a vector of all numbers between and including N-M+1 and N
numbers = dindgen( m ) + n - m + 1

; Take the product of these numbers
result = product( numbers )

;***********************************************************************
; The End

return, result
END
