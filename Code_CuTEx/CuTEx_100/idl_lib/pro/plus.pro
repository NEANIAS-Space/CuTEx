;+
; NAME:
;	PLUS
;
; PURPOSE:
;	This function returns 1 if the input is positive, 0 otherwise.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = PLUS( Y )
;
; INPUTS:
;	Y:  A scalar or array of type integer or floating point.
;
; OUTPUTS:
;	Result:  Returns 1 if Y is positive, 0 otherwise.
;
; PROCEDURE:
;	This function determines whether Y is greater than 0.
;
; EXAMPLE:
;	Determine if 3 is positive.
;	  result = plus( 3 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-09.
;	Modified:	Daithi A. Stone, 2000-07-06 (removed
;			LENGTH.pro).
;	Modified:	Daithi A. Stone, 2000-07-10 (removed for loop).
;-

;***********************************************************************

FUNCTION PLUS, Y

;***********************************************************************
;Determine if Y is positive.

;Output variable
ans = 0 * fix(y)

id = where( y gt 0, siz )
if siz gt 0 then ans[id] = 1

;***********************************************************************
;The End

return, ans
END
