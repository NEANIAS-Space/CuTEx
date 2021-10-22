;+
; NAME:
;	SIGN
;
; PURPOSE:
;	This function returns the sign of the input variable.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = SIGN( Yin )
;
; INPUTS:
;	Yin:  A scalar or array of type integer or floating point.
;
; OUTPUTS:
;	Result:  Returns the sign of Yin.
;
; PROCEDURE:
;	This function determines whether Yin <, or > 0.
;
; EXAMPLE:
;	Determine the sign of -3.
;	  result = sign( -3 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-09.
;	Modified:	DAS, 2000-07-06 (removed LENGTH.pro).
;	Modified:	DAS, 2000-07-10 (removed for loops).
;	Modified:	DAS, 2000-07-24 (decided 0 is positive).
;-

;***********************************************************************

FUNCTION SIGN, Yin

;***********************************************************************
;Define variables

n = n_elements(yin)
ans = 0 * fix(yin)

;***********************************************************************
;Determine the sign

;Plus
id = where( yin ge 0, siz )
if siz gt 0 then ans[id] = 1

;Minus
id = where( yin lt 0, siz )
if siz gt 0 then ans[id] = -1

;***********************************************************************
;The End

return, ans
END
