;+
; NAME:
;	ODD
;
; PURPOSE:
;	This function returns 1 if the input is odd, 0 otherwise.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = ODD( Yin )
;
; INPUTS:
;	Yin:  A scalar or array of type integer.
;
; OUTPUTS:
;	Result:  Returns 1 if Yin is odd, 0 otherwise.
;
; PROCEDURE:
;	This function determines whether Yin is divisible by 2.
;
; EXAMPLE:
;	Determine if 3 is odd.
;	  result = odd( 3 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-09.
;	Modified:	DAS, 2000-07-06 (removed LENGTH.pro).
;	Modified:	DAS, 2000-07-10 (removed for loops).
;	Modified:	DAS, 2000-08-23 (fixed bad bug).
;-

;***********************************************************************

FUNCTION ODD, Yin

;***********************************************************************
;Constants and Variables

;Number of elements in Yin
n = n_elements(yin)

;Output variable
ans = 0 * fix(yin)

;***********************************************************************
;Determine if Yin is odd.

;Check for oddity
id = where( (abs(yin)+1)/2 ne abs(yin)/2, siz )
if siz gt 0 then ans[id] = 1

;***********************************************************************
;The End

return, ans
END
