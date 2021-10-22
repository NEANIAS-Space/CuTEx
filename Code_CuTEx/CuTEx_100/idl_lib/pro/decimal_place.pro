;+
; NAME:
;	DECIMAL_PLACE
;
; PURPOSE:
;	This function returns the decimal place of the first significant
;	digit.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = DECIMAL_PLACE( Xin )
;
; INPUTS:
;	Xin:  A scalar or vector of type floating point.
;
; KEYWORD PARAMETERS:
;	GREATER:  If set, the function returns negative values for numbers 
;		outside the -1 to 1 range, indicating the placement of the 
;		leading  digit (e.g. "-2" for 100).  The default is to return 
;		"0" for such numbers. 
;
; OUTPUTS:
;	Result:  The decimal place of the first significant digit.  The 
;		default is to return "0" for all numbers not between "-1" and 
;		"1."
;
; PROCEDURE:
;	This function determines the decimal place through a recursive
;	algorithm.  For vectors the function calls itself for each
;	element.
;
; EXAMPLE:
;	Calculate the decimal place of the first significant digit in
;	0.0123.
;	  result = decimal_place( 0.0123 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-27.
;	Modified:	DAS, 2000-07-06 (removed LENGTH.pro).
;	Modified:	DAS, 2000-09-26 (added GREATER keyword).
;	Modified:	DAS, 2000-11-29 (added numerical accuracy fix).
;	Modified:	DAS, 2002-02-06 (fixed numerical accuracy fix)
;-

;***********************************************************************

FUNCTION DECIMAL_PLACE, Xin, $
                        GREATER=greateropt

;***********************************************************************
;Constants and Variables

;Length of input vector
n = n_elements(xin)

;Output initialisation
decplace = 0 * fix(xin)

;Negative decimal places option
greateropt = keyword_set(greateropt)

;Correction factor to deal with numerical accuracy
cfactor = 1.00001

;***********************************************************************
;Calculate Decimal Place

for i=0,n-1 do begin
  x = abs(xin[i])
  if x ne 0 then begin
    x = x * cfactor
    if x lt 1 then begin
      while x lt 1 do begin
        x = x * 10.
        decplace[i] = decplace[i] + 1
      endwhile
    endif else begin
      ;Negative decimal places (if abs() >= 10)
      if greateropt then begin
        while x gt 10 do begin
          x = x / 10.
          decplace[i] = decplace[i] - 1
        endwhile
      endif
    endelse
  endif
endfor

;***********************************************************************
;The End

return, decplace
END
