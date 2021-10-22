;+
; NAME:
;	FIRST_DIGIT
;
; PURPOSE:
;	This function returns the first significant digit.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = FIRST_DIGIT( Indata )
;
; INPUTS:
;	Indata:  A scalar or array of type floating point.
;
; OUTPUTS:
;	Result:  Returns the value of the first significant digit(s)
;	         of Indata.
;
; PROCEDURE:
;	This function searches recursively until it finds the first
;	signficant digit.
;
; EXAMPLE:
;	Determine the first significant digit of 0.0345.
;	  result = first_digit( 0.0345 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-20.
;	Modified:	DAS, 2000-07-06 (removed LENGTH.pro).
;	Modified:	DAS, 2000-09-25 (removed zero bug).
;	Modified:	DAS, 2001-01-11 (fixed rounding bug).
;-

;***********************************************************************

FUNCTION FIRST_DIGIT, Indata

;***********************************************************************
;Constants and Variables

;Correction factor for floating point numbers
cfactor = 1.00001

;Input
data = abs(float(indata))
n = n_elements(data)

;Output
outdata = fix(data)

;***********************************************************************
;Extract First Significant Digit

;Extract first significant digit
for i=0,n-1 do begin
  ;If data = 0
  if data[i] eq 0 then begin
    outdata[i] = 0
  endif
  ;If data >= 1
  if data[i] ge 1 then begin
    while data[i] ge 1 do data[i] = data[i] / 10.
    outdata[i] = floor(cfactor*data[i]*10)
  endif
  ;If data < 1
  if (data[i] lt 1) and (data[i] gt 0) then begin
    while data[i] lt 1 do data[i] = data[i] * 10.
    outdata[i] = floor(cfactor*data[i])
  endif
endfor

;***********************************************************************
;The End

return, outdata
END
