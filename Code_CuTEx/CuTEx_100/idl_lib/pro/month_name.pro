;+
; NAME:
;	MONTH_NAME
;
; PURPOSE:
;	This function returns the name of the desired calendar month.
;
; CATEGORY:
;	Calendar
;
; CALLING SEQUENCE:
;	Result = MONTH_NAME( Index )
;
; INPUTS:
;	Index:  The index number of the calendar month (0-11), of
;	        type integer.  Can be a scalar or array.
;
; OUTPUTS:
;	Result:  Returns the name of calendar month #Index.
;
; KEYWORD PARAMETERS:
;	ABBREVIATE:  The function returns the three-letter abbreviation
;	             of the month.  The default is the full name.
;
; USES:
;	DIMENSION.pro
;
; PROCEDURE:
;	This function returns a string with the name of month #Index.
;
; EXAMPLE:
;	Get the name of calendar month #2.
;	  result = MONTH_NAME( 2 )
;	Result = 'March'
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@uvic.ca), 2000-07-10.
;	Modified:	DAS, 2001-01-24 (changed index to 0-11).
;	Modified:	DAS, 2002-01-13 (fixed conversion algorithm to 0-11
;			interval)
;-

;***********************************************************************

FUNCTION MONTH_NAME, Index, $
                     ABBREVIATE=abbreviateopt

;***********************************************************************
;Variables and Constants

;Number of months in a year
constants, mina=mina

;Input variable
months = index - mina * floor( 1. * index / mina )
n = n_elements(index)

;Output variable
if dimension(index) gt 0 then ans = strarr(n) $
                         else ans = ''

;Abbreviate option
abbreviateopt = keyword_set(abbreviateopt)

;***********************************************************************
;Find month name(s)

for i=0,n-1 do begin
  if months[i] eq 0 then ans[i] = 'January'
  if months[i] eq 1 then ans[i] = 'February'
  if months[i] eq 2 then ans[i] = 'March'
  if months[i] eq 3 then ans[i] = 'April'
  if months[i] eq 4 then ans[i] = 'May'
  if months[i] eq 5 then ans[i] = 'June'
  if months[i] eq 6 then ans[i] = 'July'
  if months[i] eq 7 then ans[i] = 'August'
  if months[i] eq 8 then ans[i] = 'September'
  if months[i] eq 9 then ans[i] = 'October'
  if months[i] eq 10 then ans[i] = 'November'
  if months[i] eq 11 then ans[i] = 'December'
endfor

;Abbreviate option
if abbreviateopt then ans = strmid( ans, 0, 3 )

;***********************************************************************
;The End

return, ans
END
