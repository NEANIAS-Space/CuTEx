;+
; NAME:
;       WRITTEN_NUMBER
;
; PURPOSE:
;	This function returns the written form of the input number.
;
; CATEGORY:
;       Strings
;
; CALLING SEQUENCE:
;       Result = WRITTEN_NUMBER( Num )
;
; INPUTS:
;       Num:  An integer, or string of integers to be converted to written
;	      format of type string.
;
; KEYWORD PARAMETERS:
;	LOWERCASE:  If set, all text characters are in lower case.  The
;	            default is to have the first letter in each word in upper
;	            case.
;	UPPERCASE:  If set, all text characters are in upper case.  The
;	            default is to have only the first letter in each word
;	            in upper case.
;
; OUTPUTS:
;       Result:  Returns a string containing the written form of Num.
;
; USES:
;	DIMENSION.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function takes the last digit of the input number and matches
;	it with a string containing the written form.
;
; EXAMPLE:
;       Return the written value of 3.
;         result = WRITTEN_NUMBER( 3 )
;	Returns Result = 'Three'
;
; MODIFICATION HISTORY:
;       Written by:     Daithi A. Stone, (stoned@uvic.ca) 2001-02-15.
;-

;***********************************************************************

FUNCTION WRITTEN_NUMBER, Num, $
                         LOWERCASE=lowercaseopt, UPPERCASE=uppercaseopt

;***********************************************************************
;Variables

;Upper and lower case options
lowercaseopt = keyword_set(lowercaseopt)
uppercaseopt = keyword_set(uppercaseopt)

;Output vector
n = n_elements(num)
written = strarr(n)

;If input is not integer
if (var_type(num) ne 2) and (var_type(num) ne 3) then begin
  print, 'ERROR in WRITTEN_NUMBER.pro.  Value is not integer.'
  stop
endif

;***********************************************************************
;Construct Written Form

for i=0,n-1 do begin

  if num[i] lt 0 then written[i] = 'Minus ' $
                 else written[i] = ''
  val = abs(num[i])

  ;Extract last digits
  dig0 = round( val - val/10 * 10 )
  dig1 = round( val - dig0 - val/100 * 100 ) / 10
  dig2 = round( val - dig1 * 10 - dig0 - val/1000 * 1000 ) / 100

  ;Take care of numbers larger than 1 billion
  if long(val) ge 1000000000 then begin
    written[i] = written[i] + written_number( val/1000000000 ) + ' Billion '
  endif

  ;Take care of numbers larger than 1 million
  if long(val) ge 1000000 then begin
    written[i] = written[i] $
                 + written_number( val/1000000-(val/1000000000)*1000 ) $
                 + ' Million '
  endif

  ;Take care of numbers larger than 1 thousand
  if long(val) ge 1000 then begin
    written[i] = written[i] $
                 + written_number( val/1000-(val/1000000)*1000 ) + ' Thousand '
  endif

  ;Take care of the hundreds
  if dig2 ne 0 then begin
    written[i] = written[i] + written_number( dig2 ) + ' Hundred '
  endif

  ;Take care of the tens
  if dig1 ne 0 then begin
    if dig1 eq 1 then begin
      if dig0 eq 0 then written[i] = written[i] + 'Ten'
      if dig0 eq 1 then written[i] = written[i] + 'Eleven'
      if dig0 eq 2 then written[i] = written[i] + 'Twelve'
      if dig0 eq 3 then written[i] = written[i] + 'Thirteen'
      if dig0 eq 4 then written[i] = written[i] + 'Fourteen'
      if dig0 eq 5 then written[i] = written[i] + 'Fifteen'
      if dig0 eq 6 then written[i] = written[i] + 'Sixteen'
      if dig0 eq 7 then written[i] = written[i] + 'Seventeen'
      if dig0 eq 8 then written[i] = written[i] + 'Eighteen'
      if dig0 eq 9 then written[i] = written[i] + 'Nineteen'
    endif
    if dig1 eq 2 then written[i] = written[i] + 'Twenty '
    if dig1 eq 3 then written[i] = written[i] + 'Thirty '
    if dig1 eq 4 then written[i] = written[i] + 'Forty '
    if dig1 eq 5 then written[i] = written[i] + 'Fifty '
    if dig1 eq 6 then written[i] = written[i] + 'Sixty '
    if dig1 eq 7 then written[i] = written[i] + 'Seventy '
    if dig1 eq 8 then written[i] = written[i] + 'Eighty '
    if dig1 eq 9 then written[i] = written[i] + 'Ninety '
  endif

  ;Take care of the ones
  if (dig0 ne 0) and (dig1 ne 1) then begin
    if dig0 eq 1 then written[i] = written[i] + 'One'
    if dig0 eq 2 then written[i] = written[i] + 'Two'
    if dig0 eq 3 then written[i] = written[i] + 'Three'
    if dig0 eq 4 then written[i] = written[i] + 'Four'
    if dig0 eq 5 then written[i] = written[i] + 'Five'
    if dig0 eq 6 then written[i] = written[i] + 'Six'
    if dig0 eq 7 then written[i] = written[i] + 'Seven'
    if dig0 eq 8 then written[i] = written[i] + 'Eight'
    if dig0 eq 9 then written[i] = written[i] + 'Nine'
  endif

  ;Clear up extra spaces
  written[i] = strtrim( written[i], 2 )

endfor

;***********************************************************************
;Alter Output According to Preferences

;Convert to lower case if desired
if lowercaseopt then written = strlowcase( written )

;Convert to upper case if desired
if uppercaseopt then written = strupcase( written )

;Convert to scalar if necessary
if dimension(num) eq 0 then written = written[0]

;***********************************************************************
;The End

return, written
END
