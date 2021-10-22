;+
; NAME:
;       ORDINAL
;
; PURPOSE:
;	This function returns the ordinal value of the input number.
;
; CATEGORY:
;       Strings
;
; CALLING SEQUENCE:
;       Result = ORDINAL( Num )
;
; INPUTS:
;       Num:  An integer, or string of integers to be converted to ordinal
;	      format of type string.
;
; KEYWORD PARAMETERS:
;	ABBREV:  If set the function returns an abbreviated ordinal value
;	         (i.e. '1st' instead of 'First').  The default is to return
;	         the full value.
;	LOWERCASE:  If set, all text characters are in lower case.  The
;	            default is to have the first letter in each word in upper
;	            case.
;	UPPERCASE:  If set, all text characters are in upper case.  The
;	            default is to have only the first letter in each word
;	            in upper case.
;
; OUTPUTS:
;       Result:  Returns a string containing the ordinal value(s) of Num.
;
; USES:
;	DIMENSION.pro
;	WRITTEN_NUMBER.pro
;
; PROCEDURE:
;	This function takes the last digit of the input number and matches
;	it with a string containing the ordinal value.
;
; EXAMPLE:
;       Return the ordinal value of 3.
;         result = ordinal( 3 )
;	Returns Result = 'Third'
;
; MODIFICATION HISTORY:
;       Written by:     Daithi A. Stone, (stoned@uvic.ca) 1999-03-23.
;       Modified:       DAS, 2001-02-15 (Modified style, added LOWERCASE and
;			UPPERCASE keywords).
;-

;***********************************************************************

FUNCTION ORDINAL, Num, $
                  ABBREV=abbrevopt, $
                  LOWERCASE=lowercaseopt, UPPERCASE=uppercaseopt

;***********************************************************************
;Options and Variables

;Abbreviated ordinal value option
abbrevopt = keyword_set(abbrevopt)

;Upper and lower case options
lowercaseopt = keyword_set(lowercaseopt)
uppercaseopt = keyword_set(uppercaseopt)

;Output vector
n = n_elements(num)
ord = strarr(n)

;***********************************************************************
;Construct Ordinal Value

for i=0,n-1 do begin

  ;Temporary variable
  val = abs(num[i])

  ;Extract last digits
  dig0 = round( val - floor(val/10.) * 10 )
  dig1 = round( val - dig0 - floor(val/100.) * 100 )

  ;Get the ordinal value
  if abbrevopt then begin
    ;Abbreviated format
    if dig0 eq 1 then begin
      if dig1 eq 1 then ord[i] = str(val) + 'th' $
                   else ord[i] = str(val) + 'st'
    endif else if dig0 eq 2 then begin
      if dig1 eq 1 then ord[i] = str(val) + 'th' $
                   else ord[i] = str(val) + 'nd'
    endif else if dig0 eq 3 then begin
      if dig1 eq 1 then ord[i] = str(val) + 'th' $
                   else ord[i] = str(val) + 'rd'
    endif else begin
      ord[i] = str(val) + 'th'
    endelse
    if num[i] lt 0 then ord[i] = 'Minus ' + ord[i]
  endif else begin
    ;Full format
    ;Get the numbers greater than 100
    ord[i] = written_number( (num[i]/100)*100 )
    ;Deal with tens
    if dig0 eq 0 then begin
      if dig1 eq 0 then ord[i] = ord[i] + 'th'
      if dig1 eq 1 then ord[i] = ord[i] + ' Tenth'
      if dig1 eq 2 then ord[i] = ord[i] + ' Twentieth'
      if dig1 eq 3 then ord[i] = ord[i] + ' Thirtieth'
      if dig1 eq 4 then ord[i] = ord[i] + ' Fortieth'
      if dig1 eq 5 then ord[i] = ord[i] + ' Fiftieth'
      if dig1 eq 6 then ord[i] = ord[i] + ' Sixtieth'
      if dig1 eq 7 then ord[i] = ord[i] + ' Seventieth'
      if dig1 eq 8 then ord[i] = ord[i] + ' Eightieth'
      if dig1 eq 9 then ord[i] = ord[i] + ' Nintieth'
    endif
    ;Deal with the ones
    if dig0 ne 0 then begin
      if dig0 eq 1 then begin
        if dig1 eq 1 then ord[i] = ord[i] + ' Eleventh' $
                     else ord[i] = ord[i] + ' First'
      endif
      if dig0 eq 2 then begin
        if dig1 eq 1 then ord[i] = ord[i] + ' Twelfth' $
                     else ord[i] = ord[i] + ' Second'
      endif
      if dig0 eq 3 then begin
        if dig1 eq 1 then ord[i] = ord[i] + ' Thirteenth' $
                     else ord[i] = ord[i] + ' Third'
      endif
      if dig0 eq 4 then ord[i] = ord[i] + ' Fourth'
      if dig0 eq 5 then ord[i] = ord[i] + ' Fifth'
      if dig0 eq 6 then ord[i] = ord[i] + ' Sixth'
      if dig0 eq 7 then ord[i] = ord[i] + ' Seventh'
      if dig0 eq 8 then ord[i] = ord[i] + ' Eighth'
      if dig0 eq 9 then ord[i] = ord[i] + ' Ninth'
    endif
  endelse

endfor

;***********************************************************************
;Alter Output According to Preferences

;Convert to lower case if desired
if lowercaseopt then ord = strlowcase( ord )

;Convert to upper case if desired
if uppercaseopt then ord = strupcase( ord )

;Convert to scalar if necessary
if dimension(num) eq 0 then ord = ord[0]

;***********************************************************************
;The End

return, ord
END
