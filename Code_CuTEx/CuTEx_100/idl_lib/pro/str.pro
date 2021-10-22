;+
; NAME:
;	STR
;
; PURPOSE:
;	This function converts an integer or floating point number to a string.
;
; CATEGORY:
;	Strings
;
; CALLING SEQUENCE:
;	Result = STR( Value [,Ndecplace] )
;
; INPUTS:
;	Value:  A scalar or array of numbers, of type integer or floating 
;		point.
;
; OPTIONAL INPUTS:
;	Ndecplace:  The number of decimal places to round to in the output 
;		string.
;
; KEYWORD PARAMETERS:
;	FILLER:  The filler character to use to fill in extra places when 
;		LENGTH is defined.  The default is a blank space.
;	LENGTH:  The number of characters in the output string.  The function 
;		fills extra places with FILLER to satisfy this.
;
; OUTPUTS:
;	Result:  A string containing the number(s) Value.
;
; USES:
;	DIMENSION.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function uses IDL's STRING function, but adds the ability to set 
;	the number of decimal places.
;
; EXAMPLE:
;	Convert 1.23456 to a string with 2 decimal places.
;	  result = str( 1.23456, 2 )
;	Result should be '1.23'
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-07-11.
;	Modified:	DAS, 2000-08-29 (killed place holder bug).
;	Modified:	DAS, 2006-02-17 (added FILLER keyword)
;-

;***********************************************************************

FUNCTION STR, $
	Value, $
	Ndecplace, $
	FILLER=filler, LENGTH=lenstr

;***********************************************************************
; Variables and Options

; Number of decimal places to output
if var_type( ndecplace ) eq 0 then ndecplace = -1

; Set the default filler if not given
if not( keyword_set( filler ) ) then filler = ' '

; Length of string to output
if not( keyword_set( lenstr ) ) then lenstr = 0

; Number of elements in input
nval = n_elements( value )

; Output
if dimension( value ) eq 0 then begin
  strout = ''
endif else begin
  strout = strarr( nval )
endelse

;***********************************************************************
; Convert Number to String

 ;For real number rounded to Ndecplace decimal places
if ndecplace gt 0 then begin
  ; Extract digits
  factor = 10l ^ ndecplace
  rvalue = abs( round( factor * value ) )
  strout = strtrim( string( rvalue ), 2 )
  ; String length
  strsiz = strlen( strout )
  ; Define string containing the minus sign
  minus = strarr( nval )
  id = where( value lt 0, nid )
  if nid gt 0 then minus[id] = '-'
  ; Insert minus sign and decimal if Value >= 0
  id = where( rvalue ge factor, nid )
  if nid gt 0 then begin
    for i = 0, nid - 1 do begin
      strout[id[i]] = minus[id[i]] $
          + strmid( strout[id[i]], 0, strsiz[id[i]]-ndecplace ) + '.' $
          + strmid( strout[id[i]], strsiz[id[i]]-ndecplace, ndecplace )
    endfor
  endif
  ; Insert minus sign and decimal if Value < 0
  id = where( strsiz lt ndecplace, nid )
  if nid gt 0 then begin
    for i = 0, nid - 1 do begin
      for j = 0, ndecplace - strsiz[id[i]] - 1 do begin
        strout[id[i]] = '0' + strout[id[i]]
      endfor
    endfor
  endif
  id = where( rvalue lt factor, nid )
  if nid gt 0 then strout[id] = minus[id] + '0.' + strout[id]
endif

; For integer string
if ndecplace eq 0 then strout = strtrim( string( round( value ) ), 2 )

; For unformatted string
if ndecplace lt 0 then strout = strtrim( string( value ), 2 )

;***********************************************************************
; Format String Length

strsiz = strlen( strout )
id = where( strsiz lt lenstr, nid )
if nid gt 0 then begin
  for i = 0, n_elements( id ) - 1 do begin
    for j = 0, lenstr - strsiz[id[i]] - 1 do begin
      strout[id[i]] = filler + strout[id[i]]
    endfor
  endfor
endif

;***********************************************************************
; The End

return, strout
END
