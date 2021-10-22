;+
; NAME:
;	STRING_FROM_VECTOR
;
; PURPOSE:
;	This function converts a vector to a single string containing
;	the vector's entries separated by commas.
;
; CATEGORY:
;	Strings
;
; CALLING SEQUENCE:
;	Result = STRING_FROM_VECTOR( Vector [,Ndecplace] )
;
; INPUTS:
;	Vector:  A vector or array of any type.
;
; OPTIONAL INPUTS:
;       Ndecplace:  The number of decimal places to round to in the
;                   output string if Vector is of type real.
;
; KEYWORD PARAMETERS:
;	ADDAND:  Puts an "and" in the output string before the final
;	         entry.  The default is no "and."
;	NOSPACE:  Do not puts spaces after the commas.  The default
;	          is to have spaces.  If this is set, ADDAND cannot
;	          be set.
;	SPACER:  A string containing a spacer to use instead of the default
;		comma.
;
; OUTPUTS:
;	Result:  A string containing the entries of Vector separated
;	         by commas.
;
; USES:
;	STR.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function concatenates the vector values and commas.
;	It uses STR.pro to convert the values to type string if they
;	are not already.
;
; EXAMPLE:
;	Convert [1.2,1.3] to a single string, with 2 decimal places.
;	  result = string_from_vector( [1.2,1.3], 2 )
;	Returns '1.20, 1.30'.
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2001-01-15.
;	Modified:	DAS, 2004-06-16 (added SPACER keyword)
;-

;***********************************************************************

FUNCTION STRING_FROM_VECTOR, $
	Vector, $
	Ndecplace, $
	SPACER=spacer, $
	ADDAND=addandopt, $
	NOSPACE=nospaceopt

;***********************************************************************
; Variables and Options

; Set default spacer if not set
if var_type( spacer ) eq 0 then spacer = ','

; Add 'and' option
addandopt = keyword_set( addandopt )

; No spaces option
nospaceopt = keyword_set( nospaceopt )
if nospaceopt then addandopt = 0

; Variable type
vartype = var_type( vector )

; Length of vector
n = n_elements( vector )

; Initialise output string
strout = ''

;***********************************************************************
; Build Output String

; Build most of the output string
for i = 0, n - 2 do begin
  ; Extract vector element
  if vartype eq 7 then begin
    ; If it is already a string
    strout = strout + strtrim( vector[i], 2 )
  endif else begin
    ; If it needs to be converted to a string
    strout = strout + str( vector[i], ndecplace )
  endelse
  ; Add spacer.
  ; If ADDAND is set and there are only 2 elements then we do not want this
  if addandopt then begin
    if n gt 2 then strout = strout + spacer
  endif else begin
    strout = strout + spacer
  endelse
  ; Add a space if desired
  if not( nospaceopt ) then strout = strout + ' '
endfor

; Add 'and' if desired
if addandopt then strout = strout + 'and '

; Add last entry to output string
if vartype eq 7 then begin
  ; If it is already a string
  strout = strout + strtrim( vector[n-1], 2 )
endif else begin
  ; If it needs to be converted to a string
  strout = strout + str( vector[n-1], ndecplace )
endelse

;***********************************************************************
; The End

return, strout
END
