;+
; NAME:
;	ISIN
;
; PURPOSE:
;	This function will tell you whether or not a token is contained in an 
;	array.
;
; CATEGORY:
;	Array
;
; CALLING SEQUENCE:
;	result = IsIn( Data, Token )
;
; INPUTS:
;
;	Data:  The input data array (any type).
;	Token:  The item to search for in array (the same type as data).  This 
;		can be a vector of length N_TOKEN.
;
; KEYWORDS:
;	NO_CASE:  If this keyword is set all inputs are converted to uppercase 
;		before the comparison is made.
;
; OUTPUT:
;	Result:  (0,1) if (not found, found).  If Token is a vector, then 
;		result is a vector of length N_TOKEN with each element being 
;		the result for the corresponding element in Token.
;
; USES:
;	var_type.pro
;
; PROCEDURE:
;	This function uses the Where command to identify all occurences of the 
;	token in the data array and returns a 1 if at least on was found.
;
; EXAMPLE:
;	data = findgen( 10, 10 )
;	if ( IsIn( data, 10 ) ) then Print, '10 is in the data'
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-02-11.
;       Modified:       ECW, 2002-08-08, added NO_CASE keyword
;       Modified:       ECW, 2002-08-13, will now accept any type
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2008-04-25 
;			(allowed Token input to be a vector;  edited style and 
;			made variable names in code consistent with documented 
;			names)
;-

;***********************************************************************

FUNCTION ISIN, $
	Data, Token, $
	NO_CASE=no_case_opt

;***********************************************************************

; Check that the inputs are the same types.
if ( Var_Type( data ) ne Var_Type( token ) ) then Return, 0
; Count the number of tokens
n_token = n_elements( token )

; Copy inputs to temporary variables
temp_data = data
temp_token = token

; If they are strings then check if the case should be ignored.
if ( Var_Type( data ) eq 7 ) and ( Keyword_Set( no_case_opt ) ) then begin
  temp_data = StrUpCase( temp_data )
  temp_token = StrUpCase( temp_token )
endif 

; Initialise output
result = intarr( n_token )
; Iterate through tokens
for i = 0, n_token - 1 do begin
  ; Determine whether this token is in the data
  id = where( temp_data eq temp_token[i], n_id )
  if n_id gt 0 then result[i] = 1
endfor
; Return a scalar if that was given
if n_token eq 1 then result = result[0]

; The End
Return, result
END
