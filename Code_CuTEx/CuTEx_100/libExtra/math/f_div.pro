;+
; PROJECT:
;       SDAC
; NAME:
;	F_DIV
;
; PURPOSE:
;	THIS FUNCTION RETURNS THE QUOTIENT WITH ZERO CHECKING.
;
; CATEGORY:
;	MATH, NUMERICAL ANALYSIS, UTILITY
;
; CALLING SEQUENCE:
;	Result = F_DIV( Numerator, Denominator)
;
; EXAMPLES:
;	count_sec = f_div( total(counts,1) , delta_t )
;	count_sec_prm = f_div( count_sec , 1.- f_div(dead_channel # counts,delta_t) )
;
; INPUTS:
;	NUMERATOR - dividend in a quotient
;	DENOMINATOR - divisor in a quotient
;
; KEYWORDS:
;	DEFAULT - if DENOMINATOR is zero, value is set to 0.0 or to DEFAULT. (INPUT)
;
; PROCEDURE:
;	The divisor is scanned for zeroes which are excluded from the quotient.
;	The value for those elements is 0.0 or the DEFAULT.
; RESTRICTIONS:
; 	Real numbers only.
;
; COMMON BLOCKS:
;	None.
; MODIFICATION HISTORY:
;
;	mod, 22-dec-93, ras, returns vector if numerator or denominator are vectors and other scalar
;	ras, 17-jun-94 renamed div to f_div
;	ras, 14-oct-94, liberal use of temporary
;	Version 4, richard.schwartz@gsfc.nasa.gov, 7-sep-1997, more documentation
;	Version 5, richard.schwartz@gsfc.nasa.gov, 26-jul-2002, made the division work like
;		IDL's division operator.  The size of the quotient follows the size of the normal
;		division operator.  Previously, the size of the quotient was the same as the size of
;		the denominator.
;	22-jan-2003, richard.schwartz@gsfc.nasa.gov, return DENOMINATOR to original values
;		if changed.
;-

FUNCTION F_DIV, NUMERATOR, DENOMINATOR, DEFAULT = DEFAULT

if (size(numerator))(0) eq 0 then numerator = numerator + 0.0 * DENOMINATOR
if (size(DENOMINATOR))(0) eq 0 then DENOMINATOR = DENOMINATOR + 0.0 * numerator
;
ZZERO = where( DENOMINATOR eq 0.0, NZERO)
;
IF (NZERO GE 1) then begin
	NRESULT = N_ELEMENTS(NUMERATOR)<N_ELEMENTS(DENOMINATOR)
	WKEEP = WHERE( ZZERO LE (NRESULT-1), NKEEP)
	IF NKEEP GE 1 THEN BEGIN
		ZZERO = ZZERO(WKEEP)
		DENOMINATOR(ZZERO) = 1
		ENDIF ELSE NZERO = 0

	ENDIF


RESULT      = NUMERATOR / DENOMINATOR

;
IF NZERO GE 1 THEN RESULT( ZZERO ) = Exist(Default) ? Default : 0.0
;
If (NZERO GE 1) THEN IF (NKEEP GE 1) THEN DENOMINATOR(ZZERO) = 0.0
;
return, RESULT
end
