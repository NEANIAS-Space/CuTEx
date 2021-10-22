;+
; NAME:
;	UNWRAP
;
; PURPOSE:
;	This function unwraps angular time series such that is it continuous.
;
; CATEGORY:
;	Time series analysis
;
; CALLING SEQUENCE:
;	Result = UNWRAP( X )
;
; INPUTS:
;	X:  A vector of type floating point containing angular information
;
; KEYWORD PARAMETERS:
;	-
;
; OUTPUTS:
;	Result:  The unwrapped version of X.
;
; USES:
;	CONSTANTS.pro
;
; PROCEDURE:
;	This function iteratively unwraps the angular values.
;
; EXAMPLE:
;	Create an incremental vector of angles.
;	  a = 2. * !pi * findgen( 1000 ) / 100.
;	Convert this to Cartesian coordinates, then back to polar coordinates.
;         ang = atan( sin( a ), cos( a ) )
;	Unwrap the values in ANG.
;	  b = unwrap( ang )
;	B should be identical to A, while ANG will have discontinuities and be 
;	restricted to the +/-pi interval.
;
; REFERENCES:
;	-
; 
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-10-25
;-

FUNCTION UNWRAP, $
	X

;***********************************************************************
; Constants and Options

; Absolute constants
constants, pi=pi
; A numerical fix term
epsilon = 0.00001

; Length of the vector
n = n_elements( x )

; Initialise the output vector
y = x

;***********************************************************************
; Unwrap the Angular Phase Information

; Iterate through data points
for i = 1, n - 1 do begin
  ; Check if we are more the 180 degrees out of phase from the preceding value
  if abs( y[i] - y[i-1] ) gt pi then begin
    ; Loop while we are still more the 180 degrees out of phase
    while abs( y[i] - y[i-1] ) gt pi + epsilon do begin
      ; If we are too far ahead in phase
      if y[i] - y[i-1] gt 0 then begin
        ; Increment backward by one full rotation
        y[i] = y[i] - 2. * pi
      ; If we are too far behind in phase
      endif else begin
        ; Increment forward by one full rotation
        y[i] = y[i] + 2. * pi
      endelse
    endwhile
  endif
endfor

;***********************************************************************
; The End

return, y
END
