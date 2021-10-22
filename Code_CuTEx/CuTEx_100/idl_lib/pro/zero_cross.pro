;+
; NAME:
;	ZERO_CROSS
;
; PURPOSE:
;	This function returns the number of zero crossings in a given time 
;	series.
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	Result = ZERO_CROSS( Data )
;
; INPUTS:
;	Data:  A vector of type integer or floating point.
;
; KEYWORD PARAMETERS:
;	SUBTRACT_MEAN:  If set, the global mean is substracted from the time 
;		series before analysis.  The default is not to do so.
;
; OPTIONAL OUTPUTS:
;	POS:  Returns the locations in Data just before the zero crossings.
;
; OUTPUTS:
;	Result:  Returns the number of zero crossings.
;
; USES:
;	MEAN.pro
;
; PROCEDURE:
;	This function iteratively compares the signs of neighbouring points.
;
; EXAMPLE:
;	Define a vector.
;	  data = [-1,2,3,-1,2]
;	Find the number of zero crossings.
;	  result = zero_cross( data )
;	The result should be 2.
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2003-10-09.
;-

;***********************************************************************

FUNCTION ZERO_CROSS, $
	Data, $
	SUBSTRACT_MEAN=subtract_meanopt, $
	POS=pos

;***********************************************************************
; Constants and Options

; Length of the time series
nx = n_elements( data )

; We need to know if we need to use long integers for indices
if var_type( nx ) eq 2 then begin
  idtype = 1
endif else begin
  idtype = 1l
endelse

; Copy data
x = data
; Subtract mean if desired
if keyword_set( subtract_meanopt ) then x = x - mean( x )

; Initialise zero crossing counter
nzeroes = 0

; Initialise zero crossing position vector
pos = [ -1 ]

;***********************************************************************
; Determine the Location of the Zero Crossings

; Calculate the products of neighbouring values
signx = x[0*idtype:nx-2] * x[1*idtype:nx-1]

; Iterate through values
for i = 0 * idtype, nx - 2 do begin

  ; If the product of neighbouring values is negative, the values are of 
  ; different sign and so there was a zero crossing between them.
  if signx[i] lt 0 then begin
    ; Add this zero crossing to our list
    nzeroes = nzeroes + 1
    pos = [ pos, i ]

  ; If the product of neighbouring values is zero, there may still be a zero
  ; crossing occurring later.
  ; Also check that we are not in the middle of a flat zero region (ie that we 
  ; have not already counted this crossing).
  endif else if ( signx[i] eq 0 ) and ( x[i] ne 0 ) then begin
    ; Find the next non-zero value
    id = min( where( x[i+1:nx-1] ne 0, nid ) )
    ; If such a value exists
    if nid ne 0 then begin
      ; If the two values at the end of this zero region are of opposite sign
      ; then we have a crossing
      if x[i] * x[i+1+id] lt 0 then begin
        ; Add this zero crossing to our list.
        ; Note we take the location of the middle of the zero region
        nzeroes = nzeroes + 1
        pos = [ pos, i + ( id + 1 ) / 2 ]
      endif
    endif
  endif
endfor

; Remove initialising value from the locations vector
if nzeroes ne 0 then pos = pos[1*idtype:nzeroes]

;***********************************************************************
; The End

return, nzeroes
END
