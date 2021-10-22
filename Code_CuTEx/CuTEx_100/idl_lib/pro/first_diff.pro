;+
; NAME:
;	FIRST_DIFF
;
; PURPOSE:
;	This function returns the first difference vector of the input.
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	Result = FIRST_DIFF( Vect )
;
; INPUTS:
;	Vect:  A vector of type integer or floating point.
;
; KEYWORD PARAMETERS:
;	BACKWARD:  Forces calculation of the backward difference.  This
;	           is the default.
;	FORWARD:  Forces calculation of the forward difference.  The
;	          default is the backward difference.
;	CENTRED:  Forces calculation of the centred difference.  The
;	          defalut is the backward difference.
;
; OUTPUTS:
;	Result:  Returns the first difference of the input vector.
;
; PROCEDURE:
;	The function calculates the first difference of the values in
;	the input vector.  Note that it cycles around and includes the
;	difference between the first and last elements.
;
; EXAMPLE:
;	Define a vector.
;	  x = [1,2,3]
;	Calculate the forward first differenc of x.
;	  result = first_diff( x )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-27.
;	Modified:	Daithi A. Stone, 2000-07-06 (removed
;			LENGTH.pro).
;	Modified:	Daithi A. Stone, 2000-07-10 (removed for loops).
;-

;***********************************************************************

FUNCTION FIRST_DIFF, Vect, $
                     BACKWARD=backwardopt, FORWARD=forwardopt, $
                       CENTRED=centreopt

;***********************************************************************
;Variables

;Vector length
n = n_elements(vect)

;Output
fdif = fltarr(n)

;Difference type

if keyword_set(backwardopt) then begin
  forwardopt = 0
  centreopt = 0
endif else if keyword_set(forwardopt) then begin
  backwardopt = 0
  centreopt = 0
endif else if keyword_set(centreopt) then begin
  backwardopt = 0
  forwardopt = 0
endif else begin
  backwardopt = 1
  forwardopt = 0
  centreopt = 0
endelse

;***********************************************************************
;Calculate First Difference

;Backward difference
if backwardopt then begin
  id1 = indgen(n)
  ;First value = first value - last value (in other words, a fix)
  id2 = [ n-1, indgen(n-1) ]
  fdif = vect[id1] - vect[id2]
endif

;Forward difference
if forwardopt then begin
  ;Last value = first value - last value (in other words, a fix)
  id1 = [ indgen(n-1)+1, 0 ]
  id2 = indgen(n)
  fdif = vect[id1] - vect[id2]
endif

;Centred difference
if centreopt then begin
  ;First value = second value - first value (in other words, a fix)
  ;Last value = last value - second-last value (in other words, a fix)
  id1 = [ indgen(n-1)+1, 0 ]
  id2 = [ n-1, indgen(n-1) ]
  fdif = (vect[id1] - vect[id2]) / 2
endif

;***********************************************************************
;The End

return, fdif
END
