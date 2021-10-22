;+
; NAME:
;	INTERVAL_CALC
;
; PURPOSE:
;	This function calculates a convenient maximum value for a data
;	set.
;
; CATEGORY:
;	Array
;
; CALLING SEQUENCE:
;	Result = INTERVAL_CALC( Data )
;
; INPUTS:
;	Data:  The input data array.
;
; KEYWORD PARAMETERS:
;	ABSOLUTE:  If set, the function returns the convenient maximum
;	           absolute value for Data.  The default is the maximum
;	           value.
;	EVEN:  If set, the function returns the an even maximum value
;	       (or the highest significant digit is even if < 1).  The
;	       default is to return a convenient maximum value.
;
; OUTPUT:
;	Result:  The convenient maximum value of the values in Data.
;
; USES:
;	ODD.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function works recursively to find a convenient maximum
;	value for the data set.
;
; EXAMPLE:
;	Create a vector.
;	  x = [12,13,8]
;	Calculate a convenient maximum value (20).
;	  result = interval_calc( x )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-08-23.
;	Modified:	DAS, 2001-01-11 (fixed EVEN keyword bug).
;-

;***********************************************************************

FUNCTION INTERVAL_CALC, Data, $
                        ABSOLUTE=Absoluteopt, $
                        EVEN=Evenopt

;***********************************************************************
;Options

;Absolute value option
absoluteopt = keyword_set(absoluteopt)
;Even value option
evenopt = keyword_set(evenopt)

;***********************************************************************
;Set-up

;Absolute value option
if absoluteopt then maxdatum = max(abs(data[where(finite(data) eq 1)])) $
               else maxdatum = max(data[where(finite(data) eq 1)])

;Initialise output
vtype = var_type( data )
if vtype eq 2 then maxint = 1 $
              else maxint = 1.

;***********************************************************************
;Calculate Interval

;If the maximum value is > than 1
if maxdatum gt 1 then begin
  while maxint lt maxdatum do maxint = maxint * 10
  increment = maxint / 10
  while maxdatum le maxint do maxint = maxint - increment
  maxint = maxint + increment
  if evenopt then if odd(round(maxint)) then maxint = maxint + 1
endif

;If the maximum value is < 1, but > 0
if (maxdatum lt 1) and (maxdatum gt 0) then begin
  while maxint gt maxdatum do maxint = maxint / 10
  maxint = maxint * 10
  increment = maxint / 10
  while maxint gt maxdatum do maxint = maxint - increment
  maxint = maxint + increment
  if evenopt then begin
    maxint1 = maxint
    while maxint1 lt 1 do maxint1 = maxint1 * 10
    if odd(round(maxint1)) then maxint = maxint + increment
  endif
endif

;If the maximum value is < 0
if not(absoluteopt) and (maxdatum lt 0) then begin
  maxint = -maxint

  ;If the maximum value is < -1
  if maxdatum lt -1 then begin
    while maxint gt maxdatum do maxint = maxint * 10
    maxint = maxint / 10
    increment = maxint
    while maxdatum le maxint do maxint = maxint + increment
    maxint = maxint - increment
    if evenopt then if odd(round(maxint)) then maxint = maxint + 1
  endif

  ;If the maximum value is > -1, but < 0
  if maxdatum gt -1 then begin
    while maxint lt maxdatum do maxint = maxint / 10
    increment = maxint
    while maxint gt maxdatum do maxint = maxint + increment
    maxint = maxint - increment
    if evenopt then begin
      maxint1 = -maxint
      while maxint1 lt 1 do maxint1 = maxint1 * 10
      if odd(round(maxint1)) then maxint = maxint - increment
    endif
  endif

endif

;If the maximum value is = 0
if maxdatum eq 0 then maxint = 0

;***********************************************************************
;The End

return, maxint
END
