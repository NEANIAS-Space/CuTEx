;+
; NAME:
;	CHOOSE_LEVELS
;
; PURPOSE:
;	This function chooses convenient values for contour levels.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	Result = CHOOSE_LEVELS( Data )
;
; INPUTS:
;	Data:  The array of data values, of type integer or floating
;	       point.
;
; KEYWORD PARAMETERS:
;	NLEVELS:  The maximum number of levels to be chosen.  The default is
;		10.
;	RANGE:  A two-element vector containing the FIXED
;		[minimum,maximum] level values.  The result returned
;		will be a list of convenient levels within this range,
;		which may not entirely extend to these limits.  The
;		default is to alter the limiting values such that they
;		are convenient, and thus included in the levels.
;
; USES:
;	DECIMAL_PLACE.pro
;	FIRST_DIGIT.pro
;	INTERVAL_CALC.pro
;	SIGN_NEW.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function adjusts the limiting values of a level scale
;	and chooses the best number of levels such that the level values
;	are convenient decimal numbers.
;
; EXAMPLE:
;	Create a random data set.
;	  y = randomn( 1, 100 )
;	Choose convenient levels for partitioning the data.
;	  result = choose_levels( y )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-09-25
;	Modified:	DAS, 2001-01-12 (switched limit value-choosing
;			algorithm)
;	Modified:	DAS, 2001-05-08 (fixed limit-choosing bug)
;	Modified:	DAS, 2002-02-06 (switched limit value-choosing 
;			algorithm again)
;	Modified:	DAS, 2002-02-13 (added ability to ignore NaNs)
;	Modified:	DAS, 2002-03-12 (added NLEVELS keyword, added checks to
;			value-choosing)
;	Modified:	DAS, 2002-05-07 (fixed float-integer bug)
;	Modified:	DAS, 2002-05-19 (fixed round-off bug)
;	Modified:	DAS, 2002-06-02 (fine-tuning level-choosing)
;-
;	SIGN.pro -> SIGN_NEW.pro for conflict with astrolib package
;***********************************************************************

FUNCTION CHOOSE_LEVELS, $
	Data, $
	NLEVELS=maxnlevels, $
	RANGE=range

;***********************************************************************
;Constants

;Data limiting values
mindatum = min( data, /nan )
maxdatum = max( data, /nan )

;Pre-defined range limits
rangeopt = keyword_set(range)
if rangeopt and (n_elements(range) lt 2) then rangeopt = 0

;The number of levels
nlevelsopt = keyword_set( maxnlevels )
if not( nlevelsopt ) then maxnlevels = 20
nlevels = indgen( maxnlevels-2 ) + 3
realmaxnlevels = 10

;Data variable type
vartype = var_type( data )
if (vartype eq 2) or (vartype eq 3) then one = 1 $
                                    else one = 1.

;Correction factor for floating point numbers
if (vartype eq 2) or (vartype eq 3) then cfact = 1 $
                                    else cfact = 1.00001

;***********************************************************************
;Calculate Convenient Limits

;Pre-set limits
if rangeopt then begin
  if range[1] lt range[0] then begin
    temp = range[0]
    range[0] = range[1]
    range[0] = temp
  endif
endif

;Convenient limits
goodmax = interval_calc( maxdatum/cfact )
goodmin = -interval_calc( -mindatum/cfact )

;Calculate the scale of change
scalemax = decimal_place( goodmax/2., /greater )
scalemin = decimal_place( goodmin/2., /greater )
if scalemax gt scalemin then goodmax = one * 0
if scalemin gt scalemax then goodmin = one * 0
scale = min( [scalemax,scalemin] )
scale = 10. ^ ( -scale )

;Improve maximum level
check = 0
if sign_new( maxdatum ) eq 1 then temp = cfact $
                         else temp = 1. / cfact
while check ne 1 do begin
  if scale eq 0 then check = 1
  if goodmax - scale le maxdatum / temp then begin
    check = 1
  endif else begin
    goodmax = goodmax - scale
  endelse
endwhile
if goodmax lt maxdatum / temp then goodmax = goodmax + scale

;Improve minimum level
check = 0
if sign_new( mindatum ) eq 1 then temp = cfact $
                         else temp = 1. / cfact
while check ne 1 do begin
  if scale eq 0 then check = 1
  if goodmin + scale ge mindatum * temp then begin
    check = 1
  endif else begin
    goodmin = goodmin + scale
  endelse
endwhile
if goodmin gt mindatum * temp then goodmin = goodmin - scale

;Integer or real limits
if goodmax - goodmin ge 6. then begin
  goodmax = round( goodmax )
  goodmin = round( goodmin )
  one = round( one )
endif

;***********************************************************************
;Calculate Convenient Levels

;Number of legend values
dig = first_digit( cfact*(goodmax-goodmin) )
;decplace = max( decimal_place( [goodmin,goodmax] ) )
if dig le 3 then begin
;  if goodmax-goodmin lt 2 then begin
  if dig lt 2 then begin
    decplace = decimal_place( goodmax-goodmin, /greater ) + 1
    dig = round( cfact * ( goodmax - goodmin ) * 10.^decplace )
  endif else begin
    dig = dig * 10
  endelse
endif
;;Update number of levels possibilities
;if rangeopt then begin
;  if (range[0] gt goodmin) or (range[1] lt goodmax) then begin
;    nlevels = [3,4,5,6,7,8,9,10,11]
;  endif
;endif
nlevels0 = round( 2*dig/(nlevels-1)*1.*(nlevels-1) )
id = where( nlevels0 eq 2*dig, nid )
nlevels = nlevels[id[nid-1]]
if nlevelsopt then begin
  while nlevels le maxnlevels/2 do nlevels = 2 * nlevels - 1
  one = 1.
endif
levels = goodmin + findgen( nlevels ) / ( nlevels - 1 ) * ( goodmax - goodmin )
;Remove excess levels
id = where( levels gt maxdatum, nid )
if nid gt 1 then begin
  levels = levels[0:nlevels-nid]
  nlevels = n_elements( levels )
endif
id = where( levels lt mindatum, nid )
if nid gt 1 then begin
  levels = levels[nid-1:nlevels-1]
  nlevels = n_elements( levels )
endif
;If too many levels, remove every second one
if not( nlevelsopt ) then begin
  if nlevels gt realmaxnlevels then begin
    if not( odd( nlevels ) ) then begin
      if levels[0] eq 0 then begin
        levels = [ levels, 2*levels[nlevels-1]-levels[nlevels-2] ]
      endif else begin
        levels = [ 2*levels[0]-levels[1], levels ]
      endelse
    endif
    nlevels = nlevels / 2 + 1
    levels = levels[2*indgen(nlevels)]
  endif
endif

;Correct if levels are beyond pre-defined limits
if rangeopt then begin
  if range[0] lt 0 then id = where( levels ge range[0]*cfact ) $
                   else id = where( levels ge range[0]/cfact )
  levels = levels[id]
  if range[1] lt 0 then id = where( levels le range[1]/cfact ) $
                   else id = where( levels le range[1]*cfact )
  levels = levels[id]
endif

;Revert to integer if necessary
if one/2 eq 0 then levels = round( levels )

;***********************************************************************
;The End

return, levels
END
