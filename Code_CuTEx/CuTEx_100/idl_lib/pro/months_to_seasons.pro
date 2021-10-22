;+
; NAME:
;	MONTHS_TO_SEASONS
;
; PURPOSE:
;	This function extracts seasonal data from monthly data.
;
; CATEGORY:
;	Calendar
;
; CALLING SEQUENCE:
;	Result = MONTHS_TO_SEASONS( Data, Season [, Seasonlen] )
;
; INPUTS:
;	Data:  A vector containing the input time series of monthly data.  
;		It can also be an array where the last dimension is time.  The 
;		time dimension must start with a January and end with a 
;		December.  Also see OUTPUT below.
;	Season:  The index (January=0,...) value of the middle month of the 
;		season.
;	Seasonlen:  Optional.  The number of months in a season.  The default 
;		value is "3".  If Seasonlen is even the extra month is added 
;		at the end.
;
; KEYWORD PARAMETERS:
;	ANOMALY:  If set, then the anomaly to the mean over BASEPERIOD is 
;		calculated.  If DESEASONALISE is set, then the anomaly from 
;		the mean annual cycle over BASEPERIOD is calculated.  The 
;		default is to calculate total values.
;	BASEPERIOD:  A vector of [YEARSTART,YEAREND] defining the period over 
;		which to estimate the seasonal cycle when the ANOMALY or 
;		DESEASONALISE keywords are set.  YEARSTART is the first year 
;		index value (month/12) while YEAREND is the last year index 
;		value.  e.g. using [10,19] would use the month indices 120 
;		through 239.
;	CYCLE:  If DESEASONALISE is set, then cycle contains a vector or array 
;		(with calendar month as the last dimension) containing the 
;		calculated seasonal cycle.  Note this is the total seasonal 
;		cycle if ANOMALY is set, or the seasonal cycle anomaly from 
;		the annual mean if ANOMALY is not set.
;	DESEASONALISE:  If set then the seasonal cycle is removed from the 
;		data before the seasonal values are calculated.  This has an 
;		effect if NGOOD is set, because it then removes the bias 
;		arising from missing data in certain months.  It can also 
;		effect the end values if the season overlaps years.
;	NBASEGOOD:  The number of years in the base period required to have 
;		good (non-NaN) values in order for the base value to be 
;		calculated.  The default is 1.
;	NGOOD:  The number of months in a season required to have good 
;		(non-NaN) values in order to calculate a seasonal value.  The 
;		default is NGOOD=Seasonlen.
;
; OUTPUT:
;	Result:  Returns the seasonal time series as a vector or an array, 
;		depending on the dimensions of Data.  The time dimension will 
;		be shorter by a factor of 12.
;	CYCLE:  See above.
;	Data:  If the DEASONALISE and/or ANOMALY options are set then Data is 
;		returned in a deseasonalised and/or anomaly form.
;
; USES:
;	var_type.pro
;
; PROCEDURE:
;	This function calculates seasonal values based on monthly
;	values.
;
; EXAMPLE:
;	Create a random monthly time series ten years long.
;	  x = randomn( seed, 10*12 )
;	Calculate values for the summer (June-August).
;	  result = months_to_seasons( x, 6, 3 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-08-22.
;	Modified:	DAS, 2000-10-02 (debugged).
;	Modified:	DAS, 2004-12-30 (added BASEPERIOD, CYCLE, 
;			DESEASONALISE, NBASEPERIOD, NGOOD keywords; this 
;			required some altering of main algorithm)
;	Modified:	DAS, 2005-04-25 (can now deal with large number of 
;			spatial points)
;	Modified:	DAS, 2005-08-25 (removed use of constants.pro)
;	Modified:	DAS, 2006-01-06 (altered DEASONALISE keyword to not 
;			include taking anomaly;  added ANOMALY keyword)
;	Modified:	DAS, 2006-02-14 (streamlined some of the code)
;	Modified:	DAS, 2008-03-18 (allowed larger time dimensions through 
;			long integer indices)
;-

;***********************************************************************

FUNCTION MONTHS_TO_SEASONS, $
	Data, $
	Season, $
	Seasonlen, $
	ANOMALY=anomalyopt, DESEASONALISE=deseasonaliseopt, $
	BASEPERIOD=baseperiod, NBASEGOOD=nbasegood, $
	CYCLE=cycle, $
	NGOOD=ngood

;***********************************************************************
; Constants and Variables

; Number of months in a year
mina = 12l
nan = !values.f_nan

; Season length
if not( keyword_set( seasonlen ) ) then seasonlen = 3

; Dimensions of Data
dim = size( data )
; Number of dimensions of Data
ndim = dim[0]
; Number of samples
if ndim eq 1 then begin
  npoint = 1
endif else begin
  npoint = round( product( dim[1:ndim-1] ) )
endelse
; Integer type for point counters
ipoint0 = 0
if var_type( npoint ) eq 3 then ipoint0 = long( ipoint0 )
; Number of months in Data
nmonth = dim[ndim]
; Number of seasons (years)
nyear = nmonth / mina

; Vector of the season's month index values
index = indgen( seasonlen ) - ( seasonlen - 1 ) / 2 + season

; Number of non-missing months required to calculate a seasonal value
if not( keyword_set( ngood ) ) then ngood = seasonlen

; The default base period to use as the reference when deseasonalising or 
; taking the anomaly
if not( keyword_set( baseperiod ) ) then baseperiod = [ 0, nyear - 1 ]
; The number of years in this period
nbase = baseperiod[1] - baseperiod[0] + 1
; The default number of good years required for calculating a reference
if not( keyword_set( nbasegood ) ) then nbasegood = 1

; Reform Data to space-time format.
; Take the transpose too because this speeds things up later
indata = transpose( reform( data, npoint, nmonth ) )

;***********************************************************************
; Options to Deseasonalise and Take Anomalies

; If deseasonalising has been requested
if keyword_set( deseasonaliseopt ) then begin
  ; Initialise the array containing the reference seasonal cycle.
  ; Note we are doing all months in the season because this ends up being 
  ; easier and more flexible (for instance for when seasonlen>12).
  cycle = fltarr( mina, npoint )
  ; Initialise an index of a calendar month in all of the years
  indexall = indgen( nyear ) * mina
  ; Initialise an index of a calendar month in the base years
  indexbase = ( indgen( nbase ) + baseperiod[0] ) * mina
  ; Iterate through points
  for i = ipoint0, npoint - 1 do begin
    ; Iterate through months in a season
    for j = 0, mina - 1 do begin
      ; Copy the calendar month we want
      temp = indata[j+indexbase,i]
      ; Find the good values
      id = where( finite( temp ) eq 1, nid )
      ; If we do not have enough good values then record this season as missing
      if nid lt nbasegood then begin
        cycle[j,i] = nan
      ; If we do have enough good values then record their mean
      endif else begin
        cycle[j,i] = mean( temp[id] )
      endelse
    endfor
  endfor
  ; Iterate through points
  for i = ipoint0, npoint - 1 do begin
    ; Take the anomaly from the annual mean unless we want anomaly output
    if not( keyword_set( anomalyopt ) ) then begin
      cycle[*,i] = cycle[*,i] - mean( cycle[*,i], nan=1 )
    endif
    ; Iterate through months in a season
    for j = 0, mina - 1 do begin
      ; Subtract this seasonal mean from this calendar month in all years
      indata[j+indexall,i] = indata[j+indexall,i] - cycle[j,i]
    endfor
  endfor
endif

; If we want to take the anomaly from the annual mean rather than from the 
; seasonal mean
if keyword_set( anomalyopt ) $
    and not( keyword_set( deseasonaliseopt ) ) then begin
  ; Initialise an index months in the base years
  indexbase = indgen( nbase * mina ) + baseperiod[0] * mina
  ; Iterate through points
  for i = ipoint0, npoint - 1 do begin
    ; Copy the climatology data
    temp = indata[indexbase,i]
    ; Find the good values
    id = where( finite( temp ) eq 1, nid )
    ; If we do not have enough good values then record this point as missing
    if nid lt nbasegood * mina then begin
      indata[*,i] = nan
    ; If we do have enough good values then record their mean
    endif else begin
      indata[*,i] = indata[*,i] - mean( temp, nan=1 )
    endelse
  endfor
endif

;***********************************************************************
; Convert to Seasonal Data

; Initialise the output array (later reformed to the dimensions of Data)
result = nan * fltarr( npoint, nyear )

; Iterate through years
for j = 0, nyear - 1 do begin
  ; Determine which months in our season are within our time series
  id = where( ( index + mina * j ge 0 ) $
      and ( index + mina * j le nmonth - 1 ) )
  ; Iterate through points
  for i = ipoint0, npoint - 1 do begin
    ; Copy the values for this point and year
    temp = indata[j*mina+index[id],i]
    ; Determine where we have good values
    idgood = where( finite( temp ) eq 1, nidgood )
    ; If we have enough good values then calculate the seasonal mean
    if nidgood ge ngood then result[i,j] = mean( temp[idgood] )
  endfor
endfor

;***********************************************************************
; Post-processing

; Reform various output to original dimensions
if ndim eq 1 then begin
  result = reform( result )
  if keyword_set( cycle ) then cycle = reform( cycle )
endif else begin
  result = reform( result, [dim[1:ndim-1],nyear] )
  if keyword_set( cycle ) then begin
    cycle = reform( transpose( cycle ), [dim[1:ndim-1],mina] )
  endif
endelse

;***********************************************************************
; The End

return, result
END
