;+
; NAME:
;	PROCESS_LONLATMONTH
;
; PURPOSE:
;       This procedure returns a processed version of the input lon-lat-month 
;	data set.  Processing includes lon-lat interpolation masking, taking 
;	anomalies, and taking averages.
;
; CATEGORY:
;	GEOGRAPHICAL
;
; CALLING SEQUENCE:
;	process_lonlatmonth, data
;
; INPUTS:
;	Data:  A array of data to be processed.  Of dimensions 
;		N_LON*N_LAT*N_TIME.  Missing data should be denoted by NaN.  
;		Returns the processed data array.
;	ANOMALY, DATA_MEAN, DESEASONALISE, FRAC_INTERPOLATE_THRESH, 
;	  FRAC_BASE_THRESH, FRAC_SEASON_THRESH, LAT, LEN_SEASON, LON, 
;	  MASK_DATA, MASK_LON, MASK_LAT, PERIOD_BASE, SEASON, TIME
;
; KEYWORD PARAMETERS:
;       ANOMALY:  If set then anomalies with respect to the PERIOD_BASE 
;		climatology are returned.  If set to 'absolute' then the 
;		absolute anomalies are returned.  If set to 'fractional' then 
;		the fractional anomalies are returned.  The default is not set 
;		(no anomalies).
;       COVERAGE:  
;	DATA_MEAN:  Returns an array of the temporal average of Data over the 
;		PERIOD_BASE period.  Of size N_LON_OUT*N_LAT_OUT.  This is 
;		if DATA_MEAN is set or if ANOMALY is set.
;	DESEASONALISE:  If set then the seasonal cycle is removed from the 
;		data before the seasonal values are calculated.  See 
;		months_to_seasons.pro for more information.
;       FRAC_COVERAGE_THRESH:  The minimum fraction of (weighted) data that 
;		must be defined (i.e. not NaN) in order to proceed with an 
;		integration command.  If the criterion is not met then a NaN is 
;               returned from that calculation.  The default is 0.
;	FRAC_INTERPOLATE_THRESH:  The threshold fraction of the area in a new 
;		spatial cell with non-missing data in the old spatial cells 
;		being interpolated required for the interpolation to be carried 
;		out (as against being counted as missing data).  The default is 
;		0.
;	FRAC_BASE_THRESH:  The fraction of time steps in the base period 
;		required to have non-missing (non-NaN) values in order for the 
;		base value to be calculated.  If this criterion is not 
;		satisfied then a NaN is recorded.  The default is 0.
;	FRAC_SEASON_THRESH:  The fraction of the season that needs non-missing 
;		data in order to be averaged when seasonal averaging is 
;		selected.  The default is 0.
;	INTEGRATE:  A string array containing integration instructions to run 
;		in array_total.pro through the Instruct keyword.  See 
;		array_total.pro for further details.  'mean=1,2' gives the 
;		spatial average, 'total=1' gives the zonal total, 'mean=2' 
;		gives the meridional average, 'mean=3' gives the temporal 
;		average.
;	INTERPOLATE_TOTAL:  If set then when interpolating to the MASK_LAT 
;		and/or MASK_LON coordinates the area-weighted total of all 
;		values in the new grid cells that are overlapping the area in 
;		the new grid cell is taken.  The default is to return the 
;		area-weighted average.
;	LAT:  A vector of latitude values for Data.  Of length N_LAT.  Returns 
;		the vector of latitude values for the returned processed Data, 
;		length N_LAT_OUT.
;	LEN_SEASON:  The length of the season for returning seasonal data.  See 
;		months_to_seasons.pro (SEASONLEN) for more information.
;	LON:  A vector of longitude values for Data.  Of length N_LON.  Returns 
;		the vector of longitude values for the returned processed Data, 
;		of length N_LON_OUT.
;	MASK_DATA:  A masking array of dimensions N_LON_MASK*N_LAT_MASK or 
;		N_LON_MASK*N_LAT_MASK*N_TIME.  Values are 1 (keep) or NaN 
;		(discard).
;	MASK_LAT:  A vector of the latitude coordinates for the mask.  Of 
;		length N_LAT_MASK.  If not given then MASK_LAT is assumed to 
;		equal LAT.
;	MASK_LON:  A vector of the longitude coordinates for the mask.  Of 
;		length N_LON_MASK.  If not given then MASK_LON is assumed to 
;		equal LON.
;	MEAN_AREA:  If set then the spatial mean is returned.  The default is 
;		not set.
;	MEAN_LAT:  If set then the zonal mean is returned.  The default is not 
;		set.
;	MEAN_LON:  If set then the meridional mean is returned.  The default is 
;		not set.
;	PERIOD_BASE:  A vector of length two defining the climatological base 
;		period.  The first/second elements give the first/last year of 
;		the period.  The default value is the full period in TIME.
;	SEASON:  If a value is given, then the function returns seasonal mean 
;		or total data for the desired season of length LEN_SEASLON.  
;		See months_to_seasons.pro for more information.
;	TIME:  A vector of time values for Data.  Of length N_TIME.  Returns 
;		the vector of time values for the returned processed Data, of 
;		length N_TIME_OUT.
;	WEIGHT:  An array size N_LON_OUT*N_LAT_OUT*N_TIME_OUT of weightings to 
;		use when taking an average over one or more dimensions, as 
;		requested in INTEGRATE or MEAN_*.  Note this should not include 
;		the area weightings for a polar grid, which are included 
;		automatically.
;
; OUTPUTS:
;	Data
;	DATA_MEAN, LAT, LON, TIME
;
; USES:
;	add_dim.pro
;	array_total.pro
;	mask_lonlattime.pro
;	months_to_season.pro
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2008-03-26, 
;			adapted from mask_lonlatmonth.pro
;	Modified:	DAS, 2008-04-14 (Added WEIGHT keyword;  added default 
;			base period to averaging section;  added NGOOD=1 to 
;			months_to_season.pro call for time)
;	Modified:	DAS, 2009-04-07 (Corrected bug to allow anomaly 
; ;			calculation of one-dimensional data)
;-

PRO PROCESS_LONLATMONTH, $
    	Data, $
	LAT=lat, LON=lon, TIME=time, $
	ANOMALY=anomaly, $
	COVERAGE=coverage, FRAC_COVERAGE_THRESH=frac_coverage_thresh, $
	INTEGRATE=integrate, $
	MASK_DATA=mask_data, MASK_LAT=lat_mask, MASK_LON=lon_mask, $
	  FRAC_INTERPOLATE_THRESH=frac_interpolate_thresh, $
	  INTERPOLATE_TOTAL=interpolate_total_opt, $
	PERIOD_BASE=period_base, FRAC_BASE_THRESH=frac_base_thresh, $
	SEASON=season, LEN_SEASON=len_season, $
	  FRAC_SEASON_THRESH=frac_season_thresh, $
	  DESEASONALISE=deseasonalise_opt, $
	WEIGHT=weight, $
	DATA_MEAN=data_mean

;***********************************************************************
; Constants and Options

; The number of months in a year
mina = 12

; Degrees to radians conversion factor
degrad = 2 * !pi / 360.

; The number of time steps
n_time = n_elements( time )
if n_time eq 0 then n_time = n_elements( data[0,0,*] )

; The default value of frac_coverage_thresh
if n_elements( frac_coverage_thresh ) eq 0 then frac_coverage_thresh = 0.
; The default value of frac_interpolate_thresh
if n_elements( frac_interpolate_thresh ) eq 0 then frac_interpolate_thresh = 0.
; The default value of frac_base_thresh
if n_elements( frac_base_thresh ) eq 0 then frac_base_thresh = 0.
; The default value of frac_season_thresh
if n_elements( frac_season_thresh ) eq 0 then frac_season_thresh = 0.

;***********************************************************************
; Interpolate and/or Mask the Data

; Proceed if interpolation coordinates and/or a mask is given
if ( n_elements( lon_mask ) gt 0 ) or ( n_elements( lat_mask ) gt 0 ) $
    or keyword_set( mask_data ) then begin

  ; Interpolate and/or mask the data
  if keyword_set( lon ) then temp_lon = lon
  if keyword_set( lat ) then temp_lat = lat
  mask_lonlattime, data, mask_data, lon=temp_lon, lat=temp_lat, $
      mask_lon=lon_mask, mask_lat=lat_mask, $
      frac_interpolate_thresh=frac_interpolate_thresh, $
      total=interpolate_total_opt
  temp_lon = 0
  temp_lat = 0
  if keyword_set( coverage ) then begin
    if keyword_set( mask_data ) then temp = 1. * finite( mask_data )
    mask_lonlattime, coverage, temp, lon=lon, lat=lat, mask_lon=lon_mask, $
        mask_lat=lat_mask, frac_interpolate_thresh=0
    temp = 0
  endif

endif

;***********************************************************************
; Calculate Seasonal Values

; Proceed if a season has been requested
if n_elements( season ) gt 0 then begin

  ; Because of masking issues (changing spatiotemporal masks), this needs to be 
  ; done regardless of whether it was requested.  The mean will be added back 
  ; onto the data at the end if necessary.
  temp_mean = array_total( data, 'mean=3', nan=1, frac_coverage_thresh=0. )
  ;data = data - add_dim( temp_mean, 2, n_time )
  for i = 0 * n_time, n_time - 1 do data[*,*,i] = data[*,*,i] - temp_mean

  ; Determine the base period
  if keyword_set( period_base ) then begin
    id_period_base = intarr( 2 )
    id_period_base[0] = where( period_base[0] * mina eq floor( time * mina ) )
    id_period_base[1] = where( period_base[1] * mina eq floor( time * mina ) )
    id_period_base = id_period_base / mina
  endif else begin
    id_period_base = [ 0, n_time / mina - 1 ]
  endelse
  ; Calculate seasonal values
  n_base_good = floor( frac_base_thresh $
      * ( id_period_base[1] - id_period_base[0] + 1 ) )
  n_seas_good = max( [ 1, floor( frac_season_thresh * len_season ) ] )
  
  data = months_to_seasons( data, season, len_season, $
      baseperiod=id_period_base, deseasonalise=deseasonalise_opt, $
      nbasegood=n_base_good, ngood=n_seas_good )
  time = months_to_seasons( time, season, len_season, ngood=1 )
  if keyword_set( coverage ) then begin
    coverage = months_to_seasons( coverage, season, len_season )
  endif
  n_time = n_elements( time )

  ; Go back to absolute values
  ;data = data + add_dim( temp_mean, 2, n_time )
  for i = 0 * n_time, n_time - 1 do data[*,*,i] = data[*,*,i] + temp_mean
  temp_mean = 0

endif

;***********************************************************************
; Calculate the Temporal Average and Anomalies

; Proceed if requested
if keyword_set( anomaly ) or keyword_set( data_mean ) then begin

  ; Identify the base period
  if keyword_set( period_base ) and ( n_elements( time ) gt 0 ) then begin
    id = where( ( time ge period_base[0] ) and ( time le period_base[1] + 1 ), $
        n_id )
  endif else begin
    id = indgen( n_elements( data[0,0,*] ) )
  endelse

  ; Calculate the mean
  data_mean = array_total( data[*,*,id], 'mean=3', nan=1, $
      frac_coverage_thresh=frac_coverage_thresh )
  temp = intarr( 2 )
  if n_elements( lat ) gt 0 then begin
    temp[1] = n_elements( lat )
  endif
  if n_elements( lon ) gt 0 then begin
    temp[0] = n_elements( lon )
  endif else if temp[1] ne 0 then begin
    temp[0] = n_elements( data_mean ) / temp[1]
  endif
  if min( temp ) ne 0 then data_mean = reform( data_mean, temp )

  ; Take the requested anomalies
  if keyword_set( anomaly ) then begin
    temp_mean = add_dim( data_mean, 2, n_time )
    if anomaly eq 'absolute' then data = data - temp_mean
    if anomaly eq 'fractional' then data = data / temp_mean
    temp_mean = 0
  endif

endif

;***********************************************************************
; Calculate Spatial Averages

; Proceed if a spatial average is requested
if keyword_set( integrate ) then begin

  ; Create area weighting array if averaging over latitude
  if strpos( integrate, '2' ) ge 0 then begin
    ; Determine the number of longitude values
    n_lon = n_elements( lon )
    if n_lon eq 0 then n_lon = n_elements( data[*,0,0] )
    ; Create the area weighting array
    weight_all = add_dim( add_dim( cos( degrad * lat ), 0, n_lon ), 2, n_time )
  endif
  ; Meld the area weighting and input weighting together
  if keyword_set( weight_all ) and keyword_set( weight ) then begin
    weight_all = weight_all * weight
  ; Otherwise just copy the input weighting if given
  endif else if keyword_set( weight ) then begin
    weight_all = weight
  endif

  ; Calculate spatial average
  if keyword_set( weight_all ) then temp_weight = weight_all
  data = array_total( data, integrate, nan=1, weight=temp_weight, $
      frac_coverage_thresh=frac_coverage_thresh, coverage=coverage )
  if keyword_set( data_mean ) then begin
    if keyword_set( weight_all ) then temp_weight = weight_all[*,*,0]
    data_mean = array_total( data_mean, integrate, nan=1, weight=temp_weight, $
      frac_coverage_thresh=frac_coverage_thresh )
  endif
  ; Average spatial coordinates too
  if keyword_set( lon ) then lon = mean( lon )
  if keyword_set( lat ) then lat = mean( lat )

  ; Clear memory
  weight_all = 0
  temp_weight = 0

endif

;***********************************************************************
; The End

return
END
