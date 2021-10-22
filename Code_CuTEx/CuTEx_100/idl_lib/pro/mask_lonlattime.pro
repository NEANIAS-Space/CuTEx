;+
; NAME:
;	MASK_LONLATTIME
;
; PURPOSE:
; 	This procedure returns a version of the input lon-lat-time data set 
;	Data which has been interpolated to the grid of Mask and/or masked 
;       according to Mask.
;
; CATEGORY:
;	Geographical
;
; CALLING SEQUENCE:
;	mask_lonlattime, Data, Mask
;
; INPUTS:
;	Data:  A array of data to be interpolated and/or masked.  Of dimensions 
;		N_LON*N_LAT*N_TIME.  Missing data should be denoted by NaN.  
;		Returns the interpolated and/or masked data array.
;	Mask:  A masking array of dimensions N_LON_MASK*N_LAT_MASK or 
;		N_LON_MASK*N_LAT_MASK*N_TIME.  Values are 1 (keep) or NaN 
;		(discard).
;	LAT, LON, MASK_LAT, MASK_LON, FRAC_INTERPOLATE_THRESH
;
; KEYWORD PARAMETERS:
;	FRAC_INTERPOLATE_THRESH:  The threshold fraction of the area in a new 
;		spatial cell with non-missing data in the old spatial cells 
;		being interpolated required for the interpolation to be carried 
;		out (as against being counted as missing data).  The default is 
;		0.
;	LAT:  A vector of latitude values for Data.  Of length N_LAT.  Returns 
;		the vector of latitude values for the returned interpolated 
;		Data, of length N_LAT_OUT.
;	LON:  A vector of longitude values for Data.  Of length N_LON.  Returns 
;		the vector of longitude values for the returned interpolated 
;		Data, of length N_LON_OUT.
;	MASK_LAT:  A vector of the latitude coordinates for the mask.  Of 
;		length N_LAT_MASK.  If not given then MASK_LAT is assumed to 
;		equal LAT.
;	MASK_LON:  A vector of the longitude coordinates for the mask.  Of 
;		length N_LON_MASK.  If not given then MASK_LON is assumed to 
;		equal LON.
;	TOTAL:  If set then when interpolating to the MASK_LAT and/or MASK_LON 
;		coordinates the area-weighted total of all values in the new 
;		grid cells that are overlapping the area in the new grid cell 
;		is taken.  The default is to return the area-weighted average.
;
; OUTPUTS:
;	Data:  Returns the desired interpolated and/or masked data set.  Of 
;		size N_LON_OUT*N_LAT_OUT*N_TIME.  If MEAN_LON is set then 
;		N_LON_OUT=1, otherwise if MASK_LON is set then 
;		N_LON_OUT=N_LON_MASK, otherwise N_LON_OUT=N_LON.  If MEANLAT is 
;		set then N_LAT_OUT=1, otherwise if MASK_LAT is set then 
;		N_LAT_OUT=N_LAT_MASK, otherwise N_LAT_OUT=N_LAT.
;	LAT, LON
;
; USES:
;	add_dim.pro
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2008-03-19, 
;			adapted from mask_lonlatmonth.pro
;	Modified:	DAS, 2009-10-21 (fixed bug with calculation of 
;			interpolated weighting when original weightings are 
;			zero)
;-

PRO MASK_LONLATTIME, $
	Data, $
	Mask, $
	LAT=lat, LON=lon, $
	MASK_LAT=lat_mask, MASK_LON=lon_mask, $
	FRAC_INTERPOLATE_THRESH=frac_interpolate_thresh, $
	TOTAL=total_opt

;***********************************************************************
; Constants and Options

; Number of degrees in a circle
n_degree = 360
; Missing data flag
nan = !values.f_nan

; The default value of frac_interpolate_thresh
if n_elements( frac_interpolate_thresh ) eq 0 then frac_interpolate_thresh = 0.

; Note the dimensions of data
n_lon = n_elements( lon )
if n_lon eq 0 then n_lon = n_elements( data[*,0,0] )
n_lat = n_elements( lat )
if n_lat eq 0 then n_lat = n_elements( data[0,*,0] )
n_time = n_elements( data[0,0,*] )

; Get the dimensions of the mask
if keyword_set( mask ) then begin
  n_lon_mask = n_elements( mask[*,0,0] )
  n_lat_mask = n_elements( mask[0,*,0] )
  n_time_mask = n_elements( mask[0,0,*] )
; Otherwise if the mask is not given then get the dimensions from the mask 
; coordinates
endif else begin
  n_lon_mask = n_elements( lon_mask )
  n_lat_mask = n_elements( lat_mask )
  n_time_mask = 1
endelse

;***********************************************************************
; Check for Consistent Inputs

; If mask coordinates are not given, then check that mask is the same size as 
; data
if keyword_set( mask ) then begin
  if not( keyword_set( lon_mask ) ) and ( n_lon_mask ne n_lon ) then stop
  if not( keyword_set( lat_mask ) ) and ( n_lat_mask ne n_lat ) then stop
  if ( n_time_mask ne 1 ) and ( n_time_mask ne n_time ) then stop
endif

; Ensure that all longitudes are ascending
if keyword_set( lon ) then begin
  id = where( lon[1:n_lon-1] - lon[0:n_lon-2] lt 0, n_id )
  if n_id ne 0 then lon[0:id[0]] = lon[0:id[0]] - n_degree
endif
if keyword_set( lon_mask ) then begin
  id = where( lon_mask[1:n_lon_mask-1] - lon_mask[0:n_lon_mask-2] lt 0, n_id )
  if n_id ne 0 then lon_mask[0:id[0]] = lon_mask[0:id[0]] - n_degree
endif

;***********************************************************************
; Spatially Interpolate DATA to MASK

; If mask coordinates are given
if keyword_set( lat_mask ) or keyword_set( lon_mask ) then begin

  ; If the longitudes are not given for the mask then assume unchanged
  if n_lon_mask eq 0 then begin
    lon_mask = lon
    n_lon_mask = n_elements( lon_mask )
  endif
  ; If the latitudes are not given for the mask then assume unchanged
  if n_lat_mask eq 0 then begin
    lat_mask = lat
    n_lat_mask = n_elements( lat_mask )
  endif

  ; Initialise interpolation weighting arrays
  weight_lat = fltarr( n_lat_mask, n_lat )
  weight_lon = fltarr( n_lon_mask, n_lon )
  ; Initialise a new data array
  data_new = nan * fltarr( n_lon_mask, n_lat_mask, n_time )

  ; Determine the latitude band (assuming regular grid)
  lat_mask_band = fltarr( n_lat_mask, 2 )
  temp = mean( lat_mask[1:n_lat_mask-1] - lat_mask[0:n_lat_mask-2] ) / 2.
  lat_mask_band[*,0] = lat_mask - temp
  lat_mask_band[*,1] = lat_mask + temp
  lat_band = fltarr( n_lat, 2 )
  temp = mean( lat[1:n_lat-1] - lat[0:n_lat-2] ) / 2.
  lat_band[*,0] = lat - temp
  lat_band[*,1] = lat + temp
  ; Iterate through mask latitudes
  for i = 0, n_lat_mask - 1 do begin
    ; Determine data bands that overlap with this mask band
    id_lat = where( ( lat_mask_band[i,0] - lat_band[*,1] le 0. ) $
        and ( lat_mask_band[i,1] - lat_band[*,0] ge 0. ), n_id_lat )
    ; Determine the appropriate area weightings
    for j = 0, n_id_lat - 1 do begin
      temp = max( [ lat_band[id_lat[j],0], lat_mask_band[i,0] ] )
      temp_1 = min( [ lat_band[id_lat[j],1], lat_mask_band[i,1] ] )
      weight_lat[i,id_lat[j]] = ( temp_1 - temp ) $
          / ( lat_mask_band[i,1] - lat_mask_band[i,0] )
    endfor
  endfor
  ; Clear memory
  lat_band = 0
  lat_mask_band = 0

  ; Determine the longitude bands/slices (assuming regular grid)
  lon_mask_band = fltarr( n_lon_mask, 2 )
  temp = mean( lon_mask[1:n_lon_mask-1] - lon_mask[0:n_lon_mask-2] ) / 2.
  lon_mask_band[*,0] = lon_mask - temp
  lon_mask_band[*,1] = lon_mask + temp
  lon_band = fltarr( n_lon, 2 )
  temp = mean( lon[1:n_lon-1] - lon[0:n_lon-2] ) / 2.
  lon_band[*,0] = lon - temp
  lon_band[*,1] = lon + temp
  ; Account for cyclicity
  lon_band = [ lon_band - n_degree, lon_band, lon_band + n_degree ]
  ; Iterate through mask longitudes
  for i = 0, n_lon_mask - 1 do begin
    ; Determine data slices that overlap with this mask slice.
    id_lon = where( ( lon_mask_band[i,0] - lon_band[*,1] le 0. ) $
        and ( lon_mask_band[i,1] - lon_band[*,0] ge 0. ), n_id_lon )
    ; Determine the appropriate area weightings
    for j = 0, n_id_lon - 1 do begin
      temp = max( [ lon_band[id_lon[j],0], lon_mask_band[i,0] ] )
      temp_1 = min( [ lon_band[id_lon[j],1], lon_mask_band[i,1] ] )
      ; Account for our accounting of cyclicity
      id_lon[j] = id_lon[j] - n_lon
      if id_lon[j] lt 0 then id_lon[j] = id_lon[j] + n_lon
      if id_lon[j] ge n_lon then id_lon[j] = id_lon[j] - n_lon
      ; Calculate weighting
      weight_lon[i,id_lon[j]] = ( temp_1 - temp ) $
          / ( lon_mask_band[i,1] - lon_mask_band[i,0] )
    endfor
  endfor
  ; Clear memory
  lon_band = 0
  lon_mask_band = 0

  ; Now interpolate the data.
  ; Iterate through latitudes in the mask
  for i_lat = 0, n_lat_mask - 1 do begin
    ; Find non-zero values in the grid transform for this latitude
    id_lat = where( weight_lat[i_lat,*] gt 0, n_id_lat )
    if n_id_lat gt 0 then begin
      ; Iterate through longitudes
      for i_lon = 0, n_lon_mask - 1 do begin
        ; Find non-zero values in the grid transform for this longitude
        id_lon = where( weight_lon[i_lon,*] gt 0, n_id_lon )
        if n_id_lon gt 0 then begin
          ; Create weighting array
          weight = weight_lat[i_lat,id_lat] $
              ## transpose( weight_lon[i_lon,id_lon] )
          weight = weight / total( weight )
          ; Extract data
          temp_data = data[id_lon,id_lat,*]
          ; Iterate through time
          for i_time = 0, n_time - 1 do begin
            ; Determine if coverage is satisfactory
            temp = total( finite( temp_data[*,*,i_time] ) * weight )
            ; If the coverage is satisfactory then proceed with the 
            ; interpolation
            if temp ge frac_interpolate_thresh then begin
              ; Option not to normalise by coverage
              if keyword_set( total_opt ) eq 1 then temp = 1.
              ; Calculate the mask transform for this grid cell and time
              data_new[i_lon,i_lat,i_time] $
                  = total( temp_data[*,*,i_time] * weight, nan=1 ) / temp
            endif
          endfor
        endif
      endfor
    endif
  endfor
  ; Record interpolated data and clear memory
  data = temporary( data_new )
  temp_data = 0
  weight_lat = 0
  weight_lon = 0
  weight = 0

  ; Copy coordinates
  lat = lat_mask
  n_lat = n_lat_mask
  lon = lon_mask
  n_lon = n_lon_mask

endif

;***********************************************************************
; Mask the Data Array

; If a mask has been given
if keyword_set( mask ) then begin

  ; If the mask is time varying
  if n_time_mask ne 1 then begin
    ; Mask the data array
    data = data * mask
  ; If the mask is time invariant
  endif else begin
    ; Mask the data array
    for i = 0, n_time - 1 do data[*,*,i] = data[*,*,i] * mask
  endelse

endif

;***********************************************************************
; The End

return
END
