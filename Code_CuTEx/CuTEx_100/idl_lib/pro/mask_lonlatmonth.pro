;+
; NAME:
;	MASK_LONLATMONTH
;
; PURPOSE:
; 	This procedure returns a version of the input lon-lat-month data set 
;	Data which has been masked according to Mask and other selected 
;	criteria.  It will also do some area and time averaging.
;
; CATEGORY:
;	Geographical
;
; CALLING SEQUENCE:
;	mask_lonlatmonth, Data, Mask
;
; INPUTS:
;	Data:  A array of data to be masked.  Of dimensions NLON*NLAT*NTIME.
;		Missing data should be denoted by NaN.  Note this is also an 
;		output.
;	Mask:  A masking array of dimensions NMASKLON*NMASKLAT or 
;		NLON*NLAT*NTIME.  Values are 1 (keep) or NaN (discard).
;	ANOMALY, BASEPERIOD, DESEASONALISE, FRACAREAGOOD, FRACBASEGOOD, 
;	  FRACINTERPOLATEGOOD, FRACMEANLATGOOD, FRACMEANLONGOOD, FRACSEASGOOD, 
;	  LAT, LON, MASKLAT, MASKLON, MEANAREA, MEANLAT, MEANLON, SEASLEN, 
;	  SEASON, TIME
;
; KEYWORD PARAMETERS:
;	ANOMALY:  If set then the anomalies from the BASEPERIOD average are 
;		returned.  The default is not set.
;	AREAGOOD:  Returns the fraction of the domain (or of FRACMASKAREA or 
;		of the weighting in COVERAGE if either is given) with 
;		non-missing data as a function of time, in a vector of length 
;		NTIME.
;	BASEPERIOD:  A vector of length two defining the climatological base 
;		period.  The first/second elements give the first/last year of 
;		the period.  See months_to_seasons.pro for more information.
;	COVERAGE:  An array of size NLON*NLAT containing the fraction of each 
;		cell to include when spatially averaging.  This is useful for 
;		instance if a spatial subset of Data is being averaged and a 
;		certain cell straddles the border of that subset.  Values 
;		range from 0 (omit) to 1 (include fully).
;	DESEASONALISE:  If set then the seasonal cycle is removed from the 
;		data before the seasonal values are calculated.  See 
;		months_to_seasons.pro for more information.
;	FRACAREAGOOD:  The fraction of the total area (or of FRACMASKAREA if 
;		it is given) that needs non-missing data in order to be 
;		spatially averaged when MEANAREA is set.  The default is 0.5.
;	FRACBASEGOOD:  The fraction of years in the base period required to 
;		have non-missing values in order for the base value to be 
;		calculated.  The default is 0.5.
;	FRACINTERPOLATEGOOD:  The fraction of the area in a new grid cell that 
;		needs non-missing data in the old grid cells being 
;		interpolated in order for the interpolation to be carried out 
;		(as against being counted as missing data).  The default is 
;		0.5.
;	FRACMASKAREA:  The fraction of the total area to be considered when 
;		checking if the FRACAREAGOOD criterion has been passed.  If 
;		set to -1 then the mask coverage over the climatological base 
;		period is used, and this fraction of the total area is 
;		returned.  The default is to use the actual total area covered 
;		by the grid covered by LAT and LON.
;	FRACSEASGOOD:  The fraction of the season that needs non-missing data 
;		in order to be averaged when seasonal averaging is selected.  
;		The default is 0.5.
;	LAT:  A vector of latitude values for Data.  Of length NLAT.  This is 
;		required if MEANLAT or MEANAREA is set.  Also returns the 
;		vector of latitude values for the returned masked Data, of 
;		length NOUTLAT.
;	LON:  A vector of longitude values for Data.  Of length NLON.  Also 
;		returns the vector of longitude values for the returned masked 
;		Data, of length NOUTLON.
;	MASKLAT:  A vector of the latitude values of the mask.  Of length 
;		NMASKLAT.  If not set then MASKLAT is assumed to equal LAT.
;	MASKLON:  A vector of the longitude values of the mask.  Of length 
;		NMASKLON.  If not set then MASKLON is assumed to equal LON.
;	MEANAREA:  If set then the spatial mean is returned.  The default is 
;		not set.
;	MEANDATA:  Returns an array of the time average of the masked Data 
;		averaged over BASEPERIOD.  Of size NOUTLON*NOUTLAT.
;	MEANLAT:  If set then the zonal mean is returned.  The default is not 
;		set.
;	MEANLON:  If set then the meridional mean is returned.  The default is 
;		not set.
;	SEASLEN:  The length of the season for returning seasonal data.  See 
;		months_to_seasons.pro (SEASONLEN) for more information.
;	SEASON:  If a value is given, then the function returns seasonal mean 
;		or total data for the desired season of length SEASLEN.  See 
;		months_to_seasons.pro for more information.
;	TIME:  A vector of time values for Data in units of years.  Of length 
;		NTIME.  Also returns the vector of time values for the 
;		returned masked (maybe seasonally averaged) Data, of length 
;		NOUTTIME.
;
; OUTPUTS:
;	Data:  Returns the desired masked data set.  Of size 
;		NOUTLON*NOUTLAT*NOUTTIME.  If MEANAREA or MEANLON is set then 
;		NOUTLON=1, otherwise NOUTLON=NMASKLON.  If MEANAREA or 
;		MEANLAT is set then NOUTLAT=1, otherwise NOUTLAT=NMASKLAT.  If 
;		SEASON is set then NOUTTIME=NTIME/12, otherwise 
;		NOUTTIME=NTIME.
;	AREAGOOD, FRACMASKAREA, LAT, LON, MEANDATA, TIME
;
; USES:
;	dimension.pro
;	months_to_seasons.pro
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2006-05-12
;	Modified:	DAS, 2006-06-08 (added input quality checks)
;	Modified:	DAS, 2006-08-16 (replaced MEADGLOBE keyword with 
;			MEANAREA, FRACMEANGLOBEGOOD with FRACAREAGOOD, 
;			NBASEGOOD with FRACBASEGOOD;  changed default 
;			FRACAREAGOOD and FRACINTERPOLATEGOOD to 0.5;  added 
;			AREAGOOD, COVERAGE, MEANLAT, MEANLON, FRACMASKAREA 
;			keywords;  allowed no input of Data and Mask;  some 
;			cleaning of code;  allowed NTIME to be 1;  changed 
;			to skip interpolation if MASK and DATA longitude and 
;			latitude coordinates are identical;  removed 
;			dependency on var_type.pro)
;	Modified:	DAS, 2007-01-22 (fixed a bug in MEANDATA output)
;-

PRO MASK_LONLATMONTH, $
	Data, $
	Mask, $
	LAT=lat, LON=lon, TIME=time, $
	MASKLAT=masklat, MASKLON=masklon, MASKTIME=masktime, $
	SEASON=season, SEASLEN=seaslen, FRACSEASGOOD=fracseasgood, $
	  DESEASONALISE=deseasonaliseopt, $
	ANOMALY=anomalyopt, $
	BASEPERIOD=baseperiod, FRACBASEGOOD=fracbasegood, $
	MEANAREA=meanareaopt, AREAGOOD=areagood, FRACAREAGOOD=fracareagood, $
	  FRACMASKAREA=fracmaskarea, COVERAGE=coverage, $
	MEANLAT=meanlatopt, $
	MEANLON=meanlonopt, $
	FRACINTERPOLATEGOOD=fracinterpolategood, $
	MEANDATA=meandata, $
	MEANGLOBE=meanglobeopt, FRACMEANGLOBEGOOD=fracmeanglobegood, $
	  NBASEGOOD=nbasegood

;***********************************************************************
; Constants and Options

; Warn about the use of obsolete keywords
if n_elements( meanglobeopt ) ne 0 then begin
  meanareaopt = meanglobeopt
  print, 'Warning:  MEANGLOBE keyword is obsolete.  Use MEANAREA.'
endif
if n_elements( fracmeanglobegood ) ne 0 then begin
  fracareagood = fracmeanglobegood
  print, 'Warning:  FRACMEANGLOBEGOOD keyword is obsolete.  Use FRACAREAGOOD.'
endif
if n_elements( nbasegood ) ne 0 then begin
  fracbasegood = nbasegood / ( baseperiod[1] - baseperiod[0] + 1 )
  print, 'Warning:  NBASEGOOD keyword is obsolete.  Use FRACBASEGOOD.'
endif

; Number of degrees in a circle
ndegree = 360
; Degrees to radians conversion coefficient
degrad = !pi / ndegree * 2.
; Missing data flag
nan = !values.f_nan
; Months in a year
mina = 12

; Options for spatial averaging
if keyword_set( meanareaopt ) then begin
  meanareaopt = 1
endif else begin
  meanareaopt = 0
endelse
if keyword_set( meanlatopt ) then begin
  meanlatopt = 1
endif else begin
  meanlatopt = 0
endelse
if keyword_set( meanlonopt ) then begin
  meanlonopt = 1
endif else begin
  meanlonopt = 0
endelse
meanopt = max( [ meanareaopt, meanlatopt, meanlonopt ] )

; The default value of FRACAREAGOOD
if meanopt eq 1 then begin
  if not( keyword_set( fracareagood ) ) then fracareagood = 0.5
endif
; The default value of FRACINTERPOLATEGOOD
if not( keyword_set( fracinterpolategood ) ) then fracinterpolategood = 0.5
; The default value of FRACSEASGOOD
if not( keyword_set( fracseasgood ) ) then fracseasgood = 0.5

; The number of longitude steps in Data
if keyword_set( lon ) then begin
  nlon = n_elements( lon )
endif else begin
  nlon = n_elements( data[*,0,0] )
endelse
; The number of latitude steps in Data
if keyword_set( lat ) then begin
  nlat = n_elements( lat )
endif else begin
  nlat = n_elements( data[0,*,0] )
endelse
; The number of months in Data (time steps)
if keyword_set( time ) then begin
  ntime = n_elements( time )
endif else begin
  ntime = n_elements( data[0,0,*] )
endelse
; Set default Data coordinates if not given
if not( keyword_set( time ) ) then begin
  time = findgen( ntime ) / mina + 1. / mina / 2.
endif

; Get the dimensions of the mask
if keyword_set( mask ) then begin
  nmasklon = n_elements( mask[*,0,0] )
  nmasklat = n_elements( mask[0,*,0] )
  nmasktime = n_elements( mask[0,0,*] )
endif

; Check that DATA and MASK are of the same size if certain coordinates are 
; not given
if keyword_set( mask ) and keyword_set( data ) then begin
  if not( keyword_set( masklon ) ) or not( keyword_set( lon ) ) then begin
    if nmasklon ne nlon then stop
  endif
  if not( keyword_set( masklat ) ) or not( keyword_set( lat ) ) then begin
    if nmasklat ne nlat then stop
  endif
  if not( keyword_set( masktime ) ) or not( keyword_set( time ) ) then begin
    if ( nmasktime ne 1 ) and ( nmasktime ne ntime ) then stop
  endif
endif

; Ensure that all longitudes are positive
if keyword_set( lon ) then begin
  id = where( lon lt 0, nid )
  if nid ne 0 then lon[id] = lon[id] + ndegree
endif
if keyword_set( masklon ) then begin
  id = where( masklon lt 0, nid )
  if nid ne 0 then masklon[id] = masklon[id] + ndegree
endif

; The default climatological base period
if not( keyword_set( baseperiod ) ) then begin
  baseperiod = [ floor( time[0] ), floor( time[ntime-1] ) ] - floor( time[0] )
endif
; The required fraction of good data in the base period
if n_elements( fracbasegood ) eq 0 then fracbasegood = 0.5
nbasegood = ceil( fracbasegood * ( baseperiod[1] - baseperiod[0] + 1 ) )

; The default fraction of the total area to use in the spatial averaging 
; criterion (all of the area)
if n_elements( fracmaskarea ) eq 0 then fracmaskarea = 1.

;***********************************************************************
; Setup the MASK

; If a mask has been given
if keyword_set( mask ) then begin

  ; If the mask is time independent then make it time dependent
  if nmasktime eq 1 then begin
    ; Then make it time dependent
    newmask = fltarr( nmasklon, nmasklat, ntime )
    for i = 0, ntime - 1 do newmask[*,*,i] = mask
    mask = temporary( newmask )
    ; Create the mask time vector
    masktime = time
    nmasktime = ntime
  ; If the mask is time dependent already
  endif else begin
    ; If a mask time vector is defined
    if keyword_set( masktime ) then begin
      ; Retain values of data that are within the mask time period
      id = where( ( time ge masktime[0] - 1. / mina / 2. ) $
          and ( time le masktime[nmasktime-1] + 1. / mina / 2. ), ntime )
      time = time[id]
      if keyword_set( data ) then data = data[*,*,id]
      ; Retain values of the mask that are within the data time period
      id = where( ( masktime ge time[0] - 1. / mina / 2. ) $
          and ( masktime le time[ntime-1] + 1. / mina / 2. ), nmasktime )
      masktime = masktime[id]
      mask = mask[*,*,id]
    ; If no mask time vector is defined
    endif else begin
      ; Otherwise continue supposing that the time coordinates correspond 
      masktime = time
      nmasktime = n_elements( masktime )
    endelse
  endelse

endif

;***********************************************************************
; Spatially Interpolate DATA to MASK

; If a mask has been given
if keyword_set( mask ) and keyword_set( data ) then begin

  ; If longitude and latitude coordinates are given (and therefore perhaps 
  ; different) then we should interpolate to the MASK coordinates
  check = 0
  ; Check if MASK coordinates equal DATA coordinates
  if keyword_set( masklon ) and keyword_set( lon ) $
      and keyword_set( masklat ) and keyword_set( lat ) then begin
    check = 1
    if ( nmasklon eq nlon ) and ( nmasklat eq nlat ) then begin
      if ( max( abs( masklon - lon ) ) eq 0 ) $
          and ( max( abs( masklat - lat ) ) eq 0 ) then check = 0
    endif
  endif
  ; If MASK and DATA coordinates differ, then we want to interpolate
  if check eq 1 then begin

    ; Calculate the first differences
    dlat = abs( lat[1] - lat[0])
    dmasklat = abs( masklat[1] - masklat[0] )
    dlon = abs( lon[1] - lon[0] )
    dmasklon = abs( masklon[1] - masklon[0] )

    ; Initialise a new data array
    newdata = nan * fltarr( nmasklon, nmasklat, nmasktime )
    ; Iterate through mask latitudes
    for i = 0, nmasklat - 1 do begin

      ; Determine DATA bands that overlap with this mask band
      idlat = where( ( masklat[i] - dmasklat / 2. - lat - dlat / 2. le 0. ) $
          and ( masklat[i] + dmasklat / 2. - lat + dlat / 2. ge 0. ), nwlat )
      ; Determine the appropriate area weightings (assuming rectangular)
      wlat = fltarr( nwlat )
      for j = 0, nwlat - 1 do begin
        temp = max( [ lat[idlat[j]] - dlat / 2., masklat[i] - dmasklat / 2. ] )
        temp1 = min( [ lat[idlat[j]] + dlat / 2., $
            masklat[i] + dmasklat / 2. ] )
        wlat[j] = ( temp1 - temp ) / dlat
      endfor
      
      ; Iterate through data longitudes
      for j = 0, nmasklon - 1 do begin

        ; Determine DATA sectors that overlap with this mask sector.  We also 
        ; have to worry about cyclic wrapping here
        idlon = where( ( masklon[j] - dmasklon / 2. - lon - dlon / 2. le 0. ) $
            and ( masklon[j] + dmasklon / 2. - lon + dlon / 2. ge 0. ), nwlon )
        idlon1 = where( ( masklon[j] - dmasklon / 2. $
            - lon + ndegree - dlon / 2. le 0. ) $
            and ( masklon[j] + dmasklon / 2. $
            - lon + ndegree + dlon / 2. ge 0. ), nwlon1 )
        if nwlon1 ne 0 then begin
          if nwlon eq 0 then begin
            idlon = idlon1
          endif else begin
            idlon = [ idlon, idlon1 ]
          endelse
          nwlon = nwlon + nwlon1
        endif
        idlon1 = where( ( masklon[j] - dmasklon / 2. $
            - lon - ndegree - dlon / 2. le 0. ) $
            and ( masklon[j] + dmasklon / 2. $
            - lon - ndegree + dlon / 2. ge 0. ), nwlon1 )
        if nwlon1 ne 0 then begin
          if nwlon eq 0 then begin
            idlon = idlon1
          endif else begin
            idlon = [ idlon, idlon1 ]
          endelse
          nwlon = nwlon + nwlon1
        endif
        ; Determine the appropriate area weightings (assuming rectangular)
        wlon = fltarr( nwlon )
        for k = 0, nwlon - 1 do begin
          temp = max( [ lon[idlon[k]] - dlon / 2., $
              masklon[j] - dmasklon / 2. ] )
          temp1 = min( [ lon[idlon[k]] + dlon / 2., $
              masklon[j] + dmasklon / 2. ] )
          wlon[k] = ( temp1 - temp ) / dlon
        endfor

        ; Create a full weighting matrix
        weight = fltarr( nwlon, nwlat )
        for k = 0, nwlat - 1 do weight[*,k] = wlon * wlat[k]
        weight = weight / total( weight )

        ; Finally, let's interpolate the data
        for k = 0, ntime - 1 do begin
          temp = total( weight * finite( data[idlon,idlat,k] ) )
          if temp ge fracinterpolategood then begin
            newdata[j,i,k] = total( weight * data[idlon,idlat,k], nan=1 ) $
                / temp
          endif
        endfor

      endfor

    endfor

    ; Copy onto the un-interpolated DATA
    data = temporary( newdata )
    ; Copy coordinates
    lat = masklat
    nlat = nmasklat
    lon = masklon
    nlon = nmasklon

  endif

endif

;***********************************************************************
; Mask the Data Array

; If a mask has been given
if keyword_set( mask ) and keyword_set( data ) then begin

  ; Mask the data array
  data = data * mask

endif

;***********************************************************************
; Calculate the Anomaly from the Climatological Base Period

; Because of masking issues (changing spatiotemporal masks), this needs to be 
; done regardless of whether it was requested.  The mean will be added back 
; onto the data at the end if ANOMALYOPT was not set.

if keyword_set( data ) then begin
  ; Find the base period
  idtime = where( ( time - floor( time[0] ) ge baseperiod[0] ) $
      and ( floor( time ) - floor( time[0] ) le baseperiod[1] ) )
  ; Initialise the array of mean values
  meandata = data[*,*,idtime]
  ; Determine the number of good values at each grid cell over the base period
  temp = finite( meandata )
  if dimension ( meandata ) gt 2 then temp = total( temp, 3 )
  id = where( temp lt nbasegood, nid )
  if nid ne 0 then temp[id] = nan
  ; Take the average
  if dimension ( meandata ) gt 2 then meandata = total( meandata, 3, nan=1 )
  meandata = meandata / temp

  ; Substract the estimated mean from the data
  for i = 0, ntime - 1 do data[*,*,i] = data[*,*,i] - meandata
endif

;***********************************************************************
; Calculate the Seasonal Values (if requested)

; If this has been requested
if n_elements( season ) ne 0 then begin
  ; The number of months needed for a seasonal value to be calculated
  if keyword_set( fracseasgood ) then begin
    nseasgood = round( seaslen * fracseasgood )
  endif
  ; Calculate the seasonal values
  if keyword_set( data ) then begin
    data = months_to_seasons( data, season, seaslen, $
        deseasonalise=deseasonaliseopt, ngood=nseasgood, $
        baseperiod=baseperiod, nbasegood=nbasegood )
  endif
  time = months_to_seasons( time, season, seaslen )
endif

; Note the number of time steps
ntime = n_elements( time )

;***********************************************************************
; Calculate an Area Average (if requested)

; If a minimum area fraction has been requested
if keyword_set( fracareagood ) then begin
  ; Create an array of spatial weights
  weight = fltarr( nlon, nlat )
  for i = 0, nlat - 1 do weight[*,i] = cos( degrad * lat[i] )
  if keyword_set( coverage ) then weight = weight * coverage
  weight = weight / total( weight )
  ; If we need to determine the fraction of the total area covered by the mask
  if fracmaskarea eq -1 then begin
    fracmaskarea = total( finite( meandata ) * weight )
  endif
  ; Determine whether to retain latitudinal or longitudinal data
  if meanlatopt eq 1 then nlat = 1
  if meanlonopt eq 1 then nlon = 1
  if meanareaopt eq 1 then begin
    nlat = 1
    nlon = 1
  endif
  if meanopt eq 1 then begin
    nlon1 = nlon
    nlat1 = nlat
  endif else begin
    nlon1 = 1
    nlat1 = 1
  endelse
  if nlat eq 1 then lat = 0
  if nlon eq 1 then lon = 0
  ; Proceed only if there is data
  if keyword_set( data ) then begin
    ; Initialise an output arrays
    newdata = nan * fltarr( nlon, nlat, ntime )
    areagood = fltarr( ntime )
    temp1 = weight
    ; Iterate through time
    for i = 0, ntime - 1 do begin
      ; Copy the data we want
      if ( nlat gt 1 ) and ( nlon gt 1 ) then begin
        temp = data[*,*,i]
      endif
      ; Iterate through latitude
      for j = 0, nlat1 - 1 do begin
        ; Copy the data we want
        if ( nlat gt 1 ) and ( nlon le 1 ) then begin
          temp = data[*,j,i]
          temp1 = weight[*,j]
          temp1 = temp1 / total( temp1 )
        endif
        ; Iterate through longitude
        for k = 0, nlon1 - 1 do begin
          ; Copy the data we want
          if ( nlat le 1 ) and ( nlon gt 1 ) then begin
            temp = data[k,*,i]
            temp1 = weight[k,*]
            temp1 = temp1 / total( temp1 )
          endif else if ( nlat le 1 ) and ( nlon le 1 ) then begin
            temp = data[*,*,i]
          endif
          ; Determine the fraction of the area at this time step with good data
          areagood[i] = total( temp1 * finite( temp ) ) / total( temp1 )
          ; If we have enough good data
          if areagood[i] ge fracareagood * fracmaskarea then begin
            ; Record this data or its mean
            if meanopt ge 1 then begin
              newdata[k,j,i] = total( temp1 * temp, nan=1 ) / areagood[i]
            endif else begin
              newdata[*,*,i] = temp
            endelse
          endif
        endfor
      endfor
    endfor
    ; Clear memory
    data = newdata
    newdata = 0
    ; Do the same for MEANDATA
    fracgood = total( weight * finite( meandata ) )
    if fracgood lt fracareagood * fracmaskarea then meandata = nan
    if meanareaopt eq 1 then begin
      meandata = total( weight * meandata, nan=1 ) / fracgood
    endif
    ; Normalise AREAGOOD
    areagood = areagood / fracmaskarea
  endif
endif

;***********************************************************************
; Post-processing

; Return mean back to data of ANOMALYOPT is not selected
if not( keyword_set( anomalyopt ) ) and keyword_set( data ) then begin
  for i = 0, n_elements( data[0,0,*] ) - 1 do begin
    data[*,*,i] = data[*,*,i] + meandata
  endfor
endif
; Get rid of useless dimensions
if keyword_set( data ) then begin
  data = reform( data )
  if not( keyword_set( meanareaopt ) ) then meandata = reform( meandata )
endif

;***********************************************************************
; The End

return
END
