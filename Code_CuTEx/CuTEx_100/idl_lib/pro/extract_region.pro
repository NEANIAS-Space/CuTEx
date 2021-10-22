;+
; NAME:
;    EXTRACT_REGION
;
; PURPOSE:
;    This function extracts regional data from a [longitude,latitude,time] data 
;    file.
;
; CATEGORY:
;    Geographical
;
; CALLING SEQUENCE:
;    Result = extract_region( Data )
;
; INPUTS:
;    Data:  An array containing gridded data in a 
;        [longitude,latitude,something] format.  Of size N_LON_0*N_LAT_0*....
;    GIORGI, IPCC, LAT, LON, NOSHRINK, REGION
;
; KEYWORD PARAMETERS:
;    COVERAGE:  An N_LON*N_LAT*... array containing the fractional data 
;        coverage of each cell in Data.  Returns a modified array in which 
;        values have been modified to reflect the fraction of the cells with 
;        the requested region.  Thus coverage fractions will be multiplied by 1 
;        for cells entirely in the region, by between 0 and 1 for cells 
;        straddling the border of the region, and by 0 for cells outside the 
;        region (for which the corresponding elements of Data will have a NaN 
;        value).
;    GIORGI:  An obsolete input.  Use REGION keyword instead.
;    IPCC:  An obsolute input.  Use REGION keyword instead.
;    LAT:  A vector of length N_LAT_0 containing the latitude coordinates of 
;        the latitude dimension in Data.  Returns a vector of length N_LAT 
;        containing the latitude coordinates of the regional data in the output 
;        array Result.
;    LON:  A vector of length N_LON_0 containing the longitude coordinates of 
;        the longitude dimension in Data.  Returns a vector of length N_LON 
;        containing the longitude coordinates of the regional data in the 
;        output array Result.
;    NAME:  Returns a string containing the/a full name of the requested region.
;    NOSHRINK:  If set, the data is returned on the original global grid with 
;        all values outside the region(s) set to NaN.  The default is to return 
;        the data on a grid consisting of the minimum extracted box that 
;        contains all of the region(s).
;    REGION:  If a string is input then it contains the abbreviated name of a 
;        region to request, as defined in ipcc_regions.pro.  If a vector is 
;        given, or a list of names separated by commas, then data from all 
;        requested regions is included in the output.  If it is a floating 
;        point vector of length 4 then it contains the geographical boundaries 
;        of the box defining the desired region, in the form 
;          [ westernmost longitude, southernmost latitude, $
;              easternmost longitude, northernmost latitude ].
;        If a 4*N_REGION array is given then data from all N_REGION regions 
;        will be returned.
;        If this keyword (and obsolete related inputs) are not given and 
;        NOSHRINK is not set then Result will return the data on a grid 
;        consisting of the minimum extracted box that contains all of the 
;        non-NaN data.
;
; OUTPUTS:
;    Result:  Returns an array of size N_LON*N_LAT*... containing the requested 
;        regional component of Data.  If multiple box regions have been 
;        requested, then the boundary will be larger than the regions with data 
;        outside the regions replaced with NaNs.
;    COVERAGE, LAT, LON, NAME, REGION
;
; USES:
;    ipcc_regions.pro
;    var_type.pro
;
; PROCEDURE:
;    This function extracts a sub-array from the input array.
;
; EXAMPLE:
;    Create an array of global, 1x1 degree annual data.
;      data = randomn( seed, 360, 180, 100 )
;      lon = findgen( 360 ) + 0.5
;      lat = findgen( 180 ) - 90. + 0.5
;    Extract data from the region bordered between longitudes 10W and 20E and 
;    latitudes 30N and 40N.
;      result = extract_region( data, region=[-10.,30,20,40], lon=lon, lat=lat )
;    This should return:
;      RESULT as a 30*10*100 floating point array
;      LON as findgen( 30 ) - 10. + 0.5
;      LAT as findgen( 10 ) + 30. + 0.5
;
; MODIFICATION HISTORY:
;    Written by:  Daithi A. Stone (stoned@atm.ox.ac.uk), 2005-03-16
;    Modified:    DAS, 2005-09-09 (Added GIORGI and NAME keywords;  updated 
;        documentation)
;    Modified:    DAS, 2005-09-21 (Allowed negative longitudes in REGION)
;    Modified:    DAS, 2006-01-21 (Switched from using giorgi_regions.pro to 
;        ipcc_regions.pro;  Added IPCC input keyword;  Allowed definition of 
;        multiple region boxes for selecting non-rectangular regions)
;    Modified:    DAS, 2006-06-08 (Allowed inclusion of cells even if coverage 
;        is only partial, instead of requiring full coverage)
;    Modified:    DAS, 2006-07-26 (Allowed negative longitudes in LON;  added 
;        COVERAGE keyword)
;    Modified:    DAS, 2006-12-05 (Added comma-separated list capability to 
;        IPCC keyword)
;    Modified:    DAS, 2008-03-19 (Absorbed IPCC keyword input into REGION 
;        keyword input;  updated code style)
;    Modified:    DAS, 2008-04-07 (Changed spatial shrinking to the area 
;        defined by the region borders rather than the area with data within 
;        those borders)
;    Modified:    DAS, 2008-05-15 (Switched to a less memory-intensive method 
;        of copying the large data array)
;    Modified:    DAS, 2009-04-07 (Altered to accept data with N_LON=1 and/or 
;        N_LAT=1;  removed assumptions about extra dimensions;  allowed 
;        COVERAGE to be an input too)
;    Modified:    DAS, 2010-02-10 (Altered border restriction comparison 
;        estimation, ie lt to le, to hopefully handle numerical precision
;        issues better;  edited documentation formatting)
;    Modified:    DAS, 2010-02-26 (Allowed no regions to be specified)
;    Modified:    DAS, 2010-03-02 (Fixed bug with region flagging resulting 
;        from 2010-02-26 modification)
;    Modified:    DAS, 2010-03-09 (Fixed bug which removed gaps in latitude)
;-

FUNCTION EXTRACT_REGION, $
    Data, $
    COVERAGE=coverage, $
    LAT=lat, LON=lon, $
    NAME=name, $
    NOSHRINK=noshrink_opt, $
    REGION=region, $
    GIORGI=giorgi, IPCC=ipcc

;***********************************************************************
; Constants and Options

; Absolute constants
n_degree = 360
nan = !values.f_nan

; Input data array size
size_data = size( data, dimension=1 )
n_size_data = n_elements( size_data )
n_lon = max( [ n_elements( lon ), size_data[0] ] )
n_lat = max( [ n_elements( lat ), size_data[1] ] )
if n_size_data le 2 then begin
  n_data_3 = 1
endif else begin
  n_data_3 = round( product( size_data[2:n_size_data-1] ) )
endelse
; Reform input array to convenient dimensions
data = reform( data, n_lon, n_lat, n_data_3 )
if keyword_set( coverage ) then begin
  coverage = reform( coverage, n_lon, n_lat, n_data_3 )
endif

; Create a temporary data array if one is not inputed.  This is needed for 
; the algorithm to work
if not( keyword_set( data ) ) then begin
  data = fltarr( n_lon, n_lat )
  nodata_opt = 1
endif

; Copy obsolete GIORGI keyword input to REGION keyword
if keyword_set( giorgi ) then begin
  region = giorgi
  print, 'Warning:  GIORGI keyword is obsolete.  Use REGION instead.'
endif
; Copy obsolete IPCC keyword input to REGION keyword
if keyword_set( ipcc ) then begin
  region = giorgi
  print, 'Warning:  IPCC keyword is obsolete.  Use REGION instead.'
endif

;***********************************************************************
; Select Regional Boundaries if Necessary

; If a region has been specified
if keyword_set( region ) then begin

  ; If named regions have been inputed
  if var_type( region ) eq 7 then begin
    ; Load the boundaries of the requested regions
    borders = ipcc_regions( strsplit( region, ',', extract=1 ), name=name )
  ; If region borders have been inputed
  endif else begin
    borders = region
  endelse

  ; Count the number of regions
  n_region = n_elements( borders[0,*] )
  ; Remove negative longitudes
  id = where( lon lt 0, n_id )
  if n_id ne 0 then lon[id] = lon[id] + n_degree
  for i = 0, n_region - 1 do begin
    if borders[0,i] lt 0 then borders[0,i] = borders[0,i] + n_degree
    if borders[2,i] lt 0 then borders[2,i] = borders[2,i] + n_degree
  endfor

; Otherwise specify no regions
endif else begin

  n_region = 0

endelse

;***********************************************************************
; Extract Data

; Restrict to region
if n_region gt 0 then begin

  ; Initialise masking and output arrays
  mask = nan * fltarr( n_lon, n_lat )
  cov = fltarr( n_lon, n_lat )
  lon_cov = cov
  lat_cov = cov
  ; Initialise array flagging grid cells within a region
  if not( keyword_set( noshrink_opt ) ) then inregion = intarr( n_lon, n_lat )

  ; Iterate through regions
  for i = 0, n_region - 1 do begin
    ; Determine the longitude range.
    ; If we have N_LON=1 then assume zonal mean of global data
    id_s = sort( lon )
    lon_s = lon[id_s]
    if n_lon eq 1 then begin
      id_lon_1 = 0
      n_id_lon = 1
    endif else begin
      if borders[0,i] ge borders[2,i] then begin
        temp_1 = [ ( lon_s[1:n_lon-1] + lon_s[0:n_lon-2] ) / 2., $
            lon_s[n_lon-1] + ( lon_s[n_lon-1] - lon_s[n_lon-2] ) / 2. ]
        temp_2 = [ lon_s[0] - ( lon_s[1] - lon_s[0] ) / 2., $
            ( lon_s[1:n_lon-1] + lon_s[0:n_lon-2] ) / 2. ]
        ;id_lon_1 = where( ( temp_1 gt borders[0,i] ) $
        ;    or ( temp_2 lt borders[2,i] ), n_id_lon )
        id_lon_1 = where( ( temp_1 ge borders[0,i] ) $
            or ( temp_2 le borders[2,i] ), n_id_lon )
      endif else begin
        temp_1 = [ ( lon_s[1:n_lon-1] + lon_s[0:n_lon-2] ) / 2., $
            lon_s[n_lon-1] + ( lon_s[n_lon-1] - lon_s[n_lon-2] ) / 2. ]
        temp_2 = [ lon_s[0] - ( lon_s[1] - lon_s[0] ) / 2., $
            ( lon_s[1:n_lon-1] + lon_s[0:n_lon-2] ) / 2. ]
        ;id_lon_1 = where( ( temp_1 gt borders[0,i] ) $
        ;    and ( temp_2 lt borders[2,i] ), n_id_lon  )
        id_lon_1 = where( ( temp_1 ge borders[0,i] ) $
            and ( temp_2 le borders[2,i] ), n_id_lon  )
      endelse
    endelse
    id_lon = id_s[id_lon_1]
    ; Determine longitude coverage
    lon_cov[*,*] = 0.
    lon_cov[id_lon,*] = 1.
    if n_lon gt 1 then begin
      id = where( temp_1 gt borders[0,i] )
      id_1 = where( temp_1[id] eq min( temp_1[id] ) )
      id = id[id_1]
      if temp_2[id] lt temp_1[id] then begin
        temp_diff_1 = temp_1[id] - temp_2[id]
      endif else begin
        temp_diff_1 = temp_1[id] + n_degree - temp_2[id]
      endelse
      if borders[0,i] lt temp_1[id] then begin
        temp_diff_2 = temp_1[id] - borders[0,i]
      endif else begin
        temp_diff_2 = temp_1[id] + n_degree - borders[0,i]
      endelse
      lon_cov[id_s[id],*] = temp_diff_2 / temp_diff_1
      id = where( temp_2 lt borders[2,i] )
      id_1 = where( temp_2[id] eq max( temp_2[id] ) )
      id = id[id_1]
      if temp_2[id] lt temp_1[id] then begin
        temp_diff_1 = temp_1[id] - temp_2[id]
      endif else begin
        temp_diff_1 = temp_1[id] + n_degree - temp_2[id]
      endelse
      if temp_2[id] lt borders[2,i] then begin
        temp_diff_2 = borders[2,i] - temp_2[id]
      endif else begin
        temp_diff_2 = borders[2,i] + n_degree - temp_2[id]
      endelse
      lon_cov[id_s[id],*] = temp_diff_2 / temp_diff_1
    endif
    ; Determine the latitude range.
    ; If N_LAT=1 then assume meridional average of global data
    if n_lat eq 1 then begin
      id_lat = 0
      n_id_lat = 1
    endif else begin
      temp_1 = [ ( lat[1:n_lat-1] + lat[0:n_lat-2] ) / 2., $
          lat[n_lat-1] + ( lat[n_lat-1] - lat[n_lat-2] ) / 2. ]
      temp_2 = [ lat[0] - ( lat[1] - lat[0] ) / 2., $
          ( lat[1:n_lat-1] + lat[0:n_lat-2] ) / 2. ]
      ;id_lat = where( ( temp_1 gt borders[1,i] ) $
      ;    and ( temp_2 lt borders[3,i] ), n_id_lat )
      id_lat = where( ( temp_1 ge borders[1,i] ) $
          and ( temp_2 le borders[3,i] ), n_id_lat )
    endelse
    ; Determine latitude coverage
    lat_cov[*,*] = 0.
    if n_lat eq 1 then begin
      lat_cov[*,id_lat] = 1.
    endif else begin
      if n_id_lat gt 2 then lat_cov[*,id_lat[1:n_id_lat-2]] = 1.
      lat_cov[*,id_lat[0]] = ( temp_1[id_lat[0]] - borders[1,i] ) $
          / ( temp_1[id_lat[0]] - temp_2[id_lat[0]] )
      lat_cov[*,id_lat[n_id_lat-1]] $
          = ( borders[3,i] - temp_2[id_lat[n_id_lat-1]] ) $
          / ( temp_1[id_lat[n_id_lat-1]] - temp_2[id_lat[n_id_lat-1]] )
    endelse
    ; Mark this data for copying
    mask[id_lon,id_lat,0] = 1
    cov = cov + lat_cov * lon_cov
    ; Flag this area as being within the region
    inregion[id_lon,id_lat,0] = 1
  endfor
  ; Retain the data in the region
  for i = 0, n_data_3 - 1 do data[*,*,i] = data[*,*,i] * mask
  if keyword_set( coverage ) then begin
    for i = 0, n_data_3 - 1 do coverage[*,*,i] = coverage[*,*,i] * cov
  endif else begin
    coverage = cov
  endelse

  ; Clear memory
  lon_cov = 0
  lat_cov = 0
  mask = 0

; Otherwise flag everything as within the region
endif else begin

  if n_data_3 eq 1 then begin
    inregion = finite( data )
  endif else begin
    inregion = total( finite( data ), 3, integer=1 )
  endelse
  id = where( inregion ne 0, n_id )
  if n_id gt 0 then inregion[id] = 1

endelse

; Print a warning if there is no data
if max( finite( data ) ) eq 0 then begin
  print, 'Warning:  ' + region + ' has no data'
endif

; Extract data from within requested boundaries
if not( keyword_set( noshrink_opt ) ) $
    and ( max( finite( data ) ) eq 1 ) then begin
  ; Determine which longitudes have data
  temp = intarr( n_lon )
  for i = 0, n_lon - 1 do temp[i] = max( inregion[i,*] )
  ; If there are gaps
  if min( temp ) eq 0 then begin
    ; Determine the beginning of gaps
    id_0 = where( ( temp[[n_lon-1,indgen(n_lon-1)]] eq 1 ) and ( temp eq 0 ), $
        n_id_0 )
    ; Determine the length of these gaps
    id_1 = 0 * id_0
    for i = 0, n_id_0 - 1 do begin
      id_1[i] = id_0[i] + min( where( temp[id_0[i]:n_lon-1] eq 1 ) ) - 1
    endfor
    if id_1[n_id_0-1] lt id_0[n_id_0-1] then begin
      id_1[n_id_0-1] = min( where( temp eq 1 ) ) - 1 + n_lon
    endif
    ; Find the longest gap
    id = ( where( id_1 - id_0 eq max( id_1 - id_0 ) ) )[0]
    id_0 = id_0[id]
    id_1 = id_1[id]
    ; Find the longitudes not in this gap
    id_lon = indgen( n_lon )
    ; If the gap contains the Greenwich Meridian
    if id_1 ge n_lon then begin
      id_lon = where( ( id_lon gt id_1 - n_lon ) and ( id_lon lt id_0 ) )
    ; If the gap does not contain the Greenwich Meridian
    endif else begin
      id = where( id_lon gt id_1, n_id_lon )
      if n_id_lon eq 0 then begin
        id_lon = where( id_lon lt id_0 )
      endif else begin
        id = where( id_lon lt id_0, n_id )
        id_lon = where( id_lon gt id_1 )
        if n_id ne 0 then id_lon = [ id_lon, id ]
      endelse
    endelse
  ; If there are no gaps then take everything
  endif else begin
    id_lon = indgen( n_lon )
  endelse
  ; Identify the latitudes to extract
  temp = intarr( n_lat )
  for i = 0, n_lat - 1 do temp[i] = max( inregion[*,i] )
  id_lat = where( temp eq 1, n_id_lat )
  ; Extract the box from the data
  data = data[id_lon,id_lat[0]:id_lat[n_id_lat-1],*,*]
  if keyword_set( coverage ) then begin
    coverage = coverage[id_lon,id_lat[0]:id_lat[n_id_lat-1],*]
  endif
  lon = lon[id_lon]
  lat = lat[id_lat[0]:id_lat[n_id_lat-1]]
endif

; Clear memory
inregion = 0

; Erase variables if no data was inputed
if keyword_set( nodata_opt ) then begin
  data = 0
  coverage = 0
endif

; Reform output data array back to original number of dimensions
if n_size_data ge 3 then begin
  n_lon = n_elements( lon )
  n_lat = n_elements( lat )
  data = reform( data, [ n_lon, n_lat, size_data[2:n_size_data-1] ] )
  if n_elements( coverage ) ne n_lon * n_lat then begin
    coverage = reform( coverage, [ n_lon, n_lat, size_data[2:n_size_data-1] ] )
  endif
endif

;***********************************************************************
; The End

return, data
END
