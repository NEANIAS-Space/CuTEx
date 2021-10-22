;+
; Project     :	SOHO - CDS
;
; Name        :	MAP2WCS
;
; Purpose     :	Convert a SolarSoft image map into a WCS structure.
;
; Category    :	FITS, Coordinates, WCS, Image-processing
;
; Explanation :	Converts a SolarSoft image map structure to a WCS structure.
;
; Syntax      :	MAP2WCS, MAP, WCS
;
; Inputs      :	MAP = SolarSoft image map structure.
;               
; Opt. Inputs :	None.
;
; Outputs     : WCS  = World Coordinate System structure
;
; Opt. Outputs:	None.
;
; Keywords    :	None.
;
; Calls       :	VALID_MAP, ANYTIM2UTC, ANYTIM2TAI, ADD_TAG, PB0R, TIM2CARR,
;               GET_ORBIT
;
; Common      :	None.
;
; Restrictions:	None.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 15-Apr-2005, William Thompson, GSFC
;               Version 2, 02-Mar-2006, William Thompson, GSFC
;                       Changed test for get_orbit.pro
;
; Contact     :	WTHOMPSON
;-
;
pro map2wcs, map, wcs
on_error, 2
;
;  Check the input parameters.
;
if n_params() ne 2 then message, 'Syntax: MAP2WCS, MAP, WCS'
if not valid_map(map) then message, 'Input not recognized as MAP structure'
;
;  Calculate the array size, and pixel position of image center.
;
sz = size(map.data)
n_axis = sz[0]
naxis = sz[1:n_axis]
crpix = (naxis + 1.) / 2.
;
;  Determine the coordinate type.
;
unit = strmid(strupcase(map.xunits),0,3)
if (unit eq 'ARC') or (unit eq 'DEG') or (unit eq 'MAS') or (unit eq 'RAD') $
  then coord_type = 'Helioprojective-Cartesian' else $
  coord_type = 'Heliocentric-Cartesian'
;
;  Generate a PC matrix based on the rotation angle.
;
pc = dblarr(n_axis,n_axis)
for i=0,n_axis-1 do pc[i,i] = 1
cdelt = [map.dx, map.dy]
if (cdelt[0] ne 0) and (cdelt[1] ne 0) then begin
    lambda = cdelt[1] / cdelt[0]
    cos_a = cos(map.roll_angle * !dpi / 180.d0)
    sin_a = sin(map.roll_angle * !dpi / 180.d0)
    pc[0,0] = cos_a
    pc[0,1] = -lambda * sin_a
    pc[1,0] = sin_a / lambda
    pc[1,1] = cos_a
endif
;
;  Form the basic WCS structure.
;
wcs = {coord_type: coord_type, wcsname: coord_type, variation: 'CROTA', $
       compliant: 0, projection: 'TAN', naxis: naxis, ix: 0, iy: 1, $
       crpix: crpix, crval: [map.xc, map.yc], ctype: ['SOLARX', 'SOLARY'], $
       cunit: [map.xunits, map.yunits], cdelt: cdelt, pc: pc, $
       roll_angle: map.roll_angle, simple: 1}
;
;  Form the time substructure.
;
date_obs = strtrim(map.time,2)
if date_obs ne '' then begin
    command = 'time = {observ_date: anytim2utc(map.time, /ccsds)'
    if map.dur gt 0 then begin
        date_end = anytim2utc(anytim2tai(map.time)+map.dur, /ccsds)
        command = command + ', observ_end: date_end'
    endif
    command = command + '}'
    test = execute(command)
    wcs = add_tag(wcs, time, 'TIME', /top_level)
endif
;
;  Get the information for the position substructure.
;
pos_assumed = -1
if date_obs ne '' then begin
    earth_data = pb0r(date_obs, /earth)
    hglt_earth = earth_data[1]
    hgln_earth = 0.d0           ;by definition
    dsun_earth = 6.96e8 * !radeg * 60 / earth_data[2]
    carr_earth = (tim2carr(date_obs))[0]
;
    if map.soho then begin
        message = ''
        type = ''
        test = execute('orbit = get_orbit(date_obs, type, errmsg=message)',1,1)
        if test and (message eq '') and (type ne '') then begin
            pos_assumed = 0
            gei_obs = 1000.d0 * [orbit.gci_x, orbit.gci_y, orbit.gci_z]
            gse_obs = 1000.d0 * [orbit.gse_x, orbit.gse_y, orbit.gse_z]
            gsm_obs = 1000.d0 * [orbit.gsm_x, orbit.gsm_y, orbit.gsm_z]
            hae_obs = 1000.d0 * [orbit.hec_x, orbit.hec_y, orbit.hec_z]
            dsun_obs = sqrt(total(hae_obs^2))
            solar_b0 = !radeg * orbit.hel_lat_soho
        endif
    endif
    if pos_assumed eq -1 then begin
        pos_assumed = 1
        dsun_obs = dsun_earth
        if map.soho then dsun_obs = 0.99 * dsun_obs
        solar_b0 = hglt_earth
    endif
endif
;
;  Form the position substructure.
;
command = 'position = {soho: map.soho, pos_assumed: pos_assumed'
if n_elements(dsun_obs) eq 1 then command = command + ', dsun_obs: dsun_obs'
if n_elements(solar_b0) eq 1 then command = command + ', solar_b0: solar_b0'
if n_elements(carr_earth) eq 1 then command = command + $
  ', carr_earth: carr_earth'
;
codes = ['GEI','GSE','GSM','HAE']
for i=0,n_elements(codes)-1 do begin
    name = codes[i] + '_obs'
    test = execute('nn = n_elements(' + name + ')')
    if nn gt 0 then command = command + ', ' + name + ': ' + name
endfor
;
;  Create the POSITION structure, and add it to the WCS structure.
;
command = command + '}'
test = execute(command)
wcs = add_tag(wcs, position, 'POSITION', /top_level)
;
return
end
