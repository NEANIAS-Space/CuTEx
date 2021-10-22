;+
; Project     :	SOHO - CDS
;
; Name        :	WCS2MAP
;
; Purpose     :	Convert a WCS structure into a SolarSoft image map
;
; Category    :	FITS, Coordinates, WCS, Image-processing
;
; Explanation :	Converts a World Coordinate System structure, plus associated
;               data array, into a SolarSoft image map structure.
;
; Syntax      :	WCS2MAP, DATA, WCS, MAP, ID=ID
;
; Examples    :	WCS2MAP, DATA, WCS, MAP, ID="EIT 195"
;
; Inputs      :	DATA = Data array associated with the WCS structure
;               WCS  = World Coordinate System structure from FITSHEAD2WCS
;
; Opt. Inputs :	None.
;
; Outputs     :	MAP = SolarSoft image map structure.
;
; Opt. Outputs:	None.
;
; Keywords    :	ID = Character string describing the contents of the data.
;
; Calls       :	VALID_WCS, TAG_EXIST, ANYTIM2TAI
;
; Common      :	None.
;
; Restrictions:	The WCS must be marked as simple, i.e. wcs.simple=1
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 15-Apr-2005, William Thompson, GSFC
;
; Contact     :	WTHOMPSON
;-
;
pro wcs2map, data, wcs, map, id=id
on_error, 2
;
;  Check the input parameters.
;
if n_params() ne 3 then message, 'Syntax: WCS2MAP, DATA, WCS, MAP'
if not valid_wcs(wcs) then message, 'Input not recognized as WCS structure'
if n_elements(data) eq 0 then message, 'DATA array is undefined' 
;
;  Make sure that the data array matches the WCS structure.  Trailing
;  dimensions of 1 are allowed in either the DATA array or in the WCS
;  structure.
;
sz = size(data)
nn = n_elements(wcs.naxis) > sz[0]
dim1 = replicate(1L, nn)
if sz[0] gt 0 then dim1[0] = sz[1:sz[0]]
dim2 = replicate(1L, nn)
dim2[0] = wcs.naxis
w = where(dim1 ne dim2, count)
if count gt 0 then message, 'DATA array does not match WCS structure'
;
;  Make sure that the WCS is simple.
;
if not tag_exist(wcs, 'SIMPLE') then message, 'WCS not marked as simple'
if not wcs.simple then message, 'WCS not marked as simple'
;
;  Get the center pixel location, and its coordinates.
;
icen = (wcs.naxis - 1.)/ 2.
coord = wcs_get_coord(wcs, icen)
;
;  Get the ID string.
;
if not is_string(id) then id = ' '
;
;  Get the observation time and duration.
;
time = ''
dur = 0.
if tag_exist(wcs,'TIME') then begin
    if tag_exist(wcs.time, 'OBSERV_DATE') then begin
        time = anytim2utc(wcs.time.observ_date, /vms)
        if tag_exist(wcs.time, 'OBSERV_END') then dur = $
          anytim2tai(wcs.time.observ_end) - $
          anytim2tai(wcs.time.observ_date)
    endif
    if (dur eq 0) and tag_exist(wcs.time, 'exptime') then $
      dur = wcs.time.exptime
endif
;
;  Get the SOHO flag value.
;
soho = 0
if tag_exist(wcs, 'POSITION') then soho = wcs.position.soho
;
;  Form the MAP structure.
;
ix = wcs.ix
iy = wcs.iy
map = {data: data,                      $
       xc: coord[ix],                   $
       yc: coord[iy],                   $
       dx: wcs.cdelt[ix],               $
       dy: wcs.cdelt[iy],               $
       time: time,                      $
       id: id,                          $
       roll_angle: wcs.roll_angle,      $
       roll_center: coord[[ix,iy]],     $
       dur: dur,                        $
       xunits: wcs.cunit[ix],           $
       yunits: wcs.cunit[iy],           $
       soho: soho}
;
return
end
