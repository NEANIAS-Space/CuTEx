;+
; Project     :	STEREO
;
; Name        :	WCS_PROJ_CAR
;
; Purpose     :	Convert intermediate coordinates in CAR projection.
;
; Category    :	FITS, Coordinates, WCS
;
; Explanation :	This routine is called from WCS_GET_COORD to apply the plate
;               carree (CAR) projection to intermediate relative coordinates.
;
; Syntax      :	WCS_PROJ_CAR, WCS, COORD
;
; Examples    :	See WCS_GET_COORD
;
; Inputs      :	WCS = A World Coordinate System structure, from FITSHEAD2WCS.
;               COORD = The intermediate coordinates, relative to the reference
;                       pixel (i.e. CRVAL hasn't been applied yet).
;
; Opt. Inputs :	None.
;
; Outputs     :	The projected coordinates are returned in the COORD array.
;
; Opt. Outputs:	None.
;
; Keywords    :	QUICK      = If set, do a quick approximate calculation rather
;                            than a full-blown spherical projection.
;
;                            The projection is equivalent to the
;                            approximate calculation when the reference value
;                            is on the equator, unless overridden by projection
;                            values in the WCS structure.  Setting the
;                            reference pixel at the equator satisfies the usual
;                            expectations.  However, the speed-up realized by
;                            the /QUICK keyword is relatively minor.
;
;               MISSING    = Value to fill missing values with.  If not passed,
;                            then missing values are filled with IEEE
;                            Not-A-Number (NaN) values.  Ignored if /QUICK is
;                            used.
;
; Calls       :	TAG_EXIST, NTRIM
;
; Common      :	None.
;
; Restrictions:	Because this routine is intended to be called only from
;               WCS_GET_COORD, no error checking is performed.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 27-Apr-2005, William Thompson, GSFC
;               Version 2, 29-Apr-2005, William Thompson, GSFC
;                       Added keyword MISSING
;
; Contact     :	WTHOMPSON
;-
;
pro wcs_proj_car, wcs, coord, quick=quick, missing=k_missing
on_error, 2
halfpi = !dpi / 2.d0
;
;  Get the MISSING value.
;
if n_elements(k_missing) eq 1 then missing=k_missing else missing=!values.d_nan
;
;  Calculate the conversion from coordinate units into radians.
;
cx = !dpi / 180.d0
case wcs.cunit[wcs.ix] of
    'arcmin': cx = cx / 60.d0
    'arcsec': cx = cx / 3600.d0
    'mas':    cx = cx / 3600.d3
    'rad':    cx = 1.d0
    else:     cx = cx
endcase
;
cy = !dpi / 180.d0
case wcs.cunit[wcs.iy] of
    'arcmin': cy = cy / 60.d0
    'arcsec': cy = cy / 3600.d0
    'mas':    cy = cy / 3600.d3
    'rad':    cy = 1.d0
    else:     cy = cy
endcase
;
;  If the QUICK option was selected, then don't do the full spherical
;  projection.
;
if keyword_set(quick) then begin
    coord[wcs.ix,*] = coord[wcs.ix,*] + wcs.crval[wcs.ix]
    coord[wcs.iy,*] = coord[wcs.iy,*] + wcs.crval[wcs.iy]
    return
endif
;
;  Get the native longitude (phi0) and latitude (theta0) of the fiducial
;  point.  Look for the PV values from the FITS header.  If not found, use the
;  default values (0,0).
;
phi0 = 0.d0
if tag_exist(wcs, 'proj_names', /top_level) then begin
    name = 'PV' + ntrim(wcs.ix+1) + '_1'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then phi0 = wcs.proj_values[w[0]]
endif
;
theta0 = 0.d0
if tag_exist(wcs, 'proj_names', /top_level) then begin
    name = 'PV' + ntrim(wcs.ix+1) + '_2'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then theta0 = wcs.proj_values[w[0]]
endif
;
;  Convert phi0 and theta0 to radians
;
phi0_deg = phi0
phi0   = (!dpi / 180.d0) * phi0
theta0 = (!dpi / 180.d0) * theta0
;
;  Get the celestial longitude and latitude of the fiducial point.
;
alpha0 = wcs.crval[wcs.ix] * cx
delta0 = wcs.crval[wcs.iy] * cy
;
;  Get the native longitude (phip) of the celestial pole.  Look for the LONPOLE
;  (or PVi_3) keyword.  If not found, use the default value.  Convert to
;  radians.
;
if delta0 ge theta0 then phip = phi0_deg else phip = 180.d0 + phi0_deg
if tag_exist(wcs, 'proj_names', /top_level) then begin
    w = where(wcs.proj_names eq 'LONPOLE', count)
    if count gt 0 then phip = wcs.proj_values[w[0]]
    name = 'PV' + ntrim(wcs.ix+1) + '_3'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then phip = wcs.proj_values[w[0]]
endif
phip   = (!dpi / 180.d0) * phip
;
;  Get the native latitude (thetap) of the celestial pole.  Look for the
;  LATPOLE (or PVi_3) keyword.  If not found, use the default value.  Convert
;  to radians.
;
thetap = 90
if tag_exist(wcs, 'proj_names', /top_level) then begin
    w = where(wcs.proj_names eq 'LATPOLE', count)
    if count gt 0 then thetap = wcs.proj_values[w[0]]
    name = 'PV' + ntrim(wcs.ix+1) + '_4'
    w = where(wcs.proj_names eq name, count)
    if count gt 0 then thetap = wcs.proj_values[w[0]]
endif
thetap   = (!dpi / 180.d0) * thetap
;
;  Calculate the native spherical coordinates.
;
phi   = cx*coord[wcs.ix,*]
theta = cy*coord[wcs.iy,*]
w_missing = where(abs(theta) gt halfpi, n_missing)
if n_missing gt 0 then theta = -halfpi > theta < halfpi
;
;  Determine the pole position.
;
if (theta0 eq 0) and (delta0 eq 0) and (abs(phip - phi0) eq halfpi) then begin
    deltap = thetap
end else begin
    deltap0 = atan(sin(theta0), cos(theta0)*cos(phip-phi0))
    test = sin(delta0)/sqrt(1-cos(theta0)^2*sin(phip-phi0)^2)
    if abs(test) gt 1 then message, 'Incompatible projection parameters'
    deltap1 = acos(test)
    deltap  = deltap0 + deltap1
    deltap2 = deltap0 - deltap1
    if abs(deltap)  gt halfpi then deltap  = deltap2
    if abs(deltap2) gt halfpi then deltap2 = deltap
    if abs(deltap-thetap) gt abs(deltap2-thetap) then deltap = deltap2
endelse
;
if deltap eq halfpi then begin
    alphap = alpha0 + phip - phi0 - !dpi
end else if deltap eq -halfpi then begin
    alphap = alpha0 - phip + phi0
end else if abs(delta0) eq halfpi then begin
    alphap = alpha0
end else begin
    das = sin(phip-phi0)*cos(theta0) / cos(delta0)
    dac = (sin(theta0)-sin(deltap)*sin(delta0)) / (cos(deltap)*cos(delta0))
    if (das eq 0) and (dac eq 0) then alphap = alpha0 - !dpi else $
      alphap = alpha0 - atan(das,dac)
endelse
;
;  Calculate the celestial spherical coordinates.
;
if deltap ge halfpi then begin
    alpha = alphap + phi - phip - !dpi
    delta = theta
end else if deltap le -halfpi then begin
    alpha = alphap - phi + phip
    delta = -theta
end else begin
    dphi = phi - phip
    cos_dphi = cos(dphi)
    sin_theta = sin(theta)
    cos_theta = cos(theta)
    alpha = alphap + atan(-cos_theta*sin(dphi), $
        sin_theta*cos(deltap)-cos_theta*sin(deltap)*cos_dphi)
    delta = asin(sin_theta*sin(deltap) + $
                 cos_theta*cos(deltap)*cos_dphi)
endelse
;
;  Make sure that the longitude runs from -180 to +180.
;
w = where(alpha lt -!dpi, count)
if count gt 0 then alpha[w] = alpha[w] + 2.d0 * !dpi
w = where(alpha gt  !dpi, count)
if count gt 0 then alpha[w] = alpha[w] - 2.d0 * !dpi
;
;  Convert back into the original units.
;
coord[wcs.ix,*] = alpha / cx
coord[wcs.iy,*] = delta / cy
;
;  Flag any missing values.
;
if n_missing gt 0 then begin
    coord[wcs.ix, w_missing] = missing
    coord[wcs.iy, w_missing] = missing
endif
;
end
