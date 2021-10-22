;+
; NAME:
;	GEO_MID
;
; PURPOSE:
;	This function returns the coordinates of the geographical point
;	midway between the two input points.
;
; CATEGORY:
;	Geographical
;
; CALLING SEQUENCE:
;	Result = GEO_MID( Longitude1, Latitude1, $
;	                  Longitude2, Latitude2 )
;
; INPUTS:
;	Longitude1:  The scalar longitude, of type floating point, of
;		the first point.  North is positive.
;	Latitude1:  The scalar latitude, of type floating point, of the
;		first point.  East is positive.
;	Longitude2:  The scalar longitude, of type floating point, of
;		the second point(s).  North is positive.
;	Latitude2:  The scalar latitude, of type floating point, of
;		the second point(s).  East is positive.
;
; OUTPUTS:
;	Result:  Returns a vector of [ longitude, latitude ] coordinates
;		of the point midway between Points 1 and 2.
;
; USES:
;	CONSTANTS.pro
;
; PROCEDURE:
;	The function uses double precision floating point arithmetic
;	to calculate values.
;
; EXAMPLE:
;	Calculate the midpoint between Ottawa, ON (45.25N, 75.43W) and
;	Victoria, BC (48.25N, 123.22W).
;	  results = geo_mid( -75.43, 45.25, -123.22, 48.25 )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@uvic.ca), 2000-07-24.
;	Modified:	DAS, 2002-08-12 (complies with CONSTANTS.pro revision)
;-

;***********************************************************************

FUNCTION GEO_MID, $
	Longitude1, Latitude1, $
	Longitude2, Latitude2

;***********************************************************************
;Define Constants

;Earth radius, degrees in a circle, degrees to radians conversion
;coefficient
constants, r_earth=rearth, ndegree=ndegree, degrad=degrad
rearth = rearth / 1000.

;Check for opposing points
oppose = 0
if latitude1 eq -latitude2 then begin
  if longitude1 eq longitude2+ndegree/2 then oppose = 1
  if longitude1 eq longitude2-ndegree/2 then oppose = 1
endif

;***********************************************************************
;Calculations

;Non-opposing points
if oppose eq 0 then begin

  ;Convert to Cartesian coordinates
  x1 = cos( longitude1 * degrad ) * cos( latitude1 * degrad )
  y1 = sin( longitude1 * degrad ) * cos( latitude1 * degrad )
  z1 = sin( latitude1 * degrad )
  x2 = cos( longitude2 * degrad ) * cos( latitude2 * degrad )
  y2 = sin( longitude2 * degrad ) * cos( latitude2 * degrad )
  z2 = sin( latitude2 * degrad )

  ;Calculate Cartesian midpoint
  xmid = ( x2 + x1 ) / 2.
  ymid = ( y2 + y1 ) / 2.
  zmid = ( z2 + z1 ) / 2.

  ;Convert to spherical midpoint
  lonmid = atan( ymid / xmid ) / degrad
  if xmid lt 0 then lonmid = lonmid - ndegree / 2.
  if lonmid lt 0 then lonmid = lonmid + ndegree
  latmid = asin( zmid / sqrt( xmid^2 + ymid^2 + zmid^2 ) ) / degrad

endif

;Opposing points
;Returns the Eastern Hemisphere equatorial intercept
if oppose eq 1 then begin
  latmid = 0.
  lonmid = ( longitude1 + longitude2 ) / 2.
  if lonmid lt 0 then lonmid = lonmid + ndegree
  if lonmid gt ndegree/2 then lonmid = lonmid - ndegree / 2.
endif

;Output
pos = float( [lonmid,latmid] )

;***********************************************************************
;The End

return, pos
END
