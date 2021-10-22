;+
; Project     : SOHO - CDS     
;                   
; Name        : HEL2ARCMIN()
;               
; Purpose     : Compute position relative to sun centre from heliographic.
;               
; Explanation : Using the input heliographic coordinates of a feature,
;               calculate the position in arcmin relative to the sun centre
;               taking account of the sun's orientation (B0).  The current
;               date is assumed unless specified.  West and  North are 
;               considered positive.  
;
;		You can rely on PB0R to calculate the solar position, 
;		B angle, etc. from SOHO or Earth's vantage points, or 
;		specify complete observer coordinates in the Heliographic 
;		Spherical coordinate system:  B0, L0, R0.  You may also
;		specify the P angle.  
;               
; Use         : IDL> print, hel2armin(ns, ew, date = dat)
;                eg  print,hel2arcmin('S34','E23')
;                or  print,hel2arcmin(-34,-23)
;		 or  xy = hel2arcmin(ns,ew,visible,date=dat)
;    
; Inputs      : ns      -  the Heliographic latitude in degrees (can be a
;                          string with N/S first character instead of sign).
;               ew      -  the Heliographic longitude in degrees (can be a
;                          string with E/W first character instead of sign).
;               
; Opt. Inputs : None
;               
; Outputs     : Function returns the (x,y) location in arcmins relative to
;               sun disk centre.
;               
; Opt. Outputs: If mentioned, the VISIBLE parameter gets a boolean
;		array indicating whether each point is in front of
;		the Sun.
;               
; Keywords    : date    -  the date to use in the calculation of B0.
;               error   -  Output keyword containing error message;
;                          a null string is returned if no error occurs
;	 	B0 	-  The B angle, in degrees
;		P       -  The P angle, in degrees
;		R0      -  The distance of the observer from the Sun, 
;			   in solar radii (use "zunits" to convert between
;			   solar radii and, say, kilometers)
;		L0      -  The longitude of the observer, relative to Earth,
;			   in degrees
;               rsun      - solar radius (input in arcsecs)
;               radius    - solar radius (output in arcmins)
;               VSTRUCT   - {b0,l0,rsun}
;		
; Category    : Utilities, coordinates.
;               
; Prev. Hist. : Yohkoh routine by Hudson/Wuelser.
;
; Written     : CDS version, C D Pike, RAL, 6 Sept 93
;               
; Modified    : To use CDS time and pb0r routines, CDP, 17-May-94
;		Version 2, William Thompson, GSFC, 14 November 1994
;			Modified .DAY to .MJD
;               Version 3, 26-Feb-96, CDP
;                       Added SOHO keyword
;               Version 4, March 11, 1996, Liyun Wang, GSFC/ARC
;                  Modified such that point of view can be changed to
;                     SOHO if the env variable SC_VIEW is set to 1
;                  Added ERROR keyword
;               Version 5, Allow input format of eq 'N3.4', 'E9.12'.  CDP, 12-Apr-96
;               Version 6, 7-Jan-99, Zarro (SMA/GSFC) 
;                - made use of SOHO keyword more logical
;		Version 7, 15-Jan-99, C. DeForest
;		 - added VISIBLE boolean array on return
;		Version 8, 22-Feb-99, C. DeForest
;		 - Added b0, l0, p, r0 coordinates if desired (thwarting
;		   PB0R if enough are specified)
;		Version 9, 8-Apr-99, C. DeForest (Stanford/GSFC)
;		 - Converted R0 calculation to use arctan instead of division
;		   (correct even for smaller distances from Sun).
;		Version 10, 9-Apr-99, Andretta (CUA/GSFC)
;                - Input keywords P, B0 etc. are now protected from any 
;                  changes made in this routine (B0 used to be converted in 
;                  radians)
;		Version 11, 14-Mar-2000, DeForest (SWRI)
;               Version 12, 22-Aug-2001, Zarro (EITI/GSFC)
;                    Added ANGLES keyword
;               Version 13, 15-Mar-2002, Zarro/Andretta. Corrected
;                bugs in use of ANGLES and SOHO keywords
;               Modified, 8-Jan-2005, Zarro (L-3Com/GSFC) - added /DEBUG
;               Modified, 2-Oct-2007, Zarro (ADNET) 
;                - added VSTRUCT
;                - removed /SOHO, /DEBUG
;                - removed unnecessary _kw suffixes
;-

FUNCTION hel2arcmin, ns, ew, visible, date=datin, error=error, $
                     b0=b0,l0=l0,p=p,r0=r0,angles=angles,rsun=rsun,$
                     vstruct=vstruct,_extra=extra,radius=radius


   IF datatype(ns) EQ 'STR' THEN BEGIN
      n = float(strmid(ns, 1, 100))
      IF STRUPCASE(STRMID(ns, 0, 1)) EQ 'S' THEN n = -n
      w = float(strmid(ew, 1, 100))
      IF STRUPCASE(STRMID(ew, 0, 1)) EQ 'E' THEN w = -w
   ENDIF ELSE BEGIN
      n = ns
      w = ew
   ENDELSE

;-- check for overriding keywords

   angles=fltarr(3)
   vstruct_in=is_struct(vstruct)
   if (1-vstruct_in) then begin
    need_b0=1-is_number(b0)
    need_rad=(1-is_number(r0)) and (1-is_number(rsun))
    if need_rad or need_b0 then angles =pb0r(datin,_extra=extra)
   endif

;-- allow keywords to override individual angles

  if is_number(p) then angles[0] = p
  if is_number(b0) then angles[1] = b0
  if is_number(r0) then angles[2] = !radeg*atan(1.0/r0)*60. ; radians-to-arcmin conversion
  if is_number(rsun) then angles[2]=rsun/60.

;-- override with VSTRUCT

  if is_struct(vstruct) then begin
   if have_tag(vstruct,'b0') then angles[1]=vstruct.b0
   if have_tag(vstruct,'rsun') then angles[2]=vstruct.rsun/60.
   if have_tag(vstruct,'l0') then l0=vstruct.l0
  endif

;
;-- convert to radians, and use L0 if present
;
   if is_number(l0) then lon = (w - l0)/!radeg else lon = w/!radeg

;
; vect is the (x,y,z) location of the point for b0 = 0, where x is in the
; direction of Texas, y is west, and z is north. vect1 is rotated by b0. 
;   
   radius = angles[2]                       
   b0_r = angles[1]/!radeg
   colat = (90. - n)/!radeg
   robs = 1.0/tan(radius/!radeg/60.)
   answer = FLTARR(2, MAX([N_ELEMENTS(colat), N_ELEMENTS(lon),N_ELEMENTS(robs)]))
   xcoord = fltarr(max([n_elements(colat),n_elements(lon),n_elements(robs)]))
;
;  calculate the result
;
   scl = SIN(colat)
   ccl = COS(colat)
   cb0 = cos(b0_r)
   sb0 = sin(b0_r)
   sl = sin(lon)
   cl = cos(lon)

   xcoord[*] = ( sb0 * ccl + cb0 * cl * scl )

   answer[0,*] = atan( scl * sl,                (robs-xcoord)) * !radeg * 60
   answer[1,*] = atan( (-scl*cl*sb0 + ccl*cb0), (robs-xcoord)) * !radeg * 60

   visible = ( xcoord gt 0 )
	
   RETURN, answer

END


