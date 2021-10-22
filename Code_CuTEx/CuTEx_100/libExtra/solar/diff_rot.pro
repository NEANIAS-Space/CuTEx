;+
; PROJECT:
;       SOHO - CDS
;
; NAME:	
;       DIFF_ROT()
;
; PURPOSE:
;     Computes the differential rotation of the sun
;       
; CALLING SEQUENCE: 
;       Result = DIFF_ROT(ddays,latitude)
;
; INPUTS:
;       DDAYS    --  number of days to rotate
;       LATITUDE --  latitude in DEGREES
;       
; OUTPUTS:
;       Result -- Change in longitude over ddays days in DEGREES
;
; KEYWORD PARAMETERS: 
;       ALLEN    -- use values from Allen, Astrophysical Quantities (DEFAULT)
;       HOWARD   -- use values for small magnetic features from Howard et al.
;       SIDEREAL -- use sidereal rotation rate (DEFAULT)
;       SYNODIC  -- use synodic rotation rate
;       RIGID    -- rotate as rigid body
;       RATE     -- user specified rotation rate in degrees per day
;                   (only used if /RIGID)
;
; PREVIOUS HISTORY:
;       Written T. Metcalf  June 1992
;
; MODIFICATION HISTORY:
;       Version 1, Liyun Wang, GSFC/ARC, November 17, 1994
;          Incorporated into the CDS library
;       Version 2, Zarro, GSFC, 1 July 1997 - made Howard the default
;       Version 3, Zarro, GSFC, 19 Sept 1997 - corrected Howard coeff's
;       Version 4, Zarro (EER/GSFC) 22 Feb 2003 - added /RIGID
;       Version 5, Zarro (EER/GSFC) 29 Mar 2003 - added /RATE
;
;-

FUNCTION DIFF_ROT, ddays, latitude, howard=howard, allen=allen,debug=debug,$
                   synodic=synodic, sidereal=sidereal,rigid=rigid,rate=rate

;-- check if rotating as rigid body

   if keyword_set(rigid) then begin
    sz=size(latitude) 
    if n_elements(sz) lt 4 then sin2l=0. else $
     sin2l=make_array(size=size(latitude))
    sin4l=sin2l
    if is_number(rate) then begin
     if keyword_set(debug) then message,'using rigid rate of '+trim(rate),/cont
     if rate gt 0 then return,ddays*rate+sin2l
    endif else if keyword_set(debug) then message,'using rigid body rotation',/cont
   endif else begin
    sin2l = (SIN(FLOAT(latitude*!dtor)))^2
    sin4l = sin2l*sin2l
   endelse

   IF KEYWORD_SET(allen) THEN BEGIN

;  Allen, Astrophysical Quantities

    rotation = ddays*(14.44-3.0*sin2l)
   ENDIF ELSE BEGIN

;  Small magnetic features 
;  (Howard, Harvey, and Forgach, Solar Physics, 130, 295, 1990)

    rotation = 1.e-6*ddays*(2.894-0.428*sin2l-0.37*sin4l)*24.*3600./!dtor
   ENDELSE

   IF KEYWORD_SET(synodic) THEN BEGIN
    rotation = rotation-0.9856*ddays
   ENDIF 

   RETURN, rotation
END

