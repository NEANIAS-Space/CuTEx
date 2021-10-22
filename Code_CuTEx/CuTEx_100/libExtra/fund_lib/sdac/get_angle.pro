;+
; Project     : SDAC   
;                   
; Name        : GET_ANGLE
;               
; Purpose     : This function returns the angle (degrees) between two vectors
;		specified in spherical coordinates of an azimuthal angle
;		and declination, usually right ascension and declination
;               
; Category    : GEN, Coordinate Manipulations
;               
; Explanation : The vectors are computed to Cartesian coordinates, their
;		inner product taken, then the inverse cosine is used to 
;		return the angle.
;               
; Use         : Angle = GET_ANGLE( Radec1, Radec2 )
;    
; Inputs      : Radec1 - Vector 1 right ascension and declination in degrees 
;		Radec1 may be an array of vectors, dimensioned 2xN_elements
;             : Radec2 - Vector 2 right ascension and declination in degrees 
;               
; Opt. Inputs : None
;               
; Outputs     : Returns the angle in degrees.
;
; Opt. Outputs: None
;               
; Keywords    : 
;
; Calls       : SPHCART
;
; Common      : None
;               
; Restrictions: 
;               
; Side effects: None.
;               
; Prev. Hist  : This was originally written as angle.pro in 1987 by RAS.
;
; Modified    : Version 1, RAS, 26-Dec-1996
;
;-            
;==============================================================================
function get_angle, radec1, radec2

v1 = sphcart( (radec1(0,*))(*), (radec1(1,*))(*) )
v2 = fltarr(3,1) + (sphcart( radec2(0), radec2(1)))(*)
angle = !radeg * acos( v1 # v2 )

return, angle
end

