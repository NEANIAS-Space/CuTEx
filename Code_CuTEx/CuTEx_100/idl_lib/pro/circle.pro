;+
; NAME:
;	CIRCLE
;
; PURPOSE:
;	This function returns the x- and y- coordinates of a circle.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	Result = CIRCLE( Xcentre, Ycentre, $
;	                 Radius, $
;	                 Npoints )
;
; INPUTS:
;	Xcentre:  The x-coordinate of the circle's centre, of type
;	          floating point.
;	Ycentre:  The y-coordinate of the circle's centre, of type
;	          floating point.
;	Radius:  The radius of the circle, of type floating point.
;
; OPTIONAL INPUTS:
;	Npoints:  The number of points to be defined on the circle,
;	          of type integer.  The default is 100.
;
; KEYWORD PARAMETERS:
;	OVERLAP:  If set, the "ends" of the circle overlap.  This produces 
;		better results if plotting the circles with thick lines.
;
; OUTPUTS:
;	Result:  Returns the x- and y- coordinates of the circle in
;	         a 2*Npoints array.
;
; PROCEDURE:
;	This function calculates Npoints evenly-spaced points along a
;	circle.
;
; EXAMPLE:
;	Define a circle of unit radius centred on the origin.
;	  result = circle( 0, 0, 5 )
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 1997-12-10.
;	Modified by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-07-05 
;			(added Npoints optional input).
;	Modified by:	DAS, 2004-11-26 (added OVERLAP keyword)
;-

;***********************************************************************

FUNCTION CIRCLE, $
	Xcenter, Ycenter, $
	Radius, $
	Npoints, $
	OVERLAP=overlap

;***********************************************************************
; Constants

; Number of points
if not( keyword_set( npoints ) ) then npoints = 100

;***********************************************************************
; Define the Points around a Circle

; If the OVERLAP keyword is set
if keyword_set( overlap ) then begin
  ; Create a vector of angles around the circle with ends that overlap
  points = ( 2 * !pi / ( npoints - 2 ) ) * findgen( npoints )
endif else begin
  ; Create a vector of angles around the circle with ends that meet exactly
  points = ( 2 * !pi / ( npoints - 1 ) ) * findgen( npoints )
endelse
; Calculate the positions of the circle description
x = xcenter + radius * cos( points )
y = ycenter + radius * sin( points )
result = transpose( [ [x], [y] ] )

;***********************************************************************
; The End

return, result
END
