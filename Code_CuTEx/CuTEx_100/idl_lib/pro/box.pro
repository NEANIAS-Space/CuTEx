;+
; NAME: 
;	BOX
;
; PURPOSE:
;	This procedure draws a rectangular box on the screen.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	BOX, X0, Y0, W, H
;
; INPUTS:
;	X0:  The x-coordinate of the left edge.
;	Y0:  The y-coordinate of the bottom edge.
;	W:  The horizontal width of the box.
;	H:  The vertical height of the box.
;
; KEYWORD PARAMETERS:
;	COLOUR:  The optional color index for drawing the box.
;	DATA:  If set then the box is plotted following data coordinates.
;	DEVICE:  If set then the box is plotted following device coordinates 
;		(default).
;	NORMAL:  If set then the box is plotted following normal coordinates.
;	THICK:  The optional line thickness for drawing the box.
;
; EXAMPLE:
;	plot, [0,1], nodata=1
;	box, 0.2, 0.3, 0.4, 0.4, data=1
;
; MODIFICATION HISTORY:
;	Written by:	Edward C. Wiebe, 2000-07-14.
;	Modified:	Daithi Stone (stoned@csag.uct.ac.za), 2009-12-08 (added 
;			documentation)
;-

pro Box,x0,y0,w,h                         $
       , DATA = data                      $
       , NORMAL = normal                  $
       , DEVICE = device                  $
       , COLOUR = colour                  $
       , THICK = thick

;  This routine draws a box on the screen



   If (Keyword_Set(data)) then data = 1 else data = 0
   If (Keyword_Set(device)) then device = 1 else device = 0
   If (Keyword_Set(normal)) then normal = 1 else normal = 0
 
   If (not Keyword_set(colour)) then colour = !P.COLOR

   If ((not device) and (not data) and (not normal)) then device = 1
 
   style=0
   Plots,[x0,x0+w,x0+w,x0,x0],[y0,y0,y0+h,y0+h,y0], $
         LINESTYLE=style, $
         NORMAL = normal, DATA = data, DEVICE = device, $
         COLOR = colour, THICK = thick

   Return
end
