;+
; NAME:
;	ARROWS
;
; PURPOSE:
;	This function plots arrows onto the current window.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	ARROWS, X, Y, Length, Angle
;
; INPUTS:
;	X:  The x-coordinate of the tail of the arrow, of type floating point.
;		This can be a vector.
;	Y:  The y-coordinate of the tail of the arrow, of type floating point.
;		This can be a vector.
;	Length:  The length of the arrow, of type floating point.  This can be
;		a vector.
;	Angle:  The angle of the arrow direction in degrees from the positive
;		x-axis, of type floating point.  This input is ignored if
;		the HEADX and HEADY keyword parameters are inputted.  This can
;		be a vector.
;
; KEYWORD PARAMETERS:
;	COLOR:  An index value for the arrow color.  This can be a vector.
;	HEADANGLE:  The angle of the head fins in degrees, of type floating
;		point.  The default is 45 degrees.
;	HEADLENGTH:  The length of the head fins as a fraction of arrow length,
;		of type floating point.  The default is 0.25.
;	HEADX:  The x-coordinate of the tail of the arrow, of type floating
;		point.  Use of HEADX and HEADY overrides the Angle input.
;		This can be a vector.
;	HEADY:  The y-coordinate of the tail of the arrow, of type floating
;		point.  Use of HEADX and HEADY overrides the Angle input.
;		This can be a vector.
;	LINESTYLE:  The line style index for drawing the arrow.
;	NORMAL:  If set, values are taken in normal coordinates.  The default
;		is data coordinates.
;	SOLID:  If set, the head fin is filled.  The default is no fill.
;	TAIL:  If set, tail fins are also plotted, according to the parameters
;		set for the head fins.
;	THICK:  The thickness of the lines forming the arrow, of type integer.
;		This can be a vector.
;
; USES:
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function overplots an arrow over the current plot.
;
; EXAMPLE:
;	Make a plot.
;	  plot, [0,1], /nodata
;	Plot an arrow from the middle of the plot to halfway to the upper
;	right corner.
;	  arrows, 0.5, 0.5, 0.25, 45.
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2003-06-06.
;-

;***********************************************************************

PRO ARROWS, $
	X, Y, Length, Angle, $
	COLOR=color, $
	HEADANGLE=headangle, HEADLENGTH=headlength, $
	HEADX=headx, HEADY=heady, $
	LINESTYLE=linestyle, $
	NORMAL=normalopt, $
	SOLID=solidopt, $
	TAIL=tailopt, $
	THICK=thick


;***********************************************************************
; Constants and Options

; Absolute constants
constants, degrad=degrad

; Option for solid filling head (and tail) fin
solidopt = keyword_set( solidopt )

; Option for plotting tail fins
tailopt = keyword_set( tailopt )

; Number of arrows to plot
n = n_elements( x )
if n ne n_elements( y ) then stop

;***********************************************************************
; Initialise Variables

; Reform LENGTH into vector (vectors are denoted with the "v" prefix)
if n eq n_elements( length ) then begin
  vlength = length
endif else begin
  vlength = length[0] + fltarr( n )
endelse

; Reform ANGLE into vector (vectors are denoted with the "v" prefix)
if var_type( angle ) ne 0 then begin
  if n eq n_elements( angle ) then begin
    vangle = angle
  endif else begin
    vangle = angle[0] + fltarr( n )
  endelse
endif

; Calculate head positions
if ( var_type( headx ) eq 0 ) or ( var_type( heady ) eq 0 ) then begin
  vheadx = x + vlength * cos( degrad * vangle )
  vheady = y + vlength * sin( degrad * vangle )
endif else begin
  ; Reform HEADX into vector (vectors are denoted with the "v" prefix)
  if n eq n_elements( headx ) then begin
    vheadx = headx
  endif else begin
    vheadx = headx[0] + fltarr( n )
  endelse
  ; Reform HEADY into vector (vectors are denoted with the "v" prefix)
  if n eq n_elements( heady ) then begin
    vheady = heady
  endif else begin
    vheady = heady[0] + fltarr( n )
  endelse
endelse

; Reform COLOR into vector (vectors are denoted with the "v" prefix)
if var_type( color ) ne 0 then begin
  if n eq n_elements( color ) then begin
    vcolor = color
  endif else begin
    vcolor = color[0] + intarr( n )
  endelse
endif else begin
  vcolor = !p.color + intarr( n )
endelse

; Reform LINESTYLE into vector (vectors are denoted with the "v" prefix)
if var_type( linestyle ) ne 0 then begin
  if n eq n_elements( linestyle ) then begin
    vlinestyle = linestyle
  endif else begin
    vlinestyle = linestyle[0] + intarr( n )
  endelse
endif else begin
  vlinestyle = !p.linestyle + intarr( n )
endelse

; Reform THICK into vector (vectors are denoted with the "v" prefix)
if var_type( thick ) ne 0 then begin
  if n eq n_elements( thick ) then begin
    vthick = thick
  endif else begin
    vthick = thick[0] + fltarr( n )
  endelse
endif else begin
  vthick = !p.thick + intarr( n )
endelse

; Set head fin length
if var_type( headlength ) eq 0 then begin
  headlength = 0.25
endif

; Set head fin angle
if var_type( headangle ) eq 0 then begin
  headangle = 45.
endif

; Calculate head fin end points
vheadxe1 = vheadx + headlength * vlength $
           * cos( degrad * ( vangle - 180 - headangle ) )
vheadye1 = vheady + headlength * vlength $
           * sin( degrad * ( vangle - 180 - headangle ) )
vheadxe2 = vheadx + headlength * vlength $
           * cos( degrad * ( vangle - 180 + headangle ) )
vheadye2 = vheady + headlength * vlength $
           * sin( degrad * ( vangle - 180 + headangle ) )

; Calculate tail fin end points
if tailopt then begin
  vtailxe1 = x + headlength * vlength $
             * cos( degrad * ( vangle - 180 - headangle ) )
  vtailye1 = y + headlength * vlength $
             * sin( degrad * ( vangle - 180 - headangle ) )
  vtailxe2 = x + headlength * vlength $
             * cos( degrad * ( vangle - 180 + headangle ) )
  vtailye2 = y + headlength * vlength $
             * sin( degrad * ( vangle - 180 + headangle ) )
endif

;***********************************************************************
; Plot 

; Iterate through arrows
for i = 0, n - 1 do begin
  ; Plot main line
  plots, [x[i],vheadx[i]], [y[i],vheady[i]], color=vcolor[i], $
         thick=vthick[i], normal=normalopt, linestyle=vlinestyle[i]
  ; Plot head fins
  if solidopt then begin
    polyfill, [vheadx[i],vheadxe1[i],vheadxe2[i],vheadx[i]], $
              [vheady[i],vheadye1[i],vheadye2[i],vheady[i]], $
              color=vcolor[i], normal=normalopt
  endif else begin
    plots, [vheadx[i],vheadxe1[i]], [vheady[i],vheadye1[i]], color=vcolor[i], $
           thick=vthick[i], normal=normalopt
    plots, [vheadx[i],vheadxe2[i]], [vheady[i],vheadye2[i]], color=vcolor[i], $
           thick=vthick[i], normal=normalopt
  endelse
  ; Plot tail fins
  if tailopt then begin
    if solidopt then begin
      polyfill, [x[i],vtailxe1[i],vtailxe2[i],vtailx[i]], $
                [y[i],vtailye1[i],vtailye2[i],vtaily[i]], $
                color=vcolor[i], normal=normalopt
    endif else begin
      plots, [x[i],vtailxe1[i]], [y[i],vtailye1[i]], color=vcolor[i], $
             thick=vthick[i], normal=normalopt
      plots, [x[i],vtailxe2[i]], [y[i],vtailye2[i]], color=vcolor[i], $
             thick=vthick[i], normal=normalopt
    endelse
  endif
endfor

;***********************************************************************
; The End

return
END
