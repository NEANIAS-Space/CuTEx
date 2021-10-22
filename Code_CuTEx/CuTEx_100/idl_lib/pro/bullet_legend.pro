;+
; NAME:
;	BULLET_LEGEND
;
; PURPOSE:
;	This procedure plots a legend for bullet plots.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	BULLET_LEGEND, Position, Radius, Maxdatum
;
; INPUTS:
;	Position:  A 2-element vector containing the position ([x,y]) in
;	           normal coordinates of the lower left corner of the
;	           box containing the legend.
;	Radius:  The radius of the largest bullet, in normal coordinates.
;	Maxdatum:  The value of the largest bullet.
;
; KEYWORD PARAMETERS:
;	COLOR:  If set, negative values are plotted as blue filled
;	        bullets and positive values as red filled bullets.
;	        If set to a 2-element vector, negative values are
;	        plotted as filled bullets of colour index COLOR[0],
;	        and positive values as filleds of colour index COLOR[1].
;	        If not set, negative values are plotted as unfilled
;	        bullets and positive values as filled bullets.
;	SYMBOL:  A 2*N array containing the x- and y- coordinates of
;	         a bullet symbol to be plotted.  The default is a 20-
;	         point circle.
;	NVALUES:  The number of bullets to plot in the legend.  The
;	          default is 4.
;	TITLE:  A string containing the title of the legend.
;	NDECPLACE:  Number of decimal places to be displayed in the
;	            legend's bullet labels.  The procedure can make a
;	            decent minimal guess at this.
;	AREASCALE:  If set, the scale used to plot the bullets goes by
;	            the bullet area, rather than the bullet diameter.
;
; USES:
;	DECIMAL_PLACE.pro
;	DIMENSION.pro
;	STR.pro
;
; PROCEDURE:
;	This procedure uses the input values to construct an appropriate
;	box containing the legend for a bullet plot.
;
; EXAMPLE:
;	Create a legend for a bullet plot of temperature anomaly data
;	with a maximum value of 6 deg C.
;	  bullet_legend, [0.2,0.2], 0.05, 6, /color, $
;	                 symbol=circle(0,0,1,21), $
;	                 title='Temperature Anomaly (deg C)'
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-08-24.
;	Modified:	DAS, 2000-08-28 (added AREASCALE keyword).
;	Modified:	DAS, 2000-09-12 (made compatible with !P.MULTI).
;-

;***********************************************************************

PRO BULLET_LEGEND, Position, $
                   Radius, $
                   Maxdatum, $
                   COLOR=color, $
                   SYMBOL=symbol, $
                   NVALUES=nvalues, $
                   TITLE=title, $
                   NDECPLACE=ndecplace, $
                   AREASCALE=areascaleopt

;***********************************************************************
;Variables and Options

;Colour option
coloropt = keyword_set(color)
if coloropt then begin
  if dimension(color) eq 0 then color = [4,2]
endif else begin
  color = [!p.color,!p.color]
endelse

;Number of bullets in the legend
if not(keyword_set(nvalues)) then nvalues = 4

;Determine the legend box corner positions
xpos = position[[0,0]]
ypos = position[[1,1]]
if !p.multi[1] ne 0 then begin
  if !p.multi[0] eq 0 then pmulti0 = !p.multi[1] * !p.multi[2] $
                      else pmulti0 = !p.multi[0]
  xpos = (pmulti0+!p.multi[1]-1) / !p.multi[1] - pmulti0 / 1. / !p.multi[1] $
         + xpos / !p.multi[1]
endif
if !p.multi[2] ne 0 then begin
  if !p.multi[0] eq 0 then pmulti0 = !p.multi[1] * !p.multi[2] $
                      else pmulti0 = !p.multi[0]
  ypos = ( ypos + (pmulti0-1) / !p.multi[1] ) / 1. / !p.multi[2]
endif

ypos[1] = ypos[0] + (2*nvalues+2) * radius

;Character scale
ydif = convert_coord( xpos, ypos, /normal, /to_device )
ydif = ydif[1,1] - ydif[1,0]
charsize = 0.7 * ydif / (nvalues+1.) / !d.y_ch_size

;Areascale option
areascaleopt = keyword_set(areascaleopt)

;The bullet labels
levels = (2. * findgen(nvalues) / (nvalues-1.) - 1.) * maxdatum
;Number of decimal places in the bullet labels
if not(keyword_set(ndecplace)) then begin
  ndecplace = decimal_place( min( 1.00001*abs(levels) ))
endif

;Re-determine the legend box corner positions
ystrlen = max( strlen( str( levels, ndecplace ) ) )
xpos[1] = xpos[0] + 1.5 * (ypos[1] - ypos[0])
id = where( strlen( str( levels, ndecplace ) ) eq ystrlen )
xyouts, xpos[0]+radius, ypos[0]+radius, str(levels[id[0]],ndecplace), $
        /normal, charsize=charsize, width=strwidth
xpos[1] = xpos[0] + 5 * radius + strwidth

;***********************************************************************
;Plot Legend

;Clear area for legend box
polyfill, xpos[[0,1,1,0,0]], ypos[[0,0,1,1,0]]+[0,0,1,1,0]*3.1*radius, $
          color=!p.background, /normal

;Draw the legend border
plots, xpos[[0,1,1,0,0]], ypos[[0,0,1,1,0]], color=!p.color, /normal

;Draw the bullets
for i=0,nvalues/2-1 do begin
  siz = (nvalues-2.*i-1.) / (nvalues-1.)
  if areascaleopt then siz = sign(siz) * sqrt(abs(siz))
  polyfill, xpos[0]+2*radius+siz*radius*symbol[0,*], $
            ypos[1]-2*(i+1)*radius+siz*radius*symbol[1,*], color=color[1], $
            /normal
  if coloropt then begin
    polyfill, xpos[0]+2*radius+siz*radius*symbol[0,*], $
              ypos[0]+2*(i+1)*radius+siz*radius*symbol[1,*], color=color[0], $
              /normal
  endif else begin
    plots, xpos[0]+2*radius+siz*radius*symbol[0,*], $
           ypos[0]+2*(i+1)*radius+siz*radius*symbol[1,*], color=color[0], $
           /normal
  endelse
endfor

;Label the bullets
xyouts, 0*intarr(nvalues)+xpos[1]-radius, $
        ypos[0]+(2*indgen(nvalues)+1)*radius, $
        str(levels,ndecplace), /normal, charsize=charsize, alignment=1

;Title
if keyword_set(title) then begin
  xyouts, (xpos[1]+xpos[0])/2., ypos[1]+radius, title, /normal, $
          charsize=charsize, alignment=0.5
endif

;***********************************************************************
;The End

return
END
