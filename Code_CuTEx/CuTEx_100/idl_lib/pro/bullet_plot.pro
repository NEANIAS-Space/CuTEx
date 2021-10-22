;+
; NAME:
;	BULLET_PLOT
;
; PURPOSE:
;	This procedure draws bullet plots with bullets whose sizes are
;	scaled to values in an input vector.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	BULLET_PLOT, Indata, Xpos, Ypos
;
; INPUTS:
;	Data:  An data vector containing the values for scaling the
;	       bullet sizes, of type integer or floating point.
;	Xpos:  A vector of x-coordinates for the bullets, of type
;	       integer or floating point.
;	Ypos:  A vector of y-coordinates for the bullets, of type
;	       integer or floating point.
;
; KEYWORD PARAMETERS:
;	AREASCALE:  If set, the scale to plot the bullets size goes
;	            according to the bullet area, rather than the bullet
;	            diameter.
;	COLOR:  If set, negative values are plotted as blue filled
;	        bullets and positive values as red filled bullets.
;	        If set to a 2-element vector, negative values are
;	        plotted as filled bullets of colour index COLOR[0],
;	        and positive values as filleds of colour index COLOR[1].
;	        If not set, negative values are plotted as unfilled
;	        bullets and positive values as filled bullets.
;	LEGEND:  A 2-element vector containing the position ([x,y]) in
;	         normal coordinates of the lower left corner of a box
;	         containing the legend.
;	LTITLE:  A string containing the title for the legend.
;	MAXVAL:  The datum value corresponding to the largest bullet.
;	         The default is to take the largest datum.
;	OVERPLOT:  If set, the procedure plots on top of the pre-
;	           existing plot, without erasing it or changing any
;	           graphics settings.
;	RADIUS:  The radius of the largest bullet, in data coordinates.
;	         The procedure determines a value by default.
;	[X,Y]RANGE:  A 2-element vector containing the minimum and
;	             maximum [X,Y]-coordinates to be plotted.
;	[X,Y]STYLE:  See the IDL help for the use of these keywords in
;	             plotting routines.
;	SYMBOL:  A 2*N array containing the x- and y- coordinates of
;	         a bullet symbol to be plotted.  The default is a 20-
;	         point circle.
;	TITLE:  A title, of type string, for the plot.
;	UNFILL:  If set, the symbols are left open, i.e. not filled.
;	         The default is to fill the symbols if COLOR is set,
;	         or to only fill the symbols for positive values if
;	         COLOR is not set.
;
; USES:
;	BULLET_LEGEND.pro
;	CIRCLE.pro
;	DIMENSION.pro
;	FACTORS.pro
;	FIRST_DIGIT.pro
;	INTERVAL_CALC.pro
;	ODD.pro
;	SIGN.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This procedure uses the IDL procedure POLYFILL and/or OPLOT to
;	plot the bullets at the specified locations and of the specified
;	size.
;
; EXAMPLE:
;	Randomly define data and coordinate vectors of length 30.
;	  data = 2. * randomu( seed, 30 ) - 1.
;	  xpos = randomu( seed, 30 )
;	  ypos = randomu( seed, 30 )
;	Plot filled, coloured circle bullets.
;	  bullet_plot, data, xpos, ypos, /color
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@uvic.ca), 2000-07-17.
;	Modified:	DAS, 2000-08-24 (added legend option).
;	Modified:	DAS, 2000-08-28 (added AREASCALE keyword).
;	Modified:	DAS, 2000-09-12 (made bullets a bit bigger).
;	Modified:	DAS, 2001-02-07 (added MAXVAL keyword).
;	Modified:	DAS, 2001-02-15 (added UNFILL keyword).
;	Modified:	DAS, 2001-03-06 (reduced memory usage).
;-

;***********************************************************************

PRO BULLET_PLOT, Indata, Xpos, Ypos, $
                 AREASCALE=areascaleopt, $
                 COLOR=color, $
                 LEGEND=legend, LTITLE=ltitle, $
                 MAXVAL=maxval, $
                 OVERPLOT=overplotopt, $
                 RADIUS=radius, $
                 XRANGE=xrange, YRANGE=yrange, $
                 XSTYLE=xstyle, YSTYLE=ystyle, $
                 SYMBOL=symbol, $
                 TITLE=title, $
                 UNFILL=unfillopt

;***********************************************************************
;Variables and Options

;Input data
data = indata

;Areascale option
areascaleopt = keyword_set(areascaleopt)
if areascaleopt then data = temporary( sign(data) * sqrt(abs(data)) )

;Overplot option
overplotopt = keyword_set(overplotopt)

;Do not fill symbols option
unfillopt = keyword_set(unfillopt)

;Coloured bullets option
coloropt = keyword_set(color)
if coloropt then begin
  if dimension(color) eq 0 then begin
    color = [4,2]
    if not(overplotopt) then tek_color
  endif
endif else begin
  color = [!p.color,!p.color]
endelse

;Coordinates for the default circle bullet
circopt = 0
if not(keyword_set(symbol)) then begin
  circopt = 1
endif else if dimension(symbol) ne 2 then begin
  circopt = 1
endif else if n_elements(symbol[*,0]) ne 2 then begin
  circopt = 1
endif
if circopt then begin
  ncirc = 21
  symbol = circle( 0, 0, 1, ncirc )
endif

;Number of data points
n = min([n_elements(data),n_elements(xpos),n_elements(ypos)])

;Legend option
legendopt = keyword_set(legend)
if legendopt then begin
  if n_elements(legend) ne 2 then legendopt = 0
endif

;Maximum data value
maxvalopt = var_type(maxval)
if maxvalopt ne 0 then begin
  ;Inputed maximum value in MAXVAL
  inmaxdatum = maxval
endif else begin
  ;Calculate maximum value
  inmaxdatum = max(abs(indata[where(finite(indata) eq 1)]))
endelse
;Force a more convenient maximum value if plotting a legend
if legendopt then begin
  ;More convenient maximum
  ;(note this assumes that MAXVAL, if set, is already convenient)
  if maxvalopt eq 0 then begin
    inmaxdatum = interval_calc( inmaxdatum )
    maxval = inmaxdatum
  endif
  ;Number of legend values
  dig = first_digit( 1.00001* inmaxdatum )
  nlegend = 6
  if round(3*(dig/3.-dig/3)) eq 0 then nlegend = 4
  if dig eq 7 then nlegend = 8
  if dig eq 8 then begin
    inmaxdatum = 9. / 8. * inmaxdatum
    nlegend = 4
  endif
endif
if areascaleopt then maxdatum = sqrt(inmaxdatum) $
                else maxdatum = inmaxdatum

;***********************************************************************
;Plot Set-up

if not(overplotopt) then begin
  plot, [min(xpos),max(xpos)], [min(ypos),max(ypos)], /nodata, xstyle=xstyle, $
        ystyle=ystyle, xrange=xrange, yrange=yrange, title=title
endif

;***********************************************************************
;Convert to Normal Coordinates

xpos0 = convert_coord( xpos, ypos, /data, /to_normal )
ypos0 = reform(xpos0[1,*])
xpos0 = reform(xpos0[0,*])

;Bullet size
if not(keyword_set(radius)) then begin
  ;Counter for coincident points
  zerodist = 0
  ;Initialise radius value
  radius = temporary( sqrt( (max(xpos0)-min(xpos0))^2 $
                            + (max(ypos0)-min(ypos0))^2 ) )
  ;Calculate radius value
  for i=1,n-1 do begin
    ;Search for candidate neighbours closer than Radius
    idx = where( xpos0[0:i-1]-xpos0[i] lt radius, siz )
    if siz gt 0 then begin
      ;Search for candidate neighbours closer than Radius
      idy = where( ypos0[idx]-ypos0[i] lt radius, siz )
      if siz gt 0 then begin
        ;Calculate distance
        dist = sqrt( (xpos0[idx[idy]]-xpos0[i])^2 $
                     + (ypos0[idx[idy]]-ypos0[i])^2 )
	;Count coincident points
        if dimension( where(dist eq 0) ) gt 0 then zerodist = zerodist + 1
        ;Calculate new(?) radius
        idd = where( (dist lt radius) and (dist ne 0), siz )
        if siz gt 0 then radius = min(dist[idd])
      endif
    endif
  endfor
  ;Clear some memory
  idx = 0
  idy = 0
  idd = 0
  ;Allow neighbouring bullets to not overlap
  radius = radius / 2.
  ;Report coincident points
  if zerodist gt 0 then begin
    print, 'WARNING:  There were '+str(zerodist)+' coincident data points.'
  endif
  ;Make bullets a bit bigger
  radius = radius * 1.5
endif

;***********************************************************************
;Draw Bullets

for i=0,n-1 do begin
  siz = radius * data[i] / maxdatum
  if (xpos0[i] gt 0) and (xpos0[i] lt 1) then begin
    if (ypos0[i] gt 0) and (ypos0[i] lt 1) then begin
      if sign(data[i]) ge 0 then begin
        if unfillopt then begin
          normpos = convert_coord( xpos0[i]+siz*symbol[0,*], $
                                   ypos0[i]+siz*symbol[1,*], /normal, $
                                   /to_data )
          oplot, normpos[0,*], normpos[1,*], color=color[1]
        endif else begin
          polyfill, xpos0[i]+siz*symbol[0,*], ypos0[i]+siz*symbol[1,*], $
                    color=color[1], /normal
        endelse
      endif else begin
        if coloropt and not(unfillopt) then begin
          polyfill, xpos0[i]+siz*symbol[0,*], ypos0[i]+siz*symbol[1,*], $
                  color=color[0], /normal
        endif else begin
          normpos = convert_coord( xpos0[i]+siz*symbol[0,*], $
                                   ypos0[i]+siz*symbol[1,*], /normal, $
                                   /to_data )
          oplot, normpos[0,*], normpos[1,*], color=color[0]
        endelse
      endelse
    endif
  endif
endfor

;Clear some memory
data = 0
xpos0 = 0
ypos0 = 0
if keyword_set(normpos) then normpos = 0

;***********************************************************************
;Draw Legend

if legendopt then begin
  bullet_legend, legend, radius, inmaxdatum, color=coloropt, symbol=symbol, $
                 nvalues=nlegend, title=ltitle, areascale=areascaleopt
endif

;***********************************************************************
;The End

return
END
