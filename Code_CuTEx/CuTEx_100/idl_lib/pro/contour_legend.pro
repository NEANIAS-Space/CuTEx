;+
; NAME:
;	CONTOUR_LEGEND
;
; PURPOSE:
;	This procedure plots a legend for colour-contour plots.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	CONTOUR_LEGEND, Xpos, Ypos
;
; INPUTS:
;	Xpos:  The x-coordinates of the legend box, of integer or
;	       floating point type, in the form of a 2-element vector.
;	       The vector is of the form [ leftmost, rightmost ].
;	Ypos:  The y-coordinates of the legend box, of integer or
;	       floating point type, in the form of a 2-element vector.
;	       The vector is of the form [ bottom, top ].
;
; KEYWORD PARAMETERS:
;	ARROWEND:  If set, then pointed end levels are plotted, indicating 
;		that the end levels extend beyond the range of the plot.  The 
;		default is rectangular end levels, like all the other levels.  
;		A 2 element vector can also be input, where the first element 
;		gives the setting (0 or 1) for the left/bottom end and the 
;		second for the right/top end.
;	C_COLORS:  A vector of the contour level colour indices, of type
;		integer.  This vector should be of size NLEVELS-1.
;	CHARSIZE:  The size of the text characters.
;	COLOR:  The colour index of the text and border, of type integer.
;	FONT:  The standard IDL font keyword.
;	HORIZONTAL:  If set, the procedure plots a horizontal legend.  This is 
;		the default.
;	LEVELS:  A vector of contour level values, of type integer or floating 
;		point.
;	[MIN,MAX]LEVEL:  The [minimum, maximum] level value, of type integer 
;		or floating point.  If used, both values must be set.  If 
;		LEVELS is set it overrides these values.
;	NLEVELS:  The number of contour level values, of type integer.  If 
;		LEVELS is set, its size overrides NLEVELS.  If set to a 
;		negative value, then the routine picks a comfortable number of 
;		levels with -NLEVELS being a maximum.  The default is to pick 
;		a comfortable limit according to choose_levels.pro.
;	NORMAL:  If set the procedure uses normalised coordinates.  The 
;		default is data coordinates.
;	NTICKS:  Number of tick marks, of type integer.  If TICKNAME is set, 
;		its size overrides NTICKS.
;	SUBTITLE:  Subtitle of the legend, giving the data units, of type 
;		string.
;	THICK:  The line thickness.
;	TICK_DECPLACE:  Number of decimal places to print in the tick labels, 
;		of type integer.  This is used only if TICKNAME is not defined.
;	TICKLEN:  The length of the tick marks in terms of fraction of the 
;		legend box size, of type floating point.
;	TICKNAME:  A string vector containing the labels for each tick mark.
;	TICKV:  An integer or floating vector containing the positions of the 
;		tick marks.
;	TITLE:  The title of the legend, of type string.  If set to a 
;		non-string type (i.e. "/TITLE") then "Legend" is used.
;	VERTICAL:  If set, the procedure plots a vertical legend.  The default 
;		is horizontal.
;
; USES:
;	choose_levels.pro
;	decimal_place.pro
;       str.pro
;	var_type.pro
;
; PROCEDURE:
;	This procedure draws a box and fills it with colour-contoured
;	levels.  It then adds ticks and labels to the box.
;
; EXAMPLE:
;	Plot a horizontal 30-level colour-contoured legend at the bottom
;	of the display.
;	  contour_legend, [0.2,0.8], [0.1,0.2], nlevels=30, title=1, $
;	      horizontal=1, normal=1
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-07-11.
;	Modified:	DAS, 2000-07-12 (corrected HORIZONTAL and
;			VERTICAL keywords, added MINLEVEL and MAXLEVEL
;			keywords).
;	Modified:	DAS, 2000-07-19 (added tick-choosing algorithm).
;	Modified:	DAS, 2000-09-21 (cleaning, debugging,
;	                documentation).
;	Modified:	DAS, 2000-09-25 (modified tick-choosing).
;	Modified:	DAS, 2000-11-28 (modified tick-choosing).
;	Modified:	DAS, 2002-11-22 (added FONT keyword).
;	Modified:	DAS, 2007-02-06 (added ARROWEND option;  changed 
;			plotting method)
;	Modified:	DAS, 2007-09-18 (allowed a flexible number of levels 
;			to be chosen;  some standardisation of code)
;	Modified:	DAS, 2009-05-04 (added the THICK keyword;  added noclip 
;			property to borders and ticks;  fixed bug with TICKNAME 
;			implementation when ARROWEND is set)
;	Modified:	DAS, 2009-10-27 (added TICKV keyword)
;-

;***********************************************************************

PRO CONTOUR_LEGEND, $
	Xpos, Ypos, $
	LEVELS=levels, NLEVELS=n_levels, MINLEVEL=min_level, $
	  MAXLEVEL=max_level, $
	C_COLORS=c_colors, $
	TICKNAME=tickname, TICK_DECPLACE=tickdecplace, TICKV=tickv, $
	NTICKS=n_ticks, TICKLEN=ticklen, $
	COLOR=color, $
	CHARSIZE=charsize, $
	FONT=font, $
	THICK=thick, $
	TITLE=title, SUBTITLE=subtitle, $
	HORIZONTAL=horizontal_opt, VERTICAL=vertical_opt, $
	NORMAL=normal_opt, $
	ARROWEND=arrowend_opt

;***********************************************************************
; Constants and Options

; Legend box dimensions
xdim = xpos[1] - xpos[0]
ydim = ypos[1] - ypos[0]

; Plotting orientation
horizontal_opt = keyword_set( horizontal_opt ) $
    or not( keyword_set( vertical_opt ) )
vertical_opt = not( horizontal_opt )

; Normal coordinates option
normal_opt = keyword_set( normal_opt )

; Arrowed end option
if keyword_set( arrowend_opt ) then begin
  if n_elements( arrowend_opt ) eq 1 then begin
    arrowend_opt = [ 0, 0 ] + arrowend_opt
  endif
endif else begin
  arrowend_opt = [ 0, 0 ]
endelse

;***********************************************************************
; Default Settings

; Contour levels
if not( keyword_set( n_levels ) ) then begin
  if keyword_set( levels ) then begin
    n_levels = n_elements( levels )
  endif else if keyword_set( c_colors ) then begin
    n_levels = n_elements( c_colors ) + 1
  endif else begin
    n_levels = 0
  endelse
endif
if not( keyword_set( levels ) ) then begin
  if ( n_elements( min_level ) ne 0 ) $
      and ( n_elements( max_level ) ne 0 ) then begin
    if n_levels gt 0 then begin
      levels = min_level + findgen( n_levels ) / ( n_levels - 1 ) $
          * ( max_level - min_level )
    endif else begin
      levels = choose_levels( [ min_level, max_level ], nlevels=-n_levels )
      levels = choose_levels( [ min( levels ), max( levels ) ], $
          nlevels=-n_levels )
    endelse
  endif else begin
    levels = indgen( abs( n_levels ) )
  endelse
endif
n_levels = n_elements( levels )

; Contour colours
if not( keyword_set( c_colors ) ) then c_colors = indgen( n_levels - 1 ) + 1

; Level ticks
; Number of ticks
if not( keyword_set( tickname ) ) then begin
  if keyword_set( tickv ) then begin
    tickname = str( tickv, decplace )
    n_ticks = n_elements( tickv )
  endif else if keyword_set( n_ticks ) then begin
    tickname = findgen( n_ticks ) / ( n_ticks - 1 ) $
        * ( levels[n_levels-1] - levels[0] ) + levels[0]
    id = where( abs( tickname ) gt 0 )
    decplace = decimal_place( min( abs( tickname[id] ) ) )
    tickname = str( tickname, decplace )
  endif else begin
    tickname = choose_levels( levels, range=[levels[0],levels[n_levels-1]] )
    tickname = str( tickname, max( decimal_place( tickname ) ) )
    n_ticks = n_elements( tickname )
  endelse
endif else begin
  if not( keyword_set( n_ticks ) ) then begin
    if keyword_set( tickv ) then begin
      n_ticks = n_elements( tickv )
    endif else begin
      n_ticks = n_elements( tickname )
    endelse
  endif
endelse
if not( keyword_set( tickv ) ) then tickv = tickname
; Tick mark length
if not( keyword_set( ticklen ) ) then ticklen = 0.1

; Border and text colour
if not( keyword_set( color ) ) then color = !p.color

; Set up the plotting coordinates
if normal_opt then begin
  plot, [0,1], [0,1], nodata=1, xstyle=5, ystyle=5, noerase=1, xmargin=[0,0], $
      ymargin=[0,0]
endif

; Text character constants
; Character size
if not( keyword_set( charsize ) ) then charsize = 1.
; Character scale
xcharshift = 1. * !d.x_ch_size / !d.x_size
ycharshift = 1. * !d.y_ch_size / !d.y_size
origin = convert_coord( [0], [0], data=1, to_normal=1 )
charshift = convert_coord( [xcharshift+origin[0]], [ycharshift+origin[1]], $
    normal=1, to_data=1 )
xcharshift = charshift[0]
ycharshift = charshift[1]

; Legend title
if keyword_set( title ) then begin
  if var_type( title ) ne 7 then title = 'Legend'
endif

;***********************************************************************
; Plot the Coloured Contour Levels

; Create the contoured grid for vertical legend
if vertical_opt then begin
  yvec = ypos[0] $
      + ( levels - levels[0] ) / ( levels[n_levels-1] - levels[0] ) * ydim
  yvec = [ [ yvec ], [ yvec ] ]
  xvec = fltarr( n_levels, 2 )
  xvec[1:n_levels-2,0] = xpos[0]
  xvec[1:n_levels-2,1] = xpos[1]
  if arrowend_opt[0] eq 0 then begin
    xvec[0,*] = xpos
  endif else begin
    xvec[0,*] = mean( xpos )
  endelse
  if arrowend_opt[1] eq 0 then begin
    xvec[n_levels-1,*] = xpos
  endif else begin
    xvec[n_levels-1,*] = mean( xpos )
  endelse
endif

; Create the contoured grid for horizontal legend
if horizontal_opt then begin
  xvec = xpos[0] $
      + ( levels - levels[0] ) / ( levels[n_levels-1] - levels[0] ) * xdim
  xvec = [ [ xvec ], [ xvec ] ]
  yvec = fltarr( n_levels, 2 )
  yvec[1:n_levels-2,0] = ypos[0]
  yvec[1:n_levels-2,1] = ypos[1]
  if arrowend_opt[0] eq 0 then begin
    yvec[0,*] = ypos
  endif else begin
    yvec[0,*] = mean( ypos )
  endelse
  if arrowend_opt[1] eq 0 then begin
    yvec[n_levels-1,*] = ypos
  endif else begin
    yvec[n_levels-1,*] = mean( ypos )
  endelse
endif

; Draw the coloured contour levels
for i = 0, n_levels - 2 do begin
  temp = [ xvec[i,0], xvec[i,1], xvec[i+1,1], xvec[i+1,0] ]
  temp1 = [ yvec[i,0], yvec[i,1], yvec[i+1,1], yvec[i+1,0] ]
  polyfill, temp, temp1, color=c_colors[i]
endfor

;***********************************************************************
; Draw the Legend Border

; Left or bottom border
oplot, xvec[0,*], yvec[0,*], color=color, thick=thick, noclip=1
; Right or top border
oplot, xvec[n_levels-1,*], yvec[n_levels-1,*], color=color, thick=thick, $
    noclip=1
; Top or left border
oplot, xvec[*,0], yvec[*,0], color=color, thick=thick, noclip=1
; Bottom or right border
oplot, xvec[*,1], yvec[*,1], color=color, thick=thick, noclip=1

;***********************************************************************
; Legend Tick Marks and Names

; Vertical ticks and tick names
if vertical_opt then begin
  ; The width of longest tick name (required for subtitle)
  xwidth = 0
  for i = 0, n_ticks - 1 do begin
    ; Only do this if not an arrowed end
    check = 1
    if ( arrowend_opt[0] eq 1 ) and ( tickv[i] lt levels[1] ) then check = 0
    if ( arrowend_opt[1] eq 1 ) and ( tickv[i] gt levels[n_levels-2] ) $
        then check = 0
    if check eq 1 then begin
      ytick = ( tickv[i] - min( levels ) ) / ( max( levels ) - min( levels ) ) $
          * ydim
      ; Tick marks
      oplot, [xpos[0]-xdim*ticklen,xpos[0]], ypos[0]+ytick*[1,1], color=color, $
          thick=thick, noclip=1
      ; Tick names
      xyouts, xpos[0]-1.5*ticklen*xdim, $
          ypos[0]+ytick-0.5*ycharshift*charsize, tickname[i], $
          charsize=charsize, color=color, alignment=1, width=width, font=font
      if width gt xwidth then xwidth = width
    endif
  endfor
  xwidth = ( convert_coord( [width+origin[0]], [0], normal=1, to_data=1 ) )[0]
endif

; Horizontal ticks and tick names
if horizontal_opt then begin
  for i = 0, n_ticks - 1 do begin
    ; Only do this if not an arrowed end
    check = 1
    if ( arrowend_opt[0] eq 1 ) and ( tickv[i] lt levels[1] ) then check = 0
    if ( arrowend_opt[1] eq 1 ) and ( tickv[i] gt levels[n_levels-2] ) $
        then check = 0
    if check eq 1 then begin
      xtick = ( tickv[i] - min( levels ) ) / ( max( levels ) - min( levels ) ) $
          * xdim
      ; Tick marks
      oplot, xpos[0]+xtick*[1,1], [ypos[0]-ydim*ticklen,ypos[0]], color=color, $
          thick=thick, noclip=1
      ; Tick names
      xyouts, xpos[0]+xtick, ypos[0]-ticklen*ydim-1.5*ycharshift*charsize, $
          tickname[i], charsize=charsize, color=color, alignment=0.5, font=font
    endif
  endfor
endif

;***********************************************************************
; Titles

; Main title
if keyword_set( title ) then begin
  xyouts, xpos[0]+xdim/2., ypos[1]+ycharshift*charsize, title, $
      charsize=charsize, alignment=0.5, color=color, font=font
endif

; Units title
if keyword_set( subtitle ) then begin
  ; Vertical legend
  if vertical_opt then begin
    xyouts, xpos[0]-1.5*xdim*ticklen-xwidth-0.5*xcharshift*charsize, $
        ypos[0]+ydim/2., subtitle, charsize=charsize, alignment=0.5, $
        orientation=90, color=color, font=font
  endif
  ; Horizontal legend
  if horizontal_opt then begin
    xyouts, xpos[0]+xdim/2., ypos[0]-ticklen*ydim-3*charsize*ycharshift, $
        subtitle, charsize=charsize, alignment=0.5, color=color, font=font
  endif
endif

;***********************************************************************
; The End

return
END
