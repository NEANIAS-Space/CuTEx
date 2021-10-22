;+
; NAME:
;	BAR_GRAPH
;
; PURPOSE:
;	This procedure plots bar graphs.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	BAR_GRAPH, [Xval,] Yval
;
; INPUTS:
;	Yval:  A vector of values to be plotted, of type integer or
;	       floating point.
;
; OPTIONAL INPUTS:
;	Xval:  A vector of ordinate values for the plot, of type integer
;	       or floating point.
;
; KEYWORD PARAMETERS:
;	BARBORDER:  The color of the border of the bars.  The default is the
;		same as BARCOLOR.
;	BARCOLOR:  The color of the bars of the bar plot.
;	BARGAP:  The size of the gap between bars in unit of fraction of the
;		bar width.  The default is 0 (no gap).
;	CHARSIZE:  The size of the text characters.
;	COLOR:  The color of the plot axes and text.
;	FONT:  The index of the font table to be used.
;	OVERPLOT:  If set the procedure plots on top of the exising plot.
;	SUBTITLE:  A subtitle for the plot, of type string.
;	TITLE:  A title, of type string, for the plot.
;	VARWIDTH:  If set the bars are all with variable widths such that
;		there are no gaps between them.  The default is to plot all
;		bars with the same width.
;	[X,Y]MINOR:  The number of minor tick marks between major tick marks.
;	[X,Y]RANGE:  A 2-element vector containing the minimum and maximum
;		X- or Y-coordinates to be plotted.
;	[X,Y]STYLE:  See the IDL help for the use of these keywords in plotting
;		routines.
;	[X,Y]THICK:  The line thickness.
;	[X,Y]TICKLEN:  The length of the ticks on the X- or Y-axis, in units of
;		fraction of the window size.  The default is !p.ticklen (0.02).
;	[X,Y]TICKNAME:  A vector, of type string, of labels for the major ticks
;		on the X- or Y-axis.  There should be [X,Y]TICKS + 1 values in
;		the vector.
;	[X,Y]TICKS:  The number of major ticks on the X- or Y-axis minus one.
;	[X,Y]TICKV:  The location of the major ticks on the X- or Y-axis.
;	[X,Y]TITLE:  A label for the X or Y axis, of type string.
;
; USES:
;	FIRST_DIFF.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This procedure uses the POLYFILL procedure to draw solid bar
;	graphs.
;
; EXAMPLE:
;	Plot 10 step-like bars.
;	  bar_graph, indgen(10)+1, barcolor=2, bargap=0.1
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-09-11.
;	Modified:	DAS, 2001-10-22 (Added [X,Y]TICKLEN keywords).
;	Modified:	DAS, 2003-06-16 (added FONT, [X,Y]MINOR, [X,Y]THICK
;			keywords)
;-

;***********************************************************************

PRO BAR_GRAPH, $
	Xval, Yval, $
	BARBORDER=barborder, BARCOLOR=barcolor, COLOR=color, $
	BARGAP=bargap, $
	CHARSIZE=charsize, $
	FONT=font, $
	OVERPLOT=overplotopt, $
	TITLE=title, SUBTITLE=subtitle, XTITLE=xtitle, YTITLE=ytitle, $
	VARWIDTH=varwidth, $
	XMINOR=xminor, YMINOR=yminor, $
	XSTYLE=xstyle, YSTYLE=ystyle, $
	XTHICK=xthick, ythick=ythick, $
	XTICKLEN=xticklen, YTICKLEN=yticklen, $
	XTICKNAME=xtickname, YTICKNAME=ytickname, $
	XTICKS=xticks, YTICKS=yticks, $
	XTICKV=xtickv, YTICKV=ytickv, $
	XRANGE=xrange, YRANGE=yrange

;***********************************************************************
; Constants and Variables

; Ordinate vector for data
if n_params() eq 1 then begin
  yval = xval
  xval = indgen( n_elements( yval ) )
endif
nval = n_elements( yval )

; Colors
; Bar fill color
if var_type( barcolor ) eq 0 then barcolor = !p.color
; Bar border color
if var_type( barborder ) eq 0 then barborder = barcolor
; Plot axes and text color
if var_type( color ) eq 0 then color = !p.color

;BARGAP
if not( keyword_set( bargap ) ) then bargap = 0

;***********************************************************************
; Prepare Data for Plotting

; Vector data sorted according to the ordinate vector
id = sort( xval )
xsort = xval[id]
ysort = yval[id]
id = where( (finite( xsort ) eq 1) and (finite( ysort ) eq 1) )
xsort = xsort[id]
ysort = ysort[id]
nval = n_elements( id )

; Ordinate vector for plotting
if keyword_set( varwidth ) then begin
  xcoord = first_diff( xsort, /forward ) / 2.
  xcoord = [ (xsort[1]-xsort[0])/2., xcoord ]
  xcoord[nval] = ( xsort[nval-1] - xsort[nval-2] ) / 2.
endif else begin
  mindiff = min( (first_diff( xsort, /forward ))[0:nval-2] ) / 2.
  xcoord = mindiff + 0 * [0,xsort]
endelse

; XRANGE
if not( keyword_set( xrange )) then begin
  xrange = [ xsort[0]-xcoord[0], xsort[nval-1]+xcoord[nval] ]
endif

; YRANGE
if not( keyword_set( yrange ) ) then yrange = [ min( ysort ), max( ysort ) ]

;***********************************************************************
; Plot Bar Graph

; Plot set-up
if not( keyword_set( overplotopt ) ) then begin
  plot, xrange, yrange, charsize=charsize, color=color, /nodata, $
      subtitle=subtitle, title=title, xstyle=xstyle, ystyle=ystyle, $
      xthick=xthick, ythick=ythick, xticklen=xticklen, yticklen=yticklen, $
      xtickname=xtickname, ytickname=ytickname, xticks=xticks, $
      yticks=yticks, xtickv=xtickv, ytickv=ytickv, xtitle=xtitle, $
      ytitle=ytitle, font=font, xminor=xminor, yminor=yminor
  ; Y=0 line
  oplot, xrange, [0,0], color=color, line=1
endif

; Plot bars
for i = 0, nval - 1 do begin
  ; Solid bar
  polyfill, xsort[i]+(1-bargap)*[-1,1,1,-1,-1]*xcoord[i+[0,1,1,0,0]], $
      [0,0,ysort[i],ysort[i],0], color=barcolor
  ; Bar border
  oplot, xsort[i]+(1-bargap)*[-1,1,1,-1,-1]*xcoord[i+[0,1,1,0,0]], $
      [0,0,ysort[i],ysort[i],0], color=barborder
endfor

;***********************************************************************
; The END

return
END
