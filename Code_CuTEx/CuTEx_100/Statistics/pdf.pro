;+
; NAME:
;	PDF
;
; PURPOSE:
;	This procedure estimates the one or two dimensional probability 
;	density function of a given data set.
;
; CATEGORY:
;	Statistics
;
; CALLING SEQUENCE:
;	PDF, Indata, X [, Y]
;
; INPUT:
;	X:  The X-coordinates of the data values.
;
; OPTIONAL INPUT:
;	Y:  The Y-coordinates of the data values (for two dimensional PDFs).
;
; KEYWORD PARAMETERS:
;	BANDWIDTH:  If set, the procedure modifies the smoothing parameter size
;		depending upon the data density near each point.  Otherwise, 
;		the smoothing parameter is not modified.
;	C_COLORS:  A vector of colour indices for the contoured levels.  This
;		applies to two dimensional data only).
;	CHARSIZE:  The size of the text characters.  The default is 1.
;	COLOR:  The colour index of the axes and text.  The default is set in 
;		!P.COLOR.
;	FILL:  If set, the contours are filled.  Otherwise, only contour lines
;		are plotted.  This applies to two dimensional data only.
;	NLEVELS:  The number of level values for the contour plot.  This 
;		applies to two dimensional data only.
;	NOVERBOSE:  If set, messages are not printed.
;	NOPLOT:  If set, the PDF is not plotted.
;	[X,Y]RANGE:  A 2-element vector containing the minimum and
;		maximum [x,y]-coordinates to be plotted.
;	SCORE:  If set, the procedure calculates the near optimal smoothing
;		parameter value.  If not set, a value is estimated assuming a
;		multivariate Gaussian distribution.
;	[MIN,MAX]SCORE:  The minimum, maximum input value for the smoothing
;		parameter estimation score function.  The default is 0.1, 1.0.
;	NSCORE:  The size (resolution) of the smoothing parameter estimation 
;		score function.  The default is 29 points.
;	NPDF:  The size (resolution) of the PDF axes.  The default is 31 
;		points.
;	TITLE:  A string containing the title of the plot.
;	[X,Y]TITLE:  A string containing the label for the X,Y axis.
;
; OPTIONAL OUTPUT:
;	FSCORE:  The score function for calculating the near optimal smoothing
;		parameter value.
;	[X,Y]ID:  The X, Y coordinate values of the PDF array (or vector).
;	PDF:  The probability density function array (or vector).	
;
; USES:
;	choose_levels.pro
;	odd.pro
;
; PROCEDURE:
;	This procedure uses formulae from Silverman (1986) to estimate the 
;	PDF.  See these references for more information:
;	Brunet, G.  1994.  Empirical normal-mode analysis of atmospheric data.
;	  Journal of Atmospheric Sciences, 51, 932-952.
;	Kimoto, M., and M. Ghil.  1993.  Multiple flow regimes in the Northern 
;	  Hemisphere winter.  Part I:  Methodology and hemispheric regimes.  
;	  Journal of Atmospheric Sciences, 50, 2625-2643.
;	Silverman, B. W.  1986.  Density Estimation for Statistics and Data
;	  Analysis. Chapman and Hall, 175p.
;
; EXAMPLE:
;	Create a vector of Gaussian noise.
;	  x = randomn( seed, 100 )
;	Estimate and plot the PDF of the data.  Make the best estimate.
;	  pdf, x, BANDWIDTH=1, SCORE=1
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2001-05-09
;	Modified:	DAS, 2001-06-04 (modified the pilot smoothing parameter
;			estimate)
;	Modified:	DAS, 2002-03-15 (modified style, streamlined eta
;			estimator, allowed eta estimator in 1D)
;	Modified:	DAS, 2002-04-10 (switched std.pro to stddev)
;	Modified:	DAS, 2003-01-30 (minorly optimised score calculation)
;	Modified:	DAS, 2003-05-28 (added NOVERBOSE keyword)
;	Modified:	DAS, 2004-08-20 (modified FSCORE to return output even 
;			if SCORE is not set)
;	Modified:	DAS, 2005-08-05 (replaced sum.pro use with total)
;	Modified:	DAS, 2007-05-24 (removed use of constants.pro; 
;			changed faulty normalisation method to simple 
;			calculation)
;-

;***********************************************************************

PRO PDF, $
	X, Y, $
	BANDWIDTH=bandwidthopt, $
	C_COLORS=c_colors, COLOR=COLOR, $
	CHARSIZE=charsize, $
	FILL=fillopt, $
	NLEVELS=nlevels, $
	NOPLOT=noplotopt, $
	NOVERBOSE=noverboseopt, $
	XRANGE=xrange, YRANGE=yrange, $
	SCORE=scoreopt, NSCORE=nscore, MINSCORE=minscore, MAXSCORE=maxscore, $
	  FSCORE=fscore, $
	NPDF=npdf, $
	TITLE=title, XTITLE=xtitle, YTITLE=ytitle, $
	PDF=pdf, $
	XID=xid, YID=yid

;***********************************************************************
; Constants

; Absolute constants
pi = !pi

; Plotting constants
; Character size
if not( keyword_set( charsize ) ) then charsize = 1.

; Size of data set
nxdata = n_elements( x )
nydata = n_elements( y )
if not( ( nydata eq 0 ) or ( nydata eq nxdata ) ) then stop

; PDF axis sizes (must be odd)
if not( keyword_set( npdf ) ) then npdf = 31
if not( odd( npdf ) ) then npdf = npdf + 1
; X-axis size
xnpdf = npdf
; Y-axis size
ynpdf = npdf

; Dimension of data
if not( keyword_set( y ) ) then begin
  ; Dimension of data
  dim = 1
  ; Y-axis size
  ynpdf = 1
  ; Change Y format to facilitate calculation
  y = 0 * fltarr( nxdata )
endif else begin
  ; Dimension of data
  dim = 2
endelse

; Smoothing parameter estimate score function parameters
; Option for calculating score function
scoreopt = keyword_set( scoreopt ) or keyword_set( nscore ) $
           or keyword_set( minscore ) or keyword_set( maxscore )
; Score function size (resolution)
if not( keyword_set( nscore ) ) then nscore = 19
; Minimum and maximum score function input values
if not( keyword_set( minscore ) ) then minscore = 0.1
if not( keyword_set( maxscore ) ) then maxscore = 1.

; Bandwidth estimation option
bandwidthopt = keyword_set( bandwidthopt )

; No plot option
noplotopt = keyword_set( noplotopt )

; No verbose option
noverboseopt = keyword_set( noverboseopt )

;***********************************************************************
; Calculation Set-up

; Define X-axis vector
if n_elements( xrange ) eq 2 then begin
  ; Pre-defined limits
  xmin = xrange[0]
  xmax = xrange[1]
endif else begin
  ; Convenient limits
  ;xmin = -interval_calc( -x )
  ;xmax = interval_calc( x )
  temp = choose_levels( x )
  xmin = min( temp )
  xmax = max( temp )
endelse
xid = findgen( xnpdf ) / ( xnpdf - 1. ) * ( xmax - xmin ) + xmin

; Define Y-axis vector
if dim eq 2 then begin
  if n_elements( yrange ) eq 2 then begin
    ; Pre-defined limits
    ymin = yrange[0]
    ymax = yrange[1]
  endif else begin
    ; Convenient limits
    ;ymin = -interval_calc( -y )
    ;ymax = interval_calc( y )
    temp = choose_levels( y )
    ymin = min( temp )
    ymax = max( temp )
  endelse
  yid = findgen( ynpdf ) / ( ynpdf - 1. ) * ( ymax - ymin ) + ymin
endif else begin
  ; Convenient format of calculation
  yid = 0 * fltarr( xnpdf )
endelse

; Pilot smoothing parameter estimate (Silverman, 1986, p.86-87)
; This assumes a multivariate Gaussian distribution.
;hp = ( stddev( x, /nan )^2 + stddev( y, /nan )^2 ) / dim $
;     * (4. / (2. * dim + 1.) / nxdata)^(1. / (dim + 4.))
hp = sqrt( stddev( x, /nan )^2 + stddev( y, /nan )^2 ) / dim $
     * ( 4. / ( 2. * dim + 1. ) / nxdata )^( 1. / ( dim + 4. ) )

; Normalisation factor
;normfact = 1. / ( nxdata * (sqrt(2. * pi) * hp)^dim )
normfact = 1. / ( nxdata * ( sqrt( 2. * pi ) )^dim )
; Area weighting for complete normalisation
area = xid[1] - xid[0]
if dim eq 2 then area = area * ( yid[1] - yid[0] )

; Initialise PDF array
pdf = fltarr( xnpdf, ynpdf )

;***********************************************************************
; PDF Pilot Estimate

; Pilot estimate of PDF
for i = 0, ynpdf - 1 do begin
  for j = 0, xnpdf - 1 do begin
    pdf[j,i] = total( exp( -0.5 * ( ( xid[j] - x )^2 $
                                    + ( yid[i] - y )^2 ) / hp^2 ) )
  endfor
endfor

; Normalise PDF
pdf = pdf / total( pdf ) * area
;pdf = pdf * normfact / hp^dim

;***********************************************************************
; Estimate Bandwidth (ETA)

; Initialise bandwidth modifier
eta = 1 + 0 * fltarr( nxdata )

; Calculate bandwidth modifier if desired
if bandwidthopt then begin

  ; X-axis data position index vector
  xval = intarr( nxdata )
  for i = 0, nxdata - 1 do begin
    dummy = where( abs( x[i]-xid ) eq min( abs( x[i]-xid ) ) )
    xval[i] = dummy(0)
  endfor

  ; Y-axis data position index vector
  yval = intarr( nxdata )
  if dim eq 1 then begin
    yval[*] = 0
  endif else begin
    for i = 0, nxdata - 1 do begin
	  dummy = where( abs( y[i]-yid ) eq min( abs( y[i]-yid ) ) )
      yval[i] = dummy(0)
    endfor
  endelse

  ; Calculate bandwidth (ETA)
  temp = 0.
  for j = 0, nxdata - 1 do temp = temp + alog10( pdf[xval[j],yval[j]] )
  temp = 10.^( temp / nxdata )
  for i=0,nxdata - 1 do eta[i] = 1. / sqrt( pdf[xval[i],yval[i]] / temp )

  ; Clear variables
  xval = 0
  yval = 0

endif

;***********************************************************************
; Calculate Smoothing Parameter Value

if scoreopt then begin

  ; Score function input values
  hvec = findgen( nscore ) / ( nscore - 1. ) * ( maxscore - minscore ) $
         + minscore

  ; Initialise score function
  mh = fltarr( nscore )

  ; Calculate smoothing parameter score function
  for h = 0, nscore - 1 do begin

    ; Calculate PDF
    for i = 0, ynpdf - 1 do begin
      for j = 0, xnpdf - 1 do begin
        pdf[j,i] = total( 1. / eta^dim $
            * exp( -0.5 * ( ( xid[j] - x )^2 + ( yid[i] - y )^2 ) $
            / ( eta * hvec[h] )^2 ) )
      endfor
    endfor
    ; Normalise PDF
    pdf = pdf / total( pdf ) * area
    ;pdf = pdf * normfact / hvec[h]^dim

    ; Evaluate PDF error using bootstrap method
    sumpdfcv = 0.
    idvec0 = indgen( nxdata )
    for i = 0l, nxdata - 1 do begin
      idvec = idvec0[ where( idvec0 ne i ) ]
      eta1 = eta[idvec]
      sumpdfcv = sumpdfcv + total( 1. / eta1^dim $
          * exp( -0.5 * ( ( x[i] - x[idvec] )^2 + ( y[i] - y[idvec] )^2 ) $
          / ( eta1 * hvec[h] )^2 ) )
    endfor
    sumpdfcv = sumpdfcv * normfact / hvec[h]^dim
    intpdf = total( pdf^2 * area )

    ; Calculate score function value
    mh[h] = intpdf - 2. / nxdata * sumpdfcv

  endfor

  ; Choose optimal smoothing parameter
  id = ( where( mh eq min(mh) ) )[0]
  h0 = (hvec[id])[0]
  ; Warning messages if program suspects minimum outside range
  if not( noverboseopt ) then begin
    if ( id eq 0 ) or ( id eq nscore-1 ) then begin
      print, 'WARNING:  Smoothing parameter (' + str(h0) $
             + ') outside of specified range.'
    endif
  endif

  ; Output
  fscore = [ [hvec], [mh] ]
  ; Clear variables
  hvec = 0
  mh = 0
  idvec = 0
  idvec0 = 0

endif else begin

  ; Adopt pilot smoothing parameter estimate
  h0 = hp
  ; Output
  fscore = h0

endelse

;***********************************************************************
; Final PDF Estimate

; Estimate PDF
if scoreopt or bandwidthopt then begin

  ; Estimate PDF
  for i = 0, ynpdf - 1 do begin
    for j = 0, xnpdf - 1 do begin
      pdf[j,i] = total( 1. / eta^dim $
          * exp( -0.5 * ( ( xid[j] - x )^2 + ( yid[i] - y )^2 ) $
          / ( eta * h0 )^2 ) )
    endfor
  endfor

  ; Normalise PDF
  ;pdf = pdf * normfact / h0^dim
  pdf = pdf / total( pdf ) * area

endif

; Clear variables
eta = 0

;***********************************************************************
;Plot Set-up

if not( noplotopt ) then begin

  ; Contour plot settings
  if dim eq 2 then begin
    ; Number of contour levels
    if not( keyword_set( nlevels ) ) then nlevels = 29
    ; Contour levels
    levels = findgen( nlevels ) / ( nlevels - 1. ) * max( pdf )
    ; Colour contour levels
    if keyword_set( fillopt ) then begin
      c_colors = indgen( nlevels ) + 2
    endif else begin
      c_colors = !p.color + 0 * intarr( nlevels )
    endelse
  endif

endif

;***********************************************************************
; Plot PDF

if not( noplotopt ) then begin

  if dim eq 1 then begin

    ; One dimensional PDF
    plot, xid, pdf, psym=10, xstyle=1, charsize=charsize, color=charcolor, $
          xrange=xrange, yrange=yrange, title=title, xtitle=xtitle, $
          ytitle=ytitle

  endif else begin

    ; Two dimensional PDF
    contour, pdf, xid, yid, levels=levels, c_colors=c_colors, $
             cell_fill=fillopt, /isotropic, charsize=charsize, xrange=xrange, $
             yrange=yrange, xstyle=1, ystyle=1, color=color, title=title, $
             xtitle=xtitle, ytitle=ytitle

  endelse

endif

;***********************************************************************
; The End

return
END
