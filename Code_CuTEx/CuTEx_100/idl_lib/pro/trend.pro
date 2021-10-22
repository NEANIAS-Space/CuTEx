;+
; NAME:
;	TREND
;
; PURPOSE:
;	This function calculates the least-squares linear trend in the
;	input vector.
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	Result = TREND( [X,] Y )
;
; INPUTS:
;	Y:  The input vector of type integer or floating point and length N.
;
; OPTIONAL INPUTS:
;	X:  An ordinate vector of type integer or floating point.  If
;		not set, Y-elements are positioned at [0,1,2,...,N-1].
;
; KEYWORD PARAMETERS:
;	ALPHA:  The significance level for statistics output.  The
;		default value is 0.05.
;	CTREND:  The critical trend for the calculation of the one-sided
;		significance level of the output trend.  The default
;		is 0.  The significance level is returned in SIGLEV.
;	NAN:  If set, the function ignores NaNs (Not-a-Number) as missing 
;		values if any appear in X or Y.  The default is to return NaN.
;
; OUTPUTS:
;	Result:  Returns the least-squares linear trend of Y(X).
;	CONFINT:  The (1-ALPHA) confidence interval of Result.
;	SIGLEV:  The ALPHA one-sided significance level of the difference
;		between Result and CTREND.
;	FIT:  Returns a vector of the linear least-squares fit values,
;		with X as the ordinate.
;
; USES:
;	-
;
; PROCEDURE:
;	This function uses IDL's POLY_FIT.pro to calculate the least-
;	squares linear trend to Y(X).  It also calculates statistics of
;	the calculated trend value.
;
; EXAMPLE:
;	Create a vector of 20 data elements.
;	  y = randomn( seed, 20 )
;	Calculate the linear trend in y.
;	  tr = trend( y )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-07-12.
;	Modified:	DAS, 2001-04-03 (fixed bug with SIGLEV and CONFINT).
;	Modified:	DAS, 2005-08-05 (replaced SUM.PRO use with TOTAL; 
;			removed CONSTANTS.PRO use; edited coding style; 
;			reversed definition of NAN keyword for consistency 
;			with other routines)
;	Modified:	DAS, 2005-11-23 (fixed bug with NAN option).
;-

;***********************************************************************

FUNCTION TREND, $
	X, Y, $
	ALPHA=alpha, $
	CTREND=ctrend, $
	NAN=nanopt, $
	CONFINT=confint, $
	SIGLEV=siglev, $
	FIT=fit

;***********************************************************************
; Variables, Constants, and Options

; Load constants
nan = !values.f_nan

; Input vector
if n_params() eq 1 then begin
  yvec = x
endif else begin
  yvec = y
endelse
n = n_elements( yvec )

; Ordinate vector
if n_params() eq 1 then begin
  xvec = findgen( n )
endif else begin
  xvec = x
endelse
if n_elements( xvec ) ne n then return, nan

; The significance level
if not( keyword_set( alpha ) ) then alpha = 0.05

; The critical trend value for significance level calculation
if not( keyword_set( ctrend ) ) then ctrend = 0.

; Not-a-Number option
if keyword_set( nanopt ) then begin
  nanopt = 1
endif else begin
  nanopt = 0
endelse

; Statistics output
if arg_present( confint ) then begin
  confintopt = 1
endif else begin
  confintopt = 0
endelse
if arg_present( siglev ) then begin
  siglevopt = 1
endif else begin
  siglevopt = 0
endelse

;***********************************************************************
; Setup

; Remove NaNs if NANOPT is set
if nanopt eq 1 then begin
  ; Search for good values
  id = where( ( finite( yvec ) eq 1 ) and ( finite( xvec ) eq 1 ), nid )
  ; If we have some good values then remove the bad ones
  if nid gt 1 then begin
    xvec = xvec[id]
    yvec = yvec[id]
  ; If we have no good values then return
  endif else begin
    return, nan
  endelse
endif

;***********************************************************************
; Calculate Trend

; Subtract mean (gives better accuracy)
ymean = mean( yvec )
yvec = yvec - ymean
xmean = mean( xvec )
xvec = xvec - xmean

; Calculate trend
ytrend = ( poly_fit( xvec, yvec, 1, fit ) )[1]
fit = fit + ymean

;***********************************************************************
; Statistics

; Calculate confidence interval
if ( confintopt eq 1 ) or ( siglevopt eq 1 ) then begin
  se = sqrt( ( total( ( yvec + ymean - fit ) ^ 2 ) ) / ( n - 2. ) )
  sxx = sqrt( total( xvec ^ 2 ) )
  confint = t_cvf( alpha / 2., n - 2 ) * se / sxx
endif

; Calculate significance level
if siglevopt eq 1 then begin
  t = ( ytrend - ctrend ) * sxx / se
  siglev = 1 - t_pdf( t, n - 2 )
endif

;***********************************************************************
; The End

return, ytrend
END
