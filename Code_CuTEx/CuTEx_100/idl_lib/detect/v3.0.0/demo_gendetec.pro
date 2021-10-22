;+
; NAME:
;	DEMO_GENDETEC
;
; PURPOSE:
;	This function is a simple example demonstration of how to use the 
;	GENDETEC Optimal Detection Package.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	DEMO_GENDETEC
;
; INPUTS:
;	Nscen:  The number of scenarios (independent variables in the 
;		regression) to use.  The default is 3.
;
; KEYWORD PARAMETERS:
;	-
;
; OUTPUTS:
;	-
;
; USES:
;	betas_hist.pro
;	gendetec.pro
;
; PROCEDURE:
;	This function creates a multiple linear regression problem and then 
;	has GENDETEC.pro solve it.
;	Let us say we have an observed quantity YOBS.  We think that YOBS is 
;	the weighted linear sum of NSCEN scenarios plus some Gaussian 
;	"noise".  So 
;	YOBS = YSCEN # BETAREAL + AMPEPSILON * EPSILON.  
;	Y_OBS is our observations of length NT.  BETAREAL is a vector 
;	containing the NSCEN regression coefficients.  YSCEN is a matrix of 
;	size NT*NSCEN containing the NT values for each scenario.  
;	AMPEPSILON * EPSILON is the residual from this regression model, which 
;	we suppose to be distributed as a Gaussian EPSILON of standard 
;	deviation AMPEPSILON.  Now if our scenarios YSCEN are estimated from 
;	some other model, then there is noise there too which we can reduce by 
;	sampling NSCENSAMP times and averaging.
;
; REFERENCES:
;	Allen, M. R., and P. A. Stott.  2003.  Estimating signal amplitudes in 
;		optimal fingerprinting. Part I: theory.  Climate Dynamics, 21, 
;		477-491.
;	Stott, P. A., M. R. Allen, and G. S. Jones.  2003.  Estimating signal 
;		amplitudes in optimal fingerprinting. Part II: application to 
;		general circulation models.  Climate Dynamics, 21, 493-500.
;	Stone, D. A., and M. R. Allen.  2004.  The end-to-end attribution 
;		problem: from emissions to impacts.  Climatic Change, 
;		submitted.
;
; EXAMPLE:
;	DEMO_GENDETEC
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28
;	Modified:	DAS, 2005-03-15 (added PDF estimation)
;	Modified:	DAS, 2005-09-01 (majorly expanded PDF plotting;  added 
;			Nscen input;  v3.0 of Optimal Detection Package)
;-

PRO DEMO_GENDETEC, $
	Nscen

;***********************************************************************
; Constants

; Initialise the random seed
;seed=1

; Length of time series
nt = 100
; Number of scenarios (polynomial or sinusoidal functions)
if not( keyword_set( nscen ) ) then nscen = 3
; Number of samples of each scenario
nscensamp = 3 + round( randomu( seed, nscen ) * 4 )
; Number of control samples for estimating noise
nctrl = 90
; Number of independent control samples for use in significance testing
nctrlind = 80

; Actual regression coefficient values
betareal = randomu( seed, nscen )
; Amplitude of noise in the variable
ampepsilon = 0.1 + 0.2 * randomu( seed )
; Amplitude of the signals in the variable
ampsignal = 0.1 + randomu( seed, nscen )

; The time step interval
dt = 0.01

; The p-value for significance tests
plimit = 0.1
; The truncation in the optimisation process
;trunc = 10
trunc = nctrl
; The number of points (~ 1/resolution) to plot in the 1-D PDFs
nhist = 100
; Initialise probability density function output
bdist = 1.
wdist = 1
; The number of isopleths of probability to sample
pdist = 100
; The number of points to sample along each isopleth of probability.
; Note this will probably be revised down a little by gendetec
npoint = 50l ^ ( nscen - 1 )
; The number of quantiles to evenly sample along the one dimensional 
; distributions of the regression coefficients
baxis1dim = nhist

; The partitioning of the noise between the scenarios.
scennoise = 1. / nscensamp

;***********************************************************************
; Initialise Variables

; Initialise scenarios (YSCEN0 is noise free, YSCEN has noise)
yscen0 = fltarr( nt, nscen )
yscen = yscen0
; Iterate through scenarios
for i = 0, nscen - 1 do begin
  ;; Create polynomial signal of order (i+1)
  ;yscen0[*,i] = ampsignal[i] * ( dt * findgen( nt ) ) ^ ( i + 1. )
  ; Create sinusoidal signal of wavenumber (i+1)
  yscen0[*,i] = ampsignal[i] $
      * sin( findgen( nt ) / nt * 2. * !pi * ( i + 1. ) )
endfor
; Add scenario's noise as sum of noise from NSCENSAMP samples
for i = 0, nscen - 1 do begin
  yscen[*,i] = yscen0[*,i] $
      + ampepsilon * randomn( seed, nt ) / sqrt( nscensamp[i] )
endfor

; Create observations with noise added
yobs = yscen0 # betareal + ampepsilon * randomn( seed, nt )

; Create control time series for estimating noise
yctrl = ampepsilon * randomn( seed, nt, nctrl )
; Create more control samples for use in significance testing
yctrlind = ampepsilon * randomn( seed, nt, nctrlind )

; Create a noise free version of the YOBS which we will use as an 
; "another" scenario
d1dim = transpose( yscen0 )

;***********************************************************************
; Estimate Regression Coefficients

; Run GENDETEC.pro to estimate the regression coefficients from the data we
; have produced.
; The output is delivered as BETAEST.  BETAEST[*,0] are the best guess 
; values.  The PLIMIT/2 and (1-PLIMIT/2) confidence limits are given by 
; BETAEST[*,1] and BETAEST[*,2] respectively. 
; Because we expect no covariance between noise estimates, let's not include 
; optimisation (no_opt=1).
gendetec, yobs, yscen, yctrl, betaest, rssq, plimit=plimit, xnoise=scennoise, $
    ctlind=yctrlind, trunc=trunc, ols=0, npoint=npoint, b_dist=bdist, $
    p_dist=pdist, w_dist=wdist, baxis1dim=baxis1dim, b1dim=b1dim, $
    presid=presid, no_opt=1, z_best=zbest, z_poss=zposs, cintvl=cintvl, $
    double=1, d1dim=d1dim, c1dim=c1dim

;***********************************************************************
; Compare Results to Truth

; Print output
print, 'The actual regression coefficients are:'
print, betareal
print, 'The estimated coefficients are:'
print, betaest[*,0]
print, 'The ' + string( plimit ) + '-level confidence intervals are:'
print, betaest[*,1]
print, betaest[*,2]
print, 'The number of samples used to estimate each scenario were:'
print, nscensamp
print, 'The significance level of the residuals in the regression is:'
if presid[0] gt 0.5 then begin
  print, 1-presid
endif else begin
  print, presid
endelse

;***********************************************************************
; Plot the PDFs of the First Two Scaling Parameters

; Plot the 2-D multivariate PDF of the first two regression coefficients.  
; Note this may be messy if NSCEN is large because the NPOINT samples we are 
; taking of the NSCEN dimensional probability volume may not be a good enough 
; sampling.
if nscen gt 1 then begin
  ; Estimate the 2-D PDF
  pdf = betas_hist( bdist[0:1,*,*], wdist, histaxis=histaxis )
  ; Contour plot the result
  contour, pdf, histaxis[*,0], histaxis[*,1], nlevels=20, xtitle='beta1', $
      ytitle='beta2', isotropic=1, title='2-D PDF of first two betas'
  ; Wait for prompt to continue
  tempstr = ''
  read, 'Press enter to continue...', tempstr
endif

;***********************************************************************
; Plot 1-D PDFs of the Scaling Parameters (Regression Coefficients)

; Plot the 1-D PDFs of each of the regression coefficients.
; There are two ways of doing this so we will use two windows
!p.multi = [ 0, 1, 2 ]
; Initialise PDF array information
bhist = fltarr( nhist, nscen )
bhistaxis = fltarr( nhist, nscen )
; Iterate through scenarios
for i = 0, nscen - 1 do begin
  ; Estimate the 1-D PDF
  temp1 = 0
  temp = betas_hist( bdist[i,*,*], wdist, histaxis=temp1, nhist=nhist )
  bhist[*,i] = temp
  bhistaxis[*,i] = temp1
endfor
; Plot the distributions
plot, bhistaxis[*,0], bhist[*,0], nodata=1, yrange=[0,max(bhist)], $
    xrange=[min(bhistaxis),max(bhistaxis)], $
    title='PDFs of betas (method 1)', ytitle='Probability', xtitle='beta value'
tek_color
for i = 0, nscen - 1 do oplot, bhistaxis[*,i], bhist[*,i], color=2+i

; Unfortunately, as NSCEN gets larger then NPOINT starts not becoming enough 
; to fully sample the NSCEN dimensional space and we end up with some oddly 
; shaped estimated PDFs.  An alternative is to get the optimal fingerprinting 
; package to do all of these calculations at a lower level.
; Plot the distributions
bhistaxis1d = ( b1dim[0:nhist-2,*] + b1dim[1:nhist-1,*] ) / 2.
bhist1d = b1dim[0:nhist-2,*]
for i = 0, nscen - 1 do begin
  bhist1d[*,i] = ( baxis1dim[1:nhist-1] - baxis1dim[0:nhist-2] ) $
      / ( b1dim[1:nhist-1,i] - b1dim[0:nhist-2,i] )
endfor
plot, bhistaxis1d[*,0], bhist1d[*,0], nodata=1, yrange=[0,max(bhist1d)], $
    xrange=[min(bhistaxis1d),max(bhistaxis1d)], $
    title='PDFs of betas (method 2)', ytitle='Probability', xtitle='beta value'
for i = 0, nscen - 1 do oplot, bhistaxis1d[*,i], bhist1d[*,i], color=2+i

; Wait for prompt to continue
tempstr = ''
read, 'Press enter to continue...', tempstr

;***********************************************************************
; Plot the Time Series

; Initialise the plot
yrange = [ min( [ [yobs], [yscen], [zbest] ], max=temp ), temp ]
plot, yobs, nodata=1, yrange=yrange, xstyle=1, xtitle='The time series'
; The YSCEN series are in colour
for i = 0, nscen - 1 do oplot, yscen[*,i], color=2+i
; YOBS is in thick gray
oplot, yobs, thick=5, color=14
; Best guess of YOBS is in thin white
oplot, zbest[*,nscen]

; Plot the confidence interval on the estimate of YOBS.
; Initialise the plot
plot, yobs, nodata=1, yrange=yrange, xstyle=1, $
    title='Confidence intervals on guess of YOBS (blue)', $
    xtitle='CI for a noise free version of YOBS in red'
; Plot the confidence interval in blue
temp = fltarr( nt, 2 )
for i = 0, nt - 1 do begin
  temp[i,0] = min( zposs[i,nscen,*,0] )
  temp[i,1] = max( zposs[i,nscen,*,0] )
endfor
oplot, temp[*,0], color=4
oplot, temp[*,1], color=4
; Plot the confidence interval from a noise free version of YOBS in red
oplot, c1dim[1,*], color=2
oplot, c1dim[2,*], color=2

;***********************************************************************
; The End

!p.multi = 0

stop
return
END
