;+
; NAME:
;	BETAS_HIST
;
; PURPOSE:
;	This function produces n-dimensional histograms of the beta scaling 
;	factors from the GENDETEC Optimal Detection Package.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	Result = BETAS_HIST( B_dist, W_dist )
;
; INPUTS:
;	B_dist:  An array containing the location (beta values) of the 
;		probability density estimates of the beta scaling parameters.  
;		Of size [NSCN,NPOINT,NDIST] where NSCN is the number of 
;		scenarios, NPOINT is the number of points sampled along an 
;		isopleth of probability, and NDIST is the number of isopleths 
;		sampled.  So elements [*,j,k] are the coordinates of the jth 
;		point on the kth isopleth of probability.
;	W_dist:  An array containing the weighting estimates of the beta 
;		scaling parameters at the locations in B_dist.  Note these are 
;		not actual density estimates because B_dist contains an 
;		irregular sampling (polar).  Of size [NPOINT,NDIST], so 
;		elements [j,k] are the fraction of the PDF at the jth point on 
;		the kth isopleth of probability.
;
; KEYWORD PARAMETERS:
;	DHIST:  A vector containing the resolution of the histogram along 
;		each scenario.  Of length [NSCN] (the number of scenarios).  
;		A default can be used and returned by the function.
;	HISTAXIS:  An array containing the values to sample for each scenario. 
;		Of size [NHIST,NSCN].  A default can be used and returned by 
;		the function.
;	HISTRANGE:  An array containing the range of values to sample for each 
;		scenario.  Of size [2,NSCN], where the first element is the 
;		minimum and the second element is the maximum.  A default can 
;		be used and returned by the function.
;	NHIST:  The number of values to sample along each scenario.  The 
;		default is 100.
;
; OUTPUTS:
;	Result:  An array containing the histogram estimator of the 
;		probability density of the scaling factors.  Of NSCN 
;		dimensions, each with NHIST elements.  The corresponding axes 
;		are in HISTAXIS.
;	DHIST, HISTAXIS, HISTRANGE, NHIST
;
; USES:
;	-
;
; PROCEDURE:
;	This function converts the information in B_dist into coordinates 
;	in the Return array, and copies the W_dist values into these 
;	coordinates in the Return array.
;
; EXAMPLE:
;	See DEMO_GENDETEC.
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2005-03-13
;	Modified:	DAS, 2005-03-16 (implemented faster 1-D algorithm, 
;			fixed normalisation bug)
;	Modified:	DAS, 2005-03-18 (implemented a much, much faster 
;			algorithm;  v3.0 of Optimal Detection Package)
;-

FUNCTION BETAS_HIST, $
	B_dist, W_dist, $
	DHIST=dhist, HISTAXIS=histaxis, HISTRANGE=histrange, NHIST=nhist

;***********************************************************************
; Constants

; Number of scenarios (dimensions) included in the input
nscn = n_elements( b_dist[*,0,0] )
; The total number of points on all isopleths of probability
ndist = n_elements( b_dist[0,*,*] )

; Range to sample for each scenario.
; Check if ranges were inputted
if keyword_set( histrange ) then begin
  ; Check that the right number of ranges were given
  if n_elements( histrange ) ne nscn * 2 then stop
  ; Reform to range-scenario format
  histrange = reform( histrange, 2, nscn )
  ; A reminder to not update this range later
  check = 0
; If ranges were not inputted but HISTAXIS was inputted
endif else if keyword_set( histaxis ) then begin
  ; Initialise range array
  histrange = fltarr( 2, nscn )
  ; Iterate through scenarios
  for i = 0, nscn - 1 do begin
    ; Determine a range for this scenario
    histrange[*,i] = [ min( histaxis, max=temp ), temp ]
  endfor
  ; A reminder to not update this range later
  check = 0
endif else begin
  ; Initialise range array
  histrange = fltarr( 2, nscn )
  ; Iterate through scenarios
  for i = 0, nscn - 1 do begin
    ; Determine a range for this scenario
    histrange[*,i] = [ min( b_dist[i,*,*], max=temp ), temp ]
  endfor
  ; A reminder to update this range later
  check = 1
endelse
; The default number of points in each dimension of our histogram of the betas
if not( keyword_set( nhist ) ) then begin
  if keyword_set( histaxis ) then begin
    nhist = n_elements( histaxis[*,0] )
  endif else begin
    nhist = 100
  endelse
endif
; The beta sampling step size for each scenario
if not( keyword_set( dhist ) ) then begin
  dhist = reform( histrange[1,*] - histrange[0,*] ) / 1. / nhist
endif
; An update on the histogram axis ranges if automatic
if check eq 1 then begin
  ; Update to ensure extreme points are included (numerically)
  histrange[0,*] = histrange[0,*] - dhist / 2.
  histrange[1,*] = histrange[1,*] + dhist / 2.
endif
; Create an array of beta values to sample in our histogram
if not( keyword_set( histaxis ) ) then begin
  histaxis = fltarr( nhist, nscn )
  for i = 0, nscn - 1 do begin
    histaxis[*,i] = findgen( nhist ) / nhist $
        * ( histrange[1,i] - histrange[0,i] ) + histrange[0,i] + dhist[i] / 2.
  endfor
endif

;***********************************************************************
; Estimate the Histogram

; Reform the NSCN dimensional distributional information so that all 
; distributional information is in a single dimension for each scenario
bdist = reform( b_dist, nscn, ndist )
bdist = transpose( bdist )
wdist = reform( w_dist, ndist )

; Initialise our histogram
hist = fltarr( nhist ^ nscn )

; Iterate through scenarios
for i = 0, nscn - 1 do begin
  ; Convert bdist values to indices
  bdist[*,i] = ( bdist[*,i] - histaxis[0,i] ) $
      / ( histaxis[nhist-1,i] - histaxis[0,i] ) * ( nhist - 1 )
endfor
; Retain only points inside of the axis range
for i = 0, nscn - 1 do begin
  id = where( ( bdist[*,i] gt -0.5 ) and ( bdist[*,i] lt nhist - 0.5 ) )
  bdist = bdist[id,*]
endfor
ndist = n_elements( bdist[*,0] )
; Convert BDIST values to indices of HIST
bdist = round( bdist )
bdist = ( nhist ^ indgen( nscn ) ) ## bdist
; Iterate through distributional values
for i = 0l, ndist - 1l do begin
  ; Add this weighting to our histogram
  id = bdist[i]
  hist[id] = hist[id] + wdist[i]
endfor

; Reform histogram to NSCN-dimensional array
hist = reform( hist, intarr( nscn ) + nhist )
; Normalise the histogram
hist = hist / product( dhist )

;***********************************************************************
; The End

return, hist
END
