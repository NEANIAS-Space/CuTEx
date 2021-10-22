;+
; NAME:
;	GENDETEC
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This procedure computes confidence intervals on undetermined model
;	parameters by multiple regression using observational constraints.  
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	gendetec, Obs, Scn, Ctl [, Bobs] [, Rssq]
;
; INPUTS:
;	Ctl:  An array of dimensions IMX by NCT1 containing the IMX 
;		spatio-temporal model data for each of the NCT1 control 
;		simulations.  This is for estimating a pre-whitening 
;		transformation.  The spatio-temporal format is identical to Obs
;		and Scn.
;	Obs:  A vector of length IMX containing the spatio-temporal 
;		observations, with no missing data allowed.
;	Scn:  An array of dimensions IMX by NSCN containing the IMX 
;		spatio-temporal model data for each of the NSCN scenario 
;		simulations.  The spatio-temporal format is identical to Obs
;		and Ctl.
;
; KEYWORD PARAMETERS:
;	B_DIST:  Returns an array containing the location (beta values) of the 
;		probability density estimates of the beta scaling parameters.  
;		Of size [NSCN,NPOINT,NDIST] where NSCN is the number of 
;		scenarios, NPOINT is the number of points sampled along an 
;		isopleth of probability, and NDIST is the number of isopleths 
;		sampled.  So elements [*,j,k] are the coordinates of the jth 
;		point on the kth isopleth of probability.  Initialise to 1 to 
;		ensure output, also initialise W_DIST and input P_DIST.  See 
;		NPOINT, P_DIST, and W_DIST for more information.
;	B1DIM:  Returns the locations of the quantiles of the one dimensional 
;		probability distributions of the betas (the regression 
;		coefficients).  The quantiles to sample are defined in 
;		BAXIS1DIM.  Returns an array of size [NB1DIM,NSCN] where NSCN 
;		is the number of scenarios and NB1DIM is the size of 
;		BAXIS1DIM.  Input from BAXIS1DIM is required.
;	BAXIS1DIM:  A vector of size NB1DIM containing the quantiles at which 
;		to sample the one dimensional probability distributions of the 
;		betas (the regression coefficients).  Values must be in the 
;		(0,1) range.
;	C1DIM:  Returns an array of size 3*N1DIM containing the best ([0,*]), 
;		lower confidence range ([1,*]), and upper confidence range 
;		([2,*]) estimates of the one dimensional attributable 
;		components given the hypothetical scenarios in D1DIM.  N1DIM 
;		is the number of scenario combinations to examine.
;	CINTVL:  Returns an NSCN*NPOINT*NSCN array containing the coordinates 
;		of the isopleth of probability surface of the estimates of 
;		the regression coefficients in various dimensions.  NSCN is 
;		the number of scenarios and NPOINT is the sample size on the 
;		isopleth surface defined in PLIMIT (two sided).  Element 
;		[I,J,K] gives the coordinate along the direction of regression 
;		coefficient I of the sampling point J on the (K+1) dimensional 
;		confidence interval surface.  If CINTVL[*,J,K]=0 then the 
;		confidence interval is unbounded (possible with TLS).
;	COVB:  An NSCN*NSCN matrix containing the estimated covariance of the 
;		noise realisations in CTLIND in the directions of the NSCN 
;		scenario patterns in Scn.  Approximate if TLS is used.
;	CTLIND:  An optional array of dimensions IMX by NCT2 containing IMX 
;		spatio-temporal model data for each of NCT2 control 
;		simulations.  This is for hypothesis testing, independent of 
;		the original noise estimate.  The spatio-temporal format is 
;		identical to Obs, Ctl, and Scn.
;	CTLSVL:  If set, then it returns a vector of length NCT1 containing 
;		the singular values of the input matrix Ctl.
;	D1DIM:  An array of scenarios (as in Scn) for which to return (in 
;		C1DIM) the one dimensional confidence intervals of the 
;		attributable component of Obs scaled by the scalings in Bobs.  
;		Of size NSCN*N1DIM where NSCN is the number of scenarios and 
;		N1DIM is the number of conditions of scenario combinations to 
;		consider.  Values can be within or outside of the range of 
;		values in SCN.
;	DOFCTR:  The number of degrees of freedom in the estimate of the noise 
;		covariance to be used when estimating probability 
;		distributions.  The default is NCT2, the number of independent 
;		noise realisations in CTLIND.
;	DOUBLE:  If set then calculations are done in double precision 
;		arithmetic.  The default is single precision.
;	ESTVAR:  If set, then the covariance estimates are scaled by dividing 
;		by the residual sum of squares (RSSQ).
;	ICOM:  A vector of length NSCN containing indices of various 
;		combinations of coefficients to output.  The format has 
;		ICOM[12,2] meaning that Bobs will return 
;		[Bobs[0]+Bobs[1],Bobs[1]].  Output from other variables is 
;		similarly rotated (not C1DIM though).  Indices must be in the 
;		range 1 to 9 and can only involve addition.
;	NO_OPT:  If set, the procedure does not optimise the fingerprints, but
;		it still projects onto the noise EOFs.  The default is to 
;		optimise the fingerprints.
;	NPOINT:  The number of points to sample along an isopleth of 
;		probability when estimating the multivariate probability 
;		density function of the scaling factors.  An isopleth of 
;		probability is an NSCN dimensional ellipsoid and is sampled by 
;		the NPOINT points in a polar coordinate system.  See B_DIST, 
;		P_DIST, and W_DIST for more information.
;	OLS:  If set, the procedure uses ordinary least squares (OLS) 
;		estimators (still prewhitened but not total least squares 
;		(TLS)).  The default is TLS, but if XNOISE is not defined then 
;		the procedure defaults to OLS.
;	P_DIST:  A vector of length NDIST containing the isopleths of 
;		probability to sample when estimating the probability density 
;		of the scaling parameters.  If a scalar value is entered then 
;		that number of equally spaced isopleths will be sampled over 
;		the (0,1) interval.  See B_DIST, NPOINT, and W_DIST for more 
;		information.
;	PLIMIT:  The two sided p-value for confidence interval estimation.
;	POBS:  If OLS is set, then this returns the transpose of the matrix of 
;		fingerprint patterns.  Note that Bobs[*,0]=Ftrans#Obs, to 
;		within the EOF space covered to the truncation TRUNC.  Of size 
;		NSCN*IMX where NSCN is the number of scenarios in Scn and IMX 
;		is the length of Obs.  If TLS is being used, this returns an 
;		unknown quantity.
;	PRESID:  Returns the P-value of the F or chi-squared test on the 
;		residuals between Obs and the best construction of Obs 
;		obtained from the regression.  If TRUNC is set to a positive 
;		integer then PRESID is a vector of length 1 containing the 
;		value corresponding to the truncation TRUNC;  otherwise PRESID 
;		is a vector of length NTRUNC+1 where PRESID[0] is the value 
;		corresponding to the maximum allowed truncation, while 
;		PRESID[1:NTRUNC] are the values corresponding to the NTRUNC 
;		truncations defined in TRUNC.
;	PREWHI:  An externally specified prewhitening transformation.
;	PV_WAVE:  If set, this procedure can run under the PV_WAVE language.
;		The default is for it to run under IDL instead.
;	RX:  Returns the correlation coefficient between Obs and the 
;		projection of Obs on the control simulation EOFs.
;	RY:  Returns the correlation coefficients between the signal patterns 
;		and the projection of the signal patterns onto the control 
;		simulation EOFs.  A vector of length NSCN (the number of 
;		scenarios).
;	TRUNC:  If set to a positive value, then this is the number of EOFs 
;		of the control simulation to retain in the noise model.  If 
;		it is not set then the maximum truncation for which 
;		PRESID > PLIMIT is calculated and used.  If is set to a 
;		negative value, then the maximum truncation in the range 
;		[1,abs(TRUNC)] for which PRESID > PLIMIT is calculated and 
;		used, and abs(TRUNC) is returned.
;	W_DIST:  Returns an array containing the weighting estimates of the 
;		beta scaling parameters at the locations in B_DIST.  Note 
;		these are not actual density estimates because B_DIST contains 
;		an irregular sampling (polar).  Of size [NPOINT,NDIST], so 
;		elements [j,k] are the fraction of the PDF at the jth point on 
;		the kth isopleth of probability.  Initialise to 1 to ensure 
;		output, also initialise B_DIST and input P_DIST.  See B_DIST, 
;		NPOINT, and P_DIST for more information.
;	WEIGHT:  A floating point vector of length IMX of weights for the
;		spatio-temporal values in the data arrays (Obs, etc.).
;	XNOISE:  A floating point array of length NSCN.  It contains the 
;		fraction of the variance of the noise in Obs contained in each 
;		of the NSCN variables in Scn.  Required for the TLS method, so 
;		if not defined then the procedure defaults to the OLS method.
;	Z_BEST:  Returns an array containing the best guess values of noise 
;		free scenarios and observations.  The array is of size 
;		[IMX,NSCN+1].  Elements [*,0:NSCN-1] contain the guesses for 
;		the scenarios, while elements [*,NSCN] contains the guess for 
;		the observations.  While the estimations are done in truncated 
;		EOF space, the results are projected back into normal space in 
;		Z_BEST.  But because we only use the leading TRUNC EOFs, the 
;		guesses will look progressively worse the smaller TRUNC is 
;		compared to IMX.
;	Z_POSS:  Returns an array containing the estimated locations (values) 
;		of the probability density estimates of the values of the 
;		estimated noise free Scn and Obs.  If size 
;		[IMX,NSCN+1,NPOINT,NSCN] where IMX is the number of values in 
;		Obs, NSCN is the number of scenarios in Scn, and NPOINT is the 
;		number of points to use to sample probability distribution.  
;		Elements [*,0:NSCN-1,J,K] correspond to the estimated values 
;		of SCN corresponding to CINTVL[*,J,K], that is the Jth sampled 
;		point on the (K+1) dimensional confidence interval surface.  
;		Elements [*,NSCN,J,K] correspond to the estimated values of 
;		YOBS corresponding to CINTVL[*,J,K].  Also see CINTVL and 
;		NPOINT.
;
; OUTPUTS:
;	Bobs:  An array of size NSCN*3 containing the best ([*,0]), lower 
;		confidence range ([*,1]), and upper confidence range ([*,2]) 
;		estimates of the projection amplitudes of the observations Obs 
;		onto the various scenarios Scn.
;	Rssq:  Returns the sum of squares of the normalised residual 
;		differences between the prewhitened Obs and the best 
;		regression estimate.  The normalisation is done for each 
;		element of Obs according to the standard deviation of the 
;		corresponding NCT2 realisations of that element in CTLIND.  An 
;		adjustment for the bias in Rssq is made if OLS is used in 
;		order to account for bias.  If scanning over multiple 
;		truncations (see TRUNC) then this returns a vector with 
;		Rssq[0] containing the value when using the maximum allowed 
;		truncation and Rssq[NSCN+1:abs(TRUNC)] containing the values 
;		for truncations of NSCN+1 to TRUNC.
;	B_DIST, B1DIM, C1DIM, CINTVL, COVB, CTLSVL, POBS, PRESID, RX, RY, 
;	TRUNC, W_DIST, Z_BEST, Z_POSS
;
; USES:
;	fcdf.pro
;	invert1k.pro
;	linmod.pro
;	svdpvw.pro
;
; PROCEDURE:
; 	This routine is primarily a driver for linmod.pro, but computes 
;	prewhitening transformations and transformations on input/output 
;	variables if uncertainties on parameter-combinations are required, and 
;	scans for maximum allowable truncation if not specified.
;	@stat_startup is used in PV_WAVE;  when in IDL an error is reported in 
;	compilation that it is missing, but the program still runs happily.
;	More description below.
;
; REFERENCES:
;	Allen, M. R., and S. F. B. Tett.  1999.  Checking internal consistency 
;		in optimal fingerprinting.  Climate Dynamics, 15, 419-434.
;	Allen, M. R., and P. A. Stott.  2003.  Estimating signal amplitudes in 
;		optimal fingerprinting. Part I: theory.  Climate Dynamics, 21, 
;		477-491.
;	Stott, P. A., M. R. Allen, and G. S. Jones.  2003.  Estimating signal 
;		amplitudes in optimal fingerprinting. Part II: application to 
;		general circulation models.  Climate Dynamics, 21, 493-500.
;
; EXAMPLE:
;	See demo_gendetec.pro for a demonstration of the use of the optimal 
;	detection routines.
;
; MODIFICATION HISTORY:
;	Written by:	Myles R. Allen (m.r.allen@rl.ac.uk), 1999-05-15 (v1.0)
;	Modified:	MRA, 1999-08-09 (Z_POSS keyword implemented; v1.3)
;	Modified:	MRA, 1999-08-13 (Name revised for DOS file transfers; 
;			v1.4)
;	Modified:	MRA, 1999-01-11 (Bug in use of weights fixed; v1.5)
;	Modified:	MRA, 2000-08-03 (Explicit PDFs computed; v2.0)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in routine library)
;	Modified:	DAS, 2005-03-13 (updated documentation, compliance 
;			with svdpvw.pro and linmod.pro)
;	Modified:	DAS, 2005-05-13 (Updated documentation)
;	Modified:	DAS, 2005-09-01 (Added B1DIM, BAXIS1DIM, DOUBLE 
;			keywords and related calculations;  updated 
;			documentation;  v3.0 of Optimal Detection Package)
;	Modified:	DAS, 2005-11-01 (Fixed bug which insisted on B_DIST 
;			input)
;-

pro gendetec, $
	obs,scn,ctl,bobs,rssq,pv_wave=pv_wave,$
	ctlind=ctlind,weight=weight,trunc=trunc,Pobs=Pobs,Covb=Covb,$
	dofctr=dofctr,Presid=Presid,prewhi=prewhi,ols=ols,$
	Plimit=Plimit,Cintvl=Cintvl,npoint=npoint,icom=icom,$
	Z_best=Z_best,Z_poss=Z_poss,$
	P_dist=P_dist,B_dist=B_dist,W_dist=W_dist,$
	no_opt=no_opt,ctlsvl=ctlsvl,xnoise=xnoise,d1dim=d1dim,$
	C1dim=C1dim,ry=ry,rX=rX, $
	B1DIM=b1dim, BAXIS1DIM=baxis1dim, $
	ESTVAR=estvar, $
	DOUBLE=doubleopt

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: Program gendetec (was gendetect)
;
; Description:
; Given a vector, obs, of observations containing no missing data,
; a set of similar vectors, scn, from model simulations with changing forcing and a
; control set, ctl, of pure noise, computes confidence intervals on undetermined model
; parameters by multiple regression using either the ordinary (ols) or total (tls)
; least squares algorithm.
; Method: This routine is primarily a driver for linmod.pro, but computes prewhitening
; transformations and transformations on input/output variables if uncertainties
; on parameter-combinations are required, and scans for maximum allowable
; truncation if not specified
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
; 1.3	   09/08/99	Z_poss keyword implemented
; 1.4	   13/08/99	Name revised for DOS file transfers
; 1.5   11/01/99	Bug in use of weights fixed
; 2.0   03/08/00	Explicit PDFs computed
;
; Code Description: IDL / PV-WAVE
;
; Category: 		Program
;
; Classification keywords: detection, pre-whitening
;
; Calling sequence: 5 arguments + keywords
;
; Example call: 	gendetect,obs,scn,ct1,bobs,rssq,$
; 					ctlind=ct2,weight=wgt,trunc=trunc,Pobs=Pobs,Covb=Covb,$
; 					dofctr=dofctr,Presid=Presid,prewhi=prewhi,ols=ols,$
; 					Plimit=Plimit,Cintvl=Cintvl,npoint=npoint,Z_poss=Z_poss,icom=icom,$
; 					no_opt=no_opt,ctlsvl=ctlsvl,xnoise=xnoise,d1dim=d1dim,C1dim=C1dim
;
; Inputs:
; 		arg1:		obs(imx)=observations
; 		arg2:		scn(imx,nscn)=runs with changing forcing: nscn experiments (scenarios)
; 		arg3:		ctl(imx,nct1)=control run for estimating pre-whitening transformation
;
; Optional Inputs:	None
;
; Keywords:
; pv_wave=use PV-wave stats routine names
; ctlind=fltarr(imx,nct2), if set, independent control run for hypothesis-testing
; weight=fltarr(imx) = vector of weights
; trunc=number of EOFs of the control run to retain in the noise model
;  if not set or negative, then max truncation for which Presid > Plimit used
;  if negative, then max truncation considered = abs(trunc), returned as truncation used
; Pobs=fltarr(imx,nscn)=Projection operators used to extract pattern amps from obs in ols
; Covb=fltarr(nscn,nscn)=estimated covariance of bobs (approximate unless ols is used)
; dofctr=degrees of freedom of noise covariance used for uncertainties (input only)
; Presid=P-value of F or chi-squared test on residuals
; Presid(0)=value for max allowed truncation (or only truncation if keyword_set(trunc)
; Presid(nscn+1:trmax)=values for truncations nscn+1:trmax if scanning over truncations
; prewhi=externally specified prewhitening transformation
; ols=use ordinary least squares estimators (still prewhitened but not tls)
; Plimit=P-value for confidence intervals
; Cintvl=fltarr(nscn,npoint,nscn):
;  Cintvl(*,j,k)=coords of jth point in (k+1)-D confidence interval
; npoint=no. of points on nscn-D confidence intervals
; Z_best = best-guess values of noise-free Z, ldp*(m+1) array
; Z_poss = returned as possible values of noise-free Z
;          ldp*(m+1)*npoint*m array
;          Z_poss(*,0:m-1,j,k)=best-fit values of predictors, X,
;          corresponding to Cintvl(*,j,k)
;          Z_poss(*,0:m-1,j,k)=best-fit values of observations, y,
;          corresponding to Cintvl(*,j,k)
; P_dist=fltarr(ndist)=requested isopleths of probability
;      can be entered as scalar = number of equal-spaced isopleths in [0,1] interval
; B_dist=fltarr(nscn,npoint,ndist) = PDF of estimators
; B_dist(*,j,k)=coordinates of jth point on kth isopleth of probability
; W_dist=fltarr(npoint,ndist) = array of weights for PDF
; W_dist(j,k)=fraction of total PDF corresponding to jth point on kth isopleth
; icom=combinations of coefficients to output
; e.g. icom=[12,2] -> output [bobs(0)+bobs(1),bobs(1)]
; no_opt=do not optimise fingerprints (but still project onto noise EOFs)
; ctlsvl=singular values of matrix of control segments
; xnoise=fltarr(nscn)=fraction variance of obs in nscn response-patterns
; d1dim(nscn,n1dim)=directions for 1-D confidence intervals (before combination in icom)
; C1dim(3,n1dim)=mid,min&max points on 1-D intervals
; ry=correlation coefficient between obs before and after projection on control EOFs.
; rX=vector of correlation coefficients of signal patterns before and after projection
; on control EOFs.
;
; Outputs:			Updated keywords &
; 		arg4:		bobs(nscn,3)=vector of best, lower and upper estimates of pattern-amplitudes
; 		arg5:		rssq=weighted residual sum of squares (trunc-nscn d.o.f.)
;       rssq(0)=value for max allowed truncation (or only truncation if keyword_set(trunc)
;       rssq(nscn+1:trmax)=values for truncations nscn+1:trmax if scanning over truncations
; Optional Outputs: None
; Return Value:    	N/A
; Common Blocks: 	None
; Side Effects: 	None known
; Restrictions:
;-
;;; Just ignore the next four lines
author_name = '$Author: higal_repository $'
date_name = '$Date: 2013/09/17 08:36:37 $'
version_name = '$Revision: 1.1.1.1 $'

if (keyword_set(pv_wave)) then begin
 @stat_startup
endif

if (not keyword_set(no_opt)) then no_opt=0

; Option for double precision (DAS addition)
one = 1.
if keyword_set( doubleopt ) then one = 1.d

; use independent estimate of control if available
if ( keyword_set( ctlind ) ) then begin
  ct2 = one * ctlind
endif else begin
  ct2 = one * ctl
endelse
if ( not( keyword_set( weight ) ) ) then begin
  wgt = replicate( one, n_elements( obs ) )
endif else begin
  wgt = one * weight
endelse

; Extract parameters of observational dataset, control and model simulations
s1=size(obs) & s2=size(scn) & s3=size(ctl) & s4=size(ct2) & s5=size(wgt)
if (s1(0) ne 1) then begin print,'obs must be 1-d in gendetect' & stop & end
if (s2(0) ne 2) then begin
  ; Resize to 2-d if 1-d (DAS addition)
  if s2[0] eq 1 then begin
    scn = reform( scn, n_elements( scn ), 1 )
    s2 = size( scn )
  ; Otherwise report error
  endif else begin
    print, 'scn must be 2-d in gendetect'
    stop
    return
  endelse
endif
if (s3(0) ne 2) then begin print,'ctl must be 2-d in gendetect' & stop & end
if (s4(0) ne 2) then begin print,'ct2 must be 2-d in gendetect' & stop & end
if (s5(0) ne 1) then begin print,'wgt must be 1-d in gendetect' & stop & end
; imx=number of datapoints in total in each field
imx=s1(1)
if (s2(1) ne imx) then begin print,'obs and scn incompatible' & stop & end
if (s3(1) ne imx) then begin print,'obs and ctl incompatible' & stop & end
if (s4(1) ne imx) then begin print,'obs and ct2 incompatible' & stop & end
if (s5(1) ne imx) then begin print,'obs and wgt incompatible' & stop & end
; nscn=number of experiments (ensembles) with changing forcing
nscn=s2(2)
; nct1=number of fields in the control model run for prewhitening
nct1=s3(2)
; nct2=number of fields in the control model run for hypothesis-testing
nct2=s4(2)

; multiply everything by wgt
obswgt = one * obs
scnwgt = one * scn
ct1wgt = one * ctl
ct2wgt = one * ct2
for i=0, imx-1 do begin
 obswgt(i)=obs(i)*wgt(i)
 scnwgt(i,*)=scn(i,*)*wgt(i)
 ct1wgt(i,*)=ctl(i,*)*wgt(i)
 ct2wgt(i,*)=ct2(i,*)*wgt(i)
endfor

; Find the EOFs of the control to estimate Bleach, the EOFs of ct1 weighted
; by inverse(sqrt(eigenvalue))
; Use IDL/PV-wave svdpvw program, which is the same as svdc with /column
; except that it transposes rather than zero-padding
; Remember the singular values^2 are nct1*eigenvalues of ct1#transpose(ct1)

if (keyword_set(prewhi)) then begin
; set U to be the left singular vectors of prewhi
 svdpvw, prewhi, W, U, V, pv_wave=pv_wave, double=doubleopt
 for k=0, n_elements(W)-1 do begin
  if (W(k) le 0.) then stop,'Singular prewhitening transformation supplied'
  W(k)=1./W(k)
 endfor
endif else begin
 svdpvw, ct1wgt, W, U, V, pv_wave=pv_wave, double=doubleopt
 W=W/sqrt(nct1)
endelse

; extract control singular values if requested
if (keyword_set(ctlsvl)) then ctlsvl=W

; Apply inverse noise weighting if (no_opt) not set
if (not keyword_set(no_opt)) then for k=0, n_elements(W)-1 do U(*,k)=U(*,k)/W(k)

; compute linear transformation on output
tr_out = one * fltarr( nscn, nscn )
icom_flag=0
if (keyword_set(icom)) then begin
 if (n_elements(icom) ne nscn) then stop,'Inconsistent icom keyword in gendetect:',icom
 for k=0, nscn-1 do begin
; unpack the integers in icom(k)
  for j=0, fix(alog10(icom(k))) do begin
   i=fix(icom(k)/10^j)-10*fix(icom(k)/10^(j+1))
   if (i ne k+1) then icom_flag=1
   if (i eq 0 or i gt nscn) then stop,'Inconsistent icom keyword in gendetect:',icom
   tr_out(i-1,k)=1.
  endfor
 endfor
endif else for k=0, nscn-1 do tr_out(k,k)=1.

; compute 1-D directions in which confidence intervals are required
; set last nscn dirns equal to tr_out
if (keyword_set(d1dim)) then begin
 n1dim=n_elements(d1dim(0,*))
 ndirn=n_elements(d1dim(*,0))
 if (ndirn ne nscn) then begin
  d1dim1 = one * fltarr( nscn, n1dim )
  d1dim1(0:ndirn-1,*)=d1dim
 endif else d1dim1=d1dim
 d1dim1=transpose([transpose(d1dim1),transpose(tr_out)])
endif else begin
 n1dim=0
 d1dim1=tr_out
endelse

; Perform the regression
if (not keyword_set(nonpar)) then nonpar=0.

; Fix bug in v1_4 which didn't pick up the weighted version
X=scnwgt
y=obswgt
if (keyword_set(ctlind)) then Unoise=ct2wgt else Unoise=0.
if (keyword_set(obsvar)) then obsvar=obsvar else obsvar=0.
if (keyword_set(estvar)) then estvar=estvar else estvar=0.
if (keyword_set(xnoise)) then xnoise=xnoise else xnoise=0.
if (keyword_set(dofctr)) then ndofn=dofctr else ndofn=nct2

if (keyword_set(Plimit)) then Plimit=Plimit else Plimit=0.05
; compute critical values of the t- or T-statistic corresponding to Plimit
T_crit = one * fltarr( nscn )
for m=1, nscn do begin
 if (keyword_set(pv_wave)) then $
  T_crit(m-1)=sqrt(m*Fcdf((one-Plimit),m,ndofn,/inverse)) $
 else $
  T_crit(m-1)=sqrt(m*f_cvf(one*Plimit,m,ndofn))
endfor

; Compute critical values of the t- or T-statistic corresponding to the 
; values in baxis1dim (DAS addition)
if keyword_set( baxis1dim ) then begin
  ; If only the size of the axis vector is given then create the axis vector
  if n_elements( baxis1dim ) eq 1 then begin
    baxis1dim = findgen( baxis1dim ) / baxis1dim + 1. / baxis1dim / 2.
  endif
  ; Initialise the vector of critical values
  bt1dim = one * fltarr( n_elements( baxis1dim ) )
  ; Iterate through values in baxis1dim
  for i = 0, n_elements( baxis1dim ) - 1 do begin
    ; For PV_Wave
    if ( keyword_set( pv_wave ) ) then begin
      if baxis1dim[i] gt 0.5 then begin
        bt1dim[i] = sqrt( Fcdf( 2.*one*baxis1dim[i], 1, ndofn, inverse=1 ) )
      endif else begin
        bt1dim[i] = sqrt( Fcdf( 2.*one*(1.-baxis1dim[i]), 1, ndofn, $
            inverse=1 ) )
      endelse
    ; For IDL
    endif else begin
      if baxis1dim[i] gt 0.5 then begin
        bt1dim[i] = -sqrt( f_cvf( 2.*one*(1.-baxis1dim[i]), 1, ndofn ) )
      endif else begin
        bt1dim[i] = sqrt( f_cvf( 2.*one*baxis1dim[i], 1, ndofn ) )
      endelse
    endelse
  endfor
endif

if (keyword_set(B_dist)) then begin
; compute critical values of T-statistic corresponding to P_dist
 if (not keyword_set(P_dist)) then P_dist=100
 if (n_elements(P_dist) eq 1) then begin
  ndist=total(P_dist)
  P_dist = ( 0.5 + one * findgen( ndist ) ) / ndist
 endif else begin
; P_dist must be in ascending order
  P_dist = one * P_dist( sort( P_dist ) )
  ndist=n_elements(P_dist)
  if (P_dist(0) le 0. or P_dist(ndist-1) ge 1.) then stop,'Impossible P-values:',P_dist(0),P_dist(ndist-1)
 endelse
 T_dist = one * fltarr( ndist )
 for k=0, ndist-1 do begin
   if (keyword_set(pv_wave)) then begin
     T_dist(k) = sqrt( nscn * Fcdf( (one-P_dist(k)), nscn, ndofn, /inverse) )
   endif else begin
     T_dist(k) = sqrt( nscn*f_cvf(one*P_dist(k), nscn, ndofn ) )
   endelse
 endfor
endif

; if trunc not set or negative, scan to determine maximum truncation for which
; Presid gt Plimit
; set maximum truncation to rank of pre-whitening operator unless specified otherwise
if (not keyword_set(trunc)) then trmax=n_elements(W) else $
 if (trunc lt 0) then trmax=abs(trunc)<n_elements(W) else trmax=0
; stop if max truncation less than nscn is requested
if (trmax ne 0 and trmax le nscn) then stop,'Impossible truncation specified:',trmax

; initialise arrays to store rssq and Presid
rssq = one * fltarr( trmax+1 )
Presid = fltarr( trmax+1 ) + one

; comment this line in to use default for number of points on ellipses
; npoint=0
; always request the residual sum of squares
rssq_k = one

; make two passes, the first to determine the truncation, only if trmax ne 0
if (trmax gt 0) then np0=0 else np0=1
for np=np0, 1 do begin
 if (np eq 0) then begin
; do not request reconstructions and confidence intervals
  Xtilde=0.
  ytilde=0.
  Ftrans=0.
  d1dimk=0.
  C1dim1=0.
  Cintvl=0.
  BetaUn=0.
  Covb2s=0.
  Z_poss=0.
  trunc_k=nscn+indgen(trmax-nscn)+1
  P_area=0.
  b_dist0 = 0
  bt1dim0 = 0
 endif else begin
; request reconstructions and confidence intervals
  Xtilde=1.
  ytilde=1.
  Ftrans=1.
  d1dimk=d1dim1
  C1dim1=1.
  Cintvl=1.
  BetaUn=1.
  Covb2s=1.
  Z_poss=1.
  trunc_k=[trunc]
  if keyword_set( B_dist ) then b_dist0 = b_dist
  if (keyword_set(B_dist)) then P_area=1.
  if keyword_set( bt1dim ) then bt1dim0 = bt1dim
 endelse

 for k=0, n_elements(trunc_k)-1 do begin
; Compute the pre-whitening transformation
  Bleach=transpose(U(*,0:trunc_k(k)-1))
  ndofd=trunc_k(k)
  bobs1=linmod(X,y,Bleach=Bleach,Unoise=Unoise,obsvar=obsvar,$
   estvar=estvar,xnoise=xnoise,nonpar=nonpar,ndofn=ndofn,ndofd=ndofd,$
   Xtilde=Xtilde,ytilde=ytilde,rssq=rssq_k,Ftrans=Ftrans,$
   T_crit=T_crit,Cintvl=Cintvl,npoint=npoint,Z_poss=Z_poss,$
   B_dist=B_dist0,T_dist=T_dist,P_area=P_area,$
   BetaUn=BetaUn,Covb2s=Covb2s,ols=ols,d1dim=d1dimk,C1dim=C1dim1,$
   status=status,colour=colour, pv_wave=pv_wave, $
   b1dim=b1dim, bt1dim=bt1dim0, double=doubleopt )
; on first pass, store results in rssq(trunc_k(k))
; on second pass, store in rssq(0) and Presid(0)
  if (np eq 0) then k_t=trunc_k(k) else k_t=k
  rssq(k_t)=rssq_k
; evaluate probability of obtaining an rssq this large if noise model adequate
  if (keyword_set(ctlind)) then begin
   if (keyword_set(pv_wave)) then $
    Presid(k_t)=1.-fcdf(rssq_k/(ndofd-nscn),ndofd-nscn,ndofn) else $
    Presid(k_t)=1.-f_pdf(rssq_k/(ndofd-nscn),ndofd-nscn,ndofn)
  endif else begin
   if (keyword_set(pv_wave)) then $
    Presid(k_t)=1.-chisqcdf(rssq_k,ndofd-nscn) else $
    Presid(k_t)=1.-chisqr_pdf(rssq_k,ndofd-nscn)
  endelse
 endfor
; on first pass, find maximum allowable truncation
 if (np eq 0) then begin
  trunc=max(where(Presid gt Plimit,count))
  if (count eq 0) then trunc=trmax
 endif
endfor
;
;NPG - output correlation coefficient between original and projected data.
y=reform(y,imx,1)
ry=transpose(y)#Colour#Bleach#y/$
  sqrt(transpose(y)#Colour#Bleach#Colour#Bleach#y*(transpose(y)#y))
rX=transpose(X)#Colour#Bleach#X/$
  sqrt(transpose(X)#Colour#Bleach#Colour#Bleach#X*(transpose(X)#X))
;Take diagonal elements
rX=rX(indgen(nscn),indgen(nscn))


Pobs=Ftrans
Covb=Covb2s
Bctl=BetaUn
Z_best=transpose([transpose(Xtilde),transpose(ytilde)])

; extract parameter estimates and limits from C1dim1
if (keyword_set(d1dim)) then C1dim=C1dim1(*,0:n1dim-1)
; transformation already included in d1dim
bobs=transpose(C1dim1(*,n1dim:n1dim+nscn-1))

; convert P_area to W_dist, weights on distribution
if keyword_set( b_dist0 ) then b_dist = b_dist0
if (keyword_set(B_dist)) then begin
 P_wgt = one * fltarr( ndist )
 P_wgt(0)=(P_dist(1)+P_dist(0))/2.
 for n=1, ndist-2 do P_wgt(n)=(P_dist(n+1)-P_dist(n-1))/2.
 P_wgt(ndist-1)=1.-(P_dist(ndist-2)+P_dist(ndist-1))/2.
 W_dist=P_area#transpose(P_wgt)
endif

; transform output if necessary
if (icom_flag) then begin
 Covb=transpose(tr_out)#Covb#tr_out
 sz=size(Cintvl)
 Cintvl=reform(Cintvl,sz(1),sz(2)*sz(3))
 Cintvl=transpose(tr_out)#Cintvl
 Cintvl=reform(Cintvl,sz(1),sz(2),sz(3))
 if (keyword_set(B_dist)) then begin
  sz=size(B_dist)
  B_dist=reform(B_dist,sz(1),sz(2)*sz(3))
  B_dist=transpose(tr_out)#B_dist
  B_dist=reform(B_dist,sz(1),sz(2),sz(3))
 endif
 tr_inv = transpose( invert1k( tr_out, status=status, double=doubleopt ) )
 Z_best(*,0:nscn-1)=Z_best(*,0:nscn-1)#tr_inv
 if (keyword_set(Z_poss)) then begin
  for m=0, nscn-1 do begin
   for j=0, npoint-1 do begin
    Z_poss(*,0:nscn-1,j,m)=reform(Z_poss(*,0:nscn-1,j,m))#tr_inv
   endfor
  endfor
 endif
endif

return
end
