;+
; NAME:
;	LINMOD
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This function performs multiple linear regression given an optional 
;	prewhitening operator, externally specified noise realisation, and 
;	optional noise on the independent variables.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	Result = linmod( X, Y )
;
; INPUTS:
;	X:  A floating point matrix of size L*MIV array containing L values 
;		of MIV independent variables.
;	Y:  A floating point vector of size L containing L values of the 
;		dependent variable.
;
; KEYWORD PARAMETERS:
;	B_DIST:  Returns an array containing the location (beta values) of the 
;		probability density estimates of the beta scaling parameters.  
;		Of size [MIV,NPOINT,NDIST] where MIV is the number of 
;		scenarios, NPOINT is the number of points sampled along an 
;		isopleth of probability, and NDIST is the number of isopleths 
;		sampled as definied in T_DIST.  So elements [*,j,k] are the 
;		coordinates of the jth point on the kth isopleth of 
;		probability.  Initialise to 1 to ensure output.  See NPOINT, 
;		P_AREA, and T_DIST for more information.
;	B1DIM:  Returns the locations of the quantiles of the one dimensional 
;		probability distributions of the betas (the regression 
;		coefficients).  The t-values of the quantiles to sample are 
;		defined in BT1DIM.  Returns an array of size [NB1DIM,MIV] 
;		where NSCN is the number of scenarios and NB1DIM is the size 
;		of BT1DIM.  Input from BT1DIM and D1DIM is required.
;	BETAUN:  An MIV*N matrix containing estimates of the MIV coefficients 
;		estimated from each of the N different noise realisations in 
;		UNOISE added to the dependent variable Y.
;	BLEACH:  A matrix containing a pre-whitening operator for the input 
;		invariables.  Of size NDOFD*L, where NDOFD is the number of 
;		degrees of freedom (truncation) to be used and L is the length 
;		of Y.
;	BT1DIM:  A vector of size NB1DIM containing the t-distribution values 
;		of the quantiles at which to sample the one dimensional 
;		probability distributions of the betas (the regression 
;		coefficients).
;	C1DIM:  Returns an array of size 3*N1DIM containing the best ([0,*]), 
;		lower confidence range ([1,*]), and upper confidence range 
;		([2,*]) estimates of the one dimensional attributable 
;		components of Y given the hypothetical parameter combinations 
;		in D1DIM.  N1DIM is the number of parameter combinations to 
;		examine.
;	CINTVL:  Returns an MIV*NPOINT*MIV array containing the coordinates of 
;		the isopleth of probability surfaces of the estimates of the 
;		regression coefficients.  MIV is the number of regression 
;		coefficients and NPOINT is the sample size on a surface.  
;		Value [I,J,K] gives the coordinate along the direction of 
;		regression coefficient I of the sampling point J on the (K+1) 
;		dimensional confidence interval surface.  The confidence 
;		intervals to use are defined in T_crit.  If Result[*,J,K]=0 
;		then the confidence interval is unbounded (possible with TLS).
;	COLOUR:  Returns the pseudo-inverse of BLEACH.
;	COVB2S:  An MIV*MIV matrix containing the estimated covariance of the 
;		noise realisations in UNOISE in the directions of the MIV 
;		scenario patterns found in the MIV independent variables of X.
;	D1DIM:  An array of parameter values (as in X) for which to return (in 
;		C1DIM) the one dimensional confidence intervals of the 
;		attributable component of Y scaled according to the beta 
;		scaling parameters.  Of size MIV*N1DIM where MIV is the 
;		number of parameters and N1DIM is the number of parameter 
;		combinations to consider.  Values can be within or outside of 
;		the range of values in X.
;	DOUBLE:  If set then calculations are done in double precision 
;		arithmetic.  The default is single precision.
;	ESTVAR:  If set, then the covariance estimates are scaled by dividing 
;		by the residual sum of squares (RSSQ).
;	FTRANS:  If OLS is set, then this returns the transpose of the matrix 
;		of fingerprint patterns.  Note that Result=Ftrans#Y, to within 
;		the EOF space covered to the truncation NDOFD.  Of size MIV*L 
;		where MIV is the number of dependent variables and L is the 
;		length of Y.  If TLS is being used, this returns an unknown 
;		quantity.
;	NDOFD:  The number of degrees of freedom of the pre-whitened data.  
;		This corresponds to the truncation in the EOF space used.
;	NDOFN:  Obsolete keyword retained for continuity.
;	NONPAR:  Obsolete keyword.
;	NPOINT:  The number of points to sample along an isopleth of 
;		probability when estimating the multivariate probability 
;		density function of the scaling factors.  See B_DIST, P_AREA, 
;		and T_DIST for more information.  The default is 2 if MIV=1 
;		(the problem is 1 dimensional), otherwise NP_FIX^(MIV-1) when 
;		MIV>1, where MIV is the number of independent variables.  An 
;		isopleth of probability is an MIV dimensional ellipsoid and is 
;		sampled by the NPOINT points in a polar coordinate system.
;	OBSVAR:  Not yet supported.
;	OLS:  If set the function uses the ordinary least squares (OLS) 
;		algorithm.  The default is to use the total least squares 
;		(TLS) algorithm.  This option overrides the use of XNOISE.
;	P_AREA:  Returns a vector containing the fraction of the total surface 
;		of an isopleth taken by each of the NPOINT points on the 
;		isopleth.  Of length NPOINT.  The total area of the isopleth 
;		is total(P_AREA)=1.  See B_DIST and NPOINT for more 
;		information.
;	PV_WAVE:  If set, this procedure can run under the PV_WAVE language.
;		The default is for it to run under IDL instead.
;	RSSQ:  Returns the sum of squares of the residual differences between 
;		the prewhitened dependent variable Y and the best regression 
;		estimate.  If UNOISE is given, then the normalised residuals 
;		are used, with the normalisation done for each element of Y 
;		according to the standard deviation of the corresponding N 
;		realisations of that element in UNOISE.
;	STATUS:  Returns 0 if the function has completed without problems, and 
;		returns 1 otherwise.
;	T_CRIT:  A vector of critical points on one dimensional to NSCN 
;		dimensional t-distributions at which to estimate the 
;		confidence intervals on the estimates of the betas (regression 
;		coefficients).  Of length NSCN, the number of independent 
;		scenarios.
;	T_DIST:  A vector of length NDIST containing the T-statistic values on 
;		the isopleths of probability to sample when estimating the 
;		probability density of the regression coefficients.  See 
;		B_DIST, NPOINT, and P_AREA for more information.
;	UNOISE:  A floating point matrix of size L*N containing N independent 
;		noise realisations of the dependent variable Y (and so of 
;		length L) for use in estimating confidence intervals.
;	XNOISE:  A floating point array of length MIV.  It contains the 
;		fraction of the variance of the noise in Y contained in each 
;		of the MIV variables in X.  Required for the TLS method, so if 
;		not defined then the procedure defaults to the OLS method.
;	XTILDE:  Returns an array containing the best guess values of noise 
;		free X.  The array is of size [L,MIV].  While the estimations 
;		are done in truncated EOF space, the results are projected 
;		back into normal space in XTILDE.  But because we only use the 
;		leading NDOFD EOFs, the guesses will look progressively worse 
;		the smaller NDOFD is compared to L.
;	YTILDE:  Returns a vector containing the best guess values of noise 
;		free Y.  The vector is of length L.  While the estimations 
;		are done in truncated EOF space, the results are projected 
;		back into normal space in YTILDE.  But because we only use the 
;		leading NDOFD EOFs, the guesses will look progressively worse 
;		the smaller NDOFD is compared to L.
;	Z_POSS:  Returns an array containing the estimated locations (values) 
;		of the probability density estimates of the values of the 
;		estimated noise free Scn and Obs.  Of size 
;		[L,MIV+1,NPOINT,MIV] where L is the number of values in Y, MIV 
;		is the number of independent variables in X, and NPOINT is the 
;		number of points to use to sample probability distribution.  
;		Elements [*,0:MIV-1,J,K] correspond to the estimated values of 
;		of X corresponding to CINTVL[*,J,K], that is the Jth sampled 
;		point on the (K+1) dimensional confidence interval surface.  
;		Elements [*,MIV,J,K] correspond to the estimated values of Y 
;		corresponding to CINTVL[*,J,K].  Also see CINTVL and NPOINT.
;
; OUTPUTS:
;	Result:  A vector of length NSCN containing the best guess of the 
;		regression coefficients (betas), where NSCN is the number of 
;		independent variables.
;	B_DIST, B1DIM, BETAUN, C1DIM, CINTVL, COLOUR, COVB2S, FTRANS, P_AREA, 
;	RSSQ, STATUS, XTILDE, YTILDE, Z_POSS
;
; USES:
;	cpar_ols.pro
;	cpar_tls.pro
;	regols.pro
;	regtls.pro
;	svdpvw.pro
;
; PROCEDURE:
; 	This function fits the model
;		y = (X+U_x)b - u_y,
;	where
;		\expect{u_y u_y^T} = C_N
;		C_N^{-1} = P^T P,
;	where P is a "pre-whitening" operator,
;	so
;		\expect{P u_y u_y^T P^T} = I_n'
;	(i.e. P u_y = z is unit variance white noise)
;	and
;		\expect{P U_x S^{-2} U_x^T P^T} = m * I_n',
;	where
;		S(k,k)^2=xnoise(k).
;	It uses either regols.pro if xnoise(*)=0 or regtls.pro otherwise.
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
;	Modified:	MRA, 1999-08-02 (Bug-fix on rssq MRA, bug spotted by 
;			PAS; v1.1)
;	Modified:	MRA, 1999-08-09 (Z_poss keyword implemented; v1.3)
;	Modified:	MRA, 2000-08-03 (B_dist keyword implemented: explicit 
;			output of PDFs; v2.0)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in routine library)
;	Modified:	DAS, 2005-03-13 (Updated documentation, added PV_WAVE 
;			keyword, updated compliance to svdpvw.pro, 
;			cpar_ols.pro, cpar_tls.pro)
;	Modified:	DAS, 2005-05-13 (Updated documentation)
;	Modified:	DAS, 2005-09-01 (Added B1DIM, BT1DIM, DOUBLE keywords; 
;			changed the default NPOINT; updated documentation;  
;			v3.0 of Optimal Detection Package)
;-

function linmod, $
	X,y,Bleach=Bleach,Unoise=Unoise,Obsvar=Obsvar,$
	estvar=estvar,xnoise=xnoise,nonpar=nonpar,ndofd=ndofd,$
	Xtilde=Xtilde,ytilde=ytilde,rssq=rssq,Ftrans=Ftrans,$
	T_crit=T_crit,Cintvl=Cintvl,npoint=npoint,Z_poss=Z_poss,$
	B_dist=B_dist,T_dist=T_dist,P_area=P_area,$
	BetaUn=BetaUn,covb2s=covb2s,ols=ols,d1dim=d1dim,C1dim=C1dim,$
	status=status,colour=colour, $
	PV_WAVE=pv_waveopt, $
	B1DIM=b1dim, BT1DIM=bt1dim, $
	NDOFN=ndofn, $
	DOUBLE=doubleopt

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: function linmod
;
; Description:
; Performs multiple linear regression given an optional
; prewhitening operator, externally-specified
; noise realisation and optional noise on indep. variables.
; Uniform weighting with no missing values
; (absorb weighting in prewhitening operator if desired)
;
; Method: fits the model
; y = (X+U_x)b - u_y where \expect{u_y u_y^T} = C_N
; C_N^{-1} = B^T B, so \expect{B u_y u_y^T B^T} = I_n'
; i.e. B u_y = z is unit variance white noise
; and \expect{B U_x S^{-2} U_x^T B^T} = m * I_n' where S(k,k)^2=xnoise(k)
; using either regols.pro if xnoise(*)=0. or regtls.pro otherwise
; No regression constant: if required, input 1s in jth column of X
; and set xnoise(j)=0. or (numerically better) subtract all means
; before calling and work it out manually.
; Indices follow standard maths notation: i.e. (row,column)
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
; 1.1   02/08/99	Bug-fix on rssq MRA, bug spotted by PAS
; 1.3   09/08/99	Z_poss keyword implemented
; 2.0   03/08/00	B_dist keyword implemented: explicit output of PDFs
;
; Code Description: IDL / PV-WAVE
;
; Category: 		Function
;
; Classification keywords: detection, pre-whitening
;
; Calling sequence: betatl=linmod(X,y) performs standard ols regression
;
; Example call: 	bobs1=linmod(X,y,Bleach=Bleach,Unoise=Unoise,obsvar=obsvar,$
; 					 estvar=estvar,xnoise=xnoise,nonpar=nonpar,ndofn=ndofn,ndofd=ndofd,$
; 					 Xtilde=Xtilde,ytilde=ytilde,rssq=rssq,Ftrans=Ftrans,$
; 					 T_crit=T_crit,Cintvl=Cintvl,npoint=npoint,$
; 					 BetaUn=BetaUn,Covb2s=Covb2s,ols=ols,d1dim=d1dim1,C1dim=C1dim1,status=status)
; Inputs:
; 		arg1:		X = l*m array of independent variable values
; 		arg2:		y = l-rank vector of dependent variable values
;
; Optional Inputs:	None
;
; Keywords:
; Bleach = prewhitening operator (e.g. W_N^{-1} V_N^T where W and V
;          are noise EOFs and singular values respectively
; Unoise = l*n array of p independent noise realisations for conf. ints.
; estvar = if set, use residual ssq to scale all variance estimates
; xnoise = if set, xnoise(k) = expected variance(BX_k)/variance(By)
; nonpar = if set, use non-parametric confidence intervals based on Unoise
; ndofn  = no. degrees of freedom of noise estimate (-> inf. if not set)
; ndofd  = no. degrees of freedom of prewhitened data (rank of Bleach)
; Xtilde = l*m array, predicted values of X in best-fit model
; ytilde = l-rank vector, predicted values of y in best-fit model
; rssq   = prewhitened residual sum of squares
; Ftrans = Transposed fingerprint matrix, or operator used to obtain betatl
; T_crit = m-rank vector of critical points on 1-D to m-D confidence intervals
; Cintvl = m*npoint*m array of confidence intervals
; Cintvl(*,j,k) = coordinates of jth point on (k+1)-D interval
; npoint = number of points on ellipsoids (defaults to 2 if m=1, np_fix^(m-1) otherwise)
; Z_poss = returned as possible values of noise-free Z
;          ldp*(m+1)*(npoint^(m-1))*m array
;          Z_poss(*,0:m-1,j,k)=best-fit values of predictors, X,
;          corresponding to Cintvl(*,j,k)
;          Z_poss(*,m,j,k)=best-fit values of observations, y,
;          corresponding to Cintvl(*,j,k)
; B_dist=fltarr(nscn,npoint,ndist) = PDF of estimators
; B_dist(*,j,k)=coordinates of jth point on kth isopleth of probability
; P_area=fltarr(npoint)
; P_area(j)=fraction of area on isopleths corresponding to jth point
; T_dist=fltarr(ndist)=T-statistic on requested isopleths of probability
; BetaUn = coefficient values estimated from Unoise
; covb2s = if xnoise not set, m*m array of covariances of coefficients
;        = if xnoise set, (m+1)*(m+1) array of covs of normalised coeffs
;          (for advanced users only -- see regrma documentation)
; ols    = use ordinary least squares algorithm even if xnoise is set
; d1dim  = m*n1dim array = parameter combins. on which uncertainties are reqd.
; C1dim  = 3*n1dim array = best, lower and upper estimates of composite parameters
; status = 0 if routine completed OK, 1 otherwise
; colour = pseudo-inverse of Bleach.
;
; Outputs:			Updated keywords & function value
;
; Optional Outputs: None
; Return Value:    	betatl = m-rank vector of regression coefficients
; Common Blocks: 	None
; Side Effects: 	None known
; Restrictions:
;-
;;; Just ignore the next four lines
author_name = '$Author: higal_repository $'
date_name = '$Date: 2013/09/17 08:36:37 $'
version_name = '$Revision: 1.1.1.1 $'

; Option for double precision (DAS)
one = 1.
if keyword_set( doubleopt ) then one = 1.d

; ldp=number of datapoints
ldp=n_elements(X(*,0))
; miv=number of independent variables
miv=n_elements(X(0,*))
; nnr=number of noise realisations
if (keyword_set(Unoise)) then nnr=n_elements(Unoise)/ldp else nnr=0
; set default d.o.f. of Unoise to nnr
if (not keyword_set(ndofn)) then ndofn=nnr
; initialise estvar
if (not keyword_set(estvar)) then estvar=0.
; initialise Obsvar
if (not keyword_set(Obsvar)) then Obsvar=0.
; set default T_crit to sqrt(m)
if (not keyword_set(T_crit)) then T_crit=sqrt(findgen(miv)+1.)
; set default null-hypothesis to zero
if ( not( keyword_set( b_null ) ) ) then b_null = one * fltarr( miv )
; set default no. of points on Cintvl
if (not keyword_set(npoint)) then begin
  if ( miv eq 1 ) then begin
    npoint = 2
  endif else if ( miv eq 2 ) then begin
    npoint = 32767
  endif else begin
    npoint = fix( 32767. ^ ( 1. / ( miv - 1 ) ) )
  endelse
endif
; initialise BXtilde and bytilde if necessary
if (keyword_set(Xtilde)) then BXtilde=1.
if (keyword_set(ytilde)) then Bytilde=1.
if (keyword_set(Ftrans)) then BFtrans=1.
if (keyword_set(d1dim)) then n1dim=n_elements(d1dim)/miv else n1dim=0

; Prewhitening step:
if (keyword_set(Bleach)) then begin
; set number of degrees of freedom of data to rank of Bleach
 ndofd=n_elements(Bleach)/ldp
 if (ndofd gt ldp) then stop,'Bleach operator cannot be rank',ndofd
 By=Bleach#y
 BX=Bleach#X
 if (keyword_set(Unoise)) then BUnoise=Bleach#Unoise else BUnoise=0.
 if (keyword_set(Obsvar)) then BObsvar=Bleach#Obsvar#transpose(Bleach) $
                          else BObsvar=0.
 if (keyword_set(Xtilde) or keyword_set(ytilde) or keyword_set(Z_poss) or keyword_set(colour)) then begin
; Colour operator = pseudo-inverse of Bleach
  svdpvw, Bleach, Q, P, R, unsort=1, pv_wave=pv_waveopt, double=doubleopt
  for k=0, ndofd-1 do begin
   if (Q(k) le 0.) then stop,'Rank-deficient prewhitening operator'
   P(*,k)=P(*,k)/Q(k)
  endfor
  Colour=R#transpose(P)
 endif
endif else begin
 ndofd=ldp
 By=y
 BX=X
 if (keyword_set(Unoise)) then BUnoise=Unoise else BUnoise=0.
endelse

if (keyword_set(xnoise) and not keyword_set(ols)) then begin

; Check all elements of xnoise are non-zero
 for m=0, miv-1 do begin
  if (xnoise(m) lt 1.e-5) then begin
   print,'Warning: xnoise input with zeros - resetting to 1e-5',xnoise
   xnoise(m)=1.e-5
  endif
  sx=sqrt(xnoise(m))
; scale i.v.s by xnoise
  BX(*,m)=BX(*,m)/sx
; scale directions for 1-D confidence intervals by xnoise
  if (keyword_set(d1dim)) then d1dim(m,*)=d1dim(m,*)/sx
 endfor
 if (keyword_set(Cintvl)) then begin
; request eigenvector and eigenvalue information from regrma
  Evects=1.
  Evalus=1.
 endif
; Perform RMA regression on prewhitened data and scaled i.v.s
 betatl=regtls(BX,By,Unoise=BUnoise,Obsvar=BObsvar,$
               estvar=estvar,Xtilde=BXtilde,ytilde=Bytilde,rssq=rssq,$
               Ftrans=BFtrans,BetaUn=BetaUn,covb2s=covb2s,$
               Evects=Evects,Evalus=Evalus,status=status, double=doubleopt )
; Compute confidence intervals using Evects and Evalus information
; Store original values in Z_poss if required
 if (keyword_set(Z_poss)) then Z_poss=transpose([transpose(BX),transpose(By)]) $
  else Z_poss=0.
 t1dim=0.
 if keyword_set( Cintvl ) or keyword_set( d1dim ) or keyword_set( Z_poss) $
     or keyword_set( bt1dim ) then begin
   Cintvl = cpar_tls( betatl, Evects, Evalus, T_crit, npoint, $
       d1dim=d1dim, C1dim=C1dim, t1dim=t1dim, Z_poss=Z_poss, $
       pv_wave=pv_waveopt, b1dim=b1dim, bt1dim=bt1dim, double=doubleopt )
 endif
 if (keyword_set(B_dist)) then $
  B_dist=cpar_tls(betatl,Evects,Evalus,T_dist,npoint,P_area=P_area, $
      pv_wave=pv_waveopt, double=doubleopt )
; rescale everything by xnoise
 for m=0, miv-1 do begin
; recompute scaling factor
  sx=sqrt(xnoise(m))
; reconstruction multiplied by sx since original data divided by sx
  if (keyword_set(Xtilde)) then BXtilde(*,m)=BXtilde(*,m)*sx
  if (keyword_set(Z_poss)) then Z_poss(*,m,*,*)=Z_poss(*,m,*,*)*sx
; estimates divided by sx, to correspond to rescaled data
  betatl(m)=betatl(m)/sx
  if (keyword_set(B_dist)) then B_dist(m,*,*)=B_dist(m,*,*)/sx
  if (keyword_set(BetaUn)) then BetaUn(m,*)=BetaUn(m,*)/sx
; rows and columns of covariance estimates divided by sx
  if (keyword_set(covb2s)) then begin
   covb2s(*,m)=covb2s(*,m)/sx
   covb2s(m,*)=covb2s(m,*)/sx
  endif
; rows of Cintvl matrix divided by sx (scaled as betatl)
  if (keyword_set(Cintvl)) then Cintvl(m,*,*)=Cintvl(m,*,*)/sx
  if (keyword_set(d1dim)) then d1dim(m,*)=d1dim(m,*)*sx
 endfor

endif else begin

; Perform standard OLS regression on prewhitened data
 betatl=regols(BX,By,Unoise=BUnoise,Obsvar=BObsvar,$
               estvar=estvar,ytilde=Bytilde,rssq=rssq,$
               Ftrans=BFtrans,BetaUn=BetaUn,covb2s=covb2s,status=status, $
     double=doubleopt )
 BXtilde=BX
 if (keyword_set(xnoise)) then begin
; inflate rows and columns of covb2s to account for noise in BX (fudge)
  covb2s=covb2s*(sqrt(1.+xnoise)#transpose(sqrt(1.+xnoise)))
; deflate residual by mean noise in BX (bigger fudge)
; tests are applied as if this noise is not present
  rssq=rssq/(1.+total(xnoise)/miv)
 endif
; compute confidence intervals
; Store original values in Z_poss if required
 if (keyword_set(Z_poss)) then Z_poss=transpose([transpose(BX),transpose(By)]) $
  else Z_poss=0.
 if keyword_set( Cintvl ) or keyword_set( d1dim ) $
     or keyword_set( bt1dim ) then begin
   Cintvl = cpar_ols( betatl, covb2s, T_crit, npoint, $
       d1dim=d1dim, C1dim=C1dim, Z_poss=Z_poss, pv_wave=pv_waveopt, $
       b1dim=b1dim, bt1dim=bt1dim, double=doubleopt )
 endif
 if (keyword_set(B_dist)) then $
  B_dist=cpar_ols(betatl,covb2s,T_dist,npoint,P_area=P_area, $
      pv_wave=pv_waveopt, double=doubleopt )
endelse

; Compute best-fit X, y and Fingerprint matrix in original coordinates
if (keyword_set(Bleach)) then begin
 if (keyword_set(ytilde)) then ytilde=Colour#Bytilde
 if (keyword_set(Xtilde)) then Xtilde=Colour#BXtilde
 if (keyword_set(Ftrans)) then Ftrans=BFtrans#Bleach
 if (keyword_set(Z_poss)) then begin
  Z_poss=reform(Z_poss,ndofd,(miv+1)*npoint*miv)
  Z_poss=Colour#Z_poss
  Z_poss=reform(Z_poss,ldp,miv+1,npoint,miv)
 endif
endif else begin
 if (keyword_set(ytilde)) then ytilde=Bytilde
 if (keyword_set(Xtilde)) then Xtilde=BXtilde
 if (keyword_set(Ftrans)) then Ftrans=BFtrans
endelse

return,betatl
end
