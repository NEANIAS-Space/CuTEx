;+
; NAME:
;	CPAR_OLS
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This function computes 1-D and m-D parametric confidence intervals 
;	from ordinary least squares regression.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	Result = cpar_ols( Betatl, Covb2s, T_crit, Npoint )
;
; INPUTS:
;	Betatl:  A vector of best fit regression coefficients.  Of length MIV.
;	Covb2s:  An array containing the estimated covariance.  Of size 
;		MIV*MIV.
;	Npoint:  The number of points to sample along an isopleth of 
;		probability when estimating the multivariate probability 
;		density function of the scaling factors.  See B_DIST, P_AREA, 
;		P_DIST, and W_DIST for more information.  The function revises 
;		the value down to 2 if MIV=1, where MIV is the number of 
;		regression coefficients, or otherwise to NP_FIX^(MIV-1) where 
;		NP_FIX is the largest integer such that the revised NPOINT is 
;		less than or equal to the original value.  An isopleth of 
;		probability is an MIV dimensional ellipsoid and is sampled by 
;		the NPOINT points in a polar coordinate system.
;	T_crit:  A vector of critical points on the t-distribution at which to 
;		estimate the confidence intervals on the estimates of the 
;		regression coefficients (betas).  Of length NTHOLD.
;
; KEYWORD PARAMETERS:
;	B1DIM:  Returns the locations of the quantiles of the one dimensional 
;		probability distributions of the regression coefficients 
;		(betas).  The t-values of the quantiles to sample are defined 
;		in BT1DIM.  Returns an array of size [NB1DIM,MIV] where MIV is 
;		the number of regression coefficients and NB1DIM is the size 
;		of BT1DIM.  Input from BT1DIM and D1DIM is required.
;	BT1DIM:  A vector of size NB1DIM containing the t-distribution values 
;		of the quantiles at which to sample the one dimensional 
;		probability distributions of the regression coefficients 
;		(betas).
;	C1DIM:  An array of size 3*N1DIM containing the best ([0,*]), lower 
;		confidence range ([1,*]), and upper confidence range ([2,*]) 
;		estimates of the one dimensional attributable components given 
;		the hypothetical parameter combinations in D1DIM.  N1DIM is 
;		the number of parameter combinations to examine.
;	D1DIM:  An array of parameter values for which to return (in C1DIM) 
;		the one dimensional confidence intervals of the attributable 
;		component.  Of size MIV*N1DIM where MIV is the number of 
;		parameters and N1DIM is the number of parameter combinations 
;		to consider.
;	DOUBLE:  If set then calculations are done in double precision 
;		arithmetic.  The default is single precision.
;	NDOFN:  Obsolete keyword retained for continuity.
;	P_AREA:  Returns a vector containing the fraction of the total surface 
;		of an isopleth taken by each of the NPOINT points on the 
;		isopleth.  Of length NPOINT.  The total area of the isopleth 
;		is total(P_AREA)=1.
;	PV_WAVE:  If set, the procedure will work under PV-WAVE.  The default 
;		is for IDL.
;	Z_POSS:  An array containing the values of the independent and 
;		dependent variables.  Of size LDP*(MIV+1) where LDP is the 
;		length of the dependent variable and MIV is the number of 
;		independent variables (and so regression coefficients).  
;		Elements [*,0:MIV-1] contain the values of the independent 
;		variables, while elements [*,MIV] contain the values of the 
;		dependent variable.
;		If the above is given as input, this returns an array 
;		containing the estimated locations (values) of the probability 
;		density estimates of the values of the estimate noise free 
;		independent and dependent variables.  Of size 
;		[LDP,MIV+1,Npoint,NTHOLD], where Npoint is the number of 
;		points to use to sample along an isopleth of probability and 
;		NTHOLD is the number of isopleths as definied in T_crit.  
;		Elements [*,0:MIV-1,J,K] correspond to the estimated values of 
;		the independent variables X corresponding to Result[*,J,K], 
;		that is the Jth sampled point on the (K+1) dimensional 
;		confidence interval surface.  Elements [*,MIV,J,K] correspond 
;		to the estimated values of the independent variable Y 
;		corresponding to Result[*,J,K].  Also see Result, Npoint, and 
;		T_crit.
;
; OUTPUTS:
;	Result:  Returns an MIV*NPOINT*NTHOLD array containing the coordinates 
;		of the isopleth of probability surfaces of the estimates of 
;		the regression coefficients.  MIV is the number of regression 
;		coefficients, NPOINT is the sample size on a surface, and 
;		NTHOLD is the number of isopleth surfaces.  Value [I,J,K] 
;		gives the coordinate along the direction of regression 
;		coefficient I of the sampling point J on the (K+1) dimensional 
;		confidence interval surface.  The confidence intervals to use 
;		are defined in T_crit.
;	B1DIM, P_AREA, Z_POSS
;
; USES:
;	svdpvw.pro
;
; PROCEDURE:
;	This function calculates the singular value decomposition (SVD) of the 
;	estimated covariance matrix COVB2S.
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
;	Modified:	MRA, 1999-08-02 (Revise to return npoint as no. of 
;			points on C-intvls; v1.1)
;	Modified:	MRA, 1999-08-09 (Include Z_poss keyword; v1.3)
;	Modified:	MRA, 2000-08-03 (Allow threshold-type prior 
;			constraints; v2.0)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in routine library)
;	Modified:	DAS, 2005-03-13 (added PV_WAVE keyword;  updated 
;			compliance with svdpvw.pro;  updated documentation)
;	Modified:	DAS, 2005-04-06 (allowed Npoint to be a long integer)
;	Modified:	DAS, 2005-09-01 (Added B1DIM, BT1DIM, DOUBLE keywords; 
;			fixed bug in Z_POSS initialisation;  updated 
;			documentation;  v3.0 of Optimal Detection Package)
;-

function cpar_ols, $
	betatl,covb2s,T_crit,npoint,$
	d1dim=d1dim,C1dim=C1dim,Z_poss=Z_poss,P_area=P_area, $
	PV_WAVE=pv_waveopt, $
	B1DIM=b1dim, BT1DIM=bt1dim, $
	NDOFN=ndofn, $
	DOUBLE=doubleopt

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: function cpar_ols
;
; Description:
; computes 1 and m-dimensional parametric conf. intervals from ols regression
;
; Method:
; svd of estimated covariance matrix, covb2s
; Indices follow standard maths notation: i.e. (row,column)
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
; 1.1	02/08/99	Revise to return npoint as no. of points on C-intvls
; 1.3	09/08/99	Include Z_poss keyword
; 2.0	03/08/00	Allow threshold-type prior constraints
;
; Code Description: IDL / PV-WAVE
;
; Category: 		Function
;
; Classification keywords: regression
;
; Calling sequence: Cintvl=cpar_ols(betatl,covb2s,T_crit,npoint)
;
; Example call: 	Cintvl=cpar_ols(betatl,covb2s,T_crit,npoint,$
; 					 ndofn=ndofn,d1dim=d1dim,C1dim=C1dim)
;
; Inputs:
; 		arg1:		betatl = m-rank vector of best-fit reg. coeffs
; 		arg2:		covb2s = mxm array estimated covariance
;		arg3:		T_crit = m-rank vector of critical points on 1-D to
;                            m-D intervals
; 		arg4:		npoint = no. of points required on ellipsoids
;							 returned as np_fix^(miv-1), np_fix integer
;
; Optional Inputs:	None
;
; Keywords:
; ndofn  = d.o.f. of noise used to estimate covb2s (0 for infinite)
; npoint = no. of points to plot on confidence ellipses
; d1dim  = m*n1dim array: directions in which 1-D confidence intervals are required
; C1dim  = 3*n1dim array: best-guess, low and high scaling factors in dirn. d1dim
; Z_poss = entered as values of predictors and observations, ldp*(m+1) array
;          Z_poss(*,0:m-1) = predictors (independent variables)
; 		   Z_poss(*,m) = observations (dependent variable)
;		   returned as possible values of noise-free Z
;          ldp*(m+1)*(npoint^(m-1))*m array
;          Z_poss(*,0:m-1,j,k)=independent variables, X
;          Z_poss(*,m,j,k)=reconstructed obs, y_poss=X#Cintvl(*,j,k)
; P_area = fltarr(npoint)
;          P_area(j) = fraction of hypersurface area corresponding to jth point
;
; Outputs:			Updated keywords & function value
;
; Optional Outputs: None
; Return Value:    	Cintvl = m*(npoint^(m-1))*m array of points on confidence intervals
;                   Cintvl(*,j,k) = coordinates of jth point on (k+1)-D confidence interval
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
pi = !pi
if keyword_set( doubleopt ) then begin
  doubleopt = 1
  one = 1.d
  pi = !dpi
endif else begin
  doubleopt = 0
endelse

; The number of regression coefficients inputted
miv = n_elements( betatl )

; generate points on the surface of a unit miv-sphere
; first check no. of points requested is consistent with dimensionality of surface
if (miv eq 1) then begin
 if (npoint ne 2) then print,'Only 2 points required for 1-D intervals - resetting'
 npoint=2
 np_fix=npoint
endif else if (miv eq 2) then begin
 np_fix=npoint
endif else if (miv gt 2) then begin
 np_fix=fix(npoint^(1./(miv-1)))
 nprevd = ( one * float( np_fix ) ) ^ ( miv - 1 )
 if (nprevd ne npoint) then print,'No. of points revised to:',nprevd
 npoint=nprevd
endif

x = one * fltarr( miv, npoint )
if ( keyword_set( P_area ) ) then P_area = replicate( one, npoint )

; the next few lines generates a set of lat-long points on a miv-sphere
if (miv eq 1) then begin
 x(0,*) = [ -one, one ]
endif else begin
 np = one * fltarr( miv )
 for m = 1, miv do np(m-1) = ( one * float( np_fix ) ) ^ ( m - one )
 z1 = exp( complex(0,1) * pi * findgen( np_fix ) / np_fix )
 z1(np_fix/2:np_fix-1)=-z1(np_fix/2:np_fix-1)
 z2 = exp( complex( 0, 1 ) * 2. * pi * findgen ( np_fix ) / np_fix )
 x0 = transpose( [ [ real_part( z2 ) ], [ imaginary( z2 ) ] ] )
 x(0:1,0:np_fix-1)=x0
 for m=2, miv-1 do begin
  indx0=indgen(np(m-1))
  for n=1, np_fix-1 do begin
   indxn=n*np(m-1)+indx0
   x(m,indxn)=imaginary(z1(n))
   x(0:m-1,indxn) = total( real_part( z1(n) ) ) * x(0:m-1,indx0)
   if (keyword_set(P_area)) then begin
     P_area(indxn) = abs( total( real_part( z1(n) ) ) ) * P_area(indx0)
   endif
  endfor
 endfor
endelse

; make the elements of P_area sum to unity
if (keyword_set(P_area)) then P_area=P_area/total(P_area)

; find the number of thresholds
nthold=n_elements(T_crit)

; initialise output array
Cintvl = one * fltarr( miv, npoint, nthold )

; initialise array Z_poss if keyword set
if (keyword_set(Z_poss)) then begin
  ldp=n_elements(Z_poss)/(miv+1)
  ; store input Z_poss (original values) in Z_orig
  Z_orig=reform(Z_poss,ldp,miv+1)
  ; Initialise Z_POSS (bug fix in last dimension by DAS)
  Z_poss = one * fltarr( ldp, miv+1, npoint, nthold )
endif

; compute 1-dimensional confidence intervals
if (keyword_set(d1dim)) then begin
 n1dim=n_elements(d1dim)/miv
 C1dim = one * fltarr( 3, n1dim )
 for n=0, n1dim-1 do begin
; compute best-guess value
  C1dim(0,n)=transpose(d1dim(*,n))#betatl
; compute uncertainty range
  d1=T_crit(0)*sqrt(transpose(d1dim(*,n))#covb2s#d1dim(*,n))
; compute min and max
  C1dim(1,n)=C1dim(0,n)-d1
  C1dim(2,n)=C1dim(0,n)+d1
 endfor
endif

; Compute 1 dimensional distributions (DAS addition)
if ( keyword_set( bt1dim ) ) then begin
  ; Initialise output of distribution values
  b1dim = one * fltarr( n_elements( bt1dim ), miv )
  ; Iterate through independent variables
  for i = 0, miv - 1 do begin
    ; Compute locations of the quantiles of the distribution
    b1dim[*,i] = betatl[i] - $
        bt1dim * ( sqrt( transpose( d1dim[*,n1dim-miv+i] ) $
        # covb2s # d1dim[*,n1dim-miv+i] ) )[0]
  endfor
endif

; compute nthold m-dimensional confidence intervals
svdpvw, covb2s, w, u, v, unsort=1, pv_wave=pv_waveopt, double=doubleopt
; Set u to sqrt(covb2s)
for k=0, miv-1 do u(*,k)=u(*,k)*sqrt(w(k))
for m=1, nthold do begin
 d2=T_crit(m-1)
 Cintvl(*,*,m-1)=d2*(u#x)+rebin(betatl,miv,npoint)
 if (keyword_set(Z_poss)) then begin
  for j = 0l, npoint - 1l do begin
   Z_poss(*,0:miv-1,j,m-1)=Z_orig(*,0:miv-1)
   Z_poss(*,miv,j,m-1)=Z_orig(*,0:miv-1)#Cintvl(*,j,m-1)
  endfor
 endif
endfor

return,Cintvl
end
