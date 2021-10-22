;+
; NAME:
;	CPAR_TLS
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This function computes 1-D and m-D parametric confidence intervals 
;	from total least squares regression.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	Result = cpar_tls( Betatl, Evects, Evalus, T_crit, Npoint )
;
; INPUTS:
;	Betatl:  A vector of best fit regression coefficients.  Of length MIV.
;	Evalus:  A floating point vector of size (MIV+1) containing the MIV+1 
;		eigenvalues of transpose([[X],[Y]])#[[X],[Y]].  Here X is an 
;		L*MIV matrix of MIV independent variables and Y is a vector 
;		containing the dependent variable of length L.
;	Evects:  A floating point matrix of size (MIV+1)*(MIV+1) containing 
;		the MIV+1 eigenvectors of transpose([[X],[Y]])#[[X],[Y]].  
;		Here X is a L*MIV matrix of MIV independent variables and Y is 
;		a vector containing the dependent variable of length L.
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
;	P_AREA:  Returns a vector containing the fraction of the total surface 
;		of an isopleth taken by each of the NPOINT points on the 
;		isopleth.  Of length NPOINT.  The total area of the isopleth 
;		is total(P_AREA)=1.
;	T1DIM:  Unknown.
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
;		are defined in T_crit.  If Result[*,J,K]=0 then the confidence 
;		interval is unbounded.
;	B1DIM, P_AREA, Z_POSS
;
; USES:
;	svdpvw.pro
;	total_1d.pro
;
; PROCEDURE:
;
; REFERENCES:
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
;	Modified:	MRA, 1999-08-02 (Simplify computation of C-intvls; 
;			v1.2)
;	Modified:	MRA, 1999-08-09 (Include Z_poss keyword; v1.3)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in routine library)
;	Modified:	DAS, 2005-03-13 (added PV_WAVE keyword, updated 
;			compliance with svdpvw.pro, updated documentation)
;	Modified:	DAS, 2005-09-01 (Added B1DIM, BT1DIM, DOUBLE keywords; 
;			updated documentation;  v3.0 of Optimal Detection 
;			Package)
;-

function cpar_tls, $
	betatl,Evects,Evalus,T_crit,npoint,$
	d1dim=d1dim,C1dim=C1dim,t1dim=t1dim,Z_poss=Z_poss,P_area=P_area, $
	PV_WAVE=pv_waveopt, $
	B1DIM=b1dim, BT1DIM=bt1dim, $
	DOUBLE=doubleopt

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: function cpar_tls
;
; Description:
; computes 1-D and m-D parametric confidence intervals from TLS regression
;
; Method:
; see techreport
; Indices follow standard maths notation: i.e. (row,column)
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
; 1.1   02/08/99	Revise to return npoint as no. of points on C-intvls
; 1.2   02/08/99	Simplify computation of C-intvls
; 1.3	09/08/99	Include Z_poss keyword
; 2.0	future		Allow threshold-type prior constraints
;
; Code Description: IDL / PV-WAVE
;
; Category: 		Function
;
; Classification keywords: regression
;
; Calling sequence: Cintvl=cpar_tls(betatl,Evects,Evalus,T_crit,npoint)
;
; Example call: 	Cintvl=cpar_tls(betatl,covb2s,T_crit,npoint,$
; 					 d1dim=d1dim,C1dim=C1dim,t1dim=t1dim,Z_poss=Z_poss)
;
; Inputs:
; 		arg1:		betatl = m-rank vector of best-fit reg. coeffs
; 		arg2:		Evects = eigenvectors of transpose([X,y])#[X,y]
; 		arg3:		Evalus = eigenvalues  of ditto
;		arg3:		T_crit = m-rank vector of critical points on 1-D to
;                            m-D intervals
; 		arg4:		npoint = no. of points required on ellipsoids
;							 returned as np_fix^(miv-1), np_fix integer
;
; Optional Inputs:	None
;
; Keywords:
; d1dim  = m*n1dim array: directions in which 1-D confidence intervals are required
; C1dim  = 3*n1dim array: best-guess, low and high scaling factors in dirn. d1dim
; t1dim  = n1dim vector: threshold values of t-statistic, returned as sqrt(rmin)
; Z_poss = entered as values of predictors and observations, ldp*(m+1) array
;          Z_poss(*,0:m-1) = predictors (independent variables)
; 		   Z_poss(*,m) = observations (dependent variable)
;		   returned as possible values of noise-free Z
;          ldp*(m+1)*(npoint^(m-1))*m array
;          Z_poss(*,*,j,k)=best-fit values of predictors and observations
;          corresponding to Cintvl(*,j,k)
; P_area = fltarr(npoint)
;          P_area(j) = fraction of hypersurface area corresponding to jth point
;
; Outputs:			Updated keywords & function value
;
; Optional Outputs: None
; Return Value:    	Cintvl = m*(npoint^(m-1))*m array of points on confidence intervals
; 					Cintvl(*,j,k) = coordinates of jth point on (k+1)-D confidence interval
; 					if (Cintvl(*,j,k) eq 0.) then confidence interval unbounded
; Common Blocks: 	None
; Side Effects: 	None known
; Restrictions:
;-
;;; Just ignore the next four lines
author_name = '$Author: higal_repository $'
date_name = '$Date: 2013/09/17 08:36:37 $'
version_name = '$Revision: 1.1.1.1 $'

test=1

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

; generate points on the surface of a sphere of radius 1.
; first check no. of points requested is consistent with dimensionality of surface
if (miv eq 1) then begin
 if (npoint ne 2) then print,'Only 2 points required for 1-D intervals - resetting'
 npoint=2
 np_fix=npoint
endif else if (miv eq 2) then begin
 np_fix=npoint
endif else if (miv gt 2) then begin
 np_fix=fix(npoint^(1./(miv-1)))
 nprevd = one * float( np_fix ) ^ ( miv - one )
 if (nprevd ne npoint) then print,'No. of points revised to:',nprevd
 npoint=nprevd
endif

; initialise array of points
x = one * fltarr( miv, npoint )
if ( keyword_set( P_area ) ) then P_area = replicate( one, npoint )

; the next few lines generates a set of lat-long points on a miv-sphere
if (miv eq 1) then begin
 x(0,*) = [ -one, one ]
endif else begin
 np = one * fltarr( miv )
 for m = 1, miv do np(m-1) = one * float( np_fix ) ^ ( m - one )
 z1 = exp( complex( 0, 1 ) * pi * findgen( np_fix ) / np_fix )
 z1(np_fix/2:np_fix-1)=-z1(np_fix/2:np_fix-1)
 z2 = exp( complex( 0, 1 ) * 2. * pi * findgen( np_fix ) / np_fix )
 x0 = transpose( [ [real_part(z2)], [imaginary(z2)] ] )
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

; initialise working and output arrays
b = one * fltarr( miv+1, npoint )
Cintvl = one * fltarr( miv, npoint, nthold )
if (keyword_set(d1dim)) then begin
  n1dim=n_elements(d1dim)/miv
  C1dim = one * fltarr( 3, n1dim )
endif else begin
  n1dim = 0
endelse

if (keyword_set(t1dim)) then begin
 if (n1dim eq 0) then stop,'d1dim required in cpar_tls if t1dim set'
 if (n_elements(t1dim) ne n1dim) then t1dim=replicate(0.*one,n1dim)
endif

; initialise array Z_poss if keyword set
if (keyword_set(Z_poss)) then begin
 ldp=n_elements(Z_poss)/(miv+1)
; store input Z_poss (original values) in Z_orig
 Z_orig=reform(Z_poss,ldp,miv+1)
 Z_poss = one * fltarr( ldp, miv+1, npoint, nthold)
endif

; compute nthold miv-D confidence intervals -- simplified to work in Evects coordinates
; m=1 corresponds to 1-D confidence interval etc.
for m=1, nthold do begin
; rescale the unit sphere to radius T_crit
 a=x*T_crit(m-1)
; compute weights on 1st miv e-vectors by dividing a by sqrt(delta-evalus)
 for i=0, miv-1 do b(i,*)=a(i,*)/sqrt(Evalus(i)-Evalus(miv))
; compute sum squared weights
 f=total_1d(b(0:miv-1,*)^2,1)
; check if the sum if greater than unity (indicates open-ended interval)
 iimag=where(f gt 1.,nimag)
 if (nimag gt 0) then begin
  f(iimag)=1.
  print,'Warning: open-ended C-intvl:',m
 endif
 ireal=where(f lt 1.,nreal)
; Don't compute anything if no bounds on C-intvl
 if (nreal eq 0) then print,'Warning: no bounds on C-intvl:',m else begin
; compute weight on (miv+1)-th e-vector from normalisation constraint
; use positive sqrt to give max projection onto last e-vector
  b(miv,*)=sqrt(1.-f)
; transform to normal coordinates
  v=Evects#b
; convert to conventional regression-coefficient-like scaling parameters
  for i=0, miv-1 do Cintvl(i,ireal,m-1)=-v(i,ireal)/v(miv,ireal)
; compute 1-D confidence limits
  if (m eq 1 and (keyword_set(d1dim) or keyword_set(t1dim))) then begin
   for n=0, n1dim-1 do begin
; project 1-D directions onto ellipsoids
    C1dim(0,n)=transpose(d1dim(*,n))#betatl
    d1d=transpose(d1dim(*,n))#reform(Cintvl(*,ireal,m-1))
    C1dim(1,n)=min(d1d)
    C1dim(2,n)=max(d1d)
    test=1
    if (keyword_set(t1dim)) then begin
     if (test) then t1dim(n)=C1dim(1,n)
; define the vector c (see notes)
     c=[d1dim(*,n),t1dim(n)]
; define a complete set of vectors orthogonal to c
     D=-c#transpose(c)/total(c^2)
     for k=0, miv do D(k,k)=D(k,k)+1.
     svdpvw, D, Q, P, R, pv_wave=pv_waveopt, double=doubleopt
     D2=transpose(Evects)#P(*,0:miv-1)
; weight the vectors by the Evalu difference
     for i=0, miv-1 do D2(i,*)=D2(i,*)*sqrt(Evalus(i)-Evalus(miv))
     v2=D2#x
     t1dim(n)=sqrt(min(total_1d(v2^2,1)))
     if (test) then print,t1dim(n),T_crit(m-1)
    endif
   endfor
  endif
; compute Z_poss if requested
  if (keyword_set(Z_poss)) then begin
   ; Iterate over real values (and allow long integer counter)
   if nreal le 2^15-1 then nreal = fix( nreal )
   for j=0*nreal, nreal-1 do begin
    Z_poss(*,*,ireal(j),m-1)=Z_orig - $
     (Z_orig#v(*,ireal(j)))#transpose(v(*,ireal(j)))
   endfor
  endif
 endelse
endfor

; Determine 1 dimensional likelihood functions for the regression coefficients
; (DAS addition).
if keyword_set( bt1dim ) then begin
  ; Initialise array of the locations of quantiles in the likelihood functions
  b1dim = one * fltarr( n_elements( bt1dim ), miv )
  ; Initialise a temporary work variable
  temp = one * fltarr( miv, npoint )
  ; Iterate through quantiles of the distribution
  for j = 0, n_elements( bt1dim ) - 1 do begin
    ; Rescale the unit sphere to radius bt1dim
    a = x * bt1dim[j]
    ; compute weights on 1st miv e-vectors by dividing a by sqrt(delta-evalus)
    for i = 0, miv - 1 do b[i,*] = a[i,*] / sqrt( Evalus[i] - Evalus[miv] )
    ; compute sum squared weights
    f = total_1d( b[0:miv-1,*] ^ 2, 1 )
    ; check if the sum is greater than unity (indicates open-ended interval)
    ireal = where( f lt 1., nreal )
    ; Don't compute anything if no bounds on C-intvl
    if nreal gt 0 then begin
      ; compute weight on (miv+1)-th e-vector from normalisation constraint
      ; use positive sqrt to give max projection onto last e-vector
      b[miv,*] = sqrt( 1. - f )
      ; transform to normal coordinates
      v = Evects # b
      ; convert to conventional regression-coefficient-like scaling parameters
      for i = 0, miv - 1 do temp[i,ireal] = -v[i,ireal] / v[miv,ireal]
      ; Compute location of current quantile
      if bt1dim[j] lt 0. then begin
        b1dim[j,*] = max( temp[*,ireal], dimension=2 ) $
            # d1dim[*,n1dim-miv:n1dim-1]
      endif else begin
        b1dim[j,*] = min( temp[*,ireal], dimension=2 ) $
            # d1dim[*,n1dim-miv:n1dim-1]
      endelse
    endif
  endfor
endif
; Clear memory
temp = 0

return,Cintvl
end
