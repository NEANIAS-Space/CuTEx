;+
; NAME:
;	SVDPVW
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This procedure is a singular value decomposition (SVD) routine for 
;	PV-Wave.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	SVDPVW, A, W, U, V
;
; INPUTS:
;	A:  A floating point matrix, of size M*N, for input.
;
; KEYWORD PARAMETERS:
;	DOUBLE:  If set, then the double precision arithmetic is used.  The 
;		default is single precision.
;	PV_WAVE:  If set, the procedure will work under PV-WAVE.  The default 
;		is for IDL.
;	UNSORT:  If set and positive, the singular values are not sorted 
;		(slightly faster).  If set and negative, the singular values 
;		are sorted in ascending order.  If not set (the default) or 
;		set to zero, the singular values are sorted in descending 
;		order.
;
; OUTPUTS:
;	U:  An orthogonal matrix of size M*K (where K=min([M,N])).  This is 
;		the left handed matrix in the SVD of the input matrix A.  
;		Values corresponding to the singular values in W are stored in 
;		the K columns.
;	V:  An orthogonal matrix of size N*K (where K=min([M,N])).  This is 
;		the right handed matrix in the SVD of the input matrix A.  
;		Values corresponding to the singular values in W are stored in 
;		the K columns.
;	W:  A vector of size K (where K=min([M,N])) of the singular values of 
;		the input matrix A.
;
; USES:
;	-
;
; PROCEDURE:
;	This procedure uses the SVD command after reforming and transposing as 
;	necessary.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Myles R. Allen (m.r.allen@rl.ac.uk), 1999-05-15 (v1.0)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in routine library)
;	Modified:	DAS, 2005-03-16 (Added DOUBLE, PV_WAVE keywords, 
;			switch from IDL's svd to svdc procedure)
;	Modified:	DAS, 2005-09-01 (Accounted for bug in IDL's svdc 
;			implementation of double precision by forcing the 
;			calculations to be done in single precision;  v3.0 of 
;			Optimal Detection Package)
;-

pro svdpvw,a,w,u,v,unsort=unsort, $
	DOUBLE=doubleopt, $
	PV_WAVE=pv_waveopt

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: function svdpvw
;
; Description:
; svd routine for PV-wave which transposes as necessary
; output is in standard math notation
;
; Method:
; uses the svd,a,w,u,v command after reforming and transposing
; as necessary
; Indices follow standard maths notation: i.e. (row,column)
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
;
; Code Description: IDL / PV-WAVE
;
; Category: 		program
;
; Classification keywords: general linear algebra
;
; Calling sequence: svdpvw,a,w,u,v
;
; Example call: 	svdpvw,a,w,u,v
;
; Inputs:
; 		arg1:		a = real mxn array
;
; Optional Inputs:	None
;
; Keywords:
; unsort = if set and positive, do not sort singular values (slightly faster)
;          if set and negative, sort singular values in ascending order
;
; Outputs:			k = smaller of m and n
; 		arg2:		w = k-rank vector of singular values
;		arg3:		u = m x k matrix of LSVs, stored columnwise
; 		arg4:		v = n x k matrix of RSVs, stored columnwise
;
; Optional Outputs: None
; Return Value:    	N/A
; Common Blocks: 	None
; Side Effects: 	None known
; Restrictions: 	Not tested for complex matrices
;-
;;; Just ignore the next four lines
author_name = '$Author: higal_repository $'
date_name = '$Date: 2013/09/17 08:36:37 $'
version_name = '$Revision: 1.1.1.1 $'

if (not keyword_set(unsort)) then unsort=0
sa=size(a)
if (sa(0) eq 0) then stop,'svdpvw not set up for scalar input'
if (sa(0) eq 1) then sa(2)=1
if (sa(0) gt 2) then stop,'svdpvw not set up for >2-D arrays'
; Use the newer SVDC procedure in IDL if IDL is being used (DAS)
if ( keyword_set( pv_waveopt ) ) then begin
  if ( sa(1) gt sa(2) ) then begin
    svd, reform( a, sa(1), sa(2) ), w, u, v
  endif else begin
    svd, reform( transpose( a ), sa(2), sa(1) ), w, v, u
  endelse
endif else begin
  ; Use IDL's newer svdc routine (DAS addition)
  if ( sa(1) gt sa(2) ) then begin
    svdc, transpose( reform( a, sa(1), sa(2) ) ), w, u, v, double=0
  endif else begin
    svdc, reform( a, sa[1], sa[2] ), w, v, u, double=0
  endelse
  u = transpose( u )
  v = transpose( v )
endelse
; Convert to double precision if requested (DAS addition).
; The SVD is not done in double precision because IDL's svdc does funny things 
; in double precision and the old svd doesn't do double precision.  I haven't 
; tested PV_WAVE's svd so I'm assuming it is the same.
if keyword_set( doubleopt ) then begin
  w = double( w )
  u = double( u )
  v = double( v )
endif
if (unsort le 0) then begin
; find ordering of singular values
 index=sort(w)
; default is to sort in descending order
 if (unsort eq 0) then index=reverse(index)
 w=w(index)
 su=size(u)
 sv=size(v)
 for k=0, su(1)-1 do u(k,*)=u(k,index)
 for k=0, sv(1)-1 do v(k,*)=v(k,index)
endif
return
end
