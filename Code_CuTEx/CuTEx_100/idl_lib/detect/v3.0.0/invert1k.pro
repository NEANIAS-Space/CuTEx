;+
; NAME:
;	INVERT1K
;
; COPYRIGHT:
;	Copyright (1999) Myles Allen, Space Science Department, 
;	Rutherford Appleton Laboratory.
; 	Prepared under contract to the Hadley Centre for Climate Prediction 
;	and Research.
;
; PURPOSE:
;	This function computes the inverse of a real square symmetric matrix 
;	and checks the solution.
;
; CATEGORY:
;	Optimal Detection Package, v3.0.0
;
; CALLING SEQUENCE:
;	Result = INVERT1K( A )
;
; INPUTS:
;	A:  A real, square, symmetric matrix for input.
;
; KEYWORD PARAMETERS:
;	DOUBLE:  If set, then the double precision arithmetic is used.  The 
;		default is single precision.  The inversion is actually done 
;		in double precision, only the format of the output is affected.
;	STATUS:  Returns 0 if the inversion went well, 1 if it did not.
;
; OUTPUTS:
;	Result:  The inverse of the input matrix A.
;
; USES:
;	-
;
; PROCEDURE:
; 	This function checks the dimensionality of the input matrix, and uses 
;	the INVERT function for >1-D matrices.
;
; EXAMPLE:
;	Result = INVERT1K( [[4,3],[3,2]] )
;	Result should be [[-2.,3.],[3.,-4.]].
;
; MODIFICATION HISTORY:
;	Written by:	Myles R. Allen (m.r.allen@rl.ac.uk), 1999-05-15 (v1.0)
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2004-06-28 
;			(Documentation for inclusion in IDL routine library)
;	Modified:	DAS, 2005-02-18 (matrix inversion in double precision; 
;			added DOUBLE keyword)
;	Modified:	DAS, 2005-09-01 (removed forced use of double 
;			precision; v3.0 of Optimal Detection Package)
;-

function invert1k,A,status=status, $
	DOUBLE=doubleopt

; Copyright (1999) Myles Allen, Space Science Department, Rutherford Appleton Laboratory
; Prepared under contract to the Hadley Centre for Climate Prediction and Research
;+
; Name: function invert1k
;
; Description:
; Identical to invert1.pro, renamed to avoid confusion with earlier versions which did
; not use keywords for status flag 
; computes the inverse of a real square symmetric matrix, allowing for 1-D matrices
; and checks the solution (necessary for PV-wave, which doesn't)
;
; Method:
; checks for dimensionality of matrix, and uses invert function for
; >1-D matrices
; Indices follow standard maths notation: i.e. (row,column)
;
; History:
; Vers.	Date		Comment			Author
; ----  -------- 	--------		--------
; 1.0   15/05/99 	Original code 	Myles Allen m.r.allen@rl.ac.uk
;
; Code Description: IDL / PV-WAVE
;
; Category: 		Function
;
; Classification keywords: general maths
;
; Calling sequence: B=invert1k(A,status=status)
;
; Example call: 	B=invert1k(A,status=status)
;
; Inputs:
; 		arg1:		A = real, square, symmetric matrix
;
; Optional Inputs:	None
;
; Keywords:			status=0 if inversion OK, 1 otherwise
;
; Optional Outputs: None
; Return Value:    	B = inverse of A
; Common Blocks: 	None
; Side Effects: 	None known
; Restrictions: 	Not tested for complex matrices
;-
;;; Just ignore the next four lines
author_name = '$Author: higal_repository $'
date_name = '$Date: 2013/09/17 08:36:37 $'
version_name = '$Revision: 1.1.1.1 $'

; Option for double precision (DAS)
one = 1.
if keyword_set( doubleopt ) then one = 1.d

s=size(A)
if (s(0) gt 2 or s(1) ne s(s(0))) then begin
 print,'Array dimensions incompatible with invert1k:',s
 stop
endif
status=0
B=A
if (s(1) eq 1) then begin
 if ( A(0) ne 0. ) then B = one / A else status = 1
endif else begin
 ; Next line modified by DAS to work in double precision
 B = invert( A, double=doubleopt )
 ; Check that invertion works
 C=B#A
 for k=0, s(1)-1 do if (abs(C(k,k)-1.) gt 1.e-5) then status=1
 if (status eq 1) then begin
  print,'Inaccurate or singular matrix inversion.'
  print,'Input matrix A:'
  pm,A
  print,'Computed inverse B:'
  pm,B
  print,'B#A'
  pm,C
 endif
endelse

return,B
end
