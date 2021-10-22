; $Id: deriv_5.pro,v 1.1.1.1 2013/09/17 08:36:37 higal_repository Exp $
;
;
; 25/08/2008 modified by Fabiana Faustini from the DERIV.pro to use the 5-point formula
; 04/09/2009 modified by Fabiana Faustini to include an equal point spacing but different from 1. 

Function Deriv_5, X, hspace=hspace
;+
; NAME:
;	DERIV_5
;
; PURPOSE:
;	Perform numerical differentiation using 3-point, Lagrangian 
;	interpolation.
;
; CATEGORY:
;	Numerical analysis.
;
; CALLING SEQUENCE:
;	Dy = Deriv(Y)	 	;Dy(i)/di, point spacing = 1.
;	Dy = Deriv(X, Y)	;Dy/Dx, unequal point spacing.
;
; INPUTS:
;	Y:  Variable to be differentiated.
;	X:  Variable to differentiate with respect to.  If omitted, unit 
;	    spacing for Y (i.e., X(i) = i) is assumed.
;
; OPTIONAL INPUT PARAMETERS:
;	As above.
;
; OUTPUTS:
;	Returns the derivative.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	See Hildebrand, Introduction to Numerical Analysis, Mc Graw
;	Hill, 1956.  Page 82.
;
; MODIFICATION HISTORY:
;   modified to calculate the numerical differenziation using 5-point Lagrangian interpolation
;-
;
; on_error,2              ;Return to caller if an error occurs
n = n_elements(x)
if n lt 5 then message, 'Parameters must have at least 5 points'

if (keyword_set(hspace)) then h=hspace(0) else h=1.

 d = -1*(shift(x,-2) - 8.0*shift(x,-1) + 8.0*shift(x,1) - shift(x,2))/(12.*h)
 d[0] = (-25.0*x[0] + 48.0*x[1] - 36.0*x[2] + 16.0*x[3] - 3.0*x[4])/(12.*h)
 d[1] = (-3.0*x[0] - 10.0*x[1] + 18.0*x[2] - 6.0*x[3] + x[4])/(12.*h)
 d[n-2] = (-x[n-5] + 6.0*x[n-4] - 18.0*x[n-3] + 10.0*x[n-2] + 3.0*x[n-1])/(12.*h)
 d[n-1] = (3.0*x[n-5] - 16.0*x[n-4] + 36.0*x[n-3] - 48.0*x[n-2] + 25.0*x[n-1])/(12.*h)

return, d
end
