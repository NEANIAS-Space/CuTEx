
;+
; NAME
;
;    GAUSS_SG
;
; EXPLANATION
;
;    Defines a Gaussian to be used when performing
;    Gaussian fits with the SPEC_GAUSS suite of IDL routines.
;
;    The format is that required by the MPFIT routines.
;
; INPUTS
;
;    X   The points at which the function is evaluated. Typically these
;        will be wavelength values.
;
;    P   A three element array that describes the Gaussian. P[0] is the
;        peak of the Gaussian, P[1] is the centroid, and P[2] is the
;        Gaussian width.
;
; OUTPUT
;
;    The values of the Gaussian defined by P at the input values X.
;
; HISTORY
;
;    Ver.1, Peter Young, 5-Aug-2005
;-

FUNCTION gauss_sg, x, p, _EXTRA=extra
;
; this is needed by mpfitexpr
;
  z=( (x-p[1])/abs(p[2]) )^2
  f=p[0] * exp(-0.5*temporary(z))
  return,f
END
