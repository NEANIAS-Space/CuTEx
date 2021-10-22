;+
; NAME: 
;       POTEM.PRO
;
; PURPOSE:
;       Compute the potential temperature of a parcel of seawater at a
;       reference pressure using the Bryden 1973 polynomial for 
;       adiabatic lapse rate and Runge=Kutta 4th order integration algorithm.
;
; CATEGORY:
;       Physics
;
; CALLING SEQUENCE:
;       theta = Potem(t, s, p, pr)
;
; INPUTS:
;       p       pressure 		dBars (m)
;       t       temperature    		deg C
;       s       salinity   		PSU
;       pr      reference pressure	dBars (m)
;
; OUTPUTS:
;       theta   potential 		temp deg C
;
; EXAMPLE:
;      See table of reference (check) values below.
;
; THETA = POT_T(S,T,P,PR) computes potential temperature at pr using
; Bryden 1973 polynomial for adiabatic lapse rate
; and Runge=Kutta 4 th order integration algorithm.
;       Ref: Bryden,H.1973, DeepSea Research,20etc.
;
; Units:
;       p       pressure 		dBars (m)
;       t       temperature    		deg C
;       s       salinity   		PSU
;       pr      reference pressure	dBars (m)
;       theta   potential 		temp deg C
;
; check values: s (psu)  t (deg C)  p (dB)  pr (dB)  theta (deg C)
;                34.75      1.0      4500      0         0.640
;                34.75      1.0      4500     4000       0.944
;                34.95      2.5      3500      0         2.207
;                34.95      2.5      3500     4000       2.558
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 1999-03-31. (Converted to an
; 	IDL function from a Matlab function)
;-

function ATG,s,t,p

  compile_opt hidden
;
; ATG required by potem.pro.
;
; ATG = ATG(S,T,P) computes the adiabatic temperature gradient deg C/dBar
;
;       pressure        p       dBar
;       temperature     t       Deg C
;       salinity        s       PSU
;       adiabatic       atg     deg C/dBar
;
; Converted to a matlab function from a fortran subroutine
; June 30, 1993 by TDM
; Converted from a Matlab function to an IDL function 
; March 31, 1999 by ECW

  ds = s - 35.0
  atg = (((-2.1687d-16 * t + 1.8676d-14) * t - 4.6206d-13) * p 		$
         + ((2.7759d-12 * t - 1.1351d-10) * ds + ((-5.4481d-14 * t 	$
         + 8.733d-12) * t - 6.7795d-10) * t + 1.8741d-8)) * p 		$
         + (-4.2393d-8 * t+1.8932d-6) * ds                              $
         + ((6.6228d-10 * t - 6.836d-8) * t + 8.5258d-6) * t + 3.5803d-5

  Return,atg
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function potem, t, s, p, pr
;
; THETA = POT_T(S,T,P,PR) computes potential temperature at pr using
; Bryden 1973 polynomial for adiabatic lapse rate
; and Runge=Kutta 4 th order integration algorithm.
; Ref: Bryden,H.1973, DeepSea Research,20etc.
;
; Units:
;       p       pressure 		dBars (m)
;       t       temperature    		deg C
;       s       salinity   		PSU
;       pr      reference pressure	dBars (m)
;       theta   potential 		temp deg C
;
; checkvalues: s (psu)  t (deg C)  p (dB)  pr (dB)  theta (deg C)
;               34.75      1.0      4500      0         0.640
;               34.75      1.0      4500     4000       0.944
;               34.95      2.5      3500      0         2.207
;               34.95      2.5      3500     4000       2.558
;
; Requires the function, ATG (defined above)
;
; Converted to a matlab function from a fortran function
; June 30, 1993 by TDM
; Converted to an IDL function from a Matlab function
; 31 March, 1999 by ECW

   tt = Double(t)
   ss = Double(s)
   pp = Double(p)
   ppr = Double(pr)


   h = ppr-pp
   xk = h*ATG(ss,tt,pp)
   tt = tt + 0.5*xk
   q = xk
   pp = pp + 0.5*h
   xk = h*atg(ss,tt,pp)
   tt = tt + 0.29289322*(xk-q)
   q = 0.58578644*xk + 0.121320344*q
   xk = h*ATG(ss,tt,pp)
   tt = tt + 1.707106781*(xk-q)
   q = 3.414213562*xk - 4.121320344*q
   pp = pp + 0.5*h
   xk = h*ATG(ss,tt,pp)
   theta= tt + (xk-2.0*q)/6.0

   Return, theta
end


