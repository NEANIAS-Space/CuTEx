;$Id: ibeta_pdf.pro,v 1.1.1.1 2013/09/17 08:36:36 higal_repository Exp $
;
; Copyright (c) 1994-2007, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;+
; NAME:
;       IBETA_PDF
;
; PURPOSE:
;       This function computes the incomplete beta function.
;       It is called by the probability density functions in 
;       this directory. See the function IBETA() in the "math"
;       subdirectory for the user-callable version of the 
;       incomplete beta function.
;-

function betacf, a, b, x
  COMPILE_OPT hidden

  ;Continued fractions.
  lc = a + b
  ln = a - 1.0
  lq = a + 1.0
  max = 100
  ja = 1.0 & jn = 1.0 
  ka = 1.0 - lc * x / lq 
  kn = 1.0
  for i = 1, max do begin
    ep  = i + 0.0
    tm  = ep + ep
    d   = ep * (b - ep) * x / ((ln + tm) * (a + tm))
    jq  = ja + d*jn
    kq  = ka + d * kn
    d   = -(a + ep) * (lc + ep) * x / ((lq + tm) * (a + tm))
    jq1 = jq + d * ja
    kq1 = kq + d * ka
    prev= ja
    jn  = jq / kq1
    kn  = kq / kq1
    ja  = jq1 / kq1
    ka  = 1.0
    if(abs(ja - prev) lt 3.0e-7 * abs(ja)) then return, ja
  endfor
end

function ibeta_pdf, x, a, b

  on_error, 2  ;Return to caller if error occurs.

  if x lt 0. or x gt 1. then message, $
    'x must be in the range: [0.0, 1.0]'

  ;gab = gamma(a) * gamma(b) 
  ;gamma(a+b)/gab * exp( a*alog(x) + b*alog(1.0-x))

  if(x ne 0 and x ne 1 ) then temp = $
    exp(lngamma(a+b)-lngamma(a)-lngamma(b)+a*alog(x)+b*alog(1.0-x)) $
  else temp = 0.0

  if(x lt (a+1.0)/(a+b+2.0)) then return, temp * betacf(a, b, x)/a $
    else return, (1.0 - temp * betacf(b, a, 1.0-x)/b)
end

