;	Calculate ergs/sec in x-ray range from goes flux
;	yclean - 2 channels of cleaned background-subtracted goes data
;	Returns array of energy/sec for each point in original array

function goes_lx, yclean

; Convert to ergs s^-1 from watts m^-2
;Distance to Sun (1 AU) = 1.496 E+13 cm

; 1 watt = 1 joule per second = 10^7 erg s^-1
; 1 watt m^-2 = (2.0 * !pi * au**2 * 1.e7) / 1.e4 erg s^-1

aucm = 1.496e+13
wpsm2eps = 2.0 * !pi * aucm^2 * 1.e3
return, wpsm2eps * total(yclean, 2)

end