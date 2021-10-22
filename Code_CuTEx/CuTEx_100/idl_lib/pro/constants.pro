;+
; NAME:
;	CONSTANTS
;
; PURPOSE:
;	This procedure returns the values of some absolute constants.
;
; CATEGORY:
;	Miscellaneous
;
; CALLING SEQUENCE:
;	CONSTANTS, $
;		[ PI=pi, NDEGREE=ndegree, DEGRAD=degrad, NAN=nan, IM=im, $
;       	DINA=dina, MINA=mina, $
;		R_EARTH=r_earth, G_EARTH=g_earth, MSLP=mslp, VA_EARTH, $
;		CELSIUS_KELVIN=celsius_kelvin, $
;		STEFAN_BOLTZMANN=stefan_boltzmann, $
;		R_GAS=r_gas ]
;
; KEYWORD PARAMETERS
;	DOUBLE:  If set, floating point numbers are returned in double
;	         precision.  The default is single precision.
;
; OPTIONAL OUTPUTS:
;	AU:  Astronomical unit (the Earth-Sun distance).
;	AVOGADRO:  Avogadro constant.
;	BOLTZMANN:  Boltzmann constant.
;	CELSIUS_KELVIN:  The conversion between degress Celsius and Kelvins.
;	CT_LWATER:  Specific heat of liquid water at 0C.
;	DEGRAD:  Degrees to radians conversion coefficient.
;	DINA:  Number of days in a year.
;	G_EARTH:  Mean gravitational acceleration at Earth's surface.
;	HF_WATER:  Latent heat of fusion of water at 0C.
;	HIND:  The number of hours in a day.
;	HV_WATER:  Latent heat of vaporisation of water at 100C.
;	IM:  The imaginary number i=sqrt(-1).
;	M_EARTH:  Mass of Earth.
;	MSLP:  Mean global sea level pressure on Earth.
;	MINA:  Number of months in a year.
;	MINH:  The number of minutes in an hour.
;	NAN:  Not-a-Number value.
;	NDEGREE:  Number of degrees in a circle.
;	OMEGA_EARTH:  Angular rotation speed of Earth.
;	PI:  pi=3.14...
;	R_EARTH:  Radius of Earth.
;	R_GAS:  Molar gas constant.
;	RHO_DRYAIR:  Density of dry air at 0C and 100kPa.
;	RHO_LWATER:  Density of liquid water at 0C.
;	SINM:  The number of seconds in a minute.
;	STEFAN_BOLTZMANN:  Stefan-Boltzmann constant.
;
; PROCEDURE:
;	This procedure defines all values.  Values are obtained from the 
;	following references.
;	Serway, R.A.  1992.  Physics for Scientist and Engineers with Modern 
;		Physics, 3rd edition, Updated Printing.  Saunders College 
;		Publishing, Toronto, Ontario, 1444p.
;	Weast, R.C., and M.J. Astle, eds.  1980.  CRC Handbook of Chemistry 
;		and Physics, 61st edition.  CRC Press, Inc., Boca Raton, 
;		Florida.
;	National Institute of Standards and Technology.  
;		http://physics.nist.gov/PhysRefData/
;
; EXAMPLE:
;	Return the value of pi.
;	  constants, pi=pi
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@uvic.ca), 2000-06-27.
;	Modified:	DAS, 2000-07-24 (added DEGRAD output keyword).
;	Modified:	DAS, 2001-05-18 (added DOUBLE keyword, and SINM, MINH,
;			HIND output keywords).
;	Modified:	DAS, 2001-08-13 (added GEARTH, CELSIUS_KELVIN output).
;	Modified:	DAS, 2001-11-22 (added STEFAN_BOLTZMANN output).
;	Modified:	DAS, 2002-08-09 (added AU, AVOGADRO, BOLTZMANN, 
;			CT_LWATER, HF_WATER, HV_WATER, M_EARTH, OMEGA_EARTH, 
;			R_GAS, RHO_DRYAIR, RHO_LWATER output, modified and 
;			referenced CELSIUS_KELVIN, G_EARTH, MSLP, R_EARTH, 
;			STEFAN_BOLTZMANN outputs).
;-

;***********************************************************************

PRO CONSTANTS, $
	DOUBLE=doubleopt, $
	DEGRAD=degrad, IM=im, NAN=nan, NDEGREE=ndegree, PI=pi, $
	DINA=dina, HIND=hind, MINA=mina, MINH=minh, SINM=sinm, $
	AU=au, G_EARTH=g_earth, MSLP=mslp, M_EARTH=m_earth, $
		OMEGA_EARTH=omega_earth, R_EARTH=r_earth, $
	CT_LWATER=ct_water, HF_WATER=hf_water, HV_WATER=hv_water, $
		RHO_LWATER=rho_lwater, $
	R_GAS=r_gas, RHO_DRYAIR=rho_dryair, $
	CELSIUS_KELVIN=celsius_kelvin, $
	AVOGADRO=avogadro, BOLTZMANN=boltzmann, $
		STEFAN_BOLTZMANN=stefan_boltzmann

;***********************************************************************
;Options

;Double precision floating point number option
doubleopt = keyword_set(doubleopt)

;***********************************************************************
;Math Constants

;pi
if keyword_set(double) then pi = !dpi $
                       else pi = !pi

;Number of degrees in a circle
ndegree = 360

;Degrees to radians conversion coefficient
degrad = pi / ndegree * 2.

;NaNQ (not a number)
nan = !values.f_nan

;Imaginary number i=sqrt(-1)
im = complex(0,1)

;***********************************************************************
;Time Constants

;Number of days in a year
dina = 365

;Number of months in a year
mina = 12

;Number of hours in a day
hind = 24

;Number of minutes in an hour
minh = 60

;Number of seconds in a minute
sinm = 60

;***********************************************************************
;Earth Constants

;Earth's radius
;Weast and Astle 1980,  In m
r_earth = 6.370949e+6

;Gravitational acceleration at Earth's surface
;From NIST 1998,  In m / s^2
g_earth = 9.80665

;Mass of Earth
;From Serway 1992,  In kg
m_earth = 5.98e+24

;Angular rotation speed of Earth ( 1 / s )
omega_earth = 7.292e-5

;Mean sea level pressure on Earth
;From NIST 1998,  In Pa = N / m^2
mslp = 101325.

;Astronomical unit
;From TCAEP 2001,  In m
au = 1.4959787e+11

;***********************************************************************
;Unit Conversions

;Degrees Celsius to Kelvin conversion
;From NIST 1998,  In K
celsius_kelvin = 273.15

;***********************************************************************
;Water Properties

;Density of liquid water at 0C
;From Weast and Astle 1980,  In kg / m^3
rho_lwater = 9.9987e+2

;Specific heat of liquid water at 0C
;From Weast and Astle 1980,  In J / kg / K
ct_lwater = 4.2177e+3

;Latent heat of vaporisation of water at 100C
;From Serway 1992,  In J / kg
hv_water = 2.26e+6

;Latent heat of fusion of water at 0C
;From Serway 1992,  In J / kg
hf_water = 3.33e+5

;***********************************************************************
;Air Properties

;Density of dry air at 0C and 100kPa
;From Weast and Astle 1980,  In kg / m^3
rho_dryair = 1.2929e+3

;Molar gas constant
;From NIST 1998,  In J / mol / K
r_gas = 8.3145

;***********************************************************************
;Physical Constants

;Avogadro constant
;From NIST 1998,  In 1 / mol
avogadro = 6.022141e+23

;Boltzmann constant
;From NIST 1998,  In J / K
boltzmann = 1.38065e-23

;Stefan-Boltzmann constant
;From NIST 1998,  In W / m^2 / K^4
stefan_boltzmann = 5.6704e-8

;***********************************************************************
;The End

return
END
