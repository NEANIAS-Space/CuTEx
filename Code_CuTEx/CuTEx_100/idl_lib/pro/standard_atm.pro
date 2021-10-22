;+
; NAME:
;	STANDARD_ATM
;
; PURPOSE:
;	This function calculates the pressure at a given altitude in the
;	troposphere, assuming a uniform, dry atmosphere.
;
; CATEGORY:
;	PHYSICS
;
; CALLING SEQUENCE:
;	Result = STANDARD_ATM( Input )
;
; INPUTS:
;	Input:  The altitude, in metres, at which the pressure should be
;		calculated.  Of type floating point.  If ALTITUDE is set, this
;		is instead the pressure at which the altitude should be
;		calculated.
;
; KEYWORD PARAMETERS:
;	ALTITUDE:  If set, the function calculates the altitude of a given
;		pressure level.  The default is the reverse.
;	BAR:  If set, the pressure unit is a bar.  The default is Pa.
;	CELSIUS:  If set, the temperature units are in degrees Celsius.  The
;		default is Kelvins.
;	HPA:  If set, the pressure unit is a hPa.  The default is Pa. 
;	KILO:  If set, the units are kPa and km.  The default is Pa and m.
;	TEMPERATURE:  If set, the function returns the average temperature, in
;		K, at the given altitude.
;
; OUTPUTS:
;	Result:  Returns the pressure at the given altitude.
;
; USES:
;	CONSTANTS.pro
;
; PROCEDURE:
;	This function is modelled after StdAtm at 
;	http://hurri.kean.edu/~yoh/calculations/hydrostatic/hydrostatic.html.
;
; EXAMPLE:
;	Calculate the pressure, in kPa, at 2757m altitude.
;	  result = STANDARD_ATM( 2.757, /KILO )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@uvic.ca), 2001-08-13
;	Modified:	DAS, 2002-05-16 (fixed bugs in GAMMA and unit options)
;	Modified:	DAS, 2002-08-12 (complies with CONSTANTS.pro revision)
;-

;***********************************************************************

FUNCTION STANDARD_ATM, $
	Input, $
	ALTITUDE=altitudeopt, $
	TEMPERATURE=temperatureopt, $
	BAR=baropt, HPA=hpaopt, CELSIUS=celsiusopt, KILO=kiloopt

;***********************************************************************
;Constants

;Pressure (Pa) and gravitational acceleration (m/s^2) at zero altitude
constants, mslp=p0, g_earth=g

;Temperature at zero altitude (K)
T0 = 288.15

;Lapse rate of temperature (K/m)
Gamma = 6.5e-3

;Dry air gas constant (J.K/kg)
R = 287.04

;Degrees Celsius to Kelvin conversion
constants, celsius_kelvin=celsius_kelvin

;***********************************************************************
;Options

;Measurement units
;Pressure in bar instead of Pa
if keyword_set( baropt ) then baropt = 1 $
                         else baropt = 0
;Pressure in hPa instead of Pa
if keyword_set( hpaopt ) then hpaopt = 1 $
                         else hpaopt = 0
;oC instead of K
celsiusopt = keyword_set( celsiusopt )
;kPa and km instead of Pa and m
if keyword_set( kiloopt ) then kiloopt = 1 $
                          else kiloopt = 0

;Function result options
;Return altitude
altitudeopt = keyword_set( altitudeopt )
;Return temperature
temperatureopt = keyword_set( temperatureopt )
;Return pressure (default)
pressureopt = not( altitudeopt or temperatureopt )

;***********************************************************************
;Calculate Result

;Return altitude
if altitudeopt then begin
  result = T0 / Gamma * ( 1 - ( input * 1000.^kiloopt * 100000.^baropt * 100.^hpaopt/ p0 )^( R * Gamma / g ) ) / 1000.^kiloopt
endif

;Return temperature
if temperatureopt then begin
  result = T0 - Gamma * input * 1000.^kiloopt
  if celsiusopt then result = result - celsius_kelvin
endif

;Return pressure
if pressureopt then begin
  result = p0 * ( 1 - Gamma * input * 1000.^kiloopt / t0 )^( g / R / Gamma ) / 1000.^kiloopt / 100000.^baropt / 100.^hpaopt
endif

;***********************************************************************
;The End

return, result
END
