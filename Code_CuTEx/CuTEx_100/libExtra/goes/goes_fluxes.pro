;+
; Project     : SDAC     
;                   
; Name        : GOES_FLUXES
;               
; Purpose     : This procedure calculates the expected fluxes from the GOES X-ray
;		detectors in Watts/meter^2 (reported units) as a function of
;		temperature and emission measure.
;               
; Category    : GOES
;               
; Explanation : This procedure utilizes the tables in GOES_TEM made with MAKE_GOES_RESP which
;		have already calculated the fluxes in both GOES channels for GOES 6,7,8,9 and
;		the others (with somewhat less confidence)
;               
; Use         : GOES_FLUXES, Temperature, Emission_meas, Flong, Fshort 
;    
; Inputs      : Temperature - temperature in MegaKelvin
;		Emission_meas- Emission measure in units of 1e49 cm^-3
;               
; Opt. Inputs : None
;               
; Outputs     : Flong - flux in Watts/meter^2 in GOES 1-8 Angstrom channel
;		Fshort- flux in Watts/meter^2 in GOES 0.5-4 Angstrom channel
;
; Opt. Outputs: None
;               
; Keywords    : SATELLITE- GOES satellite number
;		DATE- Time of observation in ANYTIM format, needed for GOES6 which changed
;		its long wavelength averaged transfer constant used in reporting measured
;		current as Watts/meter^2
; Calls       : GOES_TEM
;
; Common      : None
;               
; Restrictions: Temperature between 1 and 98 MegaKelvin, 
;               
; Side effects: None.
;               
; Prev. Hist  : VERSION 1, RAS, 30-JAN-1997
;
; Modified    : 
;
;-            
;==============================================================================
pro goes_fluxes, temperature, emission_meas, flong, fshort, satellite=satellite, date=date

flong = 0.0
fshort= 0.0

goes_tem, flong, fshort, temperature, emission_meas, satellite=satellite, date=date

end
