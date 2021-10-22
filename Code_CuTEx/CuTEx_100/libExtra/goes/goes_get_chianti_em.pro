;+
; Project:
;     SDAC
; Name:
;     GOES_GET_CHIANTI_EM
;
; Usage:
;     goes_get_chianti_em, fl, temperature, emission_meas, sat=goes, /photospheric
;
;Purpose:
;     Called by GOES_CHIANTI_TEM to derive temperature and emission measures.
;     This procedures computes the emission measure of solar plasma from the
;     temperature derived from the B4/B8 ratio together with the flux B8 in the
;     1-8 Angstrom channel using CHIANTI spectral models with coronal or
;     photospheric abundances
;     WARNING: fluxes are asssumed to be TRUE fluxes, so corrections
;     such as the (0.70,0.85) scaling of GOES 8-12 must be applied before 
;     use of this routine. GOES_CHIANTI_TEM applies these corrections.
;
;Category:
;     GOES, SPECTRA
;
;Method:
;     From the temperature the emission measure per unit B8 flux is computed
;     from a spline fit from a lookup table for 101 temperatures logT=.02 apart
;
;Inputs:
;     FL - GOES long wavelength flux in Watts/meter^2
;     TEMP - GOES temperature derived from GOES_GET_CHIANTI_TEMP in units of MK
;
;Keywords:
;     sat  - GOES satellite number, needed to get the correct response
;     photospheric - use photospheric abundances rather than the default
;             coronal abundnaces
;
;Outputs:
;     Emission_meas - Emission measure in units of cm-3 (i.e., NOT scaled)
;
;Common Blocks:
;     None.
;
;Needed Files:
;     None
;
; MODIFICATION HISTORY:
;     Stephen White, 24-Mar-2004: Initial version based on CHIANTI 4.2
;     This routine created 11/28/06 using CHIANTI version 5.2
;
; Contact     : Richard.Schwartz@gsfc.nasa.gov
;
;-
;-------------------------------------------------------------------------

pro goes_get_chianti_em, b8, temp, em, sat=sat, photospheric=photospheric

; interpolate tables of b8 vs temp to determine emission measure
; in units of cm^-3. Data values are responses to CHIANTI spectra for
; coronal and photospheric abundance

b8_cor=fltarr(12,101)     ; responses for 101 temps for each of 12 GOES satellites

 b8_cor[0,*]=[4.86e-05,9.30e-05,1.73e-04,3.09e-04,5.35e-04,8.94e-04,1.49e-03,2.36e-03,$
    3.65e-03,5.55e-03,8.31e-03,1.23e-02,1.80e-02,2.61e-02,3.73e-02,5.30e-02,7.39e-02,$
    1.02e-01,1.38e-01,1.83e-01,2.41e-01,3.13e-01,4.01e-01,5.06e-01,6.33e-01,7.84e-01,$
    9.62e-01,1.17e+00,1.41e+00,1.69e+00,2.01e+00,2.38e+00,2.79e+00,3.26e+00,3.78e+00,$
    4.35e+00,4.99e+00,5.68e+00,6.43e+00,7.24e+00,8.11e+00,9.04e+00,1.00e+01,1.10e+01,$
    1.21e+01,1.32e+01,1.44e+01,1.56e+01,1.68e+01,1.80e+01,1.92e+01,2.04e+01,2.15e+01,$
    2.26e+01,2.37e+01,2.47e+01,2.57e+01,2.66e+01,2.75e+01,2.84e+01,2.93e+01,3.02e+01,$
    3.12e+01,3.21e+01,3.31e+01,3.41e+01,3.53e+01,3.64e+01,3.77e+01,3.90e+01,4.03e+01,$
    4.18e+01,4.33e+01,4.48e+01,4.64e+01,4.81e+01,4.98e+01,5.15e+01,5.33e+01,5.51e+01,$
    5.69e+01,5.87e+01,6.05e+01,6.23e+01,6.41e+01,6.59e+01,6.76e+01,6.93e+01,7.09e+01,$
    7.25e+01,7.39e+01,7.53e+01,7.66e+01,7.78e+01,7.89e+01,7.99e+01,8.08e+01,8.17e+01,$
    8.24e+01,8.31e+01,8.37e+01]
 b8_cor[1,*]=[4.99e-05,9.55e-05,1.77e-04,3.18e-04,5.50e-04,9.19e-04,1.54e-03,2.43e-03,$
    3.75e-03,5.70e-03,8.53e-03,1.26e-02,1.85e-02,2.68e-02,3.83e-02,5.45e-02,7.59e-02,$
    1.04e-01,1.41e-01,1.89e-01,2.48e-01,3.22e-01,4.12e-01,5.20e-01,6.51e-01,8.06e-01,$
    9.88e-01,1.20e+00,1.45e+00,1.74e+00,2.07e+00,2.45e+00,2.87e+00,3.35e+00,3.88e+00,$
    4.47e+00,5.12e+00,5.83e+00,6.61e+00,7.44e+00,8.34e+00,9.29e+00,1.03e+01,1.14e+01,$
    1.25e+01,1.36e+01,1.48e+01,1.60e+01,1.72e+01,1.85e+01,1.97e+01,2.09e+01,2.21e+01,$
    2.33e+01,2.44e+01,2.54e+01,2.64e+01,2.74e+01,2.83e+01,2.92e+01,3.01e+01,3.11e+01,$
    3.20e+01,3.30e+01,3.40e+01,3.51e+01,3.62e+01,3.74e+01,3.87e+01,4.00e+01,4.14e+01,$
    4.29e+01,4.45e+01,4.61e+01,4.77e+01,4.94e+01,5.12e+01,5.30e+01,5.48e+01,5.66e+01,$
    5.85e+01,6.03e+01,6.22e+01,6.41e+01,6.59e+01,6.77e+01,6.95e+01,7.12e+01,7.29e+01,$
    7.45e+01,7.60e+01,7.74e+01,7.87e+01,8.00e+01,8.11e+01,8.21e+01,8.31e+01,8.39e+01,$
    8.47e+01,8.54e+01,8.60e+01]
 b8_cor[2,*]=[4.99e-05,9.55e-05,1.77e-04,3.18e-04,5.50e-04,9.19e-04,1.54e-03,2.43e-03,$
    3.75e-03,5.70e-03,8.53e-03,1.26e-02,1.85e-02,2.68e-02,3.83e-02,5.45e-02,7.59e-02,$
    1.04e-01,1.41e-01,1.89e-01,2.48e-01,3.22e-01,4.12e-01,5.20e-01,6.51e-01,8.06e-01,$
    9.88e-01,1.20e+00,1.45e+00,1.74e+00,2.07e+00,2.45e+00,2.87e+00,3.35e+00,3.88e+00,$
    4.47e+00,5.12e+00,5.83e+00,6.61e+00,7.44e+00,8.34e+00,9.29e+00,1.03e+01,1.14e+01,$
    1.25e+01,1.36e+01,1.48e+01,1.60e+01,1.72e+01,1.85e+01,1.97e+01,2.09e+01,2.21e+01,$
    2.33e+01,2.44e+01,2.54e+01,2.64e+01,2.74e+01,2.83e+01,2.92e+01,3.01e+01,3.11e+01,$
    3.20e+01,3.30e+01,3.40e+01,3.51e+01,3.62e+01,3.74e+01,3.87e+01,4.00e+01,4.14e+01,$
    4.29e+01,4.45e+01,4.61e+01,4.77e+01,4.94e+01,5.12e+01,5.30e+01,5.48e+01,5.66e+01,$
    5.85e+01,6.03e+01,6.22e+01,6.41e+01,6.59e+01,6.77e+01,6.95e+01,7.12e+01,7.29e+01,$
    7.45e+01,7.60e+01,7.74e+01,7.87e+01,8.00e+01,8.11e+01,8.21e+01,8.31e+01,8.39e+01,$
    8.47e+01,8.54e+01,8.60e+01]
 b8_cor[3,*]=[4.36e-05,8.34e-05,1.55e-04,2.78e-04,4.80e-04,8.02e-04,1.34e-03,2.12e-03,$
    3.28e-03,4.97e-03,7.45e-03,1.10e-02,1.61e-02,2.34e-02,3.34e-02,4.75e-02,6.63e-02,$
    9.11e-02,1.23e-01,1.65e-01,2.16e-01,2.81e-01,3.59e-01,4.54e-01,5.68e-01,7.03e-01,$
    8.63e-01,1.05e+00,1.27e+00,1.52e+00,1.81e+00,2.13e+00,2.51e+00,2.92e+00,3.39e+00,$
    3.90e+00,4.47e+00,5.09e+00,5.77e+00,6.50e+00,7.28e+00,8.11e+00,8.98e+00,9.91e+00,$
    1.09e+01,1.19e+01,1.29e+01,1.40e+01,1.50e+01,1.61e+01,1.72e+01,1.83e+01,1.93e+01,$
    2.03e+01,2.13e+01,2.22e+01,2.31e+01,2.39e+01,2.47e+01,2.55e+01,2.63e+01,2.71e+01,$
    2.79e+01,2.88e+01,2.97e+01,3.06e+01,3.16e+01,3.27e+01,3.38e+01,3.49e+01,3.62e+01,$
    3.75e+01,3.88e+01,4.02e+01,4.16e+01,4.31e+01,4.47e+01,4.62e+01,4.78e+01,4.94e+01,$
    5.10e+01,5.27e+01,5.43e+01,5.59e+01,5.75e+01,5.91e+01,6.06e+01,6.21e+01,6.36e+01,$
    6.50e+01,6.63e+01,6.76e+01,6.87e+01,6.98e+01,7.08e+01,7.17e+01,7.25e+01,7.32e+01,$
    7.39e+01,7.45e+01,7.50e+01]
 b8_cor[4,*]=[4.11e-05,7.86e-05,1.46e-04,2.61e-04,4.52e-04,7.55e-04,1.26e-03,2.00e-03,$
    3.09e-03,4.69e-03,7.02e-03,1.04e-02,1.52e-02,2.20e-02,3.15e-02,4.48e-02,6.24e-02,$
    8.58e-02,1.16e-01,1.55e-01,2.04e-01,2.64e-01,3.38e-01,4.28e-01,5.35e-01,6.62e-01,$
    8.13e-01,9.89e-01,1.19e+00,1.43e+00,1.70e+00,2.01e+00,2.36e+00,2.75e+00,3.19e+00,$
    3.68e+00,4.21e+00,4.80e+00,5.43e+00,6.12e+00,6.86e+00,7.64e+00,8.46e+00,9.33e+00,$
    1.02e+01,1.12e+01,1.22e+01,1.32e+01,1.42e+01,1.52e+01,1.62e+01,1.72e+01,1.82e+01,$
    1.91e+01,2.00e+01,2.09e+01,2.17e+01,2.25e+01,2.33e+01,2.40e+01,2.48e+01,2.55e+01,$
    2.63e+01,2.71e+01,2.80e+01,2.89e+01,2.98e+01,3.08e+01,3.18e+01,3.29e+01,3.41e+01,$
    3.53e+01,3.66e+01,3.79e+01,3.92e+01,4.06e+01,4.21e+01,4.35e+01,4.50e+01,4.66e+01,$
    4.81e+01,4.96e+01,5.12e+01,5.27e+01,5.42e+01,5.57e+01,5.71e+01,5.86e+01,5.99e+01,$
    6.12e+01,6.25e+01,6.37e+01,6.48e+01,6.58e+01,6.67e+01,6.75e+01,6.83e+01,6.90e+01,$
    6.96e+01,7.02e+01,7.07e+01]
 b8_cor[5,*]=[4.18e-05,7.95e-05,1.47e-04,2.62e-04,4.52e-04,7.53e-04,1.25e-03,1.98e-03,$
    3.05e-03,4.63e-03,6.91e-03,1.02e-02,1.49e-02,2.16e-02,3.08e-02,4.37e-02,6.08e-02,$
    8.35e-02,1.13e-01,1.50e-01,1.98e-01,2.56e-01,3.28e-01,4.14e-01,5.17e-01,6.40e-01,$
    7.84e-01,9.53e-01,1.15e+00,1.38e+00,1.64e+00,1.93e+00,2.27e+00,2.64e+00,3.06e+00,$
    3.52e+00,4.03e+00,4.59e+00,5.20e+00,5.85e+00,6.55e+00,7.29e+00,8.07e+00,8.90e+00,$
    9.76e+00,1.07e+01,1.16e+01,1.25e+01,1.35e+01,1.44e+01,1.54e+01,1.63e+01,1.72e+01,$
    1.81e+01,1.90e+01,1.98e+01,2.06e+01,2.13e+01,2.20e+01,2.27e+01,2.34e+01,2.41e+01,$
    2.48e+01,2.55e+01,2.63e+01,2.71e+01,2.80e+01,2.89e+01,2.98e+01,3.08e+01,3.19e+01,$
    3.30e+01,3.42e+01,3.54e+01,3.66e+01,3.79e+01,3.93e+01,4.06e+01,4.20e+01,4.34e+01,$
    4.48e+01,4.62e+01,4.77e+01,4.91e+01,5.05e+01,5.18e+01,5.32e+01,5.45e+01,5.57e+01,$
    5.70e+01,5.81e+01,5.92e+01,6.02e+01,6.12e+01,6.20e+01,6.28e+01,6.35e+01,6.41e+01,$
    6.47e+01,6.52e+01,6.57e+01]
 b8_cor[6,*]=[3.63e-05,7.05e-05,1.33e-04,2.40e-04,4.19e-04,7.05e-04,1.19e-03,1.89e-03,$
    2.93e-03,4.48e-03,6.75e-03,1.00e-02,1.48e-02,2.15e-02,3.10e-02,4.43e-02,6.21e-02,$
    8.57e-02,1.16e-01,1.56e-01,2.06e-01,2.68e-01,3.44e-01,4.36e-01,5.47e-01,6.79e-01,$
    8.35e-01,1.02e+00,1.23e+00,1.48e+00,1.77e+00,2.09e+00,2.46e+00,2.88e+00,3.34e+00,$
    3.86e+00,4.43e+00,5.06e+00,5.74e+00,6.48e+00,7.28e+00,8.13e+00,9.03e+00,9.97e+00,$
    1.10e+01,1.20e+01,1.31e+01,1.42e+01,1.53e+01,1.64e+01,1.76e+01,1.87e+01,1.98e+01,$
    2.08e+01,2.19e+01,2.29e+01,2.38e+01,2.48e+01,2.57e+01,2.66e+01,2.75e+01,2.84e+01,$
    2.93e+01,3.02e+01,3.12e+01,3.23e+01,3.34e+01,3.45e+01,3.58e+01,3.70e+01,3.84e+01,$
    3.98e+01,4.13e+01,4.28e+01,4.44e+01,4.60e+01,4.77e+01,4.94e+01,5.11e+01,5.28e+01,$
    5.46e+01,5.64e+01,5.82e+01,5.99e+01,6.17e+01,6.34e+01,6.51e+01,6.67e+01,6.83e+01,$
    6.98e+01,7.12e+01,7.26e+01,7.38e+01,7.50e+01,7.61e+01,7.71e+01,7.80e+01,7.88e+01,$
    7.95e+01,8.02e+01,8.07e+01]
 b8_cor[7,*]=[6.06e-05,1.14e-04,2.07e-04,3.67e-04,6.27e-04,1.04e-03,1.72e-03,2.70e-03,$
    4.14e-03,6.25e-03,9.29e-03,1.37e-02,1.99e-02,2.86e-02,4.07e-02,5.75e-02,7.98e-02,$
    1.09e-01,1.47e-01,1.96e-01,2.57e-01,3.32e-01,4.24e-01,5.34e-01,6.66e-01,8.22e-01,$
    1.01e+00,1.22e+00,1.47e+00,1.76e+00,2.09e+00,2.46e+00,2.88e+00,3.35e+00,3.88e+00,$
    4.45e+00,5.09e+00,5.78e+00,6.53e+00,7.34e+00,8.21e+00,9.12e+00,1.01e+01,1.11e+01,$
    1.22e+01,1.33e+01,1.44e+01,1.55e+01,1.67e+01,1.79e+01,1.90e+01,2.01e+01,2.12e+01,$
    2.23e+01,2.33e+01,2.43e+01,2.51e+01,2.60e+01,2.68e+01,2.76e+01,2.84e+01,2.92e+01,$
    3.00e+01,3.08e+01,3.17e+01,3.26e+01,3.36e+01,3.47e+01,3.58e+01,3.69e+01,3.82e+01,$
    3.95e+01,4.08e+01,4.22e+01,4.37e+01,4.52e+01,4.68e+01,4.83e+01,5.00e+01,5.16e+01,$
    5.33e+01,5.49e+01,5.66e+01,5.82e+01,5.99e+01,6.15e+01,6.30e+01,6.46e+01,6.60e+01,$
    6.75e+01,6.88e+01,7.01e+01,7.13e+01,7.24e+01,7.34e+01,7.43e+01,7.51e+01,7.59e+01,$
    7.65e+01,7.71e+01,7.77e+01]
 b8_cor[8,*]=[6.38e-05,1.19e-04,2.17e-04,3.83e-04,6.54e-04,1.08e-03,1.79e-03,2.80e-03,$
    4.29e-03,6.47e-03,9.61e-03,1.41e-02,2.05e-02,2.95e-02,4.19e-02,5.91e-02,8.20e-02,$
    1.12e-01,1.51e-01,2.01e-01,2.63e-01,3.40e-01,4.33e-01,5.46e-01,6.80e-01,8.40e-01,$
    1.03e+00,1.25e+00,1.50e+00,1.79e+00,2.12e+00,2.50e+00,2.93e+00,3.41e+00,3.94e+00,$
    4.52e+00,5.17e+00,5.87e+00,6.62e+00,7.44e+00,8.31e+00,9.24e+00,1.02e+01,1.12e+01,$
    1.23e+01,1.34e+01,1.45e+01,1.57e+01,1.68e+01,1.80e+01,1.92e+01,2.03e+01,2.14e+01,$
    2.25e+01,2.35e+01,2.44e+01,2.53e+01,2.61e+01,2.69e+01,2.77e+01,2.85e+01,2.92e+01,$
    3.00e+01,3.09e+01,3.17e+01,3.27e+01,3.36e+01,3.47e+01,3.58e+01,3.69e+01,3.82e+01,$
    3.94e+01,4.08e+01,4.22e+01,4.36e+01,4.51e+01,4.67e+01,4.82e+01,4.98e+01,5.15e+01,$
    5.31e+01,5.48e+01,5.64e+01,5.80e+01,5.97e+01,6.13e+01,6.28e+01,6.43e+01,6.58e+01,$
    6.72e+01,6.86e+01,6.98e+01,7.10e+01,7.21e+01,7.31e+01,7.40e+01,7.48e+01,7.56e+01,$
    7.62e+01,7.68e+01,7.74e+01]
 b8_cor[9,*]=[4.11e-05,7.93e-05,1.48e-04,2.67e-04,4.64e-04,7.78e-04,1.31e-03,2.07e-03,$
    3.21e-03,4.89e-03,7.34e-03,1.09e-02,1.60e-02,2.32e-02,3.33e-02,4.75e-02,6.63e-02,$
    9.13e-02,1.24e-01,1.65e-01,2.18e-01,2.83e-01,3.63e-01,4.60e-01,5.75e-01,7.13e-01,$
    8.76e-01,1.07e+00,1.29e+00,1.55e+00,1.84e+00,2.18e+00,2.56e+00,2.99e+00,3.47e+00,$
    4.00e+00,4.59e+00,5.23e+00,5.93e+00,6.69e+00,7.50e+00,8.37e+00,9.28e+00,1.02e+01,$
    1.13e+01,1.23e+01,1.34e+01,1.45e+01,1.56e+01,1.68e+01,1.79e+01,1.90e+01,2.01e+01,$
    2.12e+01,2.22e+01,2.32e+01,2.42e+01,2.50e+01,2.59e+01,2.68e+01,2.77e+01,2.86e+01,$
    2.95e+01,3.04e+01,3.14e+01,3.24e+01,3.34e+01,3.46e+01,3.58e+01,3.70e+01,3.84e+01,$
    3.97e+01,4.12e+01,4.27e+01,4.42e+01,4.58e+01,4.75e+01,4.91e+01,5.08e+01,5.26e+01,$
    5.43e+01,5.61e+01,5.78e+01,5.95e+01,6.13e+01,6.30e+01,6.46e+01,6.62e+01,6.78e+01,$
    6.92e+01,7.07e+01,7.20e+01,7.33e+01,7.44e+01,7.55e+01,7.64e+01,7.73e+01,7.81e+01,$
    7.88e+01,7.95e+01,8.00e+01]
b8_cor[10,*]=[4.11e-05,7.93e-05,1.48e-04,2.67e-04,4.64e-04,7.78e-04,1.31e-03,2.07e-03,$
    3.21e-03,4.89e-03,7.34e-03,1.09e-02,1.60e-02,2.32e-02,3.33e-02,4.75e-02,6.63e-02,$
    9.13e-02,1.24e-01,1.65e-01,2.18e-01,2.83e-01,3.63e-01,4.60e-01,5.75e-01,7.13e-01,$
    8.76e-01,1.07e+00,1.29e+00,1.55e+00,1.84e+00,2.18e+00,2.56e+00,2.99e+00,3.47e+00,$
    4.00e+00,4.59e+00,5.23e+00,5.93e+00,6.69e+00,7.50e+00,8.37e+00,9.28e+00,1.02e+01,$
    1.13e+01,1.23e+01,1.34e+01,1.45e+01,1.56e+01,1.68e+01,1.79e+01,1.90e+01,2.01e+01,$
    2.12e+01,2.22e+01,2.32e+01,2.42e+01,2.50e+01,2.59e+01,2.68e+01,2.77e+01,2.86e+01,$
    2.95e+01,3.04e+01,3.14e+01,3.24e+01,3.34e+01,3.46e+01,3.58e+01,3.70e+01,3.84e+01,$
    3.97e+01,4.12e+01,4.27e+01,4.42e+01,4.58e+01,4.75e+01,4.91e+01,5.08e+01,5.26e+01,$
    5.43e+01,5.61e+01,5.78e+01,5.95e+01,6.13e+01,6.30e+01,6.46e+01,6.62e+01,6.78e+01,$
    6.92e+01,7.07e+01,7.20e+01,7.33e+01,7.44e+01,7.55e+01,7.64e+01,7.73e+01,7.81e+01,$
    7.88e+01,7.95e+01,8.00e+01]
b8_cor[11,*]=[4.95e-05,9.42e-05,1.74e-04,3.11e-04,5.36e-04,8.94e-04,1.49e-03,2.35e-03,$
    3.63e-03,5.50e-03,8.22e-03,1.22e-02,1.78e-02,2.57e-02,3.67e-02,5.21e-02,7.25e-02,$
    9.96e-02,1.35e-01,1.80e-01,2.36e-01,3.06e-01,3.91e-01,4.95e-01,6.18e-01,7.65e-01,$
    9.37e-01,1.14e+00,1.38e+00,1.65e+00,1.96e+00,2.31e+00,2.71e+00,3.16e+00,3.66e+00,$
    4.22e+00,4.83e+00,5.50e+00,6.23e+00,7.01e+00,7.85e+00,8.74e+00,9.68e+00,1.07e+01,$
    1.17e+01,1.28e+01,1.39e+01,1.50e+01,1.61e+01,1.73e+01,1.85e+01,1.96e+01,2.07e+01,$
    2.17e+01,2.28e+01,2.37e+01,2.47e+01,2.55e+01,2.64e+01,2.72e+01,2.80e+01,2.89e+01,$
    2.97e+01,3.06e+01,3.15e+01,3.25e+01,3.35e+01,3.46e+01,3.58e+01,3.70e+01,3.83e+01,$
    3.96e+01,4.10e+01,4.25e+01,4.40e+01,4.55e+01,4.71e+01,4.88e+01,5.04e+01,5.21e+01,$
    5.38e+01,5.55e+01,5.72e+01,5.89e+01,6.06e+01,6.22e+01,6.38e+01,6.54e+01,6.69e+01,$
    6.84e+01,6.98e+01,7.11e+01,7.23e+01,7.34e+01,7.45e+01,7.54e+01,7.62e+01,7.70e+01,$
    7.77e+01,7.83e+01,7.89e+01]

b8_pho=fltarr(12,101)

 b8_pho[0,*]=[3.42e-05,6.21e-05,1.10e-04,1.90e-04,3.20e-04,5.23e-04,8.76e-04,1.37e-03,$
    2.10e-03,3.17e-03,4.72e-03,6.94e-03,1.01e-02,1.44e-02,2.04e-02,2.87e-02,3.96e-02,$
    5.40e-02,7.24e-02,9.59e-02,1.25e-01,1.61e-01,2.04e-01,2.55e-01,3.15e-01,3.86e-01,$
    4.69e-01,5.64e-01,6.74e-01,8.01e-01,9.44e-01,1.11e+00,1.29e+00,1.50e+00,1.72e+00,$
    1.97e+00,2.25e+00,2.56e+00,2.89e+00,3.24e+00,3.63e+00,4.05e+00,4.49e+00,4.97e+00,$
    5.48e+00,6.01e+00,6.58e+00,7.17e+00,7.79e+00,8.43e+00,9.10e+00,9.77e+00,1.05e+01,$
    1.12e+01,1.19e+01,1.26e+01,1.33e+01,1.41e+01,1.48e+01,1.56e+01,1.64e+01,1.72e+01,$
    1.80e+01,1.88e+01,1.97e+01,2.06e+01,2.15e+01,2.25e+01,2.35e+01,2.46e+01,2.56e+01,$
    2.68e+01,2.79e+01,2.91e+01,3.03e+01,3.16e+01,3.28e+01,3.41e+01,3.55e+01,3.68e+01,$
    3.82e+01,3.96e+01,4.09e+01,4.23e+01,4.37e+01,4.51e+01,4.65e+01,4.79e+01,4.92e+01,$
    5.06e+01,5.19e+01,5.31e+01,5.44e+01,5.56e+01,5.68e+01,5.79e+01,5.90e+01,6.00e+01,$
    6.10e+01,6.20e+01,6.29e+01]
 b8_pho[1,*]=[3.52e-05,6.38e-05,1.13e-04,1.96e-04,3.29e-04,5.37e-04,9.00e-04,1.41e-03,$
    2.16e-03,3.26e-03,4.85e-03,7.13e-03,1.04e-02,1.48e-02,2.10e-02,2.95e-02,4.07e-02,$
    5.54e-02,7.44e-02,9.85e-02,1.29e-01,1.65e-01,2.09e-01,2.62e-01,3.24e-01,3.97e-01,$
    4.82e-01,5.80e-01,6.93e-01,8.23e-01,9.70e-01,1.14e+00,1.33e+00,1.54e+00,1.77e+00,$
    2.03e+00,2.31e+00,2.63e+00,2.97e+00,3.33e+00,3.73e+00,4.16e+00,4.62e+00,5.11e+00,$
    5.63e+00,6.18e+00,6.76e+00,7.37e+00,8.00e+00,8.66e+00,9.35e+00,1.00e+01,1.08e+01,$
    1.15e+01,1.22e+01,1.30e+01,1.37e+01,1.45e+01,1.52e+01,1.60e+01,1.68e+01,1.77e+01,$
    1.85e+01,1.94e+01,2.03e+01,2.12e+01,2.21e+01,2.31e+01,2.42e+01,2.52e+01,2.63e+01,$
    2.75e+01,2.87e+01,2.99e+01,3.11e+01,3.24e+01,3.37e+01,3.51e+01,3.64e+01,3.78e+01,$
    3.92e+01,4.07e+01,4.21e+01,4.35e+01,4.49e+01,4.64e+01,4.78e+01,4.92e+01,5.06e+01,$
    5.20e+01,5.33e+01,5.46e+01,5.59e+01,5.71e+01,5.83e+01,5.95e+01,6.06e+01,6.17e+01,$
    6.27e+01,6.37e+01,6.46e+01]
 b8_pho[2,*]=[3.52e-05,6.38e-05,1.13e-04,1.96e-04,3.29e-04,5.37e-04,9.00e-04,1.41e-03,$
    2.16e-03,3.26e-03,4.85e-03,7.13e-03,1.04e-02,1.48e-02,2.10e-02,2.95e-02,4.07e-02,$
    5.54e-02,7.44e-02,9.85e-02,1.29e-01,1.65e-01,2.09e-01,2.62e-01,3.24e-01,3.97e-01,$
    4.82e-01,5.80e-01,6.93e-01,8.23e-01,9.70e-01,1.14e+00,1.33e+00,1.54e+00,1.77e+00,$
    2.03e+00,2.31e+00,2.63e+00,2.97e+00,3.33e+00,3.73e+00,4.16e+00,4.62e+00,5.11e+00,$
    5.63e+00,6.18e+00,6.76e+00,7.37e+00,8.00e+00,8.66e+00,9.35e+00,1.00e+01,1.08e+01,$
    1.15e+01,1.22e+01,1.30e+01,1.37e+01,1.45e+01,1.52e+01,1.60e+01,1.68e+01,1.77e+01,$
    1.85e+01,1.94e+01,2.03e+01,2.12e+01,2.21e+01,2.31e+01,2.42e+01,2.52e+01,2.63e+01,$
    2.75e+01,2.87e+01,2.99e+01,3.11e+01,3.24e+01,3.37e+01,3.51e+01,3.64e+01,3.78e+01,$
    3.92e+01,4.07e+01,4.21e+01,4.35e+01,4.49e+01,4.64e+01,4.78e+01,4.92e+01,5.06e+01,$
    5.20e+01,5.33e+01,5.46e+01,5.59e+01,5.71e+01,5.83e+01,5.95e+01,6.06e+01,6.17e+01,$
    6.27e+01,6.37e+01,6.46e+01]
 b8_pho[3,*]=[3.07e-05,5.57e-05,9.88e-05,1.71e-04,2.87e-04,4.69e-04,7.86e-04,1.23e-03,$
    1.89e-03,2.85e-03,4.24e-03,6.23e-03,9.04e-03,1.30e-02,1.83e-02,2.57e-02,3.55e-02,$
    4.84e-02,6.50e-02,8.60e-02,1.12e-01,1.44e-01,1.83e-01,2.29e-01,2.83e-01,3.46e-01,$
    4.20e-01,5.06e-01,6.05e-01,7.18e-01,8.47e-01,9.93e-01,1.16e+00,1.34e+00,1.55e+00,$
    1.77e+00,2.02e+00,2.29e+00,2.59e+00,2.91e+00,3.26e+00,3.63e+00,4.03e+00,4.46e+00,$
    4.91e+00,5.39e+00,5.90e+00,6.43e+00,6.99e+00,7.56e+00,8.16e+00,8.77e+00,9.39e+00,$
    1.00e+01,1.07e+01,1.13e+01,1.20e+01,1.26e+01,1.33e+01,1.40e+01,1.47e+01,1.54e+01,$
    1.61e+01,1.69e+01,1.77e+01,1.85e+01,1.93e+01,2.02e+01,2.11e+01,2.20e+01,2.30e+01,$
    2.40e+01,2.50e+01,2.61e+01,2.72e+01,2.83e+01,2.94e+01,3.06e+01,3.18e+01,3.30e+01,$
    3.42e+01,3.55e+01,3.67e+01,3.80e+01,3.92e+01,4.05e+01,4.17e+01,4.29e+01,4.42e+01,$
    4.53e+01,4.65e+01,4.77e+01,4.88e+01,4.99e+01,5.09e+01,5.19e+01,5.29e+01,5.38e+01,$
    5.47e+01,5.56e+01,5.64e+01]
 b8_pho[4,*]=[2.89e-05,5.24e-05,9.31e-05,1.61e-04,2.70e-04,4.42e-04,7.40e-04,1.16e-03,$
    1.78e-03,2.68e-03,3.99e-03,5.87e-03,8.51e-03,1.22e-02,1.73e-02,2.42e-02,3.35e-02,$
    4.56e-02,6.12e-02,8.10e-02,1.06e-01,1.36e-01,1.72e-01,2.15e-01,2.66e-01,3.26e-01,$
    3.96e-01,4.77e-01,5.70e-01,6.77e-01,7.98e-01,9.36e-01,1.09e+00,1.26e+00,1.46e+00,$
    1.67e+00,1.90e+00,2.16e+00,2.44e+00,2.74e+00,3.07e+00,3.42e+00,3.80e+00,4.20e+00,$
    4.63e+00,5.08e+00,5.56e+00,6.06e+00,6.58e+00,7.12e+00,7.69e+00,8.26e+00,8.84e+00,$
    9.44e+00,1.00e+01,1.07e+01,1.13e+01,1.19e+01,1.25e+01,1.32e+01,1.38e+01,1.45e+01,$
    1.52e+01,1.59e+01,1.67e+01,1.74e+01,1.82e+01,1.90e+01,1.99e+01,2.08e+01,2.17e+01,$
    2.26e+01,2.36e+01,2.46e+01,2.56e+01,2.67e+01,2.77e+01,2.88e+01,3.00e+01,3.11e+01,$
    3.23e+01,3.34e+01,3.46e+01,3.58e+01,3.70e+01,3.81e+01,3.93e+01,4.05e+01,4.16e+01,$
    4.27e+01,4.38e+01,4.49e+01,4.60e+01,4.70e+01,4.80e+01,4.89e+01,4.98e+01,5.07e+01,$
    5.16e+01,5.24e+01,5.32e+01]
 b8_pho[5,*]=[2.97e-05,5.36e-05,9.46e-05,1.63e-04,2.73e-04,4.44e-04,7.41e-04,1.16e-03,$
    1.77e-03,2.66e-03,3.95e-03,5.79e-03,8.38e-03,1.20e-02,1.69e-02,2.37e-02,3.27e-02,$
    4.44e-02,5.96e-02,7.87e-02,1.03e-01,1.32e-01,1.67e-01,2.08e-01,2.57e-01,3.15e-01,$
    3.82e-01,4.59e-01,5.48e-01,6.50e-01,7.66e-01,8.98e-01,1.05e+00,1.21e+00,1.39e+00,$
    1.60e+00,1.82e+00,2.06e+00,2.33e+00,2.61e+00,2.92e+00,3.26e+00,3.61e+00,3.99e+00,$
    4.40e+00,4.82e+00,5.27e+00,5.74e+00,6.24e+00,6.75e+00,7.28e+00,7.81e+00,8.36e+00,$
    8.92e+00,9.49e+00,1.01e+01,1.06e+01,1.12e+01,1.18e+01,1.24e+01,1.30e+01,1.36e+01,$
    1.43e+01,1.50e+01,1.56e+01,1.63e+01,1.71e+01,1.78e+01,1.86e+01,1.94e+01,2.03e+01,$
    2.12e+01,2.21e+01,2.30e+01,2.39e+01,2.49e+01,2.59e+01,2.69e+01,2.80e+01,2.90e+01,$
    3.01e+01,3.12e+01,3.23e+01,3.34e+01,3.45e+01,3.55e+01,3.66e+01,3.77e+01,3.88e+01,$
    3.98e+01,4.08e+01,4.18e+01,4.28e+01,4.37e+01,4.46e+01,4.55e+01,4.64e+01,4.72e+01,$
    4.80e+01,4.87e+01,4.95e+01]
 b8_pho[6,*]=[2.49e-05,4.60e-05,8.27e-05,1.45e-04,2.46e-04,4.05e-04,6.85e-04,1.08e-03,$
    1.67e-03,2.54e-03,3.80e-03,5.63e-03,8.22e-03,1.19e-02,1.69e-02,2.39e-02,3.31e-02,$
    4.53e-02,6.12e-02,8.13e-02,1.07e-01,1.37e-01,1.75e-01,2.20e-01,2.73e-01,3.35e-01,$
    4.08e-01,4.93e-01,5.92e-01,7.04e-01,8.33e-01,9.80e-01,1.15e+00,1.33e+00,1.54e+00,$
    1.77e+00,2.02e+00,2.30e+00,2.60e+00,2.93e+00,3.29e+00,3.68e+00,4.09e+00,4.54e+00,$
    5.01e+00,5.51e+00,6.04e+00,6.60e+00,7.18e+00,7.79e+00,8.41e+00,9.06e+00,9.72e+00,$
    1.04e+01,1.11e+01,1.18e+01,1.25e+01,1.32e+01,1.39e+01,1.47e+01,1.54e+01,1.62e+01,$
    1.70e+01,1.78e+01,1.87e+01,1.96e+01,2.05e+01,2.14e+01,2.24e+01,2.34e+01,2.44e+01,$
    2.55e+01,2.66e+01,2.78e+01,2.90e+01,3.02e+01,3.14e+01,3.27e+01,3.40e+01,3.53e+01,$
    3.66e+01,3.80e+01,3.93e+01,4.07e+01,4.20e+01,4.34e+01,4.47e+01,4.60e+01,4.73e+01,$
    4.86e+01,4.99e+01,5.11e+01,5.24e+01,5.35e+01,5.47e+01,5.58e+01,5.68e+01,5.78e+01,$
    5.88e+01,5.98e+01,6.07e+01]
 b8_pho[7,*]=[4.39e-05,7.82e-05,1.37e-04,2.33e-04,3.86e-04,6.23e-04,1.03e-03,1.60e-03,$
    2.43e-03,3.63e-03,5.36e-03,7.82e-03,1.13e-02,1.60e-02,2.25e-02,3.14e-02,4.31e-02,$
    5.84e-02,7.80e-02,1.03e-01,1.33e-01,1.71e-01,2.15e-01,2.68e-01,3.31e-01,4.04e-01,$
    4.89e-01,5.86e-01,6.99e-01,8.27e-01,9.72e-01,1.14e+00,1.32e+00,1.53e+00,1.75e+00,$
    2.00e+00,2.28e+00,2.58e+00,2.90e+00,3.26e+00,3.64e+00,4.04e+00,4.48e+00,4.94e+00,$
    5.43e+00,5.95e+00,6.50e+00,7.07e+00,7.66e+00,8.28e+00,8.92e+00,9.56e+00,1.02e+01,$
    1.09e+01,1.16e+01,1.22e+01,1.29e+01,1.36e+01,1.43e+01,1.50e+01,1.57e+01,1.65e+01,$
    1.72e+01,1.80e+01,1.88e+01,1.96e+01,2.05e+01,2.14e+01,2.23e+01,2.33e+01,2.43e+01,$
    2.53e+01,2.63e+01,2.74e+01,2.86e+01,2.97e+01,3.09e+01,3.21e+01,3.33e+01,3.45e+01,$
    3.58e+01,3.71e+01,3.84e+01,3.96e+01,4.09e+01,4.22e+01,4.35e+01,4.47e+01,4.60e+01,$
    4.72e+01,4.84e+01,4.96e+01,5.07e+01,5.18e+01,5.29e+01,5.39e+01,5.49e+01,5.59e+01,$
    5.68e+01,5.77e+01,5.85e+01]
 b8_pho[8,*]=[4.65e-05,8.25e-05,1.44e-04,2.44e-04,4.04e-04,6.52e-04,1.08e-03,1.66e-03,$
    2.53e-03,3.77e-03,5.56e-03,8.10e-03,1.16e-02,1.65e-02,2.32e-02,3.23e-02,4.43e-02,$
    6.00e-02,8.01e-02,1.05e-01,1.37e-01,1.75e-01,2.20e-01,2.74e-01,3.38e-01,4.12e-01,$
    4.98e-01,5.97e-01,7.11e-01,8.41e-01,9.88e-01,1.15e+00,1.34e+00,1.55e+00,1.78e+00,$
    2.03e+00,2.31e+00,2.61e+00,2.94e+00,3.29e+00,3.68e+00,4.08e+00,4.52e+00,4.99e+00,$
    5.48e+00,6.00e+00,6.55e+00,7.12e+00,7.72e+00,8.33e+00,8.97e+00,9.62e+00,1.03e+01,$
    1.09e+01,1.16e+01,1.23e+01,1.30e+01,1.37e+01,1.43e+01,1.50e+01,1.58e+01,1.65e+01,$
    1.72e+01,1.80e+01,1.88e+01,1.96e+01,2.05e+01,2.14e+01,2.23e+01,2.32e+01,2.42e+01,$
    2.53e+01,2.63e+01,2.74e+01,2.85e+01,2.97e+01,3.08e+01,3.20e+01,3.32e+01,3.45e+01,$
    3.57e+01,3.70e+01,3.83e+01,3.95e+01,4.08e+01,4.21e+01,4.33e+01,4.46e+01,4.58e+01,$
    4.70e+01,4.82e+01,4.94e+01,5.05e+01,5.16e+01,5.27e+01,5.37e+01,5.47e+01,5.57e+01,$
    5.66e+01,5.75e+01,5.83e+01]
 b8_pho[9,*]=[2.86e-05,5.23e-05,9.34e-05,1.62e-04,2.75e-04,4.51e-04,7.59e-04,1.19e-03,$
    1.84e-03,2.78e-03,4.15e-03,6.12e-03,8.91e-03,1.28e-02,1.82e-02,2.56e-02,3.55e-02,$
    4.84e-02,6.51e-02,8.64e-02,1.13e-01,1.45e-01,1.85e-01,2.31e-01,2.87e-01,3.52e-01,$
    4.28e-01,5.16e-01,6.17e-01,7.34e-01,8.66e-01,1.02e+00,1.19e+00,1.38e+00,1.59e+00,$
    1.82e+00,2.08e+00,2.37e+00,2.68e+00,3.01e+00,3.37e+00,3.77e+00,4.19e+00,4.63e+00,$
    5.11e+00,5.62e+00,6.15e+00,6.71e+00,7.29e+00,7.90e+00,8.53e+00,9.18e+00,9.84e+00,$
    1.05e+01,1.12e+01,1.19e+01,1.26e+01,1.33e+01,1.40e+01,1.47e+01,1.55e+01,1.63e+01,$
    1.71e+01,1.79e+01,1.87e+01,1.96e+01,2.05e+01,2.14e+01,2.24e+01,2.34e+01,2.44e+01,$
    2.55e+01,2.66e+01,2.77e+01,2.89e+01,3.01e+01,3.13e+01,3.26e+01,3.38e+01,3.51e+01,$
    3.64e+01,3.78e+01,3.91e+01,4.04e+01,4.18e+01,4.31e+01,4.44e+01,4.57e+01,4.70e+01,$
    4.83e+01,4.96e+01,5.08e+01,5.20e+01,5.31e+01,5.43e+01,5.53e+01,5.64e+01,5.74e+01,$
    5.84e+01,5.93e+01,6.02e+01]
b8_pho[10,*]=[2.86e-05,5.23e-05,9.34e-05,1.62e-04,2.75e-04,4.51e-04,7.59e-04,1.19e-03,$
    1.84e-03,2.78e-03,4.15e-03,6.12e-03,8.91e-03,1.28e-02,1.82e-02,2.56e-02,3.55e-02,$
    4.84e-02,6.51e-02,8.64e-02,1.13e-01,1.45e-01,1.85e-01,2.31e-01,2.87e-01,3.52e-01,$
    4.28e-01,5.16e-01,6.17e-01,7.34e-01,8.66e-01,1.02e+00,1.19e+00,1.38e+00,1.59e+00,$
    1.82e+00,2.08e+00,2.37e+00,2.68e+00,3.01e+00,3.37e+00,3.77e+00,4.19e+00,4.63e+00,$
    5.11e+00,5.62e+00,6.15e+00,6.71e+00,7.29e+00,7.90e+00,8.53e+00,9.18e+00,9.84e+00,$
    1.05e+01,1.12e+01,1.19e+01,1.26e+01,1.33e+01,1.40e+01,1.47e+01,1.55e+01,1.63e+01,$
    1.71e+01,1.79e+01,1.87e+01,1.96e+01,2.05e+01,2.14e+01,2.24e+01,2.34e+01,2.44e+01,$
    2.55e+01,2.66e+01,2.77e+01,2.89e+01,3.01e+01,3.13e+01,3.26e+01,3.38e+01,3.51e+01,$
    3.64e+01,3.78e+01,3.91e+01,4.04e+01,4.18e+01,4.31e+01,4.44e+01,4.57e+01,4.70e+01,$
    4.83e+01,4.96e+01,5.08e+01,5.20e+01,5.31e+01,5.43e+01,5.53e+01,5.64e+01,5.74e+01,$
    5.84e+01,5.93e+01,6.02e+01]
b8_pho[11,*]=[3.51e-05,6.34e-05,1.12e-04,1.93e-04,3.23e-04,5.26e-04,8.79e-04,1.37e-03,$
    2.10e-03,3.16e-03,4.70e-03,6.89e-03,9.97e-03,1.43e-02,2.02e-02,2.83e-02,3.90e-02,$
    5.30e-02,7.11e-02,9.39e-02,1.22e-01,1.57e-01,1.99e-01,2.49e-01,3.07e-01,3.76e-01,$
    4.56e-01,5.49e-01,6.56e-01,7.78e-01,9.17e-01,1.07e+00,1.25e+00,1.45e+00,1.67e+00,$
    1.91e+00,2.18e+00,2.47e+00,2.79e+00,3.13e+00,3.50e+00,3.90e+00,4.33e+00,4.79e+00,$
    5.27e+00,5.79e+00,6.33e+00,6.89e+00,7.48e+00,8.09e+00,8.73e+00,9.37e+00,1.00e+01,$
    1.07e+01,1.14e+01,1.21e+01,1.28e+01,1.35e+01,1.42e+01,1.49e+01,1.56e+01,1.64e+01,$
    1.72e+01,1.79e+01,1.88e+01,1.96e+01,2.05e+01,2.14e+01,2.24e+01,2.33e+01,2.43e+01,$
    2.54e+01,2.65e+01,2.76e+01,2.87e+01,2.99e+01,3.11e+01,3.23e+01,3.36e+01,3.49e+01,$
    3.62e+01,3.74e+01,3.88e+01,4.01e+01,4.14e+01,4.27e+01,4.40e+01,4.53e+01,4.65e+01,$
    4.78e+01,4.90e+01,5.02e+01,5.14e+01,5.25e+01,5.36e+01,5.47e+01,5.57e+01,5.67e+01,$
    5.76e+01,5.85e+01,5.94e+01]

if keyword_set(sat) then gsat=fix(sat-1)>0 else gsat=8-1 ; subtract 1 to get array index
if keyword_set(photospheric) then b8dat=reform(b8_pho[gsat,*]) else b8dat=reform(b8_cor[gsat,*])

; do spline fit

logtemp=findgen(101)*0.02d0     ; temp in MK as in goes_tem
b8_ftn=spl_init(logtemp,b8dat,/double)
denom=spl_interp(logtemp,b8dat,b8_ftn,alog10(temp),/double)
; print,denom
; print,'Spline result: ',temp
; assume that B8 = flux is in W/m^2, calibrate accordingly
em=1.d55*b8/denom

end