;+
; Name:
;	GOES_TEM
; Purpose:
;     Drop-in replacement for old TEM_CALC that allows abundance choices.
;	TEM_CALC calculates the temperature and emission measure from the GOES ionization X-ray chambers.
;
; 	The data array must already be cleaned of gain change spikes.  The background
;	in each channel is input through avback or bkarray calling argument.
;
; Explanation:
;	This procedure organizes the inputs and outputs into the GOES_MEWE_TEM and GOES_CHIANTI_TEM procedures.
;   GOES_MEWE_TEM interpolates on
;	the ratio between the long and short wavelength channel fluxes using pre-calcuated tables for the
;	fluxes at a series of temperatures for fixed emission measure.  Note that ratios which lie outside of
;	the default lower limits for the fluxes,
;	and particularly negative values in the case of background subtracted inputs, are
;	returned with fixed values of 4.0 MegaKelvin and 1.0e47 cm-3 for the temperature and emission measure.
;	Normally, those limits are 1e-7 and 1e-9 for the long and short wavelength channel inputs.
;
; Keywords:
;  Mandatory Inputs:
;	TARRAY- time array
;	YCLEAN-  GOES data cleaned of spikes.
;
;  Optional Inputs:
;   AVBACK - A single background value for each channel
;   BKARRAY - An array of background values for each channel (matches yclean array)
;	NOBACKSUB- If set, no background is subtracted
;	SAVESAT  - Number of GOES satellite, calibration changes at 8
;	DATE - date in anytim format if tarray is not fully qualified (only sec of day)
;  Outputs:
;
;	NOSUBTEMPR-temperature in Megakelvin, if background not subtracted.
;	NOSUBEMIS-emission measure in 1e49 cm-3, if background subtracted.
;
;	TEMPR- temperature in Megakelvin, if background subtracted.
;	EMIS-emission measure in 1e49 cm-3, if background subtracted.
;	ADUND-abundances to use: 0=coronal, 1=photospheric, 2=Meyer
; Common Blocks:
;	None
; Calls:
;	goes_mewe_tem, goes_chianti_tem
; History:
;	Kim Tolbert 11/92
;	Documented RAS, 15-May-1996,
;	changed cutoff limits, changed default response
;	to GOES_TEM, eback and sback removed as keywords since they did nothing.
;	ras, 22-july-1996, add savesat
;	ras, 20-nov-1996, added GOES6 and GOES7 as GOES_TEM was upgraded
;	ras, 29-jan-1997, add date keyword needed for correct GOES6 calculation!
;	ras, 4-aug-1998, richard.schwartz@gsfc.nasa.gov, integrate GOES10 and future GOESN.
;	smw, 14-feb-2005, add abundance keyword , calls to chianti routines for
;                       photospheric or coronal abundance cases
;   Kim, 13-Dec-2005, This was called sw_tem_calc by smw.  Change to goes_tem and
;      change the routine it calls for mewe (previously goes_tem) to goes_mewe_tem. Also
;      put avback in calling arguments instead of in common.
;   Kim, 9-Jan-2006.  Added bkarray keyword, and setting avback_ch0
;-
;
;  -----------------------------------------------------------------------

pro goes_tem, tarray=tarray, yclean=yclean, tempr=tempr, emis=emis, $
    nosubtempr=nosubtempr, nosubemis=nosubemis, savesat=savesat, $
     nobacksub=nobacksub, date=date, abund=abund, avback=avback, bkarray=bkarray

;common goes_back, sback_str, eback_str, avback

date_in = (anytim(/sec,tarray))(0)
if date_in lt 86400. then  date_in = date
if not keyword_set(abund) then abund=0     ; default = coronal

ysub = yclean

cutoff0 = 1.0e-7/10.
cutoff1 = 1.0e-9/10.

dosub=1
if keyword_set(nobacksub) then dosub=0
if not(dosub) then goto, docalc

avback_ch0 = -1

if keyword_set(avback) then if avback[0] ne -1. then begin
	for ich = 0,1 do ysub(*,ich) = yclean(*,ich) - avback(ich)
	avback_ch0 = avback[0]
endif

if keyword_set(bkarray) then if bkarray[0] ne -1. then begin
	ysub = yclean - bkarray
	avback_ch0 = average(bkarray[*,0])
endif

if avback_ch0 gt 1.5e-6 then begin
	 cutoff0 = 5.e-7
	 cutoff1 = 5.e-9
endif


docalc:
q = where( ysub(*,0) lt cutoff0 or ysub(*,1) lt cutoff1, nq)

if (abund eq 2) then $
   goes_mewe_tem,  ysub(*,0), ysub(*,1), te, em, satellite=savesat,  date=date_in $
else if (abund eq 1) then $
   goes_chianti_tem, ysub(*,0), ysub(*,1), te, em, satellite=savesat, date=date_in, /photospheric $
else $
   goes_chianti_tem, ysub(*,0), ysub(*,1), te, em, satellite=savesat, date=date_in
if nq ge 1 then begin
	te(q) = 4.0
	em(q) = .01
endif
em = em
if dosub then tempr = te
if not(dosub) then nosubtempr = te
if dosub then emis = em
if not(dosub) then nosubemis = em

if dosub then begin
   dosub = 0
   ysub = yclean
   goto, docalc
endif

end
