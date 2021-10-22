function eit_find_last_cal, date
;+
;NAME
;   eit_find_last_cal
;PURPOSE 
;   finds the latest group of calibration lamps
;INPUTS
;   date  = date of observation
;OUTPUTS
;   returns the latest averaged calibration lamp
;KEYWORDS
;   none
;CREATION
;   12-Dec-2001. F. Auchere
;MODIFICATIONS
;   12-Aug-2002. F. Auchere : added fill in of missing blocks.
;   11-Jul-2003. J. Newmark : Windows compatibility
;-

  list = eit_calgroups()
  delim = get_delim()

  t0 = double(utc2sec(anytim2utc('01-jan-1996 00:00:00')))
  cal_end = strmid(list, 41, 20)
  etcal = double(utc2sec(anytim2utc(cal_end))) - t0
  time = double(utc2sec(anytim2utc(date))) - t0

  cal = where((etcal-time) lt 0, cb)

  if cb ge 1 then begin
  img = eit_cal_readfits(getenv('SSWDB')+delim+'soho'+delim+'eit'+delim+'calibrate'+$
         delim+strmid(list[cal[cb-1]], 0, 15), /silent)
    return, eit_fillmissingblocks(img>0)    
  endif else return, -1

end
;+
; Project     : SOHO/EIT
;
; Name        : EIT_CAL_READFITS
;
; Purpose     : Read EIT Calibration FITS file. 
;               Checks local directory and then remote server. 
;
; Category    : Analysis
;
; Inputs      : FILE = EIT calibration file name
;
; Outputs     : DATA = EIT calibration array
;
; Keywords    : None
;
; History     : 15-Nov-2006,  D.M. Zarro (ADNET)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU


function eit_cal_readfits,file,_ref_extra=extra

if is_blank(file) then return,-1

;-- read if found locally

chk=file_search(file,count=count)
if count eq 1 then return,readfits(file[0],_extra=extra)

;-- try remote server

server=eit_server(network=network,/full)
if ~network then return,-1

lfile=file_basename(file[0])
rfile=server+'/sdb/soho/eit/calibrate/'+lfile

sock_fits,rfile,data,_extra=extra

return,data

end
function eit_norm_response,date_obs,inwave,fits_header,verbose=verbose,$
           no_index=no_index, force_newflat=force_newflat
;+
;NAME
;   eit_norm_response
;PURPOSE
;  compute a normalized response for each eit band
;INPUTS
;   date_obs  = date of observation
;   inwave    = wavelength of observation
;   fits_header = header to obtain pointing/size information
;OUTPUTS
;   returns normalization factor to be applied to measured DN level
;   on given date such that all EIT data is normalized to 2-Feb-1996.
;KEYWORDS
;   no_index = set if do not want to use recommended Index correlation
;              for response scaling
;   verbose = set for testing
;WARNINGS
;   this routine may not always have the most recent normalizations
;   especially just following a bakeout.
;PROCEDURE
;   Uses EUV flat field determined by scaling from Calibration lamps
;CREATION
;   17-May-2001. J. Newmark and F. Auchere
;
;MODIFICATIONS
;   10-Dec-2001. J. Newmark inclusion of trend corrections
;   28-Feb-2002. J. Newmark inclusion of wavelength dependence
;                of flat field
;   11-Jul-2003. J. Newmark : Windows compatibility
;   02-Sep-2004. F. Auchère : added /nan keyword to total() when commputing
;                             the degrad parameter
;   30-May-2006. J. Newmark : Set extrapolation after last calibrated day equal
;                  to last calibrated day
;   16-Nov-2007. Zarro (ADNET): added check for undefined sflat
;-
common eit_norm_blk, wavelength, date, sflat, calorig

delim = get_delim()
if not keyword_set(verbose) then verbose = 0
if n_elements(wavelength) eq 0 then wavelength = 0
if n_elements(date) eq 0 then date = 0
mjd_1996_0 = 50083.0d0*8.64d4
date_obs = anytim2utc(date_obs,/vms)
temp_date = anytim2utc(date_obs)
date_ut = (8.64d4*temp_date.mjd + 1.e-3*temp_date.time) - mjd_1996_0
temp_obs = temp_date.mjd

ddir = getenv('SSW_EIT_RESPONSE')
filein = concat_dir(ddir,'fit_resp.dat')

; correction for CCE effect

if n_elements(sflat) eq 0 then force_newflat=1b

if (temp_obs ne date) or keyword_set(force_newflat) then begin
   date = temp_obs
   if verbose then message, /info, 'Reading in flat...'
   if n_elements(calorig) eq 0 then calorig = eit_cal_readfits(getenv('SSWDB')+delim+$
     'soho'+delim+'eit'+delim+'calibrate'+delim+'cal19951209.fts', /silent)
   visible = eit_getcal(date_obs)/calorig
   visible = visible/median(visible[922:1021, 1002:1021])
   degrad = total(visible, /nan)/total(visible ne 0)
   sflat = eit_vis2euv(visible,degrad=degrad)
endif else if verbose then message, /info, 'Using pre-read normalization'

if inwave eq 284 or inwave eq 304 then begin
   res = [0.0077716820, 0.97235556, -0.25301483, -0.24468922, -0.087938358]
   wsflat = 10^(res[0] + res[1]*alog10(sflat) + res[2]*alog10(sflat)^2.0 + $
             res[3]*alog10(sflat)^3.0 + res[4]*alog10(sflat)^4.0)
endif else wsflat = sflat

; correction for trend analysis
readcol,filein,wave,start_time,end_time,coeff_0,coeff_1,coeff_2,coeff_3,$
         format='I,D,D,D,D,D,D',/silent

bakes = eit_bakeouts()
tb = anytim2utc(bakes[*,1])
date_b = (8.64d4*tb.mjd + 1.e-3*tb.time) - mjd_1996_0
ok = where(date_b lt end_time[n_elements(end_time)-1])
bake_s = anytim2utc([bakes[ok,0],!stime])
bake_e = anytim2utc(['1996-Feb-01',bakes[ok,1]])

case 1 of
  inwave eq 171: begin
                  top = 0
                  top2 = 0.25*n_elements(end_time)
                 end
  inwave eq 195: begin
                  top = n_elements(end_time)*0.25
                  top2 = 0.5*n_elements(end_time)
                 end
  inwave eq 284: begin
                  top = n_elements(end_time)*0.5
                  top2 = 0.75*n_elements(end_time)
                 end
  inwave eq 304: begin
                  top = n_elements(end_time)*0.75
                  top2 = 1*n_elements(end_time)
                 end
  else:
endcase

date_ut = date_ut < round(end_time(top2-1))
index=(where(wave eq inwave and round(date_ut) ge round(start_time) and $
            round(date_ut) le round(end_time)))(0)

; 30- May-2006 Newmark
; ensure do not use days past final calibration (date_ut< end_time from above)
if index(0) ne -1 then begin
   grid = (temp_date.mjd < (date_ut+mjd_1996_0)/8.64d4)- bake_e(index-top).mjd
   nfactor = coeff_1(index)*exp(coeff_0(index)*grid)
endif else nfactor = 1.
if keyword_set(no_index) then nfactor = 1.
;

; proper binning and subfields
x_bin = 1 & y_bin = 1
if n_elements(fits_header) ne 0 then begin
   n_x = eit_fxpar(fits_header,'NAXIS1')
   n_y = eit_fxpar(fits_header,'NAXIS2')
endif else begin
   n_x = 1024
   n_y = 1024
endelse
corner_offset = [-1, -1, -20, -20]
if ((n_x + n_y) lt 2048) then begin
   corner = intarr(4)
   corner(0) = EIT_FXPAR(fits_header, 'P1_X')
   corner(1) = EIT_FXPAR(fits_header, 'P2_X')
   corner(2) = EIT_FXPAR(fits_header, 'P1_Y')
   corner(3) = EIT_FXPAR(fits_header, 'P2_Y')
   IF datatype(EIT_FXPAR(fits_header, 'OBJECT')) NE 'STR' THEN BEGIN
        ; Must be NRL header
        corner(0) = FXPAR(fits_header, 'P1ROW')
        corner(1) = FXPAR(fits_header, 'P2ROW')
        corner(2) = FXPAR(fits_header, 'P1COL')
        corner(3) = FXPAR(fits_header, 'P2COL')
   ENDIF
   if (corner(0) mod 2) and (corner(1) mod 2) then corner_offset = [-1,-2,-20,-20]
   corner = corner + corner_offset
endif else corner = [0, 1023, 0, 1023]

nx_grid = corner(1) - corner(0) + 1
if nx_grid gt n_x then x_bin = nx_grid/n_x

flat = wsflat(corner(0):corner(1), corner(2):corner(3))
if (x_bin ne 1) then flat = rebin(temporary(flat), n_x, n_y)

return, flat * nfactor
end
function eit_findcalgroup, date, dir, start_date = start_date, end_date = end_date
;+
;NAME
;   eit_findcalgroup
;PURPOSE 
;   finds an averages groups of calibration lamps taken within 24 hours
;INPUTS
;   date  = date of observation
;   dir = search direction. dir>=0 : forward, dir<0 : backwards
;OUTPUTS
;   returns an averaged calibration lamp
;KEYWORDS
;   start_date = date of the first calibration lamp
;   end_date = date of the last calibration lamp
;CREATION
;   18-May-2001. F. Auchere
;MODIFICATIONS
;    2-Jun-2001. F. Auchere : Completely re-written in order to use the new cal lamps data base
;   17-Sep-2001. F. Auchere : Bug fixed
;   11-Jul-2003. J. Newmark : Windows compatibility
;-

  list = eit_calgroups()
  delim = get_delim()

  t0 = double(utc2sec(anytim2utc('01-jan-1996 00:00:00')))
  cal_start = strmid(list, 18, 20)
  cal_end = strmid(list, 41, 20)
  stcal = double(utc2sec(anytim2utc(cal_start))) - t0
  etcal = double(utc2sec(anytim2utc(cal_end))) - t0
  time = double(utc2sec(anytim2utc(date))) - t0

  bake_on = double(utc2sec(anytim2utc((eit_bakeouts())[*, 0]))) - t0
  bake_off = double(utc2sec(anytim2utc((eit_bakeouts())[*, 1]))) - t0

  if dir ge 0 then begin
    cal  = where((stcal-time) gt 0, ca)                                  
    if ca ge 1 then begin
      cal = cal[0]
      next_bake  = where((bake_on-time) ge 0, ba)                                  
      if ba ge 1 then begin
        next_bake = next_bake[0]
        if bake_on[next_bake] lt etcal[cal] then begin
          bb = 0
          cal = -1
        endif
      endif
    endif
  endif else begin
    cal = where((etcal-time) lt 0, cb)
    if cb ge 1 then begin
      cal = cal[cb-1]
      previous_bake = where((bake_off-time) lt 0, bb)
      if bb ge 1 then begin
        previous_bake = previous_bake[bb-1]
        if bake_off[previous_bake] gt stcal[cal] then begin
          cb = 0
          cal = -1
        endif
      endif
    endif
  endelse 

  if cal eq -1 then begin
    if keyword_set(start_date) then start_date = ''
    if keyword_set(end_date) then end_date = ''
    return, -1 
  endif else begin
    if keyword_set(start_date) then start_date = cal_start[cal]
    if keyword_set(end_date) then end_date = cal_end[cal]
    cal = eit_cal_readfits(getenv('SSWDB')+delim+'soho'+delim+'eit'+delim+'calibrate'+$
           delim+strmid(list[cal], 0, 15), /silent)
    return, cal
  endelse

end

pro sock_eit

return & end
