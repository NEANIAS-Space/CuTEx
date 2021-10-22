;+
; Project     : SOHO/CDS
;                   
; Name        : VALID_FITS
;               
; Purpose     : check if file is a valid FITS file
;               
; Category    : FITS, utility
;               
; Syntax      : IDL> valid=valid_fits(file)
;    
; Inputs      : FILE = FITS file name
;               
; Opt. Inputs : None.
;               
; Outputs     : VALID = 1/0 if valid or not 
;
; History     : 28-Oct-98, Zarro (SMA/GSFC) - written
;               24-Feb-03, Zarro (EER/GSFC) - vectorized and added
;               support for compressed files
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-      

function valid_fits,file

if is_blank(file) then return,0b
supp_compressed=since_version('5.3')
np=n_elements(file)
valid=bytarr(np)
for i=0,np-1 do begin
 error=0
 if supp_compressed then begin
  compressed=strpos(file[i],'.gz') gt -1
  openr, unit, file[i], /get_lun, /block,error=error,compress=compressed
 endif else  openr, unit, file[i], /get_lun, /block,error=error
 if error eq 0 then begin
  status=-1
  fxhread, unit, header,status 
  valid[i]=status eq 0
 endif
 close_lun, unit
endfor

if np eq 1 then valid=valid[0]

return,valid & end

