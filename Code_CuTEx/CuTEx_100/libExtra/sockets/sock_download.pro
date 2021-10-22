;+
; Project     : HESSI
;
; Name        : SOCK_DOWNLOAD
;
; Purpose     : Download files using IDL-IDL bridge to
;               copy files in a background thread and, thus, avoid blocking.
;
; Category    : utility system sockets
;
; Syntax      : IDL> sock_download,files,out_dir=out_dir
;
; Inputs      : FILES = scalar or array of filenames
;
; Outputs     : None
;
; Keywords    : OUT_DIR = output directory to copy files
;               ERR   = string error message
;
; History     : 6-May-2006,  D.M. Zarro (L-3Com/GSFC) - Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-
;--- call back routine to notify when thread is complete

pro sock_download_callback, status, error, oBridge, userdata

   print,'SOCK_DOWNLOAD completed downloading file(s)'
   verbose=obridge->getvar('verbose')
   if verbose then begin
    copy_file=obridge->getvar('copy_file')
    if n_elements(copy_file) gt 1 then print,transpose(copy_file) else $
     if is_string(copy_file) then print,copy_file
   endif

   obj_destroy, oBridge

return & end

;---------------------------------------------------------------------------------

pro sock_download,files,out_dir=out_dir,_extra=extra,$
           verbose=verbose,clobber=clobber,err=err

err=''
if (1-since_version('6.3')) then begin
 err=' /NOWAIT option requires at least IDL version 6.3 '
 message,err,/cont
 return
endif

;-- check input

if is_blank(files) then begin
 err='missing input filenames'
 message,err,/cont
 return
endif

;-- check output directory

cur_dir=curdir()
case 1 of
 write_dir(out_dir): copy_dir=out_dir
 write_dir(cur_dir): copy_dir=cur_dir
 else: copy_dir=get_temp_dir()
endcase

;-- create IDL-IDL bridge object

oBridge = Obj_New('IDL_IDLBridge',callback='sock_download_callback')
if obridge->status() then begin
 message,'busy doing something else.',/cont
 return
endif

;-- make sure thread object has same IDL environment/path as parent

oBridge->execute, '@' + pref_get('IDL_STARTUP')

;-- pass input files and output directory name to thread object

obridge->setvar,"files",files
obridge->setvar,"copy_dir",copy_dir
obridge->setvar,"clobber",keyword_set(clobber)
obridge->setvar,"verbose",keyword_set(verbose)

;-- send copy command to thread

obridge->execute,"sock_copy,files,out_dir=copy_dir,head=0,copy_file=copy_file,clobber=clobber,verbose=verbose",/nowait

message,'started downloading to '+copy_dir,/cont

;-- check status

case obridge->status(err=err) of
 1: message,'busy executing...',/cont
 2: message,'completed',/cont
 3: message,'failed - '+err,/cont
 4: message,'aborted - '+err,/cont
 else: message,'idle',/cont
 endcase


return & end
