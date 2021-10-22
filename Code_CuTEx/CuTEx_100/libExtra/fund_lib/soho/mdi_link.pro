;+
; Project     : HESSI
;
; Name        : mdi_link
;
; Purpose     : make a link between MDI file in $MDI_MAGS and 
;               $SYNOP_DATA/fits/yymmdd
;
; Category    : synoptic gbo
;
; Syntax      : IDL> mk_mdi_link,tstart,tend,back=back
;
; Inputs      : TSTART, TEND = start and end times to consider
;               [def = current day]
;
; Outputs     : files named: mdi_mag_fd_yyyymmdd_hhmm.fits linked
;               to source files in $MDI_MAGS.
;
; Keywords    : BACK = days to look back
;               TARGET_DIR = target directory for links [def= $SYNOP_DATA/images]
;
; Restrictions: Unix only
;
; History     : Written 8 March 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro mdi_link,tstart,tend,back=back,err=err,verbose=verbose,target_dir=target_dir

verbose=keyword_set(verbose)
err=''

if not is_dir('$MDI_MAGS') then begin
 err='environment variable $MDI_MAGS is undefined'
 message,err,/cont
 return
endif

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return
endif

if datatype(target_dir) ne 'STR' then target_dir='$SYNOP_DATA/images'
if not test_dir(target_dir,err=err) then return

;-- check dates

o=obj_new('time_check',tstart,tend,back=back,/date)
o->get,dstart,dend,/vms
if verbose then o->show
obj_destroy,o

;-- find files

mfiles=mdi_files(dstart,dend,ftimes=ftimes)
if n_elements(mfiles) ne n_elements(ftimes) then return

;-- create subdir names in form yymmdd

fid=time2fid(ftimes.date_obs,/full,/time,err=err)
if err ne '' then begin
 message,err,/cont
 return
endif

yymmdd=strmid(fid,2,6)
mk_sub_dir,target_dir,yymmdd

;-- create link file names

tdir=concat_dir(target_dir,yymmdd)
lnames=tdir+'/mdi_maglc_fd_'+fid+'.fts'

;-- create link commands to spawn

if verbose then message,'constructing links...',/cont

snames=ftimes.filename
expr='ln -sf '+snames+' '+lnames

espawn,expr,out,count=count,/background

if count gt 0 then message,out,/cont

return & end


