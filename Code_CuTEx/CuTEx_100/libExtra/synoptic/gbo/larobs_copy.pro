;+
; Project     : RHESSI
;
; Name        : LAROBS_COPY
;
; Purpose     : Copy LARISSA observatory log files for MM catalog
;
; Category    : Synoptic
;
; Syntax      : IDL> larobs_copy,tstart,tend [,back=back]
;
; Inputs      : TSTART, TEND = start/end times to copy
;
; Keywords    : BACK = # of days back to look
;
; History     : Writte, 28 April 2004, Zarro (L-3Com/GSFC)
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro larobs_copy,tstart,tend,back=back,err=err,_extra=extra

out_dir='$SYNOP_LOGS/larissa'
if not test_dir(out_dir,err=err) then return

dstart=get_def_times(tstart,tend,dend=dend,/utc,/round,/no_next)
if is_number(back) then begin
 dend=dstart
 dstart.mjd=dstart.mjd-abs(back)
endif

url='http://www.larissa-dimos.gr'
time=dstart
a=obj_new('http')
a->open,url,err=err,_extra=extra
if is_string(err) then begin
 obj_destroy,a
 return
endif

while (time.mjd le dend.mjd) do begin
 fid=time2fid(time,/full)
 fname='larissa_'+fid+'.log'
 year=time2fid(time,/full,/year)
 dname='/sites'+'2003'+'/ASTEROSKOPEIO/ZARRO/'+year
 furl=url+dname+'/'+fname
 a->copy,furl,_extra=extra,out_dir=out_dir,/head
 time.mjd=time.mjd+1
endwhile


return & end

