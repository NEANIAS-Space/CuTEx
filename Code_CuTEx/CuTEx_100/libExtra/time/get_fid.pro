;+
; Project     : HESSI
;
; Name        : GET_FID
;
; Purpose     : determine YYMMDD names based on date/time 
;
; Category    : utility io 
;
; Syntax      : rdir=get_fid(tstart,tend)
;
; Inputs      : TSTART/TEND = start/end times to base search
;               e.g. TSTART = 10-may-99 -> 990510
;                    TEND   = 20-dec-99 -> 991220
;
; Outputs     : Array directory names 
;
; Keywords    : NO_DAY = exclude day from output
;               FULL= include full year in output
;               YEAR_ONLY = include year only
;               DELIM = delimiter between year, month, & day
;
; History     : Written 6 Jan 1999, D. Zarro (SM&A/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_fid,tstart,tend,_extra=extra,no_day=no_day,$
                 year_only=year_only,no_next=no_next,dstart=dstart,dend=dend

if valid_time(tstart) then dstart=anytim2utc(tstart) else get_utc,dstart
if valid_time(tend) then dend=anytim2utc(tend) else dend=dstart


;dstart=get_def_times(tstart,tend,dend=dend,no_next=no_next)

dstart.time=0 & dend.time=0
if keyword_set(no_next) then dend.mjd=dend.mjd-1

if (anytim2tai(dend) lt anytim2tai(dstart)) then begin
 temp=dend & dend=dstart & dstart=dend
endif

sdir=time2fid(dstart,no_day=no_day,year_only=year_only,_extra=extra)
edir=time2fid(dend,no_day=no_day,year_only=year_only,_extra=extra)

jdate=dstart
mstart=anytim2utc(dstart)
i=0
while ((where(edir eq sdir))(0) eq -1) do begin
 mdate=anytim2utc(jdate)
 i=i+1
 mdate.mjd=mstart.mjd+i
 jdate=anytim2utc(mdate,/ext)
 dir=time2fid(jdate,no_day=no_day,year_only=year_only,_extra=extra)
 skip=0
 if keyword_set(no_day) or keyword_set(year_only) then begin
  np=n_elements(sdir)
  skip=dir eq sdir(np-1) 
 endif
 if not skip then sdir=append_arr(sdir,dir,/no_copy)
endwhile

return,sdir & end

