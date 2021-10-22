;+
; Project     : RHESSI
;
; Name        : UPDATE_MM
;
; Purpose     : update Max Millennium catalog with flare observations
;
; Category    : synoptic gbo 
;
; Syntax      : IDL> update_mm
;
; Inputs      : TSTART,TEND = time range to process
;     
; Keywords    : REPROCESS = reprocess existing entries
;               BACK = # of days back to process
;               VERBOSE = set verbose output
;             
; Restrictions: Unix only
;
; History     : Written 10 March 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro update_mm,tstart,tend,back=back,eit=eit,sxi=sxi,trace=trace,$
              _extra=extra

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return
endif

;-- locate MM database file

db_file=chklog('$GBO_DB')
if not file_test(db_file,/write) then begin
 message,'Cannot locate or write to Max Millennium catalog',/cont
 return
endif
resolve_routine,'db_gbo'

;-- restore MM catalog info

restore_gbo
common gbo_com, gbo_records

;-- establish processing times
;-- round to start and end of day

if not valid_time(tstart) then get_utc,dstart else $
 dstart=anytim2utc(tstart)
if not valid_time(tend) then dend=dstart else $
 dend=anytim2utc(tend)

dstart.time=0 & dend.time=0
;dend.mjd=dend.mjd+1
if dend.mjd le dstart.mjd then dend.mjd=dstart.mjd+1

if is_number(back) then tback=back > 0 else tback=0
if tback gt 0 then dstart.mjd=dstart.mjd-tback

dstart=anytim2utc(dstart,/vms)
dend=anytim2utc(dend,/vms)

;-- find overlapping GOES events

rd_gev,dstart,dend,gev,status=status,/nearest

;-- find GOES flare start and end times

if status ne 0 then begin
 message,'No GOES events during specified period',/cont
 return
endif

decode_gev,gev,class=class

flares=where(stregex(class,'(C|M|X)',/bool),nflares)
if nflares eq 0 then begin
 message,'No GOES events above B level',/cont
 return
endif
gev=gev[flares]

within=6.
if keyword_set(eit) then eit_gbo,gev,_extra=extra,within=within
if keyword_set(sxi) then sxi_gbo,gev,_extra=extra,within=within
if keyword_set(trace) then trace_gbo,gev,_extra=extra,within=within

return & end

