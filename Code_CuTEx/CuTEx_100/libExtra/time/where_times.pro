;+
; Project     : HESSI
;
; Name        : WHERE_TIMES
;
; Purpose     : check where times fall within input time limits
;
; Category    : HESSI, GBO, utility, time
;
; Syntax      : IDL> check=where_times(times,tstart=tstart,tend=tend,count)
;
; Inputs      : TIMES = time array to check (TAI format)
;
; Outputs     : CHECK = index of matching times
;
; Keywords    : TSTART = lower time limit (TIMES >= TSTART)
;               TEND   = upper time limit (TIMES <= TEND)
;               COUNT  = number of matches
;
; History     : 9-Apr-1999,  D.M. Zarro (SM&A/GSFC),  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function where_times,times,tstart=tstart,tend=tend,count=count

count=0
if datatype(times) ne 'DOU' then begin
 pr_syntax,'check=where_times(times,tstart=tstart,tend=tend,count=count)'
 return,-1
endif

vstart=valid_time(tstart)
vend=valid_time(tend) 
if (not vstart) and (not vend) then begin
 count=n_elements(times)
 return,lonarr(count)
endif

t1=anytim2tai(tstart) & t2=anytim2tai(tend)
if (vstart and vend) then return,where( (times ge t1) and (times le t2),count)
if vstart then return,where(times ge t1,count)
if vend then return,where(times le t2,count)

return,-1 & end
