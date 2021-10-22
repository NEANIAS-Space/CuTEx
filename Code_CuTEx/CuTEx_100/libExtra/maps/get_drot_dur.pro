;+
; Project     : SOHO-CDS
;
; Name        : GET_DROT_DUR
;
; Purpose     : compute durations (in seconds) to solar rotate a map
;
; Category    : imaging
;
; Explanation : cater for different types of inputs 
;
; Syntax      : dur=get_drot_dur(map,duration,time=time)
;
; Inputs      : MAP = time or map structure
;               DURATION = amount to rotate by [def=hours units]
;
; Outputs     : DUR = duration in seconds
;
; Keywords    : TIME = time to rotate to (ignored if duration is input)
;               DAYS = input duration units in days
;             : SECONDS = input duration units in seconds
;  
; History     : Written 5 June 1998, D. Zarro, SAC/GSFC
;               10 Jan 2005, Zarro (L-3Com/GSFC) - permitted zero duration
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_drot_dur,map,duration,time=time,days=days,seconds=seconds

err=''

;--check inputs

if not exist(map) then begin
 pr_syntax,'dur=get_drot_dur(map,duration,[time=time])'
 return,0
endif

units='(hours)'
days=keyword_set(days)
seconds=keyword_set(seconds)
if seconds then units='(secs)'
if days then units='(days)'
cur_time=get_map_time(map,/tai)

if not exist(duration) then begin
 if valid_map(time) then tend=get_map_time(time,/tai) else begin
  if exist(time) then begin
   if is_struct(time) then tend=anytim2tai(anytim(time,/mjd)) else $
    tend=anytim2tai(time)
  endif
 endelse
 if exist(tend) then dtime=(tend-cur_time)/3600.
 if not exist(dtime) then dtime=0.
endif else dtime=duration

case units of
 '(days)': dtime=dtime*3600.*24.
 '(hours)': dtime=dtime*3600.
 else:do_nothing=1
endcase

return,dtime & end
