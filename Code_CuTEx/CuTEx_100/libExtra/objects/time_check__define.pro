;+
; Project     : HESSI
;
; Name        : TIME_CHECK
;
; Purpose     : Define a TIME_CHECK object
;
; Category    : Objects
;;
; Syntax      : IDL> o=obj_new('time_check',tstart,tend,back=back)
;              
; Inputs      : TSTART, TEND = input times to check
;
; Keywords    : BACK = # of days to check back
;
; Methods     : o->get,dstart,dend
;
; History     : Written, 10 March 2001, Zarro (EITI/GSFC)
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

;---------------------------------------------------------------------------
;-- constructor

function time_check::init,tstart,tend,_extra=extra

self->set,tstart,tend,_extra=extra
                            
return,1
end

;---------------------------------------------------------------------------
;-- destructor

pro time_check::cleanup

return & end

;----------------------------------------------------------------------------
;-- set method

pro time_check::set,tstart,tend,back=back,date_only=date_only

if valid_time(tstart) then dstart=anytim2utc(tstart) else $
 dstart=anytim2utc(!stime)

if valid_time(tend) then dend=anytim2utc(tend) else begin
 dend=dstart & dend.mjd=dend.mjd+1
endelse

if keyword_set(date_only) then begin
 dstart.time=0
 dend.time=0
endif

if exist(back) then begin
 if back gt 0 then begin
  dend=dstart
  dstart.mjd=dstart.mjd-back
 endif
endif
if dend.mjd le dstart.mjd then dend.mjd=dstart.mjd+1

self.dstart=dstart
self.dend=dend

return & end

;-------------------------------------------------------------------------
;-- get method

pro time_check::get,dstart,dend,vms=vms

dstart=self.dstart
dend=self.dend
if keyword_set(vms) then begin
 dstart=anytim2utc(dstart,/vms)
 dend=anytim2utc(dend,/vms)
endif

return & end

;-------------------------------------------------------------------------
;-- show method

pro time_check::show

self->get,dstart,dend

message,anytim2utc(dstart,/vms)+' to '+anytim2utc(dend,/vms),/cont

return & end
                                                                    

;---------------------------------------------------------------------------
;-- define class structure

pro time_check__define

time=anytim2utc(!stime)
struct={time_check,dstart:time,dend:time}

return & end
