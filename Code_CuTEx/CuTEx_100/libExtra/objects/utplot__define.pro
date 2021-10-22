;+ Project     : HESSI
;
; Name        : UTPLOT__DEFINE
;
; Purpose     : Define a UTPLOT plot class
;
; Category    : objects
;
; Syntax      : IDL> new=obj_new('utplot')
;
; History     : Written 3 April 2001, D. Zarro (EITI/GSFC)
;               Modified 6 Sept 2001, Zarro (EITI/GSFC) - added ->SET_TRANGE
;               Modified 6 Jan  2002, Zarro (EITI/GSFC) - added TIMES,DATA keywords
;               Modified 16 Sep 2002, Zarro (LAC/GSFC) - added GETDATA
;               Modified 24 Nov 2002, Zarro (EER/GSFC) - added checks for different
;                                                        input time formats
;               Modified 5 Feb 2003, Zarro (EER/GSFC) - added ability to
;                                                       override UTBASE
;               Modified 7 May 2003, Zarro (EER/GSFC) - added check for
;               simultaneous XRANGE and TIMERANGE keywords in PLOT method.
;               Previously XRANGE could override TIMERANGE. Now TIMERANGE takes
;               precedence.
;               Modified 24 Jan 2004, Zarro (L-3Com/GSFC) - allowed UTBASE
;                to be entered independently of XDATA
;               Modified 17-Apr-2004, Zarro (L-3Com/GSFC) - replaced WHERE by WHERE2
;               Modified 8-Jul-2004, Zarro (L-3Com/GSFC) - fixed UTBASE problem
;               when seconds array is entered
;               Modified 1-August-2004, Zarro (L-3Com/GSFC) - made UTBASE units
;               self-consistent with input times.
;               Modified 1-April-2005, Zarro (L-3Com/GSFC) - use _REF_EXTRA
;                to pass keyword values back to caller.
;               Modified 16-Jun-2005, Kim - allow utbase to be 0, and in set_trange,
;                if timerange is integer, float it so won't be misinterpreted by anytim
;               Modified 17-Jan-2006, Zarro. Made err and err_msg self consistent.
;               Modified 14-Sep-2006, Zarro. Added e (errorbar) argument to init
;
;
; Contact     : dzarro@solar.stanford.edu
;-

function utplot::init,t,y,e,_ref_extra=extra

return,self->xyplot::init(t,y,e,plot_type='utplot',_extra=extra)

dprint,'% UTPLOT::INIT'

end

;-----------------------------------------------------------------------
;--destroy object

pro utplot::cleanup

dprint,'% UTPLOT::CLEANUP'

self->xyplot::cleanup

return

end

;--------------------------------------------------------------------------
;-- get at underlying data arrays

function utplot::getdata,times=times,_ref_extra=extra,utbase=utbase

if arg_present(utbase) then utbase=self->get(/utbase)
if arg_present(times) then times=self->get(/xdata)
return,self->xyplot::getdata(_extra=extra)

end

;---------------------------------------------------------------------------
;-- get method

function utplot::get,_ref_extra=extra,timerange=timerange

if keyword_set(timerange) then begin
 xrange=self->xyplot::get(/xrange)
 if not valid_range(xrange) then xrange=self->get_def_xrange()
 if valid_range(xrange) then begin
  utbase=anytim2tai(self.utbase)
  return,anytim2utc(utbase+xrange,/vms)
 endif
endif

if keyword_set(utbase) then return,self.utbase

if is_string(extra) then return,self->xyplot::get(_extra=extra)
return,''

end

;----------------------------------------------------------------------------
;--set data and plot properties

pro utplot::set,times=times,data=data,_ref_extra=extra,$
                 xdata=xdata,ydata=ydata,utbase=utbase,timerange=timerange

;-- remove conflicting XRANGE keyword if TIMERANGE is set

if is_string(extra) then begin
 if valid_range(timerange,/allow,/time) then begin
  temp=strpos(strup(extra),'XRA') eq 0
  check=where2(temp,complement=complement,ncomplement=ncomplement)
  if ncomplement gt 0 then extra=extra[complement] else delvarx,extra
 endif
endif

if exist(data) then self->xyplot::set,ydata=data,_extra=extra

;-- times in XYPLOT object are stored in double precision secs since UTBASE

if exist(times) then $
 self->xyplot::set,xdata=self->tim2secs(times,_extra=extra,utbase=utbase),_extra=extra

if exist(ydata) then self->xyplot::set,ydata=ydata,_extra=extra

if exist(xdata) then $
 self->xyplot::set,xdata=self->tim2secs(xdata,_extra=extra,utbase=utbase),_extra=extra

if is_string(extra) then self->xyplot::set,_extra=extra
		; /zero added 10-jun-05 kim !!!!
if not exist(times) and not exist(xdata) and valid_time(utbase,/zero) then self->set_utbase,utbase

if exist(timerange) then self->set_trange,timerange

return & end

;-------------------------------------------------------------------------
;-- convert times to secs relative to UTBASE

function utplot::tim2secs,times,tai=tai,secs=secs,utbase=utbase,err_msg=err_msg,no_copy=no_copy

;--- if structure or string input, then UTBASE is computed
;--- if /TAI then input time is TAI else assumed to be secs since 79
;--- if /SECS then seconds since UTBASE (which must be given)

err_msg=''
type=size(times,/type)

ok=where(type eq [2,3,4,5,7,8],count)
if count eq 0 then begin
 err_msg='Invalid input TIMES'
 message,err_msg,/cont
 return,0
endif

;-- if string or structure, convert to TAI

if (type eq 7) or (type eq 8) then begin
 time=anytim2tai(times)
 tmin=min(time,/nan)
 if keyword_set(no_copy) then time=temporary(times)-tmin else time=times-tmin
 self->set_utbase,tmin,/tai
 return,time
endif

;-- if seconds since UTBASE, check UTBASE was passed

if keyword_set(secs) then begin
 if not valid_time(utbase,/zero) then begin		; /zero added 10-jun-05 kim !!!!
  utbase=self->get(/utbase)
  if not valid_time(utbase,/zero) then begin		; /zero added 10-jun-05 kim !!!!
   err_msg='Missing UTBASE'
   message,err_msg,/cont
   return,0
  endif
 endif
 self->set_utbase,utbase
 return,double(times)
endif

;-- if TAI, compute UTBASE

if keyword_set(tai) then begin
 tmin=min(times,/nan)
 if keyword_set(no_copy) then time=temporary(times)-tmin else time=times-tmin
 self->set_utbase,tmin,/tai
 return,double(time)
endif

;-- if here, must be seconds since 79 or seconds since UTBASE (if entered)

if valid_time(utbase,/zero) then tbase=utbase else begin		; /zero added 10-jun-05 kim !!!!
 tmin=min(times,/nan)
 cur_tbase=self->get(/utbase)
 if is_blank(cur_tbase) then tbase=anytim(tmin,/vms) else tbase=cur_tbase
endelse

if keyword_set(no_copy) then time=temporary(times) else time=times
self->set_utbase,tbase

return,double(time)

end

;---------------------------------------------------------------------------

function utplot::where_zero,count=count

count=0
zeros=where( (self->get(/times) eq 0.),count)

return,zeros
end


;---------------------------------------------------------------------------
pro utplot::set_utbase,utbase,tai=tai

if valid_time(utbase, /zero) then begin		; /zero added 10-jun-05 kim !!!!
 if (not is_number(utbase)) or keyword_set(tai) then $
  self.utbase=anytim2utc(utbase,/vms) else $
   self.utbase=anytim(utbase,/vms)
 return
endif

return & end

;---------------------------------------------------------------------------
;-- convert external TIMERANGE  into internal XRANGE

pro utplot::set_trange,timerange

if valid_range(timerange,/allow,/time,zeros=zeros) then begin
 if zeros then self.xrange=timerange else begin
  utbase=self->get(/utbase)
  if size(timerange,/tname) eq 'INT' then timerange=float(timerange)
  self.xrange=anytim(timerange,/tai)-anytim(utbase,/tai)
 endelse
endif

return & end

;----------------------------------------------------------------------------
;-- UTPLOT method

pro utplot::plot_cmd,x,y,overlay=overlay,_extra=extra

utbase=self->get(/utbase)

dprint,'% UTPLOT::PLOT_CMD - utbase = ',utbase

if keyword_set(overlay) then outplot,x,y,utbase,_extra=extra else $
                          utplot,x,y,utbase,_extra=extra

return
end

;------------------------------------------------------------------------------

pro utplot::show

self->xyplot::show
print,'% UTBASE: ',self->get(/utbase)

return & end

;------------------------------------------------------------------------------
;-- UTPLOT properties definition

pro utplot__define

temp={utplot,               $
      utbase:'',            $
      inherits xyplot}

return
end

