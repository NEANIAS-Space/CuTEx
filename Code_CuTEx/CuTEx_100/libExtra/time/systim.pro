;+
; Project     : RHESSI
;
; Name        : SYSTIM
;
; Purpose     : Wrapper around IDL systime that returns more sensible
;               string times
;
; Category    : utility time
;
; Syntax      : IDL> time=systim()
;
; Inputs      : Same as SYSTIME
;
; Outputs     : Same as SYSTIME, except for the following difference:
;                IDL> print,systime()
;                  Sun Jan  1 12:42:03 2006
;                IDL> print,systim()
;                  1-Jan-2006 17:41:42.254
;
; Keywords    : Same as SYSTIME
;
; History     : 1-Jan-2006, Zarro (L-3Com/GSFC) - first IDL program of 2006
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function systim,arg1,arg2,_ref_extra=extra,utc=utc

common systim,tbase,ut_offset  ;-- save fiducial and UT offset for speed

;-- need the following case statement for backwards compatability

case n_params() of
1: s=systime(arg1,_extra=extra)
2: s=systime(arg1,arg2,_extra=extra)
else: s=systime(_extra=extra)
endcase

;-- if output time is a string, convert it to a more sensible format by
;   converting to TAI first and then to ECS

if is_string(s) then begin
 if not exist(ut_offset) then ut_offset=ut_diff()*3600.d
 if keyword_set(utc) then tdiff=0.d else tdiff=ut_offset
 if not exist(tbase) then tbase=anytim2tai('1-jan-1970',/nocorrect)
 return,tai2utc(systime(/sec)+tbase+tdiff,/vms,/trunc,/nocorrect)

endif else return,s

end

 







