;+
; Project     : SOHO - CDS
;
; Name        : OPLOT_NAR
;
; Purpose     : Oplot NOAA AR pointing structures from GET_NAR
;
; Category    : planning
;
; Explanation : 
;
; Syntax      : IDL> oplot_nar,nar or oplot_nar,time,nar
;
; Inputs      : NAR = NOAA AR pointing structures from GET_NAR
;               (if TIME is input, GET_NAR is called)
;
; Opt. Inputs : None
;
; Outputs     : None
;
; Opt. Outputs: None
;
; Keywords    : EXTRA = plot keywords passed to XYOUTS
;               OFFSET = offset coordinates to shift labelling 
;               [data units, e.g., off=[100,100])
;
; Common      : None
;
; Restrictions: A base plot should exist
;
; Side effects: None
;
; History     : Version 1,  20-June-1998,  D.M. Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro oplot_nar,time,nar,quiet=quiet,offset=offset,_extra=extra

on_error,1
count=0
nar_entered=datatype(time) eq 'STC'
if nar_entered then nar_entered=tag_exist(time,'NOAA')
if nar_entered then begin
 nar=time 
 count=n_elements(nar)
endif else begin
 if not exist(time) then begin
  pr_syntax,'oplot_nar,time,nar OR oplot_nar,nar'
  return
 endif else nar=get_nar(time,count=count,quiet=quiet,/unique)
endelse

;-- any offsets

xs=0. & ys=0.
if exist(offset) then begin
 xs=offset(0)
 ys=xs
 if n_elements(offset) eq 2 then ys=offset(1)
endif

if count gt 0 then begin
 for i=0,count-1 do begin
  x=xs+nar(i).x & y=ys+nar(i).y
  xyouts,x,y,trim(string(nar(i).noaa)),/data,_extra=extra
 endfor
endif

return & end
