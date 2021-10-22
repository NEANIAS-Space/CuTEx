;+
; Project     : HESSI
;                  
; Name        : WSHOW2
;               
; Purpose     : windows friendly version of WSHOW
;                             
; Category    : display
;               
; Syntax      : IDL> wshow2
;
; Inputs      : INDEX = windows index
;                                   
; History     : Written, 4-Oct-2002, Zarro (LAC/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-    

pro wshow2,index,_extra=extra

;-- catch unplanned errors

error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 return
endif


if os_family(/lower) ne 'windows' then begin
 if is_number(index) then wshow,index,icon=0,_extra=extra else $
  wshow,icon=0,_extra=extra
endif else begin
 if is_number(index) then wshow,index,_extra=extra else $
  wshow,_extra=extra
endelse

return & end
