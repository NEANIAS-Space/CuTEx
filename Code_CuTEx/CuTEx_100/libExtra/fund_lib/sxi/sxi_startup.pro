;+
;Project      : SXI
;
; Name        : SXI_STARTUP
;
; Purpose     : Load SXI software and databases
;
; Category    : system
;                   
; Inputs      : None
;
; Outputs     : None
;
; Keywords    : QUIET = set for no output
;
; History     : 31-Jan-2003,  D.M. Zarro (EER/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro sxi_startup, err=err, _extra=extra

load_path,'$SSW/goes/sxig12/idl','sxig12_read.pro',err=err,_extra=extra,/append
           
if err ne '' then return

return & end
