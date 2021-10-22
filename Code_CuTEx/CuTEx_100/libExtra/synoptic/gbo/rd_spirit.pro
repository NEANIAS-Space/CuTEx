;+
; Project     : HESSI
;
; Name        : RD_SPIRIT
;
; Purpose     : read SPIRIT data
;
; Category    : Ancillary GBO Synoptic 
;
; Syntax      : IDL> rd_spirit,file,data,header=header
;
; History     : Written 11 Nov 2005, D. Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_spirit,file,data,header=header,map=map,err=err,_ref_extra=extra

common rd_spirit,sp

if n_params() eq 0 then begin
 pr_syntax,'rd_spirit,file,data,header=header,map=map'
 return
endif

if not obj_valid(sp) then sp=obj_new('spirit')
sp->read,file,err=err,_extra=extra
if is_string(err) then return
data=sp->get(/data)
if arg_present(header) then header=sp->get(/header)
if arg_present(map) then map=sp->get(/map)

return & end
