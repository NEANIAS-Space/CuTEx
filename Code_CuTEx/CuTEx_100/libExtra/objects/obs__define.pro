;+
; Project     : HESSI
;
; Name        : OBS__DEFINE
;
; Purpose     : Define a general site observatory object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('obs')
;
; History     : Written 25 June 2002, D. Zarro, LAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function obs::init,_ref_extra=extra

ret=self->site::init(_extra=extra)
                     
return,ret

end

;---------------------------------------------------------------------------

pro obs::cleanup

self->site::cleanup

return & end

;---------------------------------------------------------------------------
;-- read control parameter file

pro obs::get_control,file,err=err

err=''
chk=loc_file(file,err=err)
if err ne '' then return
control=rd_ascii(chk[0])
props=str_key(control)
if not is_struct(props) then begin
 err='no control properties in file'
 message,err,/cont
 return
endif

self->setprop,_extra=props

return & end

;----------------------------------------------------------------------------
;-- driver to ftp files to $SYNOP_DATA

pro obs::synop,_ref_extra=extra,err=err

err=''
rhost=self->getprop(/rhost)
if is_blank(rhost) then begin
 err='no control properties loaded'
 message,err,/cont
 return
endif

message,'copying synoptic data from '+rhost,/cont

;-- default settings

get_utc,utc
utc.time=0
self->setprop,tstart=utc,/verbose,back=10,/subdir,err=err,_extra=extra
if err eq '' then self->copy

return & end
                                         
;------------------------------------------------------------------------------
;-- OBS site structure

pro obs__define                 

self={obs,inherits site}

return & end

