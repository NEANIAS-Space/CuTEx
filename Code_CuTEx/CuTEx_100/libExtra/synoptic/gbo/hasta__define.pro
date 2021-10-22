;+
; Project     : HESSI
;
; Name        : HASTA__DEFINE
;
; Purpose     : Define a HASTA data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('hasta')
;
; History     : Written 9 Aug 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function hasta::init,_ref_extra=extra

ret=self->site::init(_extra=extra)
                     
if not ret then return,ret
           
self->setprop,rhost='malena.iafe.uba.ar',topdir='/pub/hasta/zarro',$
              org='year',/ascii,ext='.log'

return,1

end

;-----------------------------------------------------------------------------
;-- cleanup

pro hasta::cleanup

dprint,'% hasta::CLEANUP'

self->site::cleanup

return
end

;-----------------------------------------------------------------------------
;-- get remote subdirectory id's based on file dates

function hasta::get_sdir

return,''

end

;----------------------------------------------------------------------------
;-- driver to mirror hasta files from NOAA to $SYNOP_LOGS

pro hasta::synop,_extra=extra

message,'copying HASTA log files',/cont

;-- default settings

get_utc,utc
utc.time=0
self->setprop,tstart=utc,back=30,err=err,_extra=extra
if err ne '' then return

self->setprop,ldir='$SYNOP_LOGS/hasta',err=err

if err eq '' then self->copy

return & end

;---------------------------------------------------------------------------

pro hasta__define                 

self={hasta,inherits site}

return & end


