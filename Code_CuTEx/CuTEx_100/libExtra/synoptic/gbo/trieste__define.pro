;+
; Project     : HESSI
;
; Name        : trieste__DEFINE
;
; Purpose     : Define a trieste object for copying catalog data
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('trieste')
;
; History     : Written 24 Apr 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function trieste::init,_ref_extra=extra

ret=self->site::init(_extra=extra)
                     
if not ret then return,ret
           
self->setprop,rhost='140.105.77.13',org='day',ext='.log',/ascii

return,1

end

;-----------------------------------------------------------------------------
;-- get remote subdirectory id's based on file dates

function trieste::get_sdir

return,self->site::get_sdir(delim='/',/full)      

end

;----------------------------------------------------------------------------
;-- driver to ftp trieste files to $SYNOP_DATA

pro trieste::synop,_extra=extra

message,'copying TRIESTE synoptic data',/cont

;-- default settings

get_utc,utc
utc.time=0
self->setprop,tstart=utc,back=10,ldir='$SYNOP_LOGS/trieste',err=err,_extra=extra

if err ne '' then return

if err eq '' then self->copy

return & end
                                         
;------------------------------------------------------------------------------
;-- trieste site structure

pro trieste__define                 

self={trieste,inherits site}

return & end

