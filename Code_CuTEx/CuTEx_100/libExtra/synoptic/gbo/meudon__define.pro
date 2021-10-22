;+
; Project     : HESSI
;
; Name        : MEUDON__DEFINE
;
; Purpose     : Define a Meudon data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('meudon')
;
; History     : Written 20 Dec 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function meudon::init,_ref_extra=extra

ret=self->mesola::init(_extra=extra) 
if not ret then return,ret

self->setprop,/halpha,ftype='m'
return,1

end

;---------------------------------------------------------------------------

pro meudon::cleanup

self->mesola::cleanup

return & end


;---------------------------------------------------------------------------
;-- set method

pro meudon::setprop,halpha=halpha,kline=kline,_extra=extra,err=err
          
def_dir='/pub/meudon/Halpha'
if keyword_set(halpha) then topdir=def_dir
if keyword_set(kline) then topdir='/pub/meudon/K1v'

self->mesola::setprop,topdir=topdir,_extra=extra,err=err

return & end

;----------------------------------------------------------------------------
;-- driver to ftp files to $SYNOP_DATA

pro meudon::synop,_extra=extra

message,'copying MEUDON synoptic data',/cont

;-- set defaults

get_utc,utc
utc.time=0

self->setprop,ldir='$SYNOP_DATA/images',gzip=-1,$
      /subdir,tstart=utc,back=10,err=err,_extra=extra

if err ne '' then return

;-- H-alpha FITS files

self->setprop,/halpha,ext='.fits'

self->copy,count=count2

;-- K-line FITS files

self->setprop,/kline
self->copy,count=count1

if (count1 gt 0) or (count2 gt 0) then synop_link,'meud',back=10,_extra=extra

;-- H-alpha GIF files

;self->setprop,ldir='$SYNOP_DATA/www',ext='.gif',/halpha
;self->copy

;-- K-line GIF files

;self->setprop,/kline
;self->copy

return & end


;------------------------------------------------------------------------------
;-- meudon site structure

pro meudon__define                 

temp={meudon,inherits mesola}

return & end


