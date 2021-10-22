;+
; Project     : HESSI
;
; Name        : NANCAY__DEFINE
;
; Purpose     : Define a Nancay data site object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('nancay')
;
; History     : Written 3 March 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function nancay::init,_ref_extra=extra

ret=self->mesola::init(_extra=extra) 

if not ret then return,ret

self->setprop,mhz='327',ftype='n'

return,1
end

;---------------------------------------------------------------------------

pro nancay::cleanup

self->mesola::cleanup

return & end

;---------------------------------------------------------------------------
;-- set method

pro nancay::setprop,mhz=mhz,_extra=extra,err=err
          
def_dir='/pub/nancay/164MHz'
if exist(mhz) then begin 
 if fix(mhz) eq 164 then topdir=def_dir
 if fix(mhz) eq 327 then topdir='/pub/nancay/327MHz'
endif

self->mesola::setprop,topdir=topdir,_extra=extra,err=err

return & end

;---------------------------------------------------------------------------

;-- driver to ftp files to $SYNOP_DATA

pro nancay::synop,_extra=extra

message,'copying Nancay synoptic data',/cont

;-- set defaults

get_utc,utc
utc.time=0

self->setprop,ldir='$SYNOP_DATA/images',$
      /subdir,tstart=utc,back=10,err=err,_extra=extra

if err ne '' then return

self->setprop,mhz=327,ext='.fits'
self->copy,count=count1

self->setprop,mhz=164,ext='.fits'
self->copy,count=count2

if (count1 gt 0) or (count2 gt 0) then synop_link,'nanc',back=10,_extra=extra

return & end

;----------------------------------------------------------------------------
;-- convert Nancay index to FITS standard

function nancay::index2fits,index,no_copy=no_copy,err=err

err=''
if datatype(index) ne 'STC' then return,-1

if keyword_set(no_copy) then nindex=temporary(index) else nindex=index

p=pb0r(nindex[0].date_obs,/arc)
rad=p[2]
nindex.cdelt1=nindex.cdelt1*rad
nindex.cdelt2=nindex.cdelt2*rad
nindex=add_tag(nindex,trim(fix(nindex.freq))+' MHz',' TYPE')
nindex=rem_tag(nindex,['ORIGIN','WAVELNTH'])

return,nindex

end

;------------------------------------------------------------------------------
;-- Nancay site structure

pro nancay__define                 

temp={nancay,inherits mesola}

return & end


