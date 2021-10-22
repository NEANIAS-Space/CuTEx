;+
; Project     : HESSI
;
; Name        : KANZ__DEFINE
;
; Purpose     : Define a KANZ data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('kanz')
;
; History     : Written 15 March 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function kanz::init,_ref_extra=extra

ret=self->site::init(_extra=extra)
                     
if not ret then return,ret
           
self->setprop,/fits,rhost='ftp.solobskh.ac.at',$
      user='download',password='9521treffen',org='year',rename=1,ftype='kanz',gzip=-1

return,1

end

;------------------------------------------------------------------------------
;-- SET method

pro kanz::setprop,fits=fits,jpeg=jpeg,err=err,_extra=extra


;-- set file type and location to download

if keyword_set(fits) then begin
 ext='.fts' & topdir='/halpha/FITS/high'
endif

if keyword_set(jpeg) then begin
 ext='.jpg' & topdir='/halpha/JPEG/high'
endif

self->site::setprop,ext=ext,topdir=topdir,_extra=extra,err=err

return & end

;---------------------------------------------------------------------------

;-- file renamer

function kanz::rename_files,files,count=count

count=0 & names=''
if datatype(files) ne 'STR' then return,''
                           
names=files                           
count=n_elements(names)

chk=where(strpos(names,'.hi') gt -1,hcount)
if hcount gt 0 then begin
 names=str_replace(names,'.hi','') & return,names
endif

chk=where(strpos(names,'.lo') gt -1,hcount)
if hcount gt 0 then begin
 names=str_replace(files,'.lo','') & return,names
endif
     
return,names

end


;-----------------------------------------------------------------------------
;-- get remote subdirectory id's based on file dates

function kanz::get_sdir

fids=self->site::get_sdir(/year,/full)

return,fids

end

;----------------------------------------------------------------------------
;-- driver to ftp KANZ files to $SYNOP_DATA

pro kanz::synop,_extra=extra

message,'copying KANZ synoptic data',/cont

;-- default settings

get_utc,utc
utc.time=0
self->setprop,tstart=utc,back=10,/subdir,err=err,_extra=extra
if err ne '' then return

;-- start with FITS

self->setprop,ldir='$SYNOP_DATA/images',/fits,err=err

if err eq '' then begin
 self->copy,count=count
 if count gt 0 then synop_link,'kanz',back=10,_extra=extra
endif

;-- then do JPEGS

;self->setprop,ldir='$SYNOP_DATA/www',/jpeg
;if err eq '' then self->copy

return & end
                                         
;------------------------------------------------------------------------------
;-- KANZ site structure

pro kanz__define                 

self={kanz,inherits site}

return & end

