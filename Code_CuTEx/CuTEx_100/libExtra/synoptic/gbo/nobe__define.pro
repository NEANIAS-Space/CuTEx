;+
; Project     : HESSI
;
; Name        : NOBE__DEFINE
;
; Purpose     : Define a NOBE data object for Nobeyama Radio Obs.
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('nobe')
;
; History     : Written 22 Aug 2000, D. Zarro (EIT/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function nobe::init,_ref_extra=extra

ret=self->site::init(_extra=extra)
                     
if not ret then return,ret

ret=self->fits::init(_extra=extra)

if not ret then return,ret
           
self->setprop,rhost='solar.nro.nao.ac.jp',dtype='daily',ext='',org='month',$
                /rename,/passive

return,1

end

;----------------------------------------------------------------------------

pro nobe::cleanup


self->site::cleanup
self->fits::cleanup

return & end

;------------------------------------------------------------------------------
;-- SET method

pro nobe::setprop,dtype=dtype,err=err,_extra=extra

;-- set file type and location to download

root='/pub/norh/images/'
valid_dtype=['daily','10min'] 
if is_string(dtype) then begin
 chk=where(strlowcase(trim(dtype)) eq valid_dtype,count)
 if count gt 0 then begin
  topdir=root+valid_dtype[chk[0]]
  self->site::setprop,topdir=topdir
 endif
endif

self->site::setprop,_extra=extra,err=err

return & end
                                      

;-----------------------------------------------------------------------------
;-- get remote subdirectory id's based on file dates

function nobe::get_sdir
      
fids=self->site::get_sdir(/full,/no_day,delim='/')

return,fids

end

;---------------------------------------------------------------------------

function nobe::parse_time,file,_ref_extra=extra

dd='([0-9]{2})'
regex='([a-z]{3})'+dd+dd+dd+'_'+dd+dd+dd

return,parse_time(file,_extra=extra,regex=regex)

end

;----------------------------------------------------------------------------
;-- rename Nobeyama files to Synoptic convention

function nobe::rename_files,files,count=count

count=0
if size(files,/tname) ne 'STRING' then return,''

base='nobe_17GHz_'

times=self->parse_time(files,names=type)

new=base+type+'_'+time2fid(times,/time,/full,/seconds)+'.fits'

new=reform(new)
count=n_elements(new)
if count eq 1 then new=new[0]

return,new
                       
end

;----------------------------------------------------------------------------
;-- driver to ftp NOBE files to $SYNOP_DATA

pro nobe::synop,_extra=extra

message,'copying NOBE synoptic data',/cont

;-- default settings

get_utc,utc
utc.time=0
self->setprop,tstart=utc,back=10,/subdir,err=err,_extra=extra,/gzip
if err ne '' then return

;-- start with daily files

self->setprop,ldir='$SYNOP_DATA/images',/daily,err=err

if err eq '' then begin
 self->copy,count=count
 if count gt 0 then  synop_link,'nobe',back=10,_extra=extra
endif

;-- then do 10 minute files

;self->setprop,/ten_min,err=err
;if err eq '' then self->copy

return & end

;----------------------------------------------------------------------------
;-- convert Nobeyama index to FITS standard

function nobe::index2fits,index,no_copy=no_copy,err=err

err=''
if size(index,/tname) ne 'STRUCT' then return,-1

dprint,'% NOBE::INDEX2FITS'
if keyword_set(no_copy) then nindex=temporary(index) else nindex=index
;nindex=rep_tag_name(nindex,'solp','crota')
nindex.origin=nindex.origin+' '+nindex.OBS_D$FREQ

return,nindex

end
                                         
;------------------------------------------------------------------------------
;-- Nobeyama site structure

; DTYPE = 'daily' or 'ten_minute'

pro nobe__define                 

self={nobe,dtype:'',inherits site, inherits fits}

return & end

