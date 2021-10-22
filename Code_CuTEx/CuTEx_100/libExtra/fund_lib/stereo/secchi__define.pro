;+
; Project     : STEREO
;
; Name        : SECCHI__DEFINE
;
; Purpose     : Define a SECCHI data/map object
;
; Category    : Objects
;
; Syntax      : IDL> a=obj_new('secchi')
;
; Examples    : IDL> a->read,'20070501_000400_n4euA.fts' ;-- read FITS file
;               IDL> a->plot                             ;-- plot image
;               IDL> map=a->getmap()                     ;-- access map
;               IDL> data=a->getdata()                   ;-- access data
;                       
;               Searching via VSO:                                     
;               IDL> files=a->search('1-may-07','02:00 1-may-07')
;               IDL> print,files[0]
;               http://stereo-ssc.nascom.nasa.gov/data/ins_data/secchi/L0/a/img/euvi/20070501/20070501_000400_n4euA.fts
;               
;               Copying from VSO:
;               IDL> a->get,files[0],/verbose
;
; History     : Written 13 May 2007, D. Zarro (ADNET)
;               Version 2, 31-Oct-2007, William Thompson, modified for COR1/COR2
;
; Contact     : dzarro@solar.stanford.edu
;-
;------------------------------------------------------------------------
;-- init 

function secchi::init,_ref_extra=extra

if ~self->have_secchi_path(_extra=extra) then return,0

return,self->map::init(_extra=extra)
    
end
;-----------------------------------------------------------------------------
;-- check for SECCHI branch in !path

function secchi::have_secchi_path,err=err

err=''
if ~have_proc('sccreadfits') then begin
 epath=local_name('$SSW/stereo/secchi/idl')
 if is_dir(epath) then ssw_path,/secchi,/quiet
 if ~have_proc('sccreadfits') then begin
  err='STEREO/SECCHI branch of SSW not installed'
  message,err,/cont
  return,0b
 endif
endif

return,1b

end

;--------------------------------------------------------------------------
;-- FITS reader

pro secchi::read,file,index,data,_ref_extra=extra,err=err,$
            no_prep=no_prep,nodata=nodata

err=''
if is_blank(file) then begin
 err = 'Invalid filename entered'
 message,err,/cont
 return
endif

if is_url(file[0]) then begin
 self->get,file[0],copy_file=cfile,err=err,_extra=extra
endif else begin
 if file_test(file[0],/regular,/read) then cfile=file else err = 'File not found' 
endelse

if is_string(err) then begin
 message,err,/cont
 return
endif

;-- ensure that calibration directories are properly defined

SSW_SECCHI=local_name('$SSW/stereo/secchi')
mklog,'SSW_SECCHI',SSW_SECCHI
SECCHI_CAL=local_name('$SSW_SECCHI/calibration')
mklog,'SECCHI_CAL',SECCHI_CAL

no_prep=keyword_set(no_prep)
if ~no_prep then begin
 if ~is_dir('$SSW_SECCHI/calibration') then begin
  message,'Warning - $SSW_SECCHI/calibration directory not found',/cont
  no_prep=1b
 endif
endif

if no_prep then begin
 data=call_function('sccreadfits',cfile,index,nodata=nodata,_extra=extra)
 if keyword_set(nodata) then return
endif else begin
    if self->is_cor12(cfile) then $
      secchi_prep,cfile[0:2],index,data,/polar,_extra=extra else $
      secchi_prep,cfile,index,data,_extra=extra
endelse

;-- insert data into maps

self->data2map,index,data,err=err
if is_string(err) then message,err,/cont

return & end

;--------------------------------------------------------------------
;-- check if triplet of COR1 or 2 images 

function secchi::is_cor12,file

if n_elements(file) ne 3 then return,0b

mrd_head,file[0],head
chk=where(stregex(strcompress(head,/rem),"DETECTOR='HI1",/bool),count)
return,count eq 1

end

;---------------------------------------------------------------------
;-- store DATA in MAP objects

pro secchi::data2map,index,data,err=err

;-- check inputs

ndim=size(data,/n_dim)
if (ndim lt 2) or (ndim gt 3) then begin
 err='Input image is not a valid data array. Check source FITS file'
 return
endif

if ~is_struct(index) then begin
 err='Input index is not a valid structure'
 return
endif

nindex=n_elements(index)

if (ndim eq 2) and (nindex ne 1) then begin
 err='Inconsistent index and image data size'
 return
endif

if (ndim eq 3) then begin
 sz=size(data)
 ndata=sz[n_elements(sz)-3]
 if ndata ne nindex then begin
  err='Inconsistent index and image data size'
  return
 endif
endif

;-- add STEREO-specific properties

for i=0,nindex-1 do begin
 id=index[i].OBSRVTRY+' '+index[i].INSTRUME+' '+index[i].DETECTOR+' '+trim(index[i].WAVELNTH)
 self->mk_map,index[i],data[*,*,i],i,_extra=extra,id=id,$
       roll_angle=index[i].crota,roll_center=[index[i].xcen,index[i].ycen],$
       b0=index[i].hglt_obs,l0=index[i].hgln_obs,rsun=index[i].rsun
 if index[i].detector eq 'EUVI' then self->set,i,/log_scale,grid=30,/limb
 self->colors,i
endfor

return & end

;-----------------------------------------------------------------------
;-- VSO search function

function secchi::search,tstart,tend,times=times,sizes=sizes,count=count,_ref_extra=extra

files='' & times=-1d & count=0 & sizes=''
dstart=get_def_times(tstart,tend,dend=dend,_extra=extra,/vms)
records=vso_search(dstart,dend,_extra=extra,/url,inst='secchi')
if have_tag(records,'url') then begin
 files=records.url
 count=n_elements(files)
 sizes=strtrim(records.size,2)
 chk=where(long(sizes) eq 0l,dcount)
 if dcount gt 0 then sizes[chk]=''
 times=anytim2tai(records.time.start)
endif

return,files

end

;------------------------------------------------------------------------------
;-- save SECCHI color table

pro secchi::colors,k

index=self->get(k,/index)
secchi_colors,index.detector,index.wavelnth,red,green,blue
self->set,k,red=red,green=green,blue=blue,/load_colors,/has_colors

return & end

;------------------------------------------------------------------------
;-- SECCHI data structure

pro secchi__define,void                 

void={secchi, inherits map}

return & end
