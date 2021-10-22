;+
; Project     : HESSI
;
; Name        : MDI__DEFINE
;
; Purpose     : Define an MDI data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('mdi')
;
; History     : Written 17 Feb 2001, D. Zarro, EIT/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function mdi::init,_ref_extra=extra

return,self->fits::init(_extra=extra)
       
end

;----------------------------------------------------------------------------

pro mdi::cleanup

self->fits::cleanup

return & end

;----------------------------------------------------------------------------
;-- convert MDI index to SSW/FITS standard

function mdi::index2fits,index,no_copy=no_copy,err=err

err=''
if not is_struct(index) then return,-1

;-- convert some SOI fields -> SSW standards

if (gt_tagval(index[0],/crpix1) eq 0) and (gt_tagval(index[0],/center_x) ne 0) then begin
 if (index[0].cdelt1)*(index[0].cdelt2) eq 0 then begin
  index.cdelt1=gt_tagval(index,/xscale)
  index.cdelt2=gt_tagval(index,/yscale)
  index.crpix1=gt_tagval(index,/center_x)
  index.crpix2=gt_tagval(index,/center_y)
  index.solar_r=gt_tagval(index,/r_sun)
 endif
endif

;-- check type

if have_tag(index,'outfil') then begin
 chk1=where(stregex(index.outfil,'igram',/bool),count1)
 chk2=where(stregex(index.outfil,'mag',/bool),count2)
 if (count1 gt 0) or (count2 gt 0) then begin
  if not have_tag(index,'type',/exact) then index=add_tag(index,'','type')
  if count1 gt 0 then index[chk1].type='Intensitygram'
  if count2 gt 0 then index[chk2].type='Magnetogram'
 endif
endif

;-- rationalize different time styles

return,index

if have_tag(index,'time') and have_tag(index,'day') then begin
 if (index[0].time eq 0) and (index[0].day eq 0) then begin
  if strtrim(index[0].date_obs,2) eq '' then begin
   if have_tag(index[0],'reftime') then ints=anytim(index.reftime,/ints)
  endif else begin 
   if strlen(index[0].date_obs) lt 18 then $
    ints=anytim(index.date_obs + ' ' + index.time_obs,/ints) else $
     ints=anytim(gt_tagval(index,/date_obs),/ints)
  endelse
  if is_struct(ints) then begin
   index.time=ints.time
   index.day=ints.day
  endif
 endif
endif
 
return,struct2ssw(index)     

end

;--------------------------------------------------------------------------
;-- FITS reader

pro mdi::read,file,data,nodata=nodata,_ref_extra=extra,prep=prep

self->fits::read,file,data,_extra=extra,nodata=nodata
if keyword_set(nodata) then return

self->roll_correct

if keyword_set(prep) then self->limb_correct

count=self->get(/count)
for i=0,count-1 do self->set,i,grid=30,/limb

return & end

;--------------------------------------------------------------------------
;-- check if already flatfielded

function mdi::flat_fielded

return,self->has_history('Flatfield applied')

end

;--------------------------------------------------------------------------
;-- check if already darklimb corrected

function mdi::limb_corrected

return,self->has_history('Dark limb corrected')

end

;--------------------------------------------------------------------------
;-- apply limb correction

pro mdi::limb_correct,err=err
err=''

if not self->has_data() then begin
 err='No image read'
 message,err,/cont
 return
endif

;-- flatfield first and correct for possible roll

verbose=self->get(/verbose_map)

if not self->flat_fielded() then self->flat_field,err=err
if not self->roll_corrected() then self->roll_correct

if is_string(err) then return

if self->limb_corrected() then begin 
 if verbose then message,'Dark limb correction already applied',/cont
 return
endif

time=self->get(/time)
xc=self->get(/xc)
yc=self->get(/yc)
nx=self->get(/nx)
ny=self->get(/ny)
dx=self->get(/dx)
dy=self->get(/dy)
soho=self->get(/soho)
pbr=pb0r(time,soho=soho,/arcsec)
radius=2.*pbr(2)/(dx+dy)

crpix1=comp_fits_crpix(xc,dx,nx)
crpix2=comp_fits_crpix(yc,dy,ny)

map=self->get(/map,/no_copy)
if verbose then message,'Applying dark limb correction...',/cont

darklimb_correct,map.data,temp_img,limbxyr=[crpix1,crpix2,radius],lambda=6767

;bdata=cscale(temporary(temp_img),/no_copy)
map.data=temporary(temp_img)

self->set,map=map,/no_copy

;-- update history

self->update_history,'Dark limb corrected'

return & end

;-----------------------------------------------------------------------------
;-- apply flatfield

pro mdi::flat_field,err=err

common mdi_flatfield,flat_map

err=''

if not self->has_data() then begin
 err='No image read return
 message,err,/cont
 return
endif

verbose=self->get(/verbose_map)

;-- check if already flatfielded

if self->flat_fielded() then begin
 if verbose then message,'Flatfield already applied',/cont
 return
endif
 
nx=self->get(/nx)
ny=self->get(/ny)
dx=self->get(/dx)
dy=self->get(/dy)

if (nx ne 1024) or (ny ne 1024) or ((dx lt 1.) and (dy le 1.)) then begin
 err='Image is not full-disk'
 message,err,/cont
 return
endif

;-- read flat field file

if not valid_map(flat_map) then begin
 flatfield_file ='$SSWDB/soho/mdi/flatfield/mdi_flat_jan2001.fits'
 loc=loc_file(flatfield_file,count=count)
 if count eq 0 then begin
  err='Unable to locate latest MDI flatfield file - mdi_flat_jan2001.fits'
  message,err,/cont
  return
 endif
 flat=obj_new('fits')
 flat->read,flatfield_file
 flat_map=flat->get(/map,/no_copy)
 obj_destroy,flat
endif

;-- normalize MDI image

map=self->get(/map,/no_copy)

if verbose then message,'Applying flatfield correction...',/cont

map.data = temporary(flat_map.data)*temporary(map.data)

self->set,map=map,/no_copy

;-- update history

self->update_history,'Flatfield applied'

return & end

;------------------------------------------------------------------------------
;-- MDI data structure

pro mdi__define                 

self={mdi,inherits fits}

return & end



