;+
; Project     : HESSI
;
; Name        : LATEST_IMAGE
;
; Purpose     : return latest image from Synoptic archive
;
; Category    : synoptic gbo hessi
;
; Syntax      : IDL> map=latest_image(time,back=back)
;
; Inputs      : TIME = image closest to this time is returned
;     
; Output      : MAP = image in map structure or object format
;
; Keywords    : BACK = # of days back in time to search [def=3]
;               TYPE = /EIT,/BBSO,/TRACE,/MEUD,/MDI
;               LAST = return last saved image
;               FOUND = 1/0 if image is found/not found
;               OBJECT = return map as an object
;               FORCE = force a new read
;             
; History     : Written 1 Feb 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function latest_image,time,back=back,_extra=extra,err=err,$
                    last=last,verbose=verbose,found=found,$
                    object_return=object_return,force=force

err=''

common latest_image,synop,last_map,last_omap

found=0b
object_return=keyword_set(object_return)

if keyword_set(force) then begin
 delvarx,last_map
 if obj_valid(last_omap) then obj_destroy,last_omap
 if obj_valid(synop) then obj_destroy,synop
endif

;-- return last map if requested 

if keyword_set(last) and valid_map(last_map) and obj_valid(last_omap) then begin
 found=1b
 if object_return then return,last_omap else return,last_map
endif

;-- default to current UT

secs_per_day=24.*3600.d
if valid_time(time) then tend=time else get_utc,tend
if is_number(back) then tback=float(back) else tback=3.

;-- create and store a SYNOP object

if not obj_valid(synop) then synop=obj_new('synop',smode=1)

;-- set up start/end times

tend=anytim2tai(tend)+secs_per_day
tstart=tend-(tback+1.)*secs_per_day

dprint,'% TSTART/TEND: ',anytim2utc(tstart,/vms)+' - '+anytim2utc(tend,/vms)
synop->setprop,mode=1,tstart=tstart,tend=tend,verbose=0,ldir=get_temp_dir()

;-- list all images in archive

synop->setprop,clobber=keyword_set(force)
synop->list,files,times=times,count=count,err=err

no_files='No matching files found'
if count eq 0 then begin
 message,no_files,/cont
 return,-1
endif

;-- filter out type 

if is_struct(extra) then begin
 tags=arr2str(tag_names(extra),delim='|')
 chk=where(stregex(files,tags,/bool,/fold),count)
 if count eq 0 then begin
  message,no_files,/cont
  return,-1
 endif
 files=files[chk] & times=times[chk]
endif

;-- get nearest file in time

diff=times-tend
nearest=where(diff eq min(diff))
tfile=files[nearest[0]]

;-- download it

lfile=synop->fetch(tfile,err=err)

if err ne '' then begin
 message,err,/cont
 return,-1
endif

;-- read file and make a map

r=obj_new('reader')
r->read,lfile,omap,err=err
obj_destroy,r

if err ne '' then return,-1

map=omap->get(/map)
last_map=map
last_omap=omap

if object_return then return,omap else return,map

if keyword_set(verbose) then message,'Found - '+map.id+' at '+map.time,/cont

found=1b
return,map

end
