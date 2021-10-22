;+
; Project     : RHESSI
;
; Name        : SXI_GBO
;
; Purpose     : update Max Millennium catalog with SXI flare observations
;
; Category    : synoptic gbo 
;
; Syntax      : IDL> sxi_gbo,gev
;
; Inputs      : GEV = GOES events structure from RD_GEV
;     
; Keywords    : REPROCESS = reprocess existing entries
;               VERBOSE = set verbose output
;             
; History     : Written 10 March 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro sxi_gbo,gev,reprocess=reprocess,verbose=verbose,_extra=extra,$
                within=within

reprocess=keyword_set(reprocess)
verbose=keyword_set(verbose)

restore_gbo
common gbo_com, gbo_records

;-- find flares

if not is_struct(gev) then return
decode_gev,gev,gstart,gend,gpeak,/vms,class=class,_extra=extra
if verbose then message,'Processing following flares: '+arr2str(class),/cont
nflares=n_elements(gev)

;-- find SXI image nearest each GOES peak

sxi=obj_new('sxi')
new_entry=0b
do_purge=0b
for i=0,nflares-1 do begin

 this_day=anytim2utc(gpeak[i])
 this_day.time=0
 prev_day=this_day
 prev_day.mjd=prev_day.mjd-1
 next_day=this_day
 next_day.mjd=next_day.mjd+2
 new_day=1b
 if exist(old_day) then new_day=this_day.mjd ne old_day.mjd 
 if new_day then begin
  dstart=anytim2utc(prev_day,/vms)
  dend=anytim2utc(next_day,/vms)
  if verbose then message,'Searching '+dstart+' to '+ dend,/cont
  sxi->find,files,dstart,dend,count=fcount,times=times,/full,verbose=verbose
  noaa=list_nar(this_day,/all)
 endif
 old_day=this_day

 if fcount eq 0 then begin
  message,'No SXI data near GOES '+class[i]+' peak at '+gpeak[i],/cont
  continue
 endif

 diff=abs(times-anytim2tai(gpeak[i]))
 ok=where(diff eq min(diff))
 tdiff=diff[ok[0]]
 if is_number(within) then begin
  if (tdiff gt within*3600.) then begin
   message,'No SXI data within '+trim2(within)+' hour(s) of '+ gpeak[i],/cont
   continue
  endif
 endif
 file=files[ok[0]]

 day=trim2(gt_day(gev[i],/str))
 estart=strmid(trim2(gt_time(gev[i],/str)),0,5)
 result=day+','+estart+','+class[i]
 
;-- check if SXI file entry in MM catalog

 sfile=file_break(file)
 chk=where( (gbo_records.file eq sfile) and (gbo_records.deleted ne 1),count)
 new_entry=count eq 0
 old_entry=count gt 0
 if old_entry then old_id=gbo_records[chk].id

;-- update catalog

 if reprocess or new_entry then begin
  if verbose then message,'Processing '+sfile+' near GOES '+class[i]+' event '+gpeak[i],/cont
  sxi->read,sfile,/ngdc,/nodata,index=index
  if is_struct(index) then begin
   fstart=anytim2tai(index.date_obs)
   fend=fstart+index.exptime
   rstart=anytim2utc(fstart)
   rend=anytim2utc(fend)
   def_gbo,temp
   temp.dstart=rstart.mjd
   temp.tstart=rstart.time
   temp.dend=rend.mjd
   temp.tend=rend.time
   temp.observatory='GOES12/SXI'
   temp.instrument='SXI '+trim(index.wavelnth)
   temp.class='Image'
   temp.type='Soft X-ray'
   temp.subtype='Full-Disk'
   temp.format='FITS'
   temp.name='Steven Hill'
   temp.campaign='009. Default HESSI Collaboration'
   temp.email='steven.hill@sec.noaa.gov'
   temp.url=file
   temp.info='Object: '+trim(index.object)+'. Additional observations: http://sxi.ngdc.noaa.gov'
   temp.file=sfile
   temp.goes=result
   temp.noaa=noaa
   temp.submitted=anytim2tai(!stime)
   if reprocess and old_entry then temp.id=old_id 
   update_gbo,temp,/no_save,replace=reprocess
   do_purge=1b
  endif
 endif
endfor

if do_purge then purge_gbo,/save else message,'No new SXI entries added',/cont

obj_destroy,sxi
 
return & end
