;+
; Project     : RHESSI
;
; Name        : EIT_GBO
;
; Purpose     : update Max Millennium catalog with EIT flare observations
;
; Category    : synoptic gbo 
;
; Syntax      : IDL> eit_gbo,gev
;
; Inputs      : GEV = GOES events structure from RD_GEV
;     
; Keywords    : REPROCESS = reprocess existing entries
;               VERBOSE = set verbose output
;               WITHIN = record if nearest EIT observation occurs within
;                        this amount of hours relative to GOES peak.
;             
; History     : Written 10 March 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro eit_gbo,gev,reprocess=reprocess,verbose=verbose,_extra=extra,$
                within=within

reprocess=keyword_set(reprocess)
verbose=keyword_set(verbose)

restore_gbo
common gbo_com, gbo_records

decode_gev,gev,gstart,gend,gpeak,/vms,class=class,_extra=extra
if not is_struct(gev) then return
nflares=n_elements(gev)
if verbose then message,'Processing following flares: '+arr2str(class),/cont

synop_images=chklog('$SYNOP_DATA/images')
server=synop_server(path=path,/full)
url=server+path+'/images'

;-- find EIT file nearest each GOES peak

new_entry=0b
do_purge=0b
eit=obj_new('eit')

d2='([0-9]{2})'
regex='([a-z]+)([0-9]{4})'+d2+d2+'\.'+d2+d2+d2

dfiles=strarr(nflares)
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
  files=eit_files(dstart,dend)
  times=parse_time(files,regex=regex,/tai)
  noaa=list_nar(this_day,/all)
 endif
 old_day=this_day
 if is_blank(files) then begin
  message,'No EIT files near GOES peak on '+ gpeak[i],/cont
  continue
 endif

 day=trim2(gt_day(gev[i],/str))
 estart=strmid(trim2(gt_time(gev[i],/str)),0,5)
 result=day+','+estart+','+class[i]

;-- find file nearest peak

 diff=abs(times-anytim2tai(gpeak[i]))
 chk=where(diff eq min(diff))
 tdiff=diff[chk[0]]
 if is_number(within) then begin
  if (tdiff gt within*3600.) then begin
   message,'No EIT data within '+trim2(within)+' hour(s) of '+ gpeak[i],/cont
   continue
  endif
 endif
 file=files[chk[0]]

;-- check if we already processed this file

 chk=where(file eq dfiles,dcount)
 if dcount gt 0 then continue
 dfiles[i]=file
 
;-- check if file entry in MM catalog

 sfile=file_break(file)
 chk=where( (gbo_records.file eq sfile) and (gbo_records.deleted ne 1),count)
 new_entry=count eq 0
 old_entry=count gt 0
 if old_entry then old_id=gbo_records[chk].id

;-- update catalog

 if reprocess or new_entry then begin
  if verbose then message,'Processing '+sfile+' near GOES '+class[i]+' event '+gpeak[i],/cont
  eit->read,file,index=index,/nodata
  if is_struct(index) then begin
   index=index[0]
   chk=stregex(index.object,'(dark|calibration|readout|continous|lamp)',/bool,/fold)
   if chk then begin
    if verbose then message,'Skipping engineering image',/cont
    if old_entry then gbo_records[chk].deleted=1
    continue
   endif
   eit->read,file
   fstart=anytim2tai(index.date_obs)
   fend=fstart+index.exptime
   rstart=anytim2utc(fstart)
   rend=anytim2utc(fend)
   def_gbo,temp
   temp.dstart=rstart.mjd
   temp.tstart=rstart.time
   temp.dend=rend.mjd
   temp.tend=rend.time
   temp.observatory='SOHO/EIT'
   temp.class='Image'
   temp.instrument='EIT '+trim(index.wavelnth)
   temp.type='XUV,EUV,UV'
   temp.email='thompson(at)eitv3.nascom.nasa.gov'
   temp.subtype='Full-Disk'
   temp.format='FITS'
   temp.name='Barbara Thompson'
   temp.campaign='009. Default HESSI Collaboration'
   temp.file=sfile
   temp.goes=result
   temp.noaa=noaa
   temp.submitted=anytim2tai(!stime)

;-- create FITS file

   oname=eit->get_name(index,ymd=ymd)
   temp.url=url+'/'+ymd+'/'+oname+'.gz'
   if write_dir(synop_images) then begin
    outdir=concat_dir(synop_images,ymd)
    mk_dir,outdir,/a_write
    if write_dir(outdir) then begin
     chk1=loc_file(oname,path=outdir,count=fcount,/recheck)
     if fcount eq 0 then chk2=loc_file(oname+'.gz',path=outdir,count=fcount,/recheck)
     if (fcount eq 0) then begin
      eit->earth_correct
      eit->write,oname,out_dir=outdir,/compress
     endif
     file_delete,oname,/quiet
    endif
   endif
   if reprocess and old_entry then temp.id=old_id 
   update_gbo,temp,/no_save,replace=reprocess
   do_purge=1b
  endif
 endif
endfor

if do_purge then purge_gbo,/save else message,'No new EIT entries added',/cont

obj_destroy,eit
 
return & end

