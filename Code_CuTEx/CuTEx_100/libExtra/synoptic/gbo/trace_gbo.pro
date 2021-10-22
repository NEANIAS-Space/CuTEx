;+
; Project     : RHESSI
;
; Name        : TRACE_GBO
;
; Purpose     : update Max Millennium catalog with TRACE flare observations
;
; Category    : synoptic gbo 
;
; Syntax      : IDL> trace_gbo,gev
;
; Inputs      : GEV = GOES events structure from RD_GEV
;     
; Keywords    : REPROCESS = reprocess existing entries
;               VERBOSE = set verbose output
;               WITHIN = record if nearest TRACE observation occurs within
;                        this amount of hours relative to GOES peak.
;             
; History     : Written 10 March 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro trace_gbo,gev,reprocess=reprocess,verbose=verbose,_extra=extra,$
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

;-- find TRACE file nearest each GOES peak

new_entry=0b
do_purge=0b
testarray=['wave_len=171,195','naxis1>=512']
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
  
  error=0
  catch,error
  if error ne 0 then begin
   catch,/cancel
   message,err_state(),/cont
   continue
  endif

  trace_cat,dstart,dend,tcat
  ss=struct_where(tcat,test=testarray,scount,/quiet)
  if scount gt 0 then begin
   tcat=tcat[ss] & times=anytim(tcat,/tai)
  endif
  noaa=list_nar(this_day,/all)
 endif

 old_day=this_day
 if scount eq 0 then begin
  message,'No TRACE data near GOES peak on '+ gpeak[i],/cont
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
   message,'No TRACE data within '+trim2(within)+' hour(s) of '+ gpeak[i],/cont
   continue
  endif
 endif

;-- check if file entry in MM catalog

 trace_cat2data,tcat[chk[0]],index,data
 if not is_struct(index) then begin
  message,'Problems processing data near '+gpeak[i],/cont
  continue
 endif

;-- check if this file already treated

 sfile=trace_struct2filename(index,/soho,/seconds)
 chk=where(sfile eq dfiles,dcount)
 if dcount gt 0 then continue
 dfiles[i]=sfile

 chk=stregex(sfile,'[0-9]{6}_',/ext,/sub)
 ymd=strmid(chk,0,6)
 outdir=concat_dir(synop_images,ymd)
 oname=concat_dir(outdir,sfile)
 coname=oname+'.gz'
 have_file=file_test(oname,/reg)
 have_cfile=file_test(coname,/reg)
 chk=where( (gbo_records.file eq sfile) and (gbo_records.deleted ne 1),count)
 new_entry=count eq 0
 old_entry=count gt 0
 if old_entry then old_id=gbo_records[chk].id

;-- update catalog

 if reprocess or new_entry then begin
  if verbose then message,'Processing '+sfile+' near GOES '+class[i]+' event '+gpeak[i],/cont
  fstart=anytim(index,/tai)
  fend=fstart+index.sht_mdur
  rstart=anytim2utc(fstart)
  rend=anytim2utc(fend)
  
  def_gbo,temp
  temp.center=xy2hel(index.xcen,index.ycen,date=fstart)
  xsize=strtrim(string(index.naxis1*index.cdelt1/60.,'(f3.1)'),2)
  ysize=strtrim(string(index.naxis2*index.cdelt1/60.,'(f3.1)'),2)
  temp.fov=xsize+','+ysize
  temp.dstart=rstart.mjd
  temp.tstart=rstart.time
  temp.dend=rend.mjd
  temp.tend=rend.time
  temp.observatory='TRACE'
  temp.class='Image'
  temp.instrument='TRACE '+index.wave_len
  temp.type='XUV,EUV,UV'
  temp.email='dcm(at)zuni.nascom.nasa.gov'
  temp.subtype='Partial-Disk'
  temp.format='FITS'
  temp.name='Dawn Myers'
  temp.campaign='009. Default HESSI Collaboration'
  temp.file=sfile
  temp.goes=result
  temp.noaa=noaa
  temp.submitted=anytim2tai(!stime)
  temp.url=url+'/'+ymd+'/'+sfile+'.gz'

;-- create FITS file

  if (not have_file) and (not have_cfile) then begin
   if test_dir(synop_images) then begin
    mk_dir,outdir,/a_write
    if write_dir(outdir) then begin
     trace_prep,index,data,pindex,pdata,/wave2point,/norm,/float,$
                _extra=extra
     write_trace,pindex,pdata,outdir=outdir,loud=verbose,/soho,/seconds
    endif
   endif
  endif

  if reprocess and old_entry then temp.id=old_id 
  update_gbo,temp,/no_save,replace=reprocess
  do_purge=1b
 endif

;-- zip files

 if file_test(oname,/reg) then begin
  chmod,oname,/g_write,/g_read
  espawn,'gzip -f '+oname,/noshell
 endif

endfor

if do_purge then purge_gbo,/save else message,'No new TRACE entries added',/cont

return & end






