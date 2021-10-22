;+
; Project     : HESSI
;
; Name        : MDI_GBO
;
; Purpose     : update Max Millennium catalog with MDI info
;
; Category    : synoptic gbo
;
; Syntax      : IDL> mdi_gbo
;
; Inputs      : TSTART,TEND = time range to process
;     
; Keywords    : REPROCESS = reprocess existing entries
;               BACK = # of days back to process
;               VERBOSE = set verbose output
;             
; Restrictions: Unix only
;
; History     : Written 4 Dec 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro mdi_gbo,tstart,tend,reprocess=reprocess,back=back,verbose=verbose

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return
endif

if not is_dir('$SYNOP_DATA') then begin
 err='$SYNOP_DATA is undefined'
 message,err,/cont
 return
endif

;-- locate database file

db_file=loc_file('$GBO_DB',count=count,err=err,/verb)
if count eq 0 then return else db_file=db_file[0]
if not test_open(db_file,/write) then return
resolve_routine,'db_gbo'

if not valid_time(tstart) then dstart=anytim2utc(!stime) else $
 dstart=anytim2utc(tstart)

if not valid_time(tend) then dend=anytim2utc(!stime) else $
 dend=anytim2utc(tend)

dstart.time=0 & dend.time=0
if dend.mjd eq dstart.mjd then dend.mjd=dend.mjd+1
if is_number(back) then tback=back > 0 else tback=0
if tback gt 0 then dstart.mjd=dstart.mjd-tback

message,'processing: '+anytim2utc(dstart,/vms)+' to '+anytim2utc(dend,/vms),/cont

;-- GBO catalog info

reprocess=keyword_set(reprocess)
server=synop_server(path=path,/full)
url=server+path+'/images/'

restore_gbo
common gbo_com, gbo_records
def_gbo,def_gbo

omdi=obj_new('mdi')
osynop=obj_new('synop')
osynop->setprop,smode=1,mode=0,site='mdi',tstart=dstart,tend=dend
osynop->list,files,count=count
if count gt 0 then begin
 times=parse_time(files,ymd=ymd,/short,/utc,sep='')
 for k=0,count-1 do begin
  break_file,files[k],dsk,dir,name,ext
  file=name+ext
  done=where(file eq gbo_records.file,fcount)
  if (fcount eq 0) or reprocess then begin
   if fcount gt 0 and reprocess then gbo_records[done].deleted=1b
   utc=times[k]
   fid=ymd[k]
   temp_gbo=def_gbo
   temp_gbo.dstart=utc.mjd
   temp_gbo.tstart=utc.time
   temp_gbo.dend=utc.mjd
   temp_gbo.tend=utc.time
   url=server+synop_path(utc)+'/images/'+concat_dir(fid,file)
   temp_gbo.url=url
   temp_gbo.file=file
   temp_gbo.class='Image'
   temp_gbo.format='FITS'
   temp_gbo.observatory='SOHO/MDI'
   temp_gbo.instrument='MDI'
   temp_gbo.email='gregory(at)mdisas.nascom.nasa.gov'
   temp_gbo.type='Optical'
   temp_gbo.name='Sarah Gregory'
   temp_gbo.campaign='009. Default HESSI Collaboration'
   if stregex(file,'_mag',/bool) then temp_gbo.subtype='Magnetograms (line of sight)'

;-- get GOES events

   gev=nearest_gev(utc,during=during)
   if is_struct(gev) then begin
    class=trim(string(gev.st$class))
    day=trim(gt_day(gev,/str))
    fstart=strmid(trim(gt_time(gev,/str)),0,5)
    result=[day[*]+','+fstart[*]+','+class[*]]
    temp_gbo.goes=arr2str(result,delim='+')
   endif

;-- get NOAA AR's

   day=utc
   day.time=0
   temp_gbo.noaa=list_nar(day,/all)
   temp_gbo.submitted=anytim2tai(!stime)
   update_gbo,temp_gbo,/no_save
  endif
 endfor
endif

obj_destroy,osynop
obj_destroy,omdi
purge_gbo,/save
 
return & end






