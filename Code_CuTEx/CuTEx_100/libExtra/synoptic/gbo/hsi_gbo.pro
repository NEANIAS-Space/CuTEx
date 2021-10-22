;+
; Project     : HESSI
;
; Name        : hsi_gbo
;
; Purpose     : update Max Millennium catalog with RHESSI flare catalog info
;
; Category    : synoptic gbo hessi
;
; Syntax      : IDL> hsi_gbo
;
; Inputs      : TSTART,TEND = time range to process
;     
; Keywords    : REPROCESS = reprocess existing entries
;               BACK = # of days back to process
;               VERBOSE = set verbose output
;             
; Restrictions: Unix only
;
; History     : Written 28 July 2002, D. Zarro (LAC/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro hsi_gbo,tstart,tend,reprocess=reprocess,back=back,verbose=verbose

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return
endif

verbose=keyword_set(verbose)

;-- read HESSI flare catalog

flare_url="http://hesperia.gsfc.nasa.gov/hessidata/"
flare_data = hsi_read_flarelist(err=err)
if is_string(err) then return

;-- locate MM database file

db_file=loc_file('$GBO_DB',count=count,err=err,/verb)
if count eq 0 then return else db_file=db_file[0]
if not test_open(db_file,/write) then return
resolve_routine,'db_gbo'

;-- restore MM catalog info

restore_gbo
common gbo_com, gbo_records
def_gbo,def_gbo

;-- establish processing times

if not valid_time(tstart) then dstart=anytim2utc(!stime) else $
 dstart=anytim2utc(tstart)

if not valid_time(tend) then dend=dstart else $
 dend=anytim2utc(tend)

;-- round to start and end of day

dstart.time=0 & dend.time=0
dend.mjd=dend.mjd+1
if dend.mjd le dstart.mjd then dend.mjd=dstart.mjd+1

if is_number(back) then tback=back > 0 else tback=0
if tback gt 0 then dstart.mjd=dstart.mjd-tback

if verbose then $
 message,'processing: '+anytim2utc(dstart,/vms)+' to '+anytim2utc(dend,/vms),/cont

;-- figure out overlapping RHESSI events

dstart=str_format(anytim2tai(dstart))
dend=str_format(anytim2tai(dend))
fstart=str_format(anytim(flare_data.start_time,/tai))
fend=str_format(anytim(flare_data.end_time,/tai))

d1= (fstart gt dend) 
d2= (fend gt dend)
d3= (fstart lt dstart) 
d4= (fend lt dstart) 

ochk=where( (d1 and d2) or (d3 and d4),ncomplement=fcount,complement=fchk)

if fcount eq 0 then begin
 message,'no cataloged RHESSI events during specified input times',/cont
 return
endif

;-- check if any HESSI events are in MM catalog

rchk=where(gbo_records.observatory eq 'RHESSI',rcount)
reprocess=keyword_set(reprocess)
new_entry=0b

for i=0,fcount-1 do begin

 temp=def_gbo

 flare=flare_data[fchk[i]]
 flare_id='Flare ID: '+trim2(flare.id_number)
 file=flare.filename
 if is_blank(file) then file=flare_id

;-- add new events to MM catalog or replace old 
;   (if reprocess is set, or something changed)

 add_flare=1b
 if rcount gt 0 then begin
  ichk=where(flare_id eq gbo_records.info,icount)
  if (icount gt 0) then begin
   if reprocess then gbo_records[ichk].deleted=1b else begin
    if icount gt 1 then gbo_records[ichk[1:icount-1]].deleted=1b
    old_rec=gbo_records[ichk[0]]
    gbo_time,old_rec,gstart,gend
    gstart=str_format(gstart)
    gend=str_format(gend)
    add_flare=(fstart[fchk[i]] ne gstart) or (fend[fchk[i]] ne gend) or $
              (trim2(file) ne trim2(old_rec.file))
   endelse
  endif
 endif

 if add_flare then begin
  rstart=anytim(flare.start_time,/utc_int)
  rend=anytim(flare.end_time,/utc_int)
  rdir=time2fid(rstart,delim='/',/full)
  temp.dstart=rstart.mjd
  temp.tstart=rstart.time
  temp.dend=rend.mjd
  temp.tend=rend.time
  temp.observatory='RHESSI'
  temp.instrument='RHESSI'
  temp.class='Image Spectrum Lightcurve'
  temp.type='Soft X-ray, Hard X-ray, Gamma-ray'
  temp.subtype='Full-Disk'
  temp.format='FITS'
  temp.name='Jim McTiernan'
  temp.campaign='009. Default HESSI Collaboration'
  temp.email='jimm@ssl.berkeley.edu'
  temp.url=flare_url+rdir
  temp.info=trim2(flare_id)
  temp.file=trim2(file)

;-- get GOES events

  vstart=anytim(flare.start_time,/vms)
  vend=anytim(flare.end_time,/vms)

  if verbose then message,'updating '+temp.info+' '+vstart+' - '+vend,/cont

  rd_gev,vstart,vend,gev,/nearest

  if is_struct(gev) then begin
   gtimes=anytim(gev,/tai)
   ptime=anytim(flare.peak_time,/tai)
   diff=abs(gtimes-ptime)
   nearest=where(diff eq min(diff))
   gev=gev[nearest]
   class=trim2(string(gev.st$class))
   day=trim2(gt_day(gev,/str))
   estart=strmid(trim2(gt_time(gev,/str)),0,5)
   result=[day[*]+','+estart[*]+','+class[*]]
   temp.goes=arr2str(result,delim='+')
  endif

  temp.submitted=anytim2tai(!stime)
  update_gbo,temp,/no_save
  new_entry=1b
 endif

endfor

if new_entry then purge_gbo,/save else message,'No new RHESSI events added',/cont
 
return & end
