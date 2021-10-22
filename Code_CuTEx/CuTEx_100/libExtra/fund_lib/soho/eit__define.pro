;+
; Project     : HESSI
;
; Name        : EIT__DEFINE
;
; Purpose     : Define an EIT data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('eit')
;
; History     : Written 17 Feb 2001, D. Zarro, EIT/GSFC
;               Modified 17 Mar 2004, Zarro (L-3Com/GSFC) - call EIT_COLORS
;               Modified 9  Jun 2004, Gallagher (L-3Com/GSFC) - added ::LATEST
;               Modified 28 Nov 2005, Zarro (L-3Com/GSFC) 
;                - added VSO search capability
;               Modified 2 Sep 2006, Zarro (ADNET/GSFC) - added color
;                                                         protection
;               Modified 9 July 2007, Zarro (ADNET/GSFC) 
;                - redirected ::LATEST to search RHESSI Synoptic archive
;
; Contact     : dzarro@solar.stanford.edu
;-

;-------------------------------------------------------------------------
;-- check for EIT calibration data files 

function eit::have_eit_cal

cal_dir=local_name('$SSWDB/soho/eit/calibrate')
test_file='cal19951209.fts'
chk=file_search(concat_dir(cal_dir,test_file),count=count)
have_cal=count eq 1

if (~have_cal) then begin
 message,'SOHO/EIT calibration files not installed',/cont
 server=eit_server(network=network)
 if network then begin
  message,'Will attempt to access files from remote server...',/cont
  recompile,'sock_eit'
  have_cal=1b
 endif
endif

;-- ensure that calibration directories are properly defined

SSW_EIT_RESPONSE=local_name('$SSW/soho/eit/response')
mklog,'SSW_EIT_RESPONSE',SSW_EIT_RESPONSE

return,have_cal

end

;-------------------------------------------------------------------------
;-- get EIT file times from file names

function eit::times,files,_extra=extra

if is_blank(files) then return,-1d

year='([0-9]{0,2}[0-9]{2})'
mon='([0-9]{2})'
day=mon
hr='([0-9]{0,2})'
min=hr
sec=hr
regex=year+mon+day+'\.'+hr+min+sec
check=stregex(files,regex,/ext,/sub)
value=anytim2utc(!stime,/ext)
times=make_array(n_elements(files),value=value)
times.year=comdim2(check[1,*])
times.month=comdim2(check[2,*])
times.day=comdim2(check[3,*])
times.hour=comdim2(check[4,*])
times.minute=comdim2(check[5,*])
times.second=comdim2(check[6,*])
times.millisecond=0

if ~is_struct(extra) then return,anytim2tai(times)
if have_tag(extra,'tai') then return,anytim2tai(times)

return,anytim2utc(times,_extra=extra)

end

;--------------------------------------------------------------------------
;-- wrapper around ::LIST that forces VSO search

function eit::search,tstart,tend,_ref_extra=extra

return,self->list(tstart,tend,_extra=extra,/vso,/tai)

end

;-----------------------------------------------------------------------------
;-- search EIT files either from local archive if available, or use VSO
;   if unavailable

function eit::list,tstart,tend,count=count,vso=vso,verbose=verbose,$
              times=times,sizes=sizes,_ref_extra=extra,window=window,wavelength=wavelength

forward_function eit_files

if is_number(window) then trange=window else trange=5.

verbose=keyword_set(verbose)
files='' & count=0 & sizes='' 

;-- list whole day if invalid times entered or find nearest to tstart

dstart=get_def_times(tstart,tend,dend=dend,/ecs,_extra=extra)
istart=get_def_times(tstart,tend,dend=iend,/ecs,_extra=extra)
nearest=valid_time(tstart) and ~valid_time(tend)
include=valid_time(tstart) and valid_time(tend)
if include then begin dstart=istart & dend=iend & endif

;-- check what is supported

use_vso=keyword_set(vso)
sup_eit=have_proc('eit_files') and is_dir('$EIT_TOP_DATA')
sup_vso=since_version('5.6')

vso_mess='VSO access unsupported for IDL '+!version.release
eit_mess='EIT archive not found on this system'
if (~sup_eit) and (~sup_vso) then begin
 message,vso_mess,/cont
 message,eit_mess,/cont
 times=-1d
 return,''
endif

if nearest then begin
 dstart=anytim2utc(anytim2tai(tstart)-trange*3600,/ecs)
 dend=anytim2utc(anytim2tai(tstart)+trange*3600,/ecs)
endif

if verbose then message,'Searching between '+dstart+' and '+dend,/cont

if use_vso and (~sup_vso) then begin
 message,vso_mess,/cont
 use_vso=0b
endif

if (~use_vso) and (~sup_eit) then begin
 message,eit_mess,/cont
 message,'Trying VSO..',/cont
 use_vso=1b
endif

;-- search VSO

if use_vso then begin
 if verbose then message,'Searching VSO...',/cont
 files=vso_search(dstart,dend,inst='eit',/urls,/flat,clean=0b)
 if is_struct(files) then begin
  if is_number(wavelength) then begin
   chk=where(files.wave_min eq wavelength,count)
   if count gt 0 then files=files[chk].url else files=''
  endif else files=files.url
  files=rem_blanks(files,count=count)
 endif
endif else begin

;-- search EIT archives

 if verbose then message,'Searching SOHO/EIT archives...',/cont
 f1=eit_files(dstart,dend,/lz,/quiet)
 f2=eit_files(dstart,dend,/quick,/quiet)
 files=get_uniq(rem_blanks([f1,f2]),count=count)

endelse

if verbose or (count eq 0) then message,'Found '+trim(count)+' matching files',/cont

if count gt 0 then begin
 if arg_present(times) or nearest then begin
  times=self->times(files,_extra=extra)
  if nearest then begin
   index=near_time(times,tstart)
   times=times[index[0]]
   files=files[index[0]]
   count=1
  endif
 endif
endif

if count gt 1 then sizes=strarr(count)
return,files

end

;------------------------------------------------------------------------------
; get latest EIT image

pro eit::latest,ofile,out_dir=out_dir,_ref_extra=extra,err=err,$
                filter = filter, bandpass=bandpass,back=back,wave=wave

  err=''

;-- default to current directory

  if is_blank(out_dir) then odir=curdir() else odir=out_dir
  if ~test_dir(odir,err=err,out=out,/verbose) then return
  odir=out

;-- Check if network available

  server = synop_server( network = network, /full )
  
  if (network eq 0) then begin
    err='No network available'
    message,err,/cont
    return
  endif
 
  if is_blank(back) then back=10 else back=fix(back)
  efilter='195'
  if is_number(wave) then efilter=trim(wave)
  if is_number(filter) then efilter=trim(filter)
  if is_number(bandpass) then efilter=trim(bandpass)

;-- create a SYNOP object and search backward from current UT

  synop=obj_new('synop')
  get_utc,tend
  tstart=tend & tstart.mjd=tstart.mjd-back
  synop->setprop,tstart=tstart,tend=tend,site='eit',mode=0
  synop->list,files
  obj_destroy,synop
  chk=where(stregex(files,efilter+'_',/bool),count)
  if count eq 0 then begin
   err='No recent EIT '+efilter+' files since last '+trim(back)+' days'
   message,err,/cont
   return
  endif

  fname=files[chk[count-1]]
  file_loc=server+fname

;-- Copy and read data into map object

  sock_copy,file_loc,out_dir=odir,err=err,/clobber,_extra=extra,$
            copy_file=ofile
  if err ne '' then begin
   message,err,/cont
   return
  endif

  self->read,ofile,err=err,_extra=extra
  if err ne '' then begin
   message,err,/cont
   return
  endif

  rm_file,ofile 
  return & end


;----------------------------------------------------------------------------

pro eit::cleanup

self->fits::cleanup
self->flush,10

return & end

;---------------------------------------------------------------------------
;-- save EIT color table

pro eit::colors,k

if ~have_proc('eit_colors') then return

id=self->get(k,/id)

dsave=!d.name
set_plot,'Z'
wave=[195,171,304,284]
swave=strtrim(wave,2)
tvlct,r0,g0,b0,/get
for i=0,n_elements(wave)-1 do begin
 chk=strpos(id,swave[i])
 if chk[0] gt -1 then begin
  call_procedure,'eit_colors',wave[i],red,green,blue
  self->set,k,red=red,green=green,blue=blue,/load_colors,/has_colors
  if exist(r0) then tvlct,r0,g0,b0
  set_plot,dsave
  return
 endif
endfor

set_plot,dsave

return & end

;---------------------------------------------------------------------------
;-- check for EIT branch in !path

function eit::have_eit_path,err=err

err=''
if ~have_proc('read_eit') then begin
 epath=local_name('$SSW/soho/eit/idl')
 if is_dir(epath) then ssw_path,/eit,/quiet
 if ~have_proc('read_eit') then begin
  err='SOHO/EIT branch of SSW not installed'
  message,err,/cont
  return,0b
 endif
endif

return,1b

end

;--------------------------------------------------------------------------
;-- download remote URL files to local directory

pro eit::download,file,out_dir=out_dir,_ref_extra=extra,out_file=out_file

out_file=''
if is_blank(file) then return
out_file=file

url=stregex(file,'http[s]?:\/\/',/fold,/bool)

;-- create temp directory for download

if url[0] then begin
 if write_dir(out_dir,/quiet) then eit_dir=out_dir else begin
  eit_dir=self->temp_dir()
  mk_dir,eit_dir
 endelse
endif

for i=0,n_elements(url)-1 do begin
 if url[i] then begin
  sock_copy,file[i],out_dir=eit_dir,_extra=extra, copy_file=copy_file
  out_file[i]=copy_file
 endif
endfor

return & end

;------------------------------------------------------------------------
;-- EIT download directory 

function eit::temp_dir

return,concat_dir(get_temp_dir(),'eit')

end

;-------------------------------------------------------------------------
;-- flush downloaded files

pro eit::flush,days

if ~is_number(days) then return

old_files=file_since(older=days,patt='ef*',count=count,path=self->temp_dir())

if count gt 0 then file_delete,old_files,/quiet

return & end

;--------------------------------------------------------------------------
;-- FITS reader

pro eit::read,file,data,_ref_extra=extra,err=err,no_prep=no_prep,nodata=nodata

;-- download if URL
 
self->download,file,out_file=ofile,_extra=extra

self->fits::read,ofile,data,_extra=extra,err=err,nodata=nodata

if is_string(err) then return

if keyword_set(nodata) then return

if (1-keyword_set(no_prep)) then begin
 self->prep,_extra=extra
 self->roll_correct
endif

count=self->get(/count)
for i=0,count-1 do begin
 self->set,i,/log_scale,grid=30,/limb
 self->colors,i
endfor

return & end

;---------------------------------------------------------------------------

pro eit::mreadfits,file,data,index=index,_ref_extra=extra,err=err
err=''

level0=self->is_level0(file,err=err)
if err ne '' then return

if level0 and self->have_eit_path() then begin
 dfile=find_compressed(file,err=err)
 if err ne '' then begin 
  message,err,/cont
  return
 endif
 call_procedure,'read_eit',dfile,index,data,_extra=extra
endif else begin
 self->fits::mreadfits,file,data,_extra=extra,index=index,err=err
endelse

if err ne '' then return

;-- update pointing for a partial frame 

index=eit_partial(index,_extra=extra,/verb,partial=partial)

;-- update CCD positions with COMMENT values

if have_tag(index,'comment') then begin
 if ~have_tag(index,'p1_x') then begin
  index=add_tag(index,0.,'p1_x')
  index=add_tag(index,0.,'p1_y')
  index=add_tag(index,0.,'p2_x')
  index=add_tag(index,0.,'p2_y')
 endif
 np=n_elements(index)
 for i=0,np-1 do begin
  comment=index[i].comment
  stc=stc_key(comment)
  if have_tag(stc,'p1_x') and partial[i] then begin
   index[i].p1_x=float(stc.p1_x)
   index[i].p1_y=float(stc.p1_y)
   index[i].p2_x=float(stc.p2_x)
   index[i].p2_y=float(stc.p2_y)
  endif
 endfor
endif

return & end

;-----------------------------------------------------------------------------
;-- prep EIT image

pro eit::prep,k,_extra=extra,err=err,verbose=verbose

verbose=keyword_set(verbose)

count=self->get(/count)
if is_number(k) then begin
 istart=k & iend=k
endif else begin
 istart=0 & iend=count-1
endelse

for i=istart,iend do begin

 if ~self->has_data(i,err=err) then begin
  message,err,/cont
  continue
 endif

 if self->has_history('Degridded',i) then begin
  if verbose then message,'Degridding already applied to image '+trim(i),/cont
  continue
 endif

 index=self->get(i,/index)

 if ((1024 mod index.naxis1) ne 0) or ( (1024 mod index.naxis2) ne 0) then begin
  if verbose then message,'Cannot degrid non-rectangular image',/cont
  continue
 endif

 if (~self->have_eit_cal()) or (~self->have_eit_path()) then return
 if verbose then message,'Degridding image '+trim(i)+'...',/cont

 map=self->get(i,/map,/no_copy,err=err)
 call_procedure,'eit_prep',index,data=temporary(map.data),nindex,ndata,_extra=extra,$
                     /response

 map.xc=nindex.xcen
 map.yc=nindex.ycen
 map.roll_angle=nindex.sc_roll
 map.data=temporary(ndata)
 nindex.origin='SOHO'
 nindex.telescop='EIT'
 map.id=str_replace(map.id,'Rocket Science','SOHO')
 
 self->set,i,map=map,index=nindex,/no_copy,/replace

;-- check if 180 degree roll was corrected during prep

 corrected_roll=(index.sc_roll eq 180.) and (nindex.sc_roll eq 0.)
 if corrected_roll then self->update_history,'180 degree roll correction applied',i

endfor

return & end


;---------------------------------------------------------------------------
;-- create filename from INDEX

function eit::get_name,index,err=err,ymd=ymd

err=''
if ~exist(index) then index=0
case 1 of
 is_string(index): nindex=fitshead2struct(index)
 is_struct(index): nindex=index
 is_number(index): begin
  if ~self->has_index(index,err=err) then return,''
  nindex=self->get(index,/index)
 end
 else: return,''
endcase

if ~have_tag(nindex,'wavelnth') then return,''

wave='00'+trim(nindex.wavelnth)
fid=time2fid(nindex.date_obs,/time,/full,/sec,err=err)
if err ne '' then return,''

ymd=time2fid(nindex.date_obs)
name='eit_'+wave+'_'+fid+'.fts'

return,name
end

;------------------------------------------------------------------------------
;-- check if file is level 0

function eit::is_level0,file,err=err

mrd_head,file,header,err=err
chk=where(stregex(header,'FILENAME.+(EFZ|EFR|SEIT)',/bool,/fold),count)
level0=count gt 0

;-- check if prep'ed

if level0 then begin
 chk=where(stregex(header,'Degridded',/bool,/fold),count)
 level0=count eq 0
endif

return,level0
end

;----------------------------------------------------------------------------
;-- EIT help

pro eit::help

print,''
print,"IDL> eit=obj_new('eit')                           ;-- create EIT object
print,"IDL> files=eit->list(files,'1-may-01','2-may-01') ;-- list files
print,'IDL> eit->read,file_name                          ;-- read and prep
print,'IDL> eit->plot                                    ;-- plot
print,'IDL> map=eit->get(/map)                           ;-- extract map
print,'IDL> data=eit->get(/data)                         ;-- extract data
print,'IDL> obj_destroy,eit                              ;-- destroy
print,'or'
print,"IDL> eit=obj_new('eit')                         ;-- create EIT object
print,'IDL> eit->latest, filter=195                    ;-- read latest 195 image
print,'IDL> eit->plot                                  ;-- plot
print,'IDL> obj_destroy,eit                            ;-- destroy


return & end

;------------------------------------------------------------------------------
;-- eit data structure

pro eit__define,void                 

void={eit, inherits fits}

return & end
