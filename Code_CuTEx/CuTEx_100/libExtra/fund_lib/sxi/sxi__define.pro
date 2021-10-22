; Project     : SXI
;
; Name        : SXI__DEFINE
;
; Purpose     : Define an SXI data object
;
; Category    : Ancillary Synoptic Objects
;
; Syntax      : IDL> c=obj_new('sxi')
;
; History     : Written 16 Feb 2003, D. Zarro, (EER/GSFC)
;               Modified 22 Dec 2004, Zarro (L-3Com/GSFC) - added /level2
;               Modified 22 Apr 2005, Zarro (L-3Com/GSFC) - improved NGDC checking
;               Modified 7  Aug 2005, Zarro (L-3Com/GSFC) 
;               - fixed use of level2 reader
;               Modified 3 Jan 2006, Zarro (L-3Com/GSFC)
;               - renamed LIST procedure to SEARCH
;               Modified 3 Feb 2006, Zarro (L-3Com/GSFC)
;               - fixed bug in searching over multiple days
;               Modified 28 July 2006, Zarro (ADNET/GSFC)
;               - incorporated GOES13 and future satellites
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function sxi::init,_ref_extra=extra

ret=self->hfits::init(_extra=extra)

if ret eq 0b then return,0b

;-- check if SXI branch installed
    
sxi_startup,err=err
if err eq '' then self.installed=1b
self->hset,/gateway,buffsize=2048l,sat=12
       
return,ret

end

;---------------------------------------------------------------------------
;-- Set property method

pro sxi::hset,ngdc=ngdc,_ref_extra=extra,satellite=satellite
if is_number(satellite) then self.sat=satellite
if is_number(ngdc) then self.ngdc=  0b > byte(ngdc) < 1b
if is_string(extra) then self->hfits::hset,_extra=extra
return & end

;----------------------------------------------------------------------------

pro sxi::cleanup

self->hfits::cleanup

return & end

;----------------------------------------------------------------------------
; SXI front-end FITS reader

pro sxi::read,file,data,_ref_extra=extra,ngdc=ngdc,remote=remote,err=err,$
                        no_warn=no_warn

err=''
if is_blank(file) then return
file=strtrim(file,2)
chk=loc_file(file,count=count)
is_url=stregex(file,'http[s]?:\/\/',/fold,/bool)
is_url=is_url[0]
ngdc=keyword_set(ngdc) or keyword_set(remote) or (count eq 0) or is_url

installed=self->hget(/installed) 
if (not installed) and (1-keyword_set(no_warn)) then begin
 warn=['$SSW/goes/sxig12 branch does not appear to be installed on this system.',$
       'Special SXI FITS readers will not be used, resulting in images with',$
       'possibly inaccurate pointings.']
 xack,warn,/suppress
endif

no_copy=1-arg_present(data)

;-- read directly from NGDC if /NGDC

if ngdc then begin
 if not is_url then begin
  server=self->get_server(network=network,path=path,err=err,_extra=extra,/full)
  if not network then return
  temp=parse_time(file,ymd=ymd)
  file_url=server+path+'/'+ymd+'/'+file_break(file)
 endif else file_url=file
 self->hfits::read,file_url,data,_extra=extra,no_copy=no_copy,err=err
endif else self->fits::read,file,data,_extra=extra,no_copy=no_copy,err=err

if err eq '' then self->set,/log_scale

return & end

;----------------------------------------------------------------------------
; SXI low-level FITS reader

pro sxi::mreadfits,file,data,header=header,index=index,_ref_extra=extra,$
                   nodata=nodata,err=err,dscale=dscale

forward_function sxig12_read,sxig12_read_one

installed=self->hget(/installed)

dscale=keyword_set(dscale)
noscale=1b-dscale

;-- if SXI branch not installed, use standard FITS reader

err=''

if keyword_set(nodata) then begin
 mrd_head,file,header,err=err,_extra=extra
 if err eq '' then index=fitshead2struct(header)
 return
endif

level1=stregex(file,'_B',/bool) eq 1b
level2=stregex(file,'_C',/bool) eq 1b

if (not installed) then begin
 self->fits::mreadfits,file,data,header=header,index=index,$
             _extra=extra,err=err,dscale=dscale
 return
endif

if level1 then data=sxig12_read_one(file,header,noscale=noscale,_extra=extra) else $
 data=sxig12_read(file,header,_extra=extra,noscale=noscale)
if is_string(header) then index=fitshead2struct(header)

return & end

;---------------------------------------------------------------------
;-- return SXI server & path

function sxi::get_server,_ref_extra=extra

sat=self->hget(/sat)
return,sxi_server(_extra=extra,sat=sat)

end

;-----------------------------------------------------------------------
;-- list SXI files by searching NGDC catalog

function sxi::list,tstart,tend,count=count,_ref_extra=extra,times=times,$
              full_path=full_path,sizes=sizes,tai=tai

times='' & sizes=''
tcat=self->list_index(tstart,tend,count=count,_extra=extra)
if count gt 0 then begin
 times=tcat.date_obs
 if keyword_set(tai) then times=anytim2tai(times)
 files=tcat.fname+'.FTS'
 sizes=replicate('1.0M',count)
 if keyword_set(full_path) then files=tcat.path+'/'+temporary(files)
 return,files
endif else begin
 sat=strtrim(self->hget(/sat),2)
 message,'No GOES'+sat+' SXI files found',/cont
 if keyword_set(tai) then times=-1.d
 return,''
endelse

end

;----------------------------------------------------------------------------
;-- HTTP search of SXI archive at NGDC

function sxi::search,tstart,tend,_ref_extra=extra

return,self->list(tstart,tend,_extra=extra,/tai)

end

;----------------------------------------------------------------------------
;-- SXI help

pro sxi::help

print,''
print,"IDL> sxi=obj_new('sxi')                         ;-- create object
print,'IDL> files=sxi->search(tstart,tend)              ;-- search leve1 1 files at NGDC
print,'IDL> sxi->copy,file_name [,out_dir=out_dir]     ;-- download
print,'IDL> sxi->read,file_name                        ;-- read
print,'IDL> sxi->plot                                  ;-- plot
print,'IDL> map=sxi->get(/map)                         ;-- extract map
print,'IDL> data=sxi->get(/data)                       ;-- extract data
print,'IDL> obj_destroy,sxi                            ;-- destroy
print,''
self->filters

return & end

;--------------------------------------------------------------------------
;-- SXI dynamic catalog

pro sxi::cat,catalog,tstart,tend

catalog=-1

if (not valid_time(tstart)) or (not valid_time(tend)) then begin
 message,"Please enter start and end time (e.g. ->cat,catalog,'1-mar-03','2-mar-03')",/cont
 return
endif

;-- list remote file names at NGDC

ngdc_sav=self->hget(/ngdc)
self->hset,/ngdc
self->seach,files,tstart,tend,count=count
self->hset,ngdc=ngdc_sav

if count eq 0 then return

;-- read their headers

self->read,files,index=catalog,/ngdc,/nodata

return & end

;------------------------------------------------------------------------------
;-- SXI copy from NGDC

pro sxi::copy,file,out_file,_ref_extra=extra,err=err,cancelled=cancelled,status=status

;-- determine server and search directories

status=0b
cancelled=0b
err=''

if is_blank(file) then begin
 err='Missing input filename'
 message,err,/cont
 return
endif

server=self->get_server(network=network,path=path,/verb,_extra=extra,err=err)
if not network then return

dfile=file_break(file)
temp=parse_time(dfile,ymd=ymd)
file_url=server+path+'/'+ymd+'/'+dfile

nfile=n_elements(file)
if is_string(out_file) then begin
 if n_elements(out_file) ne nfile then begin
  err='# of input and output filenames must match'
  message,err,/cont
  return
 endif
 ofile=out_file
endif else ofile = replicate('', nfile) ;jmm, 22-sep-2003

for i=0,nfile-1 do begin
 self->hfits::copy,file_url[i],ofile[i],_extra=extra,status=status,cancelled=cancelled,err=err
 if cancelled then return
endfor

return & end

;------------------------------------------------------------------------------
;-- read SXI index file catalog

function sxi::read_index,tdate,count=count,verbose=verbose,err=err,$
         _extra=extra

ext='_B_12.FTS'
if have_tag(extra,'level0') then ext='_A_12.FTS'
if have_tag(extra,'level2') then ext='_C_12.FTS'
sat=self->hget(/sat)
if is_number(sat) then if (sat ne 12) then ext=str_replace(ext,'12',strup(sat))

count=0 & err=''
if not valid_time(tdate) then get_utc,udate else udate=tdate
server=self->get_server(path=path,network=network,/full,err=err)
if not network then return,''

ymd=time2fid(udate,/full)
cat_file='INDEX_'+ymd+ext
cat_dir=time2fid(udate,/full,delim='/')
url_path=server+path+'/'+cat_dir
url_file=url_path+'/'+cat_file

if keyword_set(verbose) then message,'Checking '+url_file,/cont
self->readfits,url_file,data,extension=1,err=err

if is_struct(data) then begin
 count=n_elements(data.fname)
 path=replicate({path:url_path},count)
 data=join_struct(data,path)
endif else data=''

return,data & end

;-----------------------------------------------------------------------------
;-- get NGDC catalog listing for input time range

function sxi::list_index,tstart,tend,count=count,verbose=verbose,_extra=extra

count=0
verbose=keyword_set(verbose)
nearest=valid_time(tstart) and (not valid_time(tend))

d1=get_def_times(tstart,tend,dend=d2,/utc,_extra=extra)

if verbose then begin
 message,'Searching between '+anytim2utc(d1,/vms)+' and '+$
          anytim2utc(d2,/vms),/cont
endif

ndays=d2.mjd-d1.mjd+1
if (d2.time eq 0) then ndays=(ndays-1) > 1
if nearest then ndays=1
dprint,'% SXI::LIST_INDEX: reading '+trim(ndays)+' days..'
td=d1
for i=0,ndays-1 do begin
 dcat=-1
 td.mjd=d1.mjd+i
; if verbose then message,'Reading NGDC catalog for '+anytim2utc(td,/vms,/date),/cont
 dcat=self->read_index(td,_extra=extra,verbose=verbose)
 if is_struct(dcat) then tcat=merge_struct(tcat,dcat)
endfor

count=n_elements(tcat)

if (count gt 1) then begin
 tdate=tcat.date_obs
 if nearest then begin
  ts=anytim2utc(d1,/ccsds,/trunc)
  ts=strep(ts,'T',' ')
  chk1=where( (tdate ge ts), c1)
  chk2=where( (tdate le ts), c2)
  if (c1 gt 0) then begin
   tcat1=tcat[chk1[0]]
   f1=tcat1.date_obs
   d1=abs(anytim2tai(f1)-anytim2tai(ts))
  endif
  if c2 gt 0 then begin
   tcat2=tcat[chk2[c2-1]]
   f2=tcat2.date_obs
   d2=abs(anytim2tai(f2)-anytim2tai(ts))
  endif
  case 1 of 
   (c1 gt 0) and (c2 eq 0) : tcat=tcat1
   (c1 eq 0) and (c2 gt 0) : tcat=tcat2
   (c1 gt 0) and (c2 gt 0) : begin
     if d1 lt d2 then tcat=tcat1 else tcat=tcat2
    end
   else: message,'improbable case!!!!'
  endcase
  count=1
 endif else begin
  if ((d1.time gt 0) or (d2.time gt 0)) then begin
   ts=anytim2utc(d1,/ccsds,/trunc)
   ts=strep(ts,'T',' ')
   te=anytim2utc(d2,/ccsds,/trunc)
   te=strep(te,'T',' ')
   chk=where( (tdate ge ts) and $
              (tdate le te),count)
   if count gt 0 then tcat=tcat[chk]
  endif
 endelse
endif

if count eq 0 then tcat=-1
if count eq 1 then tcat=tcat[0]
if verbose then message,'Found '+trim(count)+' files',/cont

return,tcat
end

;---------------------------------------------------------------------------
;-- get SXI URL path

function sxi::url_path,file

server=self->get_server(path=path,/no_check,/full)
dfile=file_break(file)
temp=parse_time(dfile,ymd=ymd)
return,server+path+'/'+ymd+'/'+dfile+'.FTS'

end

;----------------------------------------------------------------------------
;-- get SXI image url nearest to specified time

function sxi::nearest,time,_extra=extra,err=err,back=back,filter=filter,$
              verbose=verbose

err=''
verbose=keyword_set(verbose)

if valid_time(time) then utc=anytim2utc(time) else get_utc,utc
if is_number(back) then tback=back else tback=1.
if verbose then message,'Searching for data nearest '+anytim2utc(utc,/vms),/cont

;-- start with current time and look back 

dend=utc & dend.time=0
dstart=dend & dstart.mjd=dstart.mjd-tback
tcat=self->list_index(dstart,dend,count=count,_extra=extra)
if count eq 0 then begin
 sat=strtrim(self->hget(/sat),2)
 err='No GOES'+sat+' SXI data found at or near '+anytim2utc(utc,/vms)
 message,err,/cont
 return,''
endif

;-- get matching filter

if is_struct(extra) then begin
 self->list_filter,filters
 tags=tag_names(extra)
 chk=where_vector(tags,filters,count)
 if count gt 0 then filter=filters[chk[0]]
endif

if is_string(filter) then begin
 if count eq 0 then begin
  sat=strtrim(self->hget(/sat),2)
  err='No GOES'+sat+' SXI data found matching '+filter
  message,err,/cont
  return,''
 endif
endif

;-- get nearest time

ss=near_time(tcat.date_obs,utc)
tcat=tcat[ss]
file=self->url_path(tcat.fname)

if verbose then begin
 message,'Found SXI '+tcat.wavelnth+' data at '+ tcat.date_obs,/cont
endif

return,file & end

;------------------------------------------------------------------------------
; get latest SXI image

pro sxi::latest,ofile,out_dir=out_dir,_ref_extra=extra,$
                err=err,hours=hours,back=back,no_warn=no_warn

err='' 

;-- default to current directory

if is_blank(out_dir) then odir=curdir() else odir=out_dir
if not write_dir(odir,err=err,out=out) then return
odir=out

mhours=12.
if is_number(back) then mhours=24.*back 
if is_number(hours) then mhours=hours
get_utc,dend
dend=anytim2tai(dend)
dstart=dend
dstart=dstart-mhours*3600.

;-- get most recent file or check if filter is selected

tcat=self->list_index(dstart,dend,_extra=extra,err=err)
if is_struct(tcat) then begin
 file=self->find_filter(tcat,_extra=extra,err=err)
 if is_blank(file) then return
endif else begin
 sat=strtrim(self->hget(/sat),2)
 message,'No GOES'+sat+' SXI files cataloged within last '+trim(mhours)+' hours',/cont
 message,'Generating local listing...',/cont
 file=self->find_recent(dstart,dend,_extra=extra,err=err)
 if is_blank(file) then return
endelse

ofile=concat_dir(odir,file)
self->copy,file,out_dir=odir,err=err,/no_change,/head
if is_string(err) then return
self->read,ofile,err=err,_extra=extra,no_warn=no_warn
if is_string(err) then return

return & end

;-------------------------------------------------------------------------
;-- find most recent file in NGDC archive

function sxi::find_recent,tstart,tend,err=err,_extra=extra,$
                filter=filter

err='' 

;-- which level processing?

fid='_BB'
if have_tag(extra,'level0') then fid='_AA'
if have_tag(extra,'level2') then fid='(_CC|_CB)'

;-- which filter?

if is_string(filter) then begin
 if n_elements(filter) eq 1 then $
  sfilters=strup(str2arr(filter,delim=',')) else sfilters=strup(filter)
endif else if is_struct(extra) then sfilters=tag_names(extra) 

if is_string(sfilters) then begin
 self->list_filter,filters
 chk=where_vector(sfilters,filters,fcount)
 if fcount gt 0 then sfilters=filters[chk] else sfilters=''
endif
if is_string(sfilters) then sfilter='('+arr2str(sfilters,delim='|')+')' else $
 sfilter='^(P|B)'

dprint,'% SXI::FIND_RECENT: '+sfilter

;-- list NGDC files

self->hfits::find,files,tstart,tend,err=err,count=count,$
             pattern=fid,_extra=extra
if count eq 0 then return,''

;-- find most recent match

tfile=files[count-1]
for i=count-1,0,-1 do begin
 self->read,files[i],index=index,/nodata,/ngdc,err=err
 if is_string(err) then continue
 filter=index.wavelnth
 chk=stregex(filter,sfilter,/bool,/fold)
 if chk then return,files[i]
endfor

return,tfile

end

;--------------------------------------------------------------------------
;-- filter files based on wavelength and exposure time

function sxi::find_filter,cat,_extra=extra,filter=filter,$
         exptime=exptime,err=err,verbose=verbose,all=all

if not is_struct(cat) then return,''
verbose=keyword_set(verbose)
fid='_BB'
if have_tag(extra,'level0') then fid='_AA'
if have_tag(extra,'level2') then fid='(_CC|_CB)'

err=''
file=''
ext='.FTS'

;-- check for which level processing

index=cat
if (1-keyword_set(all)) then begin
 is_levelx=where(stregex(cat.fname,fid,/bool),ncount)
 if ncount gt 0 then index=cat[is_levelx]
endif

;--- determine desired exposure

if is_string(exptime) then begin
 chk=where_val(index.exptime,exptime,count)
 if count eq 0 then begin
  sat=strtrim(self->hget(/sat),2)
  err='No GOES'+sat+' images with matching exposure '+exptime+' found.'
  message,err,/cont
  return,''
 endif else index=index[chk]
endif

;-- check for missing extensions

count=n_elements(index)
files=index.fname
chk=where(stregex(files,ext,/bool,/fold),ncount,$
          complement=complement,ncomplement=ncomplement)
if ncomplement gt 0 then files[complement]=files[complement]+ext

exptimes=index.exptime
def_filter=index[count-1].wavelnth
def_exp=index[count-1].exptime
file=files[count-1]

;-- determine desired filter [def = P_THN_B]

if is_string(filter) then begin
 if n_elements(filter) eq 1 then $
  sfilters=strup(str2arr(filter,delim=',')) else sfilters=strup(filter)
endif else if is_struct(extra) then sfilters=tag_names(extra) 
if is_blank(sfilters) then sfilters='P_THN_B'

for i=0,n_elements(sfilters)-1 do begin
 chk=where(index.wavelnth eq sfilters[i],count)
 if count gt 0 then begin
  file=files[chk[count-1]]
  exptime=exptimes[chk[count-1]]
  if verbose then message,'Found match for '+sfilters[i]+' with '+trim(exptime)+' sec exposure.',/cont
  return,files[chk[count-1]] 
 endif
endfor
sat=strtrim(self->hget(/sat),2)
message,'No GOES'+sat+' images matching '+arr2str(sfilters)+' found.',/cont
message,'Defaulting to '+def_filter+' with '+trim(def_exp)+' sec exposure.',/cont,/noname

return,file & end

;----------------------------------------------------------------------------

pro sxi::filters

self->list_filter,/print

return & end 

;----------------------------------------------------------------------------
;-- list SXI filters

pro sxi::list_filter,filters,print=print

filters=['P_THN_A','P_THN_B','P_MED_B','B_THN_B','B_MED','B_THK','RDSH','OPEN',$
         'B_THN_A','P_THK','P_MED_A']

if keyword_set(print) then begin
 print,'Available SXI filters: '
 print,'-----------------------'
 print,transpose(filters)
endif

return & end

;------------------------------------------------------------------------------
;-- SXI data structure (inherits from HFITS class)

pro sxi__define,void                 

void={sxi, installed:0b, ngdc:0b, sat:0, inherits hfits}

return & end



