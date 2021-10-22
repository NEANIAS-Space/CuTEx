;+
; Project     : HESSI
;
; Name        : HFITS__DEFINE
;
; Purpose     : Define a HFITS class that reads remote FITS files via HTTP
;
; Explanation : 
;               
;               f='~zarro/synop/mdi_mag_fd_20001126_0136.fits ; file to read
;               a=obj_new('hfits')                ; create a FITS HTTP object
;               a->open,'orpheus.nascom.nasa.gov' ; open a URL socket 
;               a->hread,f,header                 ; read header
;               print,header                      ; print header
;               a->readfits,file,data             ; read into data array
;               a->close                          ; close socket
;
;               This works too:
;
; a->read,'orpheus.nascom.nasa.gov/~zarro/synop/mdi_mag_fd_20001126_0136.fits'
;
; Category    : objects sockets fits
;               
; Syntax      : IDL> a=obj_new('hfits')
;
; History     : Written 11 Oct 2001, D. Zarro (EITI/GSFC)
;               Modified 10 Oct 2005, Zarro (L-3Com/GSFC) 
;                 - added _ref_extra
;               Modified 11 Nov 2006, Zarro (ADNET/GSFC)
;                 - fixed bug parsing URL's without http prefix
;
; Contact     : dzarro@solar.stanford.edu
;-

;-- init HTTP socket

function hfits::init,_ref_extra=extra

chk=self->fits::init(_extra=extra)
if not chk then return,chk
dprint,'% HFITS::INIT

return,self->http::init(_extra=extra)

end

;--------------------------------------------------------------------------

pro hfits::cleanup

self->http::cleanup
self->fits::cleanup

return & end

;---------------------------------------------------------------------------
;--- read FITS header from remote URL

pro hfits::hread,url,output,err=err,count=count,_extra=extra

err=''
count=0 & output=''

self->request,url,type='*',err=err,_extra=extra,request=request

if err eq '' then self->read_header,output,err=err
self->close

count=n_elements(output)
if is_string(output) and n_params() ne 2 then print,output

return & end


;---------------------------------------------------------------------------
;--- read FITS data from remote URL

pro hfits::mreadfits_url,url,data,header=header,index=index,err=err,$
                    nodata=nodata,_ref_extra=extra
err=''

if is_blank(url) then begin
 err='blank URL filename entered'
 message,err,/cont
 return
endif

self->request,url,type='*',err=err,_extra=extra

data=-1
header=''

if err eq '' then begin
 if keyword_set(nodata) then self->read_header,header,err=err,_extra=extra else $
  self->read_data,data,header=header,err=err,_extra=extra
endif 

self->close

if is_string(header) then index=fitshead2struct(header)

return & end

;---------------------------------------------------------------------------
;-- read FITS header from server (called by ::hread)

pro hfits::read_header,header,err=err

err=''
header=''

status=0
self->strip
mrd_hread,self.unit,header,status

if is_blank(header) or (status ne 0) then begin
 err='No response from server'
 message,err,/cont
endif

return & end

;---------------------------------------------------------------------------
;-- read FITS data from server 

pro hfits::read_data,data,header=header,extension=extension,err=err,_extra=extra,$
           verbose=verbose

forward_function mrdfits

err=''
status=0
if not exist(extension) then extension=0
verbose=keyword_set(verbose)
if verbose then t1=systime(/seconds)
self->strip
data=mrdfits(self.unit,extension,header,status=status,_extra=extra,/fscale)
if verbose then t2=systime(/seconds)
if status ne 0 then begin
 err='Error reading file'
 message,err,/cont
 return
endif

if verbose then begin
 tdiff=anytim2tai(t2)-anytim2tai(t1)
 message,'Data read in '+trim(tdiff)+' seconds',/cont
endif

return & end

;--------------------------------------------------------------------------
;-- FITS reader

pro hfits::readfits,file,data,index=index,_ref_extra=extra,verbose=verbose

self->parse_url,file,server,hfile
index=-1

url_entered=is_string(server) and is_string(hfile)
if url_entered then begin
 self->mreadfits_url,file,data,index=index,_extra=extra,verbose=verbose
endif else begin
 self->mreadfits,file,data,index=index,_extra=extra,verbose=verbose
endelse

if is_struct(index) then index=self->index2fits(index,/no_copy,_extra=extra)

return & end

;---------------------------------------------------------------------
;-- return HTTP server & path

function hfits::get_server,server,path=path,network=network,_extra=extra

server='' & path='' & network=0b
message,'GET_SERVER method not included',/cont

return,'' & end

;----------------------------------------------------------------------------
;-- HTTP search of remote archive 

pro hfits::find,files,tstart,tend,sizes=sizes,pattern=pattern,$
              count=count,_extra=extra,back=back,err=err,url_path=url_path,$
              verbose=verbose,times=times,round_day=round_day

files='' & count=0 & sizes='' & times=-1 & err=''

verbose=keyword_set(verbose)

if valid_time(tstart) then dstart=anytim2utc(tstart) else begin
 get_utc,dstart
 dstart.time=0
endelse

if valid_time(tend) then dend=anytim2utc(tend) else begin
 dend=dstart
 dend.mjd=dend.mjd+1
endelse

;-- allow backward time search

if is_number(back) then begin
 if back gt 0 then begin
  dend=dstart
  dstart.mjd=dstart.mjd-back
 endif
endif

;-- round out search to include whole day

if keyword_set(round_day) then begin
 dstart.time=0
 dend.time=0
 dend.mjd=dend.mjd+1
 if dend.mjd le dstart.mjd then dend.mjd=dstart.mjd+1
endif
 
;-- determine server and search directories

server=self->get_server(path=path,network=network,err=err,/verb)
if not network then return

if verbose then begin
 message,'Searching '+anytim2utc(dstart,/vms)+' to '+anytim2utc(dend,/vms),/cont
endif

direc=get_fid(dstart,dend,/full,delim='/',_extra=extra)

np=n_elements(direc)
if np gt 1 then begin
 if valid_time(dend) then begin
  if dend.time eq 0 then direc=direc[0:np-2] 
 endif else direc=direc[0:np-2]
endif
np=n_elements(direc)
if np eq 1 then direc=direc[0]

url_path=server+path+'/'+direc+'/'
self->extract_href,url_path,files,sizes=sizes,times=times,_extra=extra,count=count

;-- filter out pattern

self->filter_pattern,files,pattern,sizes=sizes,times=times,count=count

;-- filter out time

self->filter_times,files,dstart,dend,sizes=sizes,times=times,count=count

if count eq 0 then message,'No files found for specified time(s)',/cont else begin
 if keyword_set(verbose) then message,'Found '+trim(count)+' files',/cont
endelse

return & end

;------------------------------------------------------------------------------
;-- filter on file time

pro hfits::filter_times,files,tstart,tend,times=times,sizes=sizes,count=count

count=n_elements(files)
if (size(files,/tname) ne 'STRING') or (count eq 0) then return

have_times=exist(times)
if have_times then have_times=valid_time(times[0]) and (n_elements(times) eq count)
if not have_times then times=parse_time(files,/tai)

vt1=valid_time(tstart)
vt2=valid_time(tend)
if (not vt1) and (not vt2) then return

if vt1 then dprint,'% DSTART ',anytim2utc(tstart,/vms)
if vt2 then dprint,'% DEND ',anytim2utc(tend,/vms)

;-- find in time range

if vt1 and vt2 then begin
 ok=where( (times ge anytim2tai(tstart)) and (times le anytim2tai(tend)),count)
 if count gt 0 then begin
  if count eq 1 then ok=ok[0]
  files=temporary(files[ok])
  if exist(sizes) then sizes=temporary(sizes[ok])
  times=temporary(times[ok])
 endif else delvarx,files,sizes,times
 return
endif

;-- find nearest to input time

if vt1 then tref=tstart else tref=tend
diff=abs(times-anytim2tai(tref)) 
chk=where(diff eq min(diff),count)
chk=chk[0]
times=times[chk]
files=files[chk]
if exist(sizes) then sizes=sizes[chk]

return & end

;-----------------------------------------------------------------------------
;-- filter on file pattern

pro hfits::filter_pattern,files,pattern,times=times,sizes=sizes,count=count

count=n_elements(files)
if is_blank(pattern) or (size(files,/tname) ne 'STRING') or (count eq 0) then return
chk=stregex(files,pattern,/bool,/fold)
ok=where(chk,count)
if count gt 0 then begin
 if count eq 1 then ok=ok[0]
 files=temporary(files[ok])
 if exist(sizes) then sizes=temporary(sizes[ok])
 if exist(times) then times=temporary(times[ok])
endif else delvarx,files,sizes,times

return & end

;----------------------------------------------------------------------------

pro hfits__define                 

struct={hfits, inherits http, inherits fits}

return & end

