;+
; Project     : HESSI
;
; Name        : RD_GOES_SDAC
;
; Purpose     : read GOES SDAC FITS data (a socket wrapper around GFITS_R)
;
; Category    : synoptic gbo
;
; Syntax      : IDL> rd_goes_sdac
;
; Inputs      : See GFITS_R keywords
;
; Outputs     : See GFITS_R keywords
;
; Keywords    : See GFITS_R keywords
;               STIME, ETIME = start/end times to search
;               REMOTE = force a network search
;               NO_CYCLE = don't search all satellites
;
; History     : Written 15 June 2005, D. Zarro, (L-3Com/GSFC)
;               Modified 24 Nov 2005, Zarro (L-3Com/GSFC) 
;                - preserve currently defined $GOES_FITS
;               Modified 26 Dec 2005, Zarro (L-3Com/GSFC) 
;                - support varied input time formats with anytim
;               Modified 30 Dec 2005, Zarro (L-3Com/GSFC)
;                - improved by only downloading required files
;               Modified Election Night 7 Nov 2006, Zarro (ADNET/GSFC) 
;                - check that $GOES_FITS is a valid archive 
;               Modified 22 Jan 2007, Zarro (ADNET/GSFC)
;                - corrected returned satellite number
;               Modified 5 May 2007, Zarro (ADNET/GSFC)
;                - added /NO_CYCLE 
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_goes_sdac,stime=stime,etime=etime,_ref_extra=extra,remote=remote,$
                 sat=sat,no_cycle=no_cycle

;-- if $GOES_FITS defined, then read files locally

if (not valid_time(stime)) or (not valid_time(etime)) then begin
 pr_syntax,'rd_goes_sdac,stime=stime,etime=time,tarray=time,yarray=data'
 return
endif

tstart=anytim(stime,/ecs) & tend=anytim(etime,/ecs)

if is_number(remote) then remote =  (0b >  byte(remote) < 1b) else remote=0

sats=goes_sat(/number)
no_cycle=keyword_set(no_cycle)
cycle=1-no_cycle

;-- start with user-specified satellite

if is_number(sat) then begin
 sat=sat[0]
 chk=where(sat eq sats,count)
 if count eq 0 then begin
  if no_cycle then begin
   err='GOES'+trim(sat)+' is not an operating satellite'
   message,err,/cont
   return
  endif 
 endif
endif else sat=12

;-- if not forcing a remote, check if GOES_FITS has an index file. If it does, then
;   we should search it since it is a valid GOES archive. If not, move on 

if not remote then begin
 index=concat_dir('$GOES_FITS','index.dat')
 chk=loc_file(index,count=count)
 if (count eq 1) then begin
  gfits_r,stime=tstart,etime=tend,sat=sat,_extra=extra,/sdac,no_cycle=no_cycle
  return
 endif
endif

;-- determine remote location of files

server=goes_server(network=network,_extra=extra,/sdac,path=path,/full)

;-- Use CATCH if we have problems. 

goes_fits_sav=chklog('$GOES_FITS')
error=0
catch,error
if error ne 0 then begin
 message,err_state(),/cont
 catch,/cancel
 mklog,'GOES_FITS',goes_fits_sav
 return
endif

;-- if currently defined GOES_FITS is writeable, we use it. 
;   Otherwise, create a temporary GOES_FITS directory.

use_temp=0b
chk=write_dir('$GOES_FITS',out=goes_dir)
if (1-chk) then begin
 use_temp=1b
 goes_dir=goes_temp_dir()
 if not is_dir(goes_dir) then mk_dir,goes_dir
 mklog,'GOES_FITS',goes_dir
endif

;-- cycle thru each available GOES satellite until we get a match
;   unless /no_cycle set

search_sats=sat
if cycle then begin
 chk=where(sat ne sats,count)
 if count gt 0 then search_sats=[sat,sats[chk]]
endif

goes_url=server+path
http=obj_new('http')
dsat=-1
for i=0,n_elements(search_sats)-1 do begin

 tsat=search_sats[i]

;-- determine which file names to copy

 files=goes_fits_files(tstart,tend,_extra=extra,sat=tsat,/no_comp)
 if is_blank(files) then continue

;-- check if they exist at the server, and download

 goes_files=goes_url+'/'+files
 found_sat=0b

;-- if server is down, check last downloaded files
 
 if network then begin
  for k=0,n_elements(files)-1 do begin
   http->copy,goes_files[k],out_dir=goes_dir,_extra=extra,/verb,err=err
   if is_blank(err) then found_sat=1b
  endfor
 endif else found_sat=1b

;-- if found, then read downloaded files 

 if found_sat then begin
  gfits_r,stime=tstart,etime=tend,sat=tsat,_extra=extra,error=error,/sdac,/no_cycle
 
;-- if everything is ok, then bail out otherwise try another satellite

  if (not error) then break
 endif
endfor

obj_destroy,http
mklog,'GOES_FITS',goes_fits_sav

;-- clean up old files 

if found_sat and (not error) then sat=tsat

if use_temp then begin
 old_files=file_since(older=10,patt='go*',count=count,path=goes_dir)
 if count gt 0 then file_delete,old_files,/quiet
endif


return & end
