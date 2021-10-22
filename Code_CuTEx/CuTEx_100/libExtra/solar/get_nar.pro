;+
; Project     : SOHO - CDS
;
; Name        : GET_NAR
;
; Purpose     : Wrapper around RD_NAR
;
; Category    : planning
;
; Explanation : Get NOAA AR pointing from $DIR_GEN_NAR files
;
; Syntax      : IDL>nar=get_nar(tstart)
;
; Inputs      : TSTART = start time 
;
; Opt. Inputs : TEND = end time
;
; Outputs     : NAR = structure array with NOAA info
;
; Opt. Outputs: None
;
; Keywords    : COUNT = # or entries found
;               ERR = error messages
;               QUIET = turn off messages
;               NO_HELIO = don't do heliographic conversion
;               LIMIT=limiting no of days in time range
;               UNIQUE = return unique NOAA names
;
; History     : 20-Jun-1998, Zarro (EITI/GSFC) - written
;               20-Nov-2001, Zarro - added extra checks for DB's
;               24-Nov-2004, Zarro - fixed sort problem
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function get_nar,tstart,tend,count=count,err=err,quiet=quiet,$
                 no_helio=no_helio,nearest=nearest,limit=limit,unique=unique

err=''
delvarx,nar
count=0

;-- start with error checks

if not have_proc('rd_nar') then begin
 sxt_dir='$SSW/yohkoh/gen/idl'
 if is_dir(sxt_dir,out=sdir) then add_path,sdir,/append,/expand
 if not have_proc('rd_nar') then begin
  err='cannot find RD_NAR in IDL !path'
  message,err,/cont
  return,''
 endif
endif

;-- check if NOAA active region files are loaded

if chklog('DIR_GEN_NAR') eq '' then begin
 sdb=chklog('SSWDB')
 if sdb ne '' then begin
  dir_gen_nar=concat_dir(sdb,'yohkoh/ys_dbase/nar')
  if is_dir(dir_gen_nar) then mklog,'DIR_GEN_NAR',dir_gen_nar
 endif
 if chklog('DIR_GEN_NAR') eq '' then begin
  err='cannot locate NOAA files in $DIR_GEN_NAR'
  message,err,/cont
  return,''
 endif
endif

err=''
t1=anytim2utc(tstart,err=err)
if err ne '' then get_utc,t1
t1.time=0

t2=anytim2utc(tend,err=err)
if err ne '' then begin
 t2=t1
 t2.mjd=t2.mjd+1
endif

t2.time=0
err=''

loud=1-keyword_set(quiet)
if (t2.mjd lt t1.mjd) then begin
 err='Start time must be before End time'
 if loud then message,err,/cont
 return,''
endif

if is_number(limit) then begin
 if (abs(t2.mjd-t1.mjd) gt limit) then begin
  err='Time range exceeds current limit of '+num2str(limit)+' days'
  if loud then message,err,/cont
  return,''
 endif
endif

;-- call RD_NAR

if loud then begin
 message,'retrieving NAR data for '+ anytim2utc(t1,/vms),/cont
endif

rd_nar,anytim2utc(t1,/vms),anytim2utc(t2,/vms),nar,nearest=nearest

if datatype(nar) ne 'STC' then begin
 err='NOAA data not found for specified times'
 return,''
endif
                  
;-- determine unique AR pointings

count=n_elements(nar)

if (1-keyword_set(no_helio)) then begin
 if keyword_set(unique) then begin
  sorder = uniq([nar.noaa], sort([nar.noaa]))
  nar=nar[sorder]
 endif
 count=n_elements(nar)
 for i=0,count-1 do begin
  temp=nar(i)
  helio=temp.location
  xy=hel2arcmin(helio(1),helio(0),soho=0,date=anytim(temp,/utc_int))*60.
  temp=add_tag(temp,xy(0),'x')
  temp=add_tag(temp,xy(1),'y',index='x')
  new_nar=merge_struct(new_nar,temp) 
 endfor
 return,new_nar
endif else return,nar

end


