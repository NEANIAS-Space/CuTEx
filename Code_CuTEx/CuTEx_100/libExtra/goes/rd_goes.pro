;+
; Project     : HESSI
;
; Name        : RD_GOES
;
; Purpose     : read GOES data
;
; Category    : synoptic gbo
;
; Syntax      : IDL> rd_goes,times,data,trange=trange
;
; Inputs      : None
;
; Outputs     : TIMES = time array (SECS79)
;               DATA  = data array (# TIMES x 2)
;
; Keywords    : TRANGE=[TSTART, TEND] = time interval to select
;               TAI = TIMES in TAI format
;               NO_CYCLE = don't search each satellite
;               SAT = satellite number to search
;                     (updated if NO_CYCLE=0)
;
; History     : Written 18 Feb 2001, D. Zarro, EITI/GSFC
;               14-Dec-2005 - changed err message text
;               Modified 5 May 2007, Zarro (ADNET/GSFC)
;                - changed /NO_SEARCH to /NO_CYCLE
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_goes,times,data,err=err,trange=trange,count=count,tai=tai,$
            _extra=extra,status=status,verbose=verbose,gdata=gdata,$
             type=type,no_cycle=no_cycle,sat=sat,gsat=gsat


;-- usual error checks

verbose=keyword_set(verbose)
no_cycle=keyword_set(no_cycle)
cycle=1-no_cycle

err=''
count=0
delvarx,times,data
gsat=''
type=''
status=0
res='3 sec'
sat_names=goes_sat()
sat_nums=goes_sat(/num)

time_input=0
if exist(trange) then begin
 if n_elements(trange) eq 2 then begin
  err1=''
  t1=anytim2utc(trange(0),/vms,err=err1)
  err2=''
  t2=anytim2utc(trange(1),/vms,err=err2)
  time_input=(err1 eq '') and (err2 eq '')
 endif
 if not time_input then begin
  err='invalid TRANGE input'
  message,err,/cont
  return
 endif
endif else t1=get_def_times(dend=t2,/vms,/round)

;-- GOES satellite can be entered as a number (e.g. 12) or as a keyword
;   (e.g. /GOES12)

chk=have_tag(extra,'goe',index,/start,tag=tag)
sat_name=''
if is_number(sat) then begin
 sat=sat[0]
 sat_name='GOES'+trim(sat)
endif else begin
 if chk then sat_name=tag[0]
endelse

;-- validate GOES satellite [def to GOES12 if not user-specified]

if is_blank(sat_name) then sat_name='GOES12'
ok=where(sat_name eq sat_names,count)
if (count eq 0) then begin
 if no_cycle then begin
  err=sat_name+' not an operating satellite'
  message,err,/cont
  return
 endif else begin
  sat=12 & sat_name='GOES12'
 endelse
endif else sat=sat_nums[ok[0]]

;-- try user-entered GOES first, or cycle until data is found

search_sats=sat_name
if cycle then begin
 others=where(sat_name ne sat_names,ocount)
 if ocount gt 0 then search_sats=[search_sats,sat_names[others]]
endif 

if is_struct(extra) then extra=rem_tag(extra,index)
if have_tag(extra,'fiv',/start) then res='5 min'
if have_tag(extra,'one',/start) then res='1 min'

for i=0,n_elements(search_sats)-1 do begin
 nextra=add_tag(extra,1,search_sats[i])
 if verbose then message,'Trying '+search_sats[i]+'...',/cont

 rd_gxd,t1,t2,gdata,_extra=nextra,status=status,verbose=verbose
 type=search_sats[i]+' '+res

 if is_struct(gdata) then begin
  if verbose then message,'Found '+type+' data',/cont

;-- unpack the data

  if  n_params() eq 2 then begin
   if keyword_set(tai) then times=anytim(gdata,/tai) else times=anytim(gdata)
   data=[[gdata.lo],[gdata.hi]]
  endif

  count=n_elements(gdata)
  gsat=search_sats[i]
  chk=where(gsat eq sat_names,gcount)
  if gcount gt 0 then sat=sat_nums[chk[0]]
  return
 endif
endfor

err='No '+sat_name+' data available for specified times.'
message,err,/cont
type=''
delvarx,gdata

return
end
