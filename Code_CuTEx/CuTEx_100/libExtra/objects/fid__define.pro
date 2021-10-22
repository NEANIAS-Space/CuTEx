;+
; Project     : HESSI
;
; Name        : FID__DEFINE
;
; Purpose     : Define a FID (file id) object
;
; Category    : objects
;
; History     : Written 3 April 2001, D. Zarro, EIT/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function fid::init

return, 1

end

;--------------------------------------------------------------------------

pro fid::cleanup

return & end

;--------------------------------------------------------------------------
;-- parse file time from file name: xxxxyyyymmdd_hhmmss

function fid::parse_time,file,delim=delim,err=err,names=names

err='' & names=''

if size(file,/tname) ne 'STRING' then begin
 err='invalid file input'
 message,err,/cont
 return,''
endif

file=strtrim(file,2)
dyear='([0-9]{0,2}[0-9]{2})'
d2='([0-9]{2})'
dsec='([0-9]{0,2})'
ext='(\.(.+))?'

if datatype(delim) eq 'STR' then tlim=trim(delim) else tlim='_'
regex='(.+)'+dyear+d2+d2+tlim+d2+d2+dsec+ext

s=strtrim(stregex(file,regex,/sub,/extr),2)             
np=n_elements(file)

year=reform(s[2,*],np)
mon=reform(s[3,*],np)
day=reform(s[4,*],np)
hrs=reform(s[5,*],np)
mins=reform(s[6,*],np)
secs=reform(s[7,*],np)

times={year:year,mon:mon,day:day,hrs:hrs,mins:mins,secs:secs}
names=reform(s[0,*],np)

return,times
end
          
;----------------------------------------------------------------------------
;-- extract time from parsed time structure

function fid::extract_time,input,no_sec=no_sec,err=err,delim=delim,$
              date_only=date_only,no_min=no_min,count=count,ymd=ymd

err=''
count=0

if size(input,/tname) ne 'STRUCT' then return,''

good=where( (input.year ne '') and (input.mon ne '') and (input.day ne ''), count)
if count eq 0 then begin
 err='no valid times found'
 return,''
endif

year=input.year[good]
day=input.day[good]
mon=input.mon[good]
hrs=input.hrs[good]
mins=input.mins[good]
secs=input.secs[good]

;-- Y2K pivot correction

self->y2kfix,year

ymd=strmid(year,2,2)+mon+day
                                
time=day+'-'+get_month(fix(mon)-1,/trun)+'-'+year

if (1-keyword_set(date_only)) then begin
 time=time+' '+hrs
 if (1-keyword_set(no_min)) then begin
  blank=where(mins eq '',bcount)
  if bcount gt 0 then mins[blank]='00'
  time=time+':'+mins
  if (1-keyword_set(no_sec)) then begin
   blank=where(secs eq '',bcount)
   if bcount gt 0 then secs[blank]='00'
   time=time+':'+secs
  endif
 endif
endif

time=strtrim(temporary(time),2)

count=n_elements(time)
ocount=n_elements(input.year)
ptime=strarr(ocount)
pymd=ptime
ptime[good]=time
pymd[good]=ymd
time=ptime
ymd=pymd

if ocount eq 1 then begin
 time=time[0]
 ymd=ymd[0]
endif

return,time

end 

;---------------------------------------------------------------------------------------------
;-- Y2K fix

pro fid::y2kfix,year

if min(is_number(year)) eq 0 then return
yfix=fix(year)
missing=where( yfix lt 50,bcount)
if bcount gt 0 then year[missing]='20'+year[missing] 
missing=where( (yfix ge 50) and (yfix le 99),bcount)
if bcount gt 0 then year[missing]='19'+year[missing]

return & end

;----------------------------------------------------------------------------
;-- return TAI times from filenames

function fid::file2time,files,count=count,_extra=extra,ymd=ymd,err=err,$
                        names=names,ss=ss

count=0 & ss=-1

;-- parse out file times from file names 

tstruct=self->parse_time(files,err=err,names=names,_extra=extra)
if err ne '' then return,-1
ftimes=self->extract_time(tstruct,_extra=extra,err=err,count=count,ymd=ymd)
if err ne '' then return,-1.d

ss=where(ftimes ne '',count)
if count eq 1 then ss=ss[0]

times=anytim2tai(ftimes[ss],err=err)

if err ne '' then begin
 ss=-1 & count=0
 names='' & ymd=''
 return,-1.d
endif

names=names[ss]
ymd=ymd[ss]

diff=n_elements(files)-count
if (diff ne 0) then dprint,'% FID::FILE2TIME - eliminated ',trim(diff),' invalid names'


return,times
                       
end

;----------------------------------------------------------------------------

pro fid__define

temp =  {fid,fid__define_placeholder:1}
   
end
                                                                           
