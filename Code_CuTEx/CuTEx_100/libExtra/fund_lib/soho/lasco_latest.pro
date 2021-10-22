;+
; Project     : HINODE/EIS
;
; Name        : LASCO_LATEST
;
; Purpose     : List latest LASCO C2 & C3 summary files
;
; Category    : synoptic gbo
;
; Syntax      : IDL> files=lasco_files()
;
; Keywords    : BACK = # of days back to list
;             
; Restrictions: Unix only
;
; History     : Written 15 Feb 2007, D. Zarro (ADNET/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function lasco_latest,back=back,count=count,c2=c2,c3=c3,times=times,_extra=extra

count=0
times=''

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return,''
endif

;-- check source location 

chk=is_dir('$SOHO_PUBLIC/data/summary/lasco',out=out)
if not chk then begin
 err='Non-existent LASCO summary directory'
 message,err,/cont
 return,''
endif

do_all=~keyword_set(c2) and ~keyword_set(c3) 

;-- look for files newer than BACK days

if is_number(back) then tback=trim(string(back)) else tback='1'

icount=0
if keyword_set(c2) or do_all then begin
 exp1='find '+out+' -name \*slas_c2\*.fts  -mtime -'+tback
 espawn,exp1,lc2,count=icount,/noshell
 if icount eq 0 then begin
  err='No recent LASCO C2 found'
  message,err,/cont
 endif
endif

mcount=0
if keyword_set(c3) or do_all then begin
 exp2='find '+out+' -name \*slas_c3\*.fts  -mtime -'+tback
 espawn,exp2,lc3,count=mcount,/noshell
 if mcount eq 0 then begin
  err='No recent LASCO C3 found'
  message,err,/cont
 endif
endif

if (mcount eq 0) and (icount eq 0) then return,''

if mcount gt 0 then files=lc3
if icount gt 0 then files=append_arr(files,lc2,/no_copy)

count=n_elements(files)
if count gt 0 then times=fid2time(files,_extra=extra)
return,files

end
