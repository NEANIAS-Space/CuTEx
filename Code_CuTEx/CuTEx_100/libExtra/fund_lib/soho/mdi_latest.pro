;+
; Project     : HESSI
;
; Name        : MDI_LATEST
;
; Purpose     : List latest MDI files
;
; Category    : synoptic gbo
;
; Syntax      : IDL> files=mdi_files()
;
; Keywords    : BACK = # of days back to list
;             
; Restrictions: Unix only
;
; History     : Written 29 August 2006, D. Zarro (ADNET/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function mdi_latest,back=back,count=count

count=0

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return,''
endif

;-- check source location 

chk=is_dir('$SOHO_PRIVATE/data/planning/mdi',out=out)
if not chk then begin
 err='Non-existent MDI summary directory'
 message,err,/cont
 return,''
endif

;-- look for files newer than BACK days

if is_number(back) then tback=trim(string(back)) else tback='30'

exp1='find '+out+' -name \*igram\*  -mtime -'+tback
espawn,exp1,igrams,count=icount,/noshell

if icount eq 0 then begin
 err='No recent MDI intensitygrams found'
 message,err,/cont
endif

exp2='find '+out+' -name \*maglc\*  -mtime -'+tback
espawn,exp2,maglc,count=mcount,/noshell

if mcount eq 0 then begin
 err='No recent MDI magnetograms found'
 message,err,/cont
endif

if (mcount eq 0) and (icount eq 0) then return,''

if mcount gt 0 then files=maglc
if icount gt 0 then files=append_arr(files,igrams,/no_copy)

count=n_elements(files)
return,files

end
