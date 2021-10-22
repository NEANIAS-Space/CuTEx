;+
; Project     : SOHO-CDS
;
; Name        : FTP_HEADER
;
; Purpose     : standard FTP header to include in FTP access routines
;
; Category    : planning
;
; History     : Written 14 Jan 1998 D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-


on_error,1
err=''
count=0
delvarx,files
if not exist(back) then back=0
whole=keyword_set(month_all)
if whole then back=0
quiet=keyword_set(quiet)
loud=1-quiet

;-- check write access

if not is_dir(out_dir) then out_dir=curdir()
if not write_dir(out_dir) then begin
 err='No write access to "'+out_dir+'"' & return
endif

;-- default to current date

cerr=''
hdate=anytim2utc(date,err=cerr)
if cerr ne '' then get_utc,hdate

;-- check if end date was entered

derr=''
edate=anytim2utc(edate,err=derr)
if derr eq '' then begin
 back=edate.mjd-hdate.mjd+back
 hdate=edate
endif

if loud then begin
 message,'retrieving images for '+anytim2utc(hdate,/vms,/date),/cont
 edate=hdate
 edate.mjd=hdate.mjd-back
 print,' -> '+anytim2utc(edate,/vms,/date)
endif


