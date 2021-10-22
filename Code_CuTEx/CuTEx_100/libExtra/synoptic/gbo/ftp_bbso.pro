;+
; Project     : SOHO-CDS
;
; Name        : FTP_BBSO
;
; Purpose     : FTP BBSO H-alpha files for a given date
;
; Category    : planning
;
; Explanation : FTP's BBSO Full-Disk H-alpha JPEG files
;               and renames them to SOHO convention
;
; Syntax      : ftp_bbso,date,files=files
;
; Examples    :
;
; Inputs      : DATE = start date to retrieve
;
; Opt. Inputs : EDATE = end date to retrieve
;
; Outputs     : None
;
; Opt. Outputs: None
;
; Keywords    : 
;               FILES = found and renamed filenames
;               OUT_DIR = output directory for file [def = current]
;               ERR = error string
;               COUNT = no of files copied
;               BACK= # of days backward to look [def=0]
;               MONTH = copy whole month
;
; Common      : None
;
; Restrictions: Unix only 
;
; Side effects: None
;
; History     : Written 14 May 1998 D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ftp_bbso,date,edate,files=files,count=count,err=err,quiet=quiet,$
                 out_dir=out_dir,back=back,month_all=month_all,$
                 _extra=extra

@ftp_header

server='ftp.bbso.njit.edu'
get_dir='/pub/fulldisk/halpha

;-- construct filenames to copy

months  = [ '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c' ]
for i=0,back do begin
 tdate=hdate
 tdate.mjd=tdate.mjd-i
 hcode=date_code(tdate)
 year=strmid(hcode,2,2)
 mm=fix(strmid(hcode,4,2))
 day=strmid(hcode,6,2)
 if whole then dd='*' else dd=day
 tfiles='hd'+months(mm-1)+dd+year+'r.jpg'
 get_files=append_arr(get_files,tfiles)
endfor

dprint,'% get_files: ',get_files

smart_ftp,server,get_files,get_dir,files=files,count=count,$
          quiet=quiet,err=err,out_dir='/tmp'

;-- check and rename what was copied

if count gt 0 then begin
 for i=0,count-1 do begin
  break_file,files(i),fdsk,fdir,fname,ext
  imonth=strmid(fname,2,1)
  chk=where(imonth eq months,ic)
  if ic gt 0 then mm=string(chk(0)+1,format='(i2.2)') else mm='??'
  dyear=strmid(fname,5,2)
  nyear='19'
  if fix(dyear) lt 50 then nyear='20'
  year=nyear+dyear
  day=strmid(fname,3,2)
  new_file=concat_dir(out_dir,'bbso_halph_fd_'+year+mm+day+'_0000')+ext
  espawn,'mv -f '+files(i)+' '+new_file
  files(i)=new_file 
 endfor
endif                         

if loud then begin
 if (count gt 0) then begin
  message,'got following files:',/cont
  print,files
 endif else message,'no files found',/cont
endif


return & end

