;+
; Project     : SOHO-CDS
;
; Name        : FTP_MWSO
;
; Purpose     : FTP Mt. Wilson Solar Tower images for a given date
;
; Category    : planning
;
; Explanation : spawns anonymous FTP
;
; Syntax      : ftp_mwso,date,files=files
;
; Inputs      : DATE = start date to retrieve
;
; Opt. Inputs : EDATE = end date to retrieve
;
; Keywords    : 
;               FILES = found and renamed filenames
;               OUT_DIR = output directory for files [def = current]
;               ERR = error string
;               COUNT = no of files copied
;               BACK= # of days backward to look [def=0]
;               MONTH = copy whole month
;               FITS = copy FITS files instead
;
; History     : Written 7 June 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ftp_mwso,date,edate,files=files,count=count,err=err,quiet=quiet,$
                      out_dir=out_dir,back=back,month_all=month_all,fits=fits

@ftp_header

server='howard.astro.ucla.edu'
if keyword_set(fits) then ext='fits' else ext='gif'
get_dir='/pub/obs/'+ext

;-- construct filenames to copy

for i=0,back do begin
 tdate=hdate
 tdate.mjd=tdate.mjd-i
 hcode=date_code(tdate)
 year=strmid(hcode,2,2)
 mm=strmid(hcode,4,2)
 day=strmid(hcode,6,2)
 if whole then dd='*' else dd=day
 tfiles='*'+year+mm+day+'*.'+ext
 get_files=append_arr(get_files,tfiles)
endfor

dprint,'% get_files: ',get_files

smart_ftp,server,get_files,get_dir,files=files,count=count,$
          quiet=quiet,err=err,out_dir='/tmp'


;-- rename files

types=['d','i','m']
full=['doppl','igram','magmp']

if count gt 0 then begin
 for i=0,count-1 do begin
  break_file,files(i),fdsk,fdir,fname,ext
  fname=trim(fname)
  stc=strmid(fname,0,1)
  chk=where(stc eq types,sc)
  if sc gt 0 then type=full(chk(0)) else type=full(1)
  iyear=strmid(fname,1,2)
  if is_number(iyear) then begin
   if (fix(iyear) ge 0) and (fix(iyear) le 94) then iyear='20'+iyear else $
    iyear='19'+iyear
   imon=strmid(fname,3,2)
   iday=strmid(fname,5,2)
   hr=strmid(fname,8,2)
   min=trim(round(.6*float(strmid(fname,10,2))))
   min=str_format(min,'(i2.2)')
   rest=strmid(fname,14,2)                         
   new_file=concat_dir(out_dir,'mwso_'+type+'_'+rest+'_'+iyear+imon+iday+'_'+hr+min)+ext
   espawn,'mv -f '+files(i)+' '+new_file
   files(i)=new_file 
  endif
 endfor
endif                         

if loud then begin
 if (count gt 0) then begin
  message,'got following files:',/cont
  print,files
 endif else message,'no files found',/cont
endif

return & end

