;+
; Project     : SOHO-CDS
;
; Name        : FTP_MLSO_IMAGES
;
; Purpose     : FTP MLSO MK3 coronameter GIF images
;
; Category    : planning
;
; Explanation : 
;
; Syntax      : ftp_mlso,date,files=files
;
; Examples    :
;
; Inputs      : DATE = date to retrieve
;
; Opt. Inputs : EDATE = end date to retieve
;
; Outputs     : None
;
; Opt. Outputs: None
;                            
; Keywords    : FILES = found and renamed filenames
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
; History     : Written 18 Dec 1998 D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ftp_mlso,date,edate,files=files,count=count,err=err,quiet=quiet,$
                      out_dir=out_dir,back=back,month_all=month_all,$
                      _extra=extra

@ftp_header

server='ftp.hao.ucar.edu'
port=122
raw_get_dir='/mk3/raw_daily_image'

;-- construct filenames to copy

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']

for i=0,back do begin
 tdate=hdate
 tdate.mjd=tdate.mjd-i
 hcode=date_code(tdate)
 year=strmid(hcode,0,4)
 month=strmid(hcode,4,2)
 yy=strmid(hcode,2,2)
 doy=utc2doy(tdate) 
 get_dir='/mk3/daily_images/'+year+'/'+month+'_'+months(fix(month)-1)
 if whole then dext='*' else dext='d'+num2str(doy,length=3,padchar='0')
 get_files=yy+dext+'.mk3.gif'

 dprint,'% get_dir, get_files: ',get_dir,'/',get_files

 smart_ftp,server,get_files,get_dir,files=files,count=count,port=port,$
          quiet=quiet,err=err,out_dir='/tmp'
 
 if err eq '' then begin
  if count eq 0 then begin
   if loud then begin
    message,'No files found for '+anytim2utc(tdate,/date,/vms),/cont
    message,'Checking raw daily directory...',/cont
   endif
   get_dir=raw_get_dir
   smart_ftp,server,get_files,get_dir,files=files,count=count,port=port,$
          quiet=quiet,err=err,out_dir='/tmp'
  endif
 endif
 if count gt 0 then nfiles=append_arr(nfiles,files)
endfor

;-- rename files 

count=n_elements(nfiles)
if count gt 0 then begin 
 files=nfiles
 for k=0,count-1 do begin
  break_file,nfiles(k),dsk,dir,name,ext
  yy=strmid(name,0,2)
  if (fix(yy) gt 50) and (fix(yy) le 99) then yy='19'+yy else yy='20'+yy 
  doy=fix(strmid(name,3,3)) & year=fix(yy)
  utc=doy2utc(doy,year) & hcode=date_code(utc)
  new_file=concat_dir(out_dir,'mlso_cogmk_fd_'+hcode+'_1900')+'.gif'
  espawn,'mv -f '+nfiles(k)+' '+new_file
  files(k)=new_file 
 endfor
endif

if loud then begin
 if (count gt 0) then begin
  message,'got following files:',/cont
  print,files
 endif else message,'no files found',/cont
endif

return & end

