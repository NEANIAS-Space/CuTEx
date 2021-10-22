;+
; Project     : SOHO-CDS
;
; Name        : FTP_KISF
;
; Purpose     : FTP Kiepenheuer-Institut H-alpha files for a given date
;
; Category    : planning
;
; Explanation : FTP's KIS Full-Disk H-alpha files 
;
; Syntax      : ftp_kisf,files=files,date
;
; Examples    :
;
; Inputs      : None
;
; Opt. Inputs : DATE = date to retrieve
;
; Outputs     : None
;
; Opt. Outputs: None
;
; Keywords    : FILES = found filenames
;               OUT_DIR = output directory for file [def = current]
;               ERR = error string
;               COUNT = no of files copied
;               BACK= # of days backward to look [def=0]
;               FITS = look for FITS files
;               MONTH = copy whole month
;
; Common      : None
;
; Restrictions: Unix only 
;
; Side effects: None
;
; History     : Written 8 June 1998 D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ftp_kisf,date,files=files,count=count,err=err,quiet=quiet,month_all=month_all,$
             out_dir=out_dir,back=back,fits=fits,$
                      _extra=extra

@ftp_header

;-- construct filenames to copy

server='ftp.kis.uni-freiburg.de'

if keyword_set(fits) then begin
 ext='.fts*'
 dext='_fits' 
endif else begin
 dext='_jpeg'
 ext='.jpg'
endelse


for i=0,back do begin
 tdate=hdate
 tdate.mjd=tdate.mjd-i
 hcode=date_code(tdate)
 year=strmid(hcode,0,4)
 mon=strmid(hcode,4,2)
 day=strmid(hcode,6,2)
 if keyword_set(month) then hcode=year+mon+'*'
 get_dir='/halpha/all'+dext+'/'+year+dext+'/'+mon
 get_files='kisf_halph_fd_'+hcode+'_*'+ext

 dprint,'% get_dir, get_files: ',get_dir,'/',get_files

 smart_ftp,server,get_files,get_dir,files=files,count=count,$
          quiet=quiet,err=err,out_dir=out_dir
 if count gt 0 then tfiles=append_arr(tfiles,files)
endfor

count=n_elements(tfiles)
if loud then begin
 if (count gt 0) then begin
  files=tfiles
  message,'got following files:',/cont
  print,files
 endif else message,'no files found',/cont
endif

return & end

