;+
; Project     : HESSI
;
; Name        : FTP_MESOLA
;
; Purpose     : FTP data from MESOLA site
;
; Category    : GBO ancillary
;
; Syntax      : ftp_mesol,tstart,files=files
;
; Inputs      : TSTART = start date to retrieve [def = current day]
;
; Opt. Inputs : TEND = end date to retrieve [def = end of current day]
;
; Keywords    : FILES = files found
;               OUT_DIR = output directory for file [def = /tmp]
;               ERR = error string
;               COUNT = no of files copied
;               LIST = list only
;               QUIET = suppress messages
;               SITE = PIC, NANCAY, MEUDON
;               TYPE = HALPHA, K1V, K3
;               EXT = GIF, FITS
;
; History     : 14 Dec 1999 D. Zarro (SM&A/GSFC) - written
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ftp_mesola,tstart,tend,files=files,list=list,count=count,quiet=quiet,$
               out_dir=out_dir,site=site,type=type,ext=ext

; FTP H-alpha files from ftp://mesola.obspm.fr/pub/meudon/Halpha/
; Typical remote filenames are mh991101.083700.fits.Z


files=''
server='mesola.obspm.fr'
remote_dir='/pub/meudon/Halpha/'
verbose=1-keyword_set(quiet)

if (not is_dir(out_dir)) then out_dir=get_temp_dir()

;-- construct start/end month directories

form='(i2.2)'
dstart=get_def_times(tstart,tend,dend=dend,/ext,/round)
syear=strmid(trim(dstart.year),2,2)
sdir=str_format(syear,form)+str_format(dstart.month,form)
eyear=strmid(trim(dend.year),2,2)
edir=str_format(eyear,form)+str_format(dend.month,form)

jdate=dstart
i=0
while ((where(edir eq sdir))(0) eq -1) do begin
 i=i+1
 jdate.month=dstart.month+i
 jdate=anytim2utc(jdate,/ext)
 year=strmid(trim(jdate.year),2,2)
 dir=str_format(year,form)+str_format(jdate.month,form)
 sdir=append_arr(sdir,dir)
endwhile

ndir=n_elements(sdir)

for i=0,ndir-1 do begin
 rdir=remote_dir+sdir(i)
 if verbose then message,'scanning '+rdir,/cont
 smart_ftp,server,rdir,files=sfiles,quiet=quiet,count=scount,$
   out_dir=out_dir,_extra=extra,list=list
 if scount gt 0 then ofiles=append_arr(ofiles,sfiles)
endfor

if exist(ofiles) then files=ofiles
if keyword_set(list) then return

return & end
