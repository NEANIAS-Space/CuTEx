;+
; Project     : HESSI
;
; Name        : SOCK_SIZE
;
; Purpose     : get sizes of remote files in bytes
;
; Category    : utility system sockets
;
; Syntax      : IDL> rsize=sock_size(rfile)
;                   
; Example     : IDL> rsize=sock_size('http://server.domain/filename')
;
; Inputs      : RFILE = remote file names
;
; Outputs     : RSIZE = remote file sizes
;
; Keywords    : ERR = string error
;
; History     : 1-Feb-2007,  D.M. Zarro (ADNET/GSFC) - Written
;               3-Feb-2007, Zarro (ADNET/GSFC) - added FTP support
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function sock_size,rfile,err=err,_extra=extra

;-- usual error check

err=''
if is_blank(rfile) then begin
 err='Missing input filenames'
 return,0.
endif

nfiles=n_elements(rfile)
rsize=fltarr(nfiles)

for i=0,nfiles-1 do begin
 r=url_parse(rfile[i])
 ftp=stregex(r.scheme,'ftp',/bool)

;-- use read_ftp for FTP

 if ftp then begin
  read_ftp,r.host,r.path,rsize=bsize
  rsize[i]=bsize
 endif else begin
 
;-- use head method for HTTP

  if (1-exist(http)) then http=obj_new('http')
  http->head,rfile[i],size=bsize,err=err
  if is_blank(err) then rsize[i]=bsize
 endelse
endfor

if nfiles eq 1 then rsize=rsize[0]
 
obj_destroy,http

return,rsize
end


