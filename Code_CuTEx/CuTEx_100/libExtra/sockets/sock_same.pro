;+
; Project     : HESSI
;
; Name        : SOCK_SAME
;
; Purpose     : Check if local and remote copies of files
;               are the same (in size)
;
; Category    : utility system sockets
;
; Syntax      : IDL> same=sock_same(rfile,lfile,lsize=lsize, rsize=rsize)
;                   
; Example     : IDL> same=sock_same('http://server.domain/filename','filename')
;
; Inputs      : RFILE, LFILE = remote and local file names
;
; Outputs     : SAME = 1/0 if same or different
;
; Keywords    : LSIZE, RSIZE = local and remote sizes of files
;
; History     : 6-Mar-2006,  D.M. Zarro (L-3Com/GSFC) - Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function sock_same,rfile,lfile,lsize=lsize,rsize=rsize,err=err,_extra=extra

;-- usual error checks

err=''
if is_blank(lfile) or is_blank(rfile) then begin
 err='Missing input filenames'
 return,0b
endif

if n_elements(lfile) ne n_elements(rfile) then begin
 err='# of local and remote filenames do not match'
 return,0b
endif

;-- create a http object and loop for each file
;   to check local size against remote file by
;   using HEAD requests

nfiles=n_elements(lfile)
http=obj_new('http')
lsize=-1+fltarr(nfiles) & rsize=lsize
same=bytarr(nfiles)

for i=0,nfiles-1 do begin
 lsize[i]=file_size(lfile[i])
 if lsize[i] lt 0 then continue
 http->head,rfile[i],size=bsize,err=err
 if is_blank(err) then rsize[i]=bsize
 same[i]=lsize[i] eq rsize[i]
endfor

if nfiles eq 1 then begin
 same=same[0] & lsize=lsize[0] & rsize=rsize[0]
endif
 
obj_destroy,http

return,same
end


