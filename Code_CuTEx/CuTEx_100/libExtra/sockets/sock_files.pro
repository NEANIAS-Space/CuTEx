;+
; Project     : HESSI
;
; Name        : SOCK_FILES
;
; Purpose     : List files on remote server
;
; Category    : utility sockets 
;
; Syntax      : IDL> files=sock_files(server,tstart,tend)
;
; Inputs      : SERVER = server to search
;               TSTART, TEND = start/end times to search [inclusive]
;
; Outputs     : FILES = files found, with full path
;
; Keywords    : PATH   = directory to search
;               COUNT = # of files found
;             : ERR = error string input
;
; Restrictions: Files on remote server must be organized by year/mon/day
;               subdirectories, e.g. /2002/12/10
;
; History     : Written 7 Jan 2003, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function sock_files,server,tstart,tend,count=count,path=path,err=err,type=type,$
                   _extra=extra

count=0
err=''

if is_blank(server) then begin
 pr_syntax,'files=sock_files(server,path,tstart,tend)'
 return,''
endif

if is_blank(path) then spath='' else spath=trim(path)

;-- ping server

check=have_network(server,err=err,/verbose)
if not check then return,''

;-- construct remote directory names to search

fid=get_fid(tstart,tend,/full,delim='/',dstart=dstart,dend=dend,/no_next,$
            _extra=extra)

;-- list via sockets

if is_blank(type) then stype='*.*' else stype=trim(type)

for i=0,n_elements(fid)-1 do begin
 rfiles=sock_find(server,stype,path=spath+'/'+fid[i],count=rcount,_extra=extra)
 if rcount gt 0 then begin
  if exist(files) then files=[temporary(files),temporary(rfiles)] else $
   files=temporary(rfiles)
 endif
endfor

count=n_elements(files)
if count eq 0 then return,''

if count gt 0 then begin
 fid=obj_new('fid')
 times=fid->file2time(files)
 ok=where(  (times le dend) and (times ge dstart), count)
 if count gt 0 then files=files[ok]
 obj_destroy,fid
endif

return,comdim2(files) & end


