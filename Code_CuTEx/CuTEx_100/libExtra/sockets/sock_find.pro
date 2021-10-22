;+
; Project     : HESSI
;
; Name        : SOCK_FIND
;
; Purpose     : socket version of FINDFILE
;
; Category    : utility system sockets
;
; Syntax      : IDL> files=sock_find(server,file,path=path)
;                   
; Inputs      : server = remote WWW server name
;               FILE = remote file name or pattern to search 
;
; Outputs     : Matched results
;
; Keywords    : COUNT = # of matches
;               PATH = remote path to search
;               ERR   = string error message
;               NO_CACHE = do not check cached results
;
; Example     : IDL> a=sock_find('smmdac.nascom.nasa.gov','*.fts',$
;                                 path='/synop_data/bbso')
;
; History     : 27-Dec-2001,  D.M. Zarro (EITI/GSFC) - Written
;                3-Feb-2007, Zarro (ADNET/GSFC) - Modified
;                 - return full URL path
;                 - made no-cache the default
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function sock_find,server,file,path=path,count=count,err=err,$
                   cache=cache,proxy=proxy,_extra=extra

common sock_find,saved

;--- start with error checking

err=''
count=0

if is_blank(server) then begin
 err='missing remote server name'
 message,err,/cont
 return,''
endif

if is_blank(file) then begin
 err='missing remote file name'
 message,err,/cont
 return,''
endif

;-- ensure that UNIX-style delimiters are used

temp=str_replace(file,'\','/')
sname=file_break(temp,path=sdir)

if is_blank(sdir) then begin
 if is_string(path) then sdir=trim(path) else sdir='/'
endif

sdir=str_replace(sdir,'\','/')
slash=strpos(sdir,'/')
if slash ne 0 then sdir='/'+sdir
slash=strpos(sdir,'/',/reverse_search)
len=strlen(sdir)
if slash ne (len-1) then sdir=sdir+'/'

if is_blank(sname) then sname='*'

;-- check if this directory already searched

hrefs=''
check_cache=keyword_set(cache)
if exist(saved) and check_cache then begin
 chk=where(sdir eq saved.sdir,scount)
 if scount gt 0 then hrefs=*((saved.hrefs)[chk[0]])
endif

if is_blank(hrefs) then begin
 http=obj_new('http',err=err)
 if err ne '' then return,''
 http->links,server+sdir,hrefs,err=err
 if err eq '' then begin
  temp={server:server,sdir:sdir,hrefs:ptr_new(/all)}
  *(temp.hrefs)=hrefs
  if exist(saved) then saved=[temporary(saved),temp] else saved=temp
 endif
 obj_destroy,http
endif

out=''
if is_string(hrefs) then begin
 out=str_find(hrefs,sname,count=count)
 if count gt 0 then out=sdir+out
endif

if count gt 0 then return,server+out else return,out

end


