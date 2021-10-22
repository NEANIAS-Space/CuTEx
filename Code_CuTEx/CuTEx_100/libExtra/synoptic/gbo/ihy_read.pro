;+
; Project     : HESSI
;
; Name        : IHY_READ
;
; Purpose     : Read IHY database via a socket
;
; Category    : sockets 
;
; Syntax      : IDL> ihy_read,db
;
; Keywords    : ERR = error string
;             
; Restrictions: IDL 5.4 or greater
;
; History     : Written 26 July 2004, D. Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ihy_read,db,_ref_extra=extra

delvarx,db
url_dir='http://orpheus.nascom.nasa.gov/~zarro/ihy'
file='ihy_db.sav'
out_dir=get_temp_dir()
url=url_dir+'/'+file
sock_copy,url,out_dir=out_dir,_extra=extra
file=concat_dir(out_dir,file)
chk=loc_file(file,count=count)
if count gt 0 then restore,file,_extra=extra
db=temporary2(ihy_records)
return
end
