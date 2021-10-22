;+
; Project     : HESSI
;                  
; Name        : SOCK_OPEN
;               
; Purpose     : Open a socket with some error checking
;                             
; Category    : system utility sockets
;               
; Syntax      : IDL> sock_open,host,port,err=err
;
; Inputs      : HOST = address of host
;
; Outputs     : LUN = unit number of open socket
;
; Keywords    : ERR = error string (if any)
;               PORT = host port [def=80]
;                   
; History     : 14 April 2002, Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-    

pro sock_open,host,lun,port=port,err=err,_extra=extra,$
              connect_timeout=connect_timeout,verbose=verbose

err=''
if not allow_sockets(err=err) then return

if is_blank(host) then return
if not is_number(port) then port=80
if not is_number(connect_timeout) then connect_timeout=1

if is_number(lun) then begin
 if lun gt 0 then begin
  if (fstat(lun)).open then close_lun,lun
 endif
endif 

delvarx,lun
error=1
on_ioerror,done
socket,lun,host,port,connect_timeout=connect_timeout,error=error,/get_lun,$
           _extra=extra
done: on_ioerror,null
 
if error ne 0 then begin
 close_lun,lun
 err='Could not open network connection to '+host
 if keyword_set(verbose) then begin
  message,err,/cont
  xack,err,/info
 endif
endif

return & end
