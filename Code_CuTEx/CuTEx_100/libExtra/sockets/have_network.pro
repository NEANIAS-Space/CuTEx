;+
; Project     : HESSI
;                  
; Name        : HAVE_NETWORK
;               
; Purpose     : check if network connection is available
;                             
; Category    : system utility sockets
;               
; Syntax      : IDL> a=have_network()
;
; Optional    : SERVER = server to lookup [def eq 'www.google.com']
; Inputs      :
;                                        
; Outputs     : 1/0 if yes/no
;
; Keywords    : INTERVAL = seconds between rechecking
;                          (otherwise use result of last check) 
;               RESET = set to force check without waiting INTERVAL (same as INTERVAL=0)
;                   
; History     : 8 Mar 2002, Zarro (L-3Com/GSFC)
;              22 Apr 2005, Zarro (L-3Com/GSFC) - return error message
;               when network is down.
;              1 Dec 2005, Zarro (L-3Com/GSFC) - removed http object
;               from common because of unwanted side effects.
;              13 Jan 2007, Zarro (ADNET/GSFC) - added support for
;               checking multiple servers
;
; Contact     : dzarro@solar.stanford.edu
;-    

function have_network,server,verbose=verbose,err=err,_extra=extra,$
         interval=interval,reset=reset

common have_network,servers

err=''
reset=keyword_set(reset)
verbose=keyword_set(verbose)

if reset then delvarx,servers
if is_blank(server) then test_server='www.google.com' else $
 test_server=strtrim(server,2)
if not is_number(interval) then interval=30.
now=systime(/seconds)

;-- check if this server was checked recently

count=0
if is_struct(servers) then begin
 chk=where(test_server eq servers.server,count)
 j=chk[0]
 if count eq 1 then begin
  state=servers[j].state
  time=servers[j].time

;-- return last state if last time less than interval

  if (now-time) lt interval then begin
   if not state then begin
    err='Network connection to '+test_server+' is unavailable'
    if verbose then message,err,/cont
   endif
   return,state
  endif
 endif
endif

;-- try to connect to server

state=0b
http=obj_new('http',err=err,verbose=verbose)
if is_blank(err) then begin
 http->hset,retry=0
 http->head,test_server,page,err=err
 if is_blank(err) then state=1b
 if verbose and is_string(err) then message,err,/cont
 obj_destroy,http
endif

;-- update this server

if count eq 1 then begin
 servers[j].state=state
 servers[j].time=now
endif else begin
 new_server={server:test_server,state:state,time:now}
 servers=merge_struct(servers,new_server)
endelse

return,state

end
