;+
; Project     : HESSI
;
; Name        : GOES_YOHKOH_SERVER
;
; Purpose     : return available Yohkoh GOES data server
;
; Category    : synoptic sockets
;
; Inputs      : None
;
; Outputs     : SERVER = Yohkoh GOES data server name
;
; Keywords    : FULL - If set, return full url
;               NETWORK = returns 1 if network to that server is up
;               PATH = path to data
;
; History     : Written 15-Nov-2006, Zarro (ADNET/GSFC) 
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function goes_yohkoh_server,_ref_extra=extra,full=full, path=path,network=network

primary='beauty.nascom.nasa.gov'
secondary='sohowww.nascom.nasa.gov'
path='/sdb/yohkoh/ys_dbase'

;-- primary server

server=primary
network=have_network(server,_extra=extra)

;-- if primary server is down, try secondary

if not network then begin
 server2=secondary
 message,'Trying '+server2+'...',/cont
 network=have_network(server2,_extra=extra)
 if network then server=server2
endif

return, keyword_set(full) ? 'http://'+server : server
end
