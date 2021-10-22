;+
; Project     : EIS
;
; Name        : RPC_SERVER
;
; Purpose     : return address of RPC server
;
; Category    : sockets
;                   
; Inputs      : None
;
; Outputs     : SERVER = environment value associated with $RPC_SERVER
;                        [def='orpheus.nascom.nasa.gov']
;
; Keywords    : None
;
; History     : 29-Sept-2002,  D.M. Zarro (EITI/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-


function rpc_server,full=full

server=chklog('RPC_SERVER')
if server eq '' then server='orpheus.nascom.nasa.gov'
if keyword_set(full) then server='http://'+server

return,server
end
