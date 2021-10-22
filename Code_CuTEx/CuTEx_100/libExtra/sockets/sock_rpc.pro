;+
; Project     : EIS
;
; Name        : SOCK_RPC
;
; Purpose     : Send RPC command via socket
;
; Category    : utility sockets
;
; Syntax      : IDL> sock_rpc,command,output
;                   
; Inputs      : COMMAND = RPC input command
;
; Outputs     : OUTPUT = RPC output results
;
; Keywords    : ERR = error string
;
; History     : 1-Oct-2002,  D.M. Zarro (LAC/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro sock_rpc,command,output,err=err

output=''
if is_blank(command) then begin
 pr_syntax,'sock_rpc,command,output'
 return
endif

rhost=rpc_server(/full)
if not have_network(rhost,err=err) then begin
 message,err,/cont
 return
endif

rpc_cmd=rhost+"/cgi-bin/rpc?"
query=rpc_cmd+command

;-- send query

sock_list,query,output,err=err,/cgi

return

end

