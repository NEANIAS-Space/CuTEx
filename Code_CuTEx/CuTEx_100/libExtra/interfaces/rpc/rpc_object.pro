;+
; Project     : SOLAR_B/EIS
;
; Name        : RPC_OBJECT
;
; Purpose     : Return an RPC client object
;
; Category    : utility objects
;
; Syntax      : IDL> rpc=rpc_object(status)
;
; Inputs      : None
;
; Outputs     : RPC = RPC client object
;               STATUS = 1/0 for success/failuer
;
; History     : Written 18 Feb 2006, D. Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function rpc_object,status,_extra=extra

status=0b
o=obj_new('idl_rpc_client')
if not obj_valid(o) then return,o
status=o->rpcinit(host='orpheus.nascom.nasa.gov',_extra=extra)
if not status then $
 status=o->rpcinit(host='cdsa6.nascom.nasa.gov',pm_host='cdsm9.nascom.nasa.gov',pm_address=8080)
if not status then obj_destroy,o
return,o
end
