;+
; Project     : VSO
;
; Name        : VSO_SERVER
;
; Purpose     : check and return available VSO proxy server and URI
;
; Category    : vso sockets
;
; Inputs      : None
;
; Outputs     : SERVER = VSO proxy server name
;
; Optional
;  Outputs    : URI = URI name
;
; Keywords    : NETWORK = 1 if network is up
;               NO_CHECK = return server and URI names without checking network status,
;
; History     : 1-Dec-2005,  D.M. Zarro (L-3Com/GSFC), Written
;               22-Dec-2005, J.Hourcle  (L-3Com/GSC).  changed server; URI need not resolve
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function vso_server,uri,_ref_extra=extra,network=network,$
          no_check=no_check

    check=1b-keyword_set(no_check)
    network=0b

;-- check endpoint (soap proxy)

;    proxy='http://vso.nascom.nasa.gov/cgi/vsoi_strict'
;    proxy='http://vso1.nascom.nasa.gov/cgi/vsoi_strict'
;    proxy='http://sdac2.nascom.nasa.gov/cgi/vsoi_strict'
;    proxy='http://vso.nascom.nasa.gov/cgi-bin/vsoi_strict'
    proxy='http://vso.nascom.nasa.gov/cgi-bin/vsoi_tabdelim'
    if check then network=have_network(proxy,_extra=extra) else network=1b

;- return URI

    uri='http://virtualsolar.org/VSO/VSOi'

    return,proxy
end
