;+
; Project     : HESSI
;
; Name        : GOES_SDAC_SERVER
;
; Purpose     : return available SDAC GOES FITS data server
;
; Category    : synoptic sockets
;
; Inputs      : None
;
; Outputs     : SERVER = SDAC GOES data server name
;
; Keywords    : FULL - If set, return full url
;               NETWORK = returns 1 if network to that server is up
;               PATH = path to data
;
; History     : Written  29-Dec-2001, Zarro (EITI/GSFC)
;               Modified 1-Jun-2005, Zarro (L-3Com/GSFC)
;               - added sdac keyword, and set server to hesperia for sdac.
;               - added full keyword to return full http://name.
;               - don't check sohowww alternate server if sdac is
;                 selected.
;               Modified 15-Nov-2006, Zarro (ADNET/GSFC) 
;               - added UMBRA as primary SDAC server
;               - added PATH return keyword
;               - removed SDAC keyword
;               Modified 11-July-2007, Zarro (ADNET)
;               - added check for index.dat cookie file
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function goes_sdac_server,_ref_extra=extra,full=full, path=path,network=network

primary='umbra.nascom.nasa.gov'
secondary='hesperia.gsfc.nasa.gov'
primary_path='/goes/fits'
secondary_path='/goes'

;-- try primary server

server=primary
path=primary_path
network=have_network(server,_extra=extra)

;-- check for index file

index_size=0l
if network then index_size=sock_size(server+path+'/index.dat')

;-- if primary server is down, try secondary

if (index_size eq 0) then begin
 server2=secondary
 path2=secondary_path
 message,'Trying '+server2+'...',/cont
 network=have_network(server2,_extra=extra)
 if network then begin
  server=server2
  path=path2
 endif
endif

return, keyword_set(full) ? 'http://'+server : server
end
