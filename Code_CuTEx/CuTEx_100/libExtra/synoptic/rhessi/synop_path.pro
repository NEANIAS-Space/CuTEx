;+
; Project     : HESSI
;
; Name        : SYNOP_PATH
;
; Purpose     : return path to synoptic data files based on date
;
; Category    : synoptic sockets
;                   
; Inputs      : DATE = input date [def = current time]
;               SERVER = synoptic server name to check [def = beauty]
;
; Outputs     : PATH = path to synoptic data files
;
; Keywords    : None
;
; History     : 28-Dec-2004,  D.M. Zarro (L-3Com/GSFC) - Written.
;               20-Jan-2007, Zarro (ADNET/GSFC) - added beauty as def.
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function synop_path,date,server

def_server='beauty.nascom.nasa.gov'
if is_blank(server) then server=def_server else server=trim(server)

err=''
time=anytim2tai(date,err=err)
if err ne '' then begin
 get_utc,time 
 time=anytim2tai(time)
endif

list_synop,servers,paths
chk=where(server eq servers,count)
if count eq 0 then begin
 server=def_server
 chk=0
endif

path=paths[chk[0]]
change_date=anytim2tai('15-dec-04')

if (server eq def_server) and (time lt change_date) then path='/synoptic'

return,path

end
