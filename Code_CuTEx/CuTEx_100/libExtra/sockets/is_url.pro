;+
; Project     : Hinode/EIS
;
; Name        : IS_URL
;
; Purpose     : check if file name is URL
;
; Category    : utility system sockets
;
; Inputs      : FILE = file to check
;
; Outputs     : 1/0 if valid URL
;
; Keywords    : READ_REMOTE= 1/0 if file is directly readable 
;
; History     : Written 19-Nov-2007, D.M. Zarro (ADNET/GSFC)
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function is_url,file,read_remote=read_remote

read_remote=0b
if is_blank(file) then return,0b

stc=url_parse(file)

chk=is_string(stc.host) and is_string(stc.scheme) 
if chk then read_remote= ~stregex(stc.scheme,'ftp:\/\/',/bool) and $
                         ~stregex(stc.path,'(\.gz|\.Z)$',/bool)

return,chk
end
