;+
; Project     : HESSI
;                  
; Name        : SSW_LAST_UPDATE
;               
; Purpose     : spit out time of last SSW mirror update by checking date
;               of $SSW/gen/setup/ssw_info_map.dat
;                             
; Category    : utility
;               
; Syntax      : IDL> last_ssw_update
;
; Inputs      : None
; 
; Outputs     : time of last update printed to screen
;
; Keywords    : None
;                                   
; History     : Written, 19-April-2004, Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-    


pro ssw_last_update

;-- find latest SSW map file

map_file=local_name('$SSW/gen/setup/ssw_info_map.dat')
chk=loc_file(map_file,count=count)
if count eq 0 then begin
 message,'Non-standard SSW installation',/verb
 return
endif

openr,lun,chk[0],/get_lun
head=strarr(10)
readf,lun,head
close_lun,lun

;-- time of last update

find=where(strpos(strlowcase(head),'ut time') gt -1,count)
if count eq 0 then begin
 message,'Non-standard SSW installation',/verb
 return
endif

last=head[find[0]]
last=str_replace(last,'|','')
last=str_replace(last,';','')
message,last,/cont

return
end


