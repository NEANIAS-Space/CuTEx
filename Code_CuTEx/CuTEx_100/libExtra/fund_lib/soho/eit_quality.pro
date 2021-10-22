;+
; Project     : SOHO
;
; Name        : EIT_QUALITY
;
; Purpose     : flag problematic EIT file
;
; Category    : synoptic gbo
;
; Syntax      : IDL> flag=eit_quality(file)
;
; Inputs      : FILE = EIT FITS file
;
; Outputs     : 1 if good quality science data, 0 if problem with EIT files (camera err, engineering, etc)
;
; History     : Written 20 Dec 2005, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function eit_quality,file

np=n_elements(file)
if np eq 0 then return,1b

;-- look for bad actors

bad_signs=["camera_err='yes'","object='dark'","object='calibration'",$
       "object='readout'","object='continous'","object='continuous'","object='lamp'"]

sign=arr2str(bad_signs,delim='|')
out=bytarr(np)
for i=0,np-1 do begin
 chk=loc_file(file[i],/verb,err=err)
 if is_string(err) then continue
 read_eit,file[i],header=header,/nodata
 check=where(stregex(strcompress(header,/rem),sign,/bool,/fold),count)
 out[i]=count eq 0
endfor

if np eq 1 then out=out[0]
return,out

end


