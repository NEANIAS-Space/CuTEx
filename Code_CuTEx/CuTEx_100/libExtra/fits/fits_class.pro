;+
; Project     : Hinode/EIS
;
; Name        : FITS_CLASS
;
; Purpose     : determine object class associated with FITS file
;
; Category    : objects, i/o
;
; Syntax      : IDL> fits_class,file,class
;
; Inputs      : FILE = FITS filename to test
;
; Outputs     : CLASS = class name [e.g. 'EIT'] if EIT FITS file
;
; Keywords    : ERR= error string
;
; History     : Written 12 November 2007, D. Zarro (ADNET)

pro fits_class,file,class

class=''
mrd_head,file,head,err=err
if is_string(err) then return

;-- check for DETECTOR then INSTRUMEN keyword

choices=['dete','inst']
for i=0,n_elements(choices)-1 do begin
 check=choices[i]
 chk=stregex(head,check+".+= *'? *([^ ]+) *'?",/ext,/sub,/fold)
 ok=where(chk[1,*] ne '',count)
 if count gt 0 then begin
  tclass=chk[1,ok[0]]
  if valid_class(tclass) then begin 
   class=tclass & return
  endif
 endif
endfor

return & end
