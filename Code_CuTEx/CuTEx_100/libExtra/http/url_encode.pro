function url_encode, unencstr
;+
;   Name: url_encode
;
;   Purpose: urlencode a string
;
;   Input Parameters:
;      unencstr - string or string array to encode
;
;   History:
;      5-Dec-2001 - for generating valid url-encoded POST Query string 
;                   (see post_query.pro)
;
;   Restrictions:
;      written to get something online and going - needs some
;      fleshing out to cover additional url-encdoding rules
;-

if not data_chk(unencstr,/string) then begin 
   box_message,'Requires string(s) input'
   return,''
endif else retval=unencstr

retval(0)='%'+retval(0)                ; for str_replace TODO...

echars=str2arr("/,\,;,:,=,),(")

for i=0,n_elements(echars)-1 do begin 
  anyc=total(strpos(retval,echars(i)))
  if anyc ge 0 then $
     retval=str_replace(retval,echars(i),'%'+ $
       strupcase(string(byte(echars(i)),format='(z2.2)')))
endfor

retval=str_replace(retval,' ','+')
retval(0)=strmids(retval(0),1)
if n_elements(retval) gt 1 then retval=arr2str(retval,'&')

return, retval
end
