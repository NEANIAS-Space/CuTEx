;+
; Project     : SOHO-CDS
;
; Name        : HEAD2STC
;
; Purpose     : convert FITs header to structure
;
; Category    : imaging
;
; Explanation : 
;
; Syntax      : stc=head2stc(head)
;
; Examples    :
;
; Inputs      : HEAD = FITS header 
;
; Opt. Inputs : None
;
; Outputs     : STC = structure with header keys as tags
;
; Opt. Outputs: None
;
; Keywords    : None
;
; Common      : None
;
; Restrictions: None
;
; Side effects: None
;
; History     : Written - 22 November 1996, D. Zarro, ARC/GSFC
;               Modified - 22 May 1999, Zarro (SM&A/GSC), added check
;                       for extra fields without comments
;
; Contact     : dzarro@solar.stanford.edu
;-

function head2stc,head,err=err
err=''

if datatype(head) ne 'STR' then begin
 err='need string input header'
 return,-1
endif

equ=strpos(head,'= ')
slash=strpos(head,' /')
for i=0,n_elements(head)-1 do begin
 if equ(i) gt -1 then begin
  field=strmid(head(i),0,equ(i))
  comment=strpos(strupcase(field),strupcase('comment ')) gt -1
  if slash(i) eq -1 then slash(i)=1000
  if (slash(i) gt equ(i)) and (not comment) then begin
   val=strmid(head(i),equ(i)+1,slash(i)-equ(i)-1)
   field=trim(strcompress(field))
   field=strep(field,'-','__',/all)
   val=trim(strcompress(val))
   val=strep(val,"'","",/all)
   stc=add_tag(stc,val,field,/dup)
  endif
 endif
endfor
return,stc & end
