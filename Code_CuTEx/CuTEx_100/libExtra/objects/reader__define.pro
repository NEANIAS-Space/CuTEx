;+
; Project     : HESSI
;
; Name        : READER__DEFINE
;
; Purpose     : Define a READER object
;
; Explanation : defines a READER class that figures out 
;               appropriate reader based on filename.
;               Default reader is a FITS reader
;
; Category    : objects
;               
; Syntax      : IDL> reader=obj_new('reader')
;
; History     : Written 28 Feb 2001, D. Zarro (EITI/GSFC)
;               Changed the chk in ::get_class from _? to _+ acs 2004-05-03
;               Added call to FILE_BREAK, 1-April-04, Zarro (L-3Com/GSFC)
;               Added SPIRIT CLASS, 11-Nov-2005, Zarro (L-3Com/GSFC)
;               22-Feb-06, Zarro (L-3Com/GSFC) - undid change by ACS
;               23 November 2007, Zarro (ADNEt) - added call to FITS_CLASS
;
; Contact     : dzarro@solar.stanford.edu
;-

;-- reader init

function reader::init

return,1

end

;--------------------------------------------------------------------------

pro reader::cleanup

free_var,self.file

return & end

;--------------------------------------------------------------------------

function reader::get_prefix,class

prefix=['mg1','mdi','sxi','bbso','kpno','rstn','phnx','nobe','hxr','ovsa','nanc','eit','efr','efz','osra']

class=['spirit','mdi','sxi','bbso','kpno','rstn','ethz','nobe','hxrs','ovsa_ltc','nancay','eit','eit','eit','osra']

return,prefix

end

;--------------------------------------------------------------------------
;-- determine class corresponding to this file name
 
function reader::get_class,file

if is_blank(file) then return,'fits'

types=self->get_prefix(class)
ntypes=n_elements(types)
tfile=strcompress(file_break(file),/rem)

nfiles=n_elements(tfile)
classes=replicate('fits',nfiles)

for k=0,nfiles-1 do begin
 for i=0,ntypes-1 do begin
;  chk=types[i]+'_?'
; acs change 2004-05-03
  chk=types[i]+'_?'
  test1='(\\|/| +)'+chk
  test2='^'+chk
  test='('+test1+'|'+test2+')'
  if stregex(tfile[k],test,/bool,/fold) then classes[k]=class[i]
 endfor
endfor                  

if nfiles eq 1 then classes=classes[0]

return,classes

end

;---------------------------------------------------------------------------

pro reader::read,file,object,_ref_extra=extra,err=err,nodata=nodata

err=''

error=0
catch,error
if error ne 0 then begin
 err=err_state()
 message,err,/cont
 catch,/cancel
 return
endif

fits_class,file,class
if is_blank(class) then class=self->get_class(file)

free_var,object
object=obj_new(class)

object->read,file,err=err,_extra=extra 

if keyword_set(nodata) then obj_destroy,object

ptr=self.file
ptr_alloc,ptr
*ptr=file
self.file=ptr

return & end

;--------------------------------------------------------------------------                  
;-- define reader object

pro reader__define                 

struct={reader,file:ptr_new()}

return & end

