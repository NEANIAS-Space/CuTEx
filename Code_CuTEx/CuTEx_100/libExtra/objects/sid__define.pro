;+
; Project     : HESSI
;
; Name        : SID__DEFINE
;
; Purpose     : Define a Session ID (SID) class for tracking users CGI forms
;
; Category    : HTML, Objects
;                   
; Inputs      : None
;
; Outputs     : SID = a SID class
;
; Keywords    : None
;
; History     : 13-Sept-1999,  D.M. Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function sid::init,d,_extra=extra     ;-- initialize SID object

get_sid,sid,stc=stc,_extra=extra,/quiet

if datatype(stc) eq 'STC' then begin
 self.sid=stc.sid
 self.ptr=stc.ptr
 self.born=stc.born
endif

return,1
end

;-----------------------------------------------------------------------------

pro sid::get,value

delvarx,value
if ptr_valid(self.ptr) then value=*(self.ptr)

return & end

;------------------------------------------------------------------------------

pro sid::set,value

if exist(value) and ptr_valid(self.ptr) then *(self.ptr)=value

return & end

;------------------------------------------------------------------------------

function sid::id

return,self.sid

end

;------------------------------------------------------------------------------

function sid::value

if ptr_valid(self.ptr) then return,*(self.ptr)

end

;------------------------------------------------------------------------------

function sid::born

return,anytim2utc(self.born,/vms)

end

;-----------------------------------------------------------------------------

pro sid::cleanup                         
free_pointer,self.ptr

return & end

;-----------------------------------------------------------------------------

pro sid__define                          ;-- define Session ID structure

sid={sid,sid:'',born:0d,ptr:ptr_new()}

end

