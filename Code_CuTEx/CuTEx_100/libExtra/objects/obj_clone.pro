;+
; Project     : HESSI
;
; Name        : OBJ_CLONE
;
; Purpose     : Clone an object by saving it to an IDL save file
;               and then restoring it into a new object
;
; Category    : utility objects
;
; Syntax      : IDL> clone=obj_clone(object)
;
; Inputs      : OBJECT = object to clone (array or scalar)
;
; Outputs     : CLONE = cloned object
;
; History     : Written 29 Nov 2002, D. Zarro (EER/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;
;-

function obj_clone,object,err=err,_extra=extra

err='Non-existent or invalid object entered'
if not exist(object) then begin
 message,err,/cont
 return,-1
endif

if size(object,/tname) ne 'OBJREF' then begin
 message,err,/cont
 return,object
endif

return,clone_var(object)

end
