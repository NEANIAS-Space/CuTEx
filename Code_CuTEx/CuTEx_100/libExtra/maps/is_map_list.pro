;+
; Project     : HESSI
;
; Name        : IS_MAP_LIST
;
; Purpose     : check if input object was inherited from a MAP_LIST
;
; Category    : utility objects
;
; Syntax      : IDL> check=is_map_list(obj)
;
; Inputs      : OBJECT = object variable
;
; Outputs     : 1/0 if is or isn't
;
; History     : Written 18 April 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

function is_map_list,obj

parents=obj_parents(obj,child=child)
look=stregex([child,parents],'map_list',/fold)
found=where(look gt -1,count)
return,count gt 0

end
