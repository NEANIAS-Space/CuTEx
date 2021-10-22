;+
; Project     : HESSI
;
; Name        : OBJ_PROPS
;
; Purpose     : find property names in an object
;
; Category    : utility objects
;
; Syntax      : IDL>out=obj_props(class)
;
; Inputs      : CLASS = class name or object variable name 
;
; Opt. Inputs : None
;
; Outputs     : OUT = string array of property names
;
; Opt. Outputs: None
;
; Keywords    : SUPER = check for properties in SUPER classes
;;
; History     : Written 20 May 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function obj_props,obj,super=super,err=err

obj_dissect,obj,props=props,err=err,/quiet,super=super
if err eq '' then return,props else return,''

end
