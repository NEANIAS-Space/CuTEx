;+
; Project     : HESSI
;
; Name        : OBJ_METHODS
;
; Purpose     : find methods in an object
;
; Category    : utility objects
;
; Explanation : checks CLASS__DEFINE procedure 
;
; Syntax      : IDL>out=obj_methods(class)
;
; Examples    :
;
; Inputs      : CLASS = class name or object variable name 
;
; Opt. Inputs : None
;
; Outputs     : OUT = string array of method calls
;
; Opt. Outputs: None
;
; Keywords    : ERR = error string
;
; History     : Written 20 May 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function obj_methods,obj,err=err,super=super

obj_dissect,obj,err=err,/quiet,/super,methods=methods
if err eq '' then return,methods else return,''

end
