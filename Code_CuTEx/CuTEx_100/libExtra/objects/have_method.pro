;+
; Project     : HESSI
;
; Name        : HAVE_METHOD
;
; Purpose     : check if method is supported by object
;
; Category    : utility objects
;
; Explanation : checks CLASS__DEFINE procedure 
;
; Syntax      : IDL> chk=have_method(class,method)
;
; Inputs      : CLASS = class name or object variable name
;               METHOD = name of method to check for 
;
; Outputs     : CHK = 1/0 if have/have not
;;
; Keywords    : None
;
; History     : Written 25 May 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function have_method,class,method

common have_method,saved_methods
err=''

stype=size(class,/tname)
if ((stype ne 'STRING') and (stype ne 'OBJREF')) or (size(method,/tname) ne 'STRING') then begin
 pr_syntax,'chk=have_method(class_name,method_name)'
 err='invalid inputs'
 return,0b
endif

if stype eq 'OBJREF' then class_name=obj_class(class) else class_name=class

class_name=strtrim(strlowcase(class_name),2)
method_name=strtrim(strlowcase(method),2)

;-- check commons

if exist(saved_methods) then begin
 chk=where((saved_methods.class_name eq class_name) and $
           (saved_methods.method_name eq method_name), count)
 if count gt 0 then return,1b
endif

methods=obj_methods(class,/super,err=err)
if err ne '' then return,0b

search=stregex(methods,'::'+method_name,/fold)
chk=where(search gt -1,count)
have=count gt 0

if have then begin
 tsave={class_name:class_name,method_name:method_name}
 saved_methods=merge_struct(saved_methods,tsave)
endif
                   
return,have
end
