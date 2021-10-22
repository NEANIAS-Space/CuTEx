;+
; Project     : HESSI
;
; Name        : OBJ_DISSECT
;
; Purpose     : find methods & properties of an object or class
;
; Category    : utility objects
;
; Explanation : checks CLASS name and CLASS__DEFINE procedure 
;
; Syntax      : IDL>obj_dissect,class,methods=methods,props=props
;
; Inputs      : CLASS = class name or object variable name 
;
; Outputs     : See keywords
;
; Opt. Outputs: None
;
; Keywords    : QUIET = inhibit printing
;               SUPER = check SUPER classes
;               METHODS = string array of method calls
;               PROPS = string array of property names
;
; History     : Written 20 May 1999, D. Zarro, SM&A/GSFC
;               Modified 5 Nov 2006, Zarro (ADNET/GSFC)
;                - removed EXECUTE
;
; Contact     : dzarro@solar.stanford.edu
;-

pro obj_dissect,class,methods=methods,quiet=quiet,super=super,err=err,props=props

err=''

if n_elements(class) ne 1 then begin
 err='Input argument must be scalar class name or object reference'
 message,err,/cont
 return
endif

loud=1-keyword_set(quiet)
super=keyword_set(super)
find_props=arg_present(props)
find_methods=arg_present(methods)

valid_obj=0b
if size(class,/tname) eq 'OBJREF' then begin
 valid_obj=obj_valid(class)
 if not valid_obj then begin
  err='Input object is null'
  message,err,/cont
  return
 endif
 class_name=obj_class(class) 
endif else begin
 if size(class,/tname) ne 'STRING' then begin
  if find_props then $
   pr_syntax,'props=obj_props(class_name)'
  if find_methods then $
   pr_syntax,'methods=obj_methods(class_name [,/super])'
  err='Invalid input'
  return
 endif
 class_name=class
endelse 

;-- error catch

class_err='"'+class_name+'" is probably not a valid class name'
error=0
catch,error
if error ne 0 then begin
 err=class_err
 message,err,/cont
 return
endif
                       
;-- extract properties 

if find_props then begin
 temp=obj_struct(class_name)
 if is_struct(temp) then props=tag_names(temp) else props=''
 return
endif

if not find_methods then return   
                                       
;-- extract methods calls
;-- look for __define constructor procedure. 
;   If found, avoid the overhead of creating a temporary object

class_def=strlowcase(trim(class_name))+'__define'
have_con=have_proc(class_def,out=fname)

if (not have_con) or (super and (not valid_obj)) then begin
 chk=valid_class(class_name)
 if not chk then begin
  err='Invalid class name - '+class_name
  message,err,/cont
  return
 endif
endif

if fname eq '' then begin
 def_err='Could not locate "'+class_def+'" constructor'
 err=def_err
 message,err,/cont
 return
endif

temp=''
chkarg,fname,out=temp,/quiet,/reset

;-- search for method calls with '::'

out=''
calls=where(strpos(temp,'::') gt -1,count)
if count gt 0 then begin
 if count gt 0 then out=temp[calls]
 if count eq 1 then out=out[0]
 if loud then for i=0,count-1 do print,out[i]
endif else message,'No methods found for "'+class_name+'"',/cont

;-- check for super classes

if super then begin
 sclass=obj_class(class_name,/super)
 if trim(sclass[0]) ne '' then begin
  for i=0,n_elements(sclass)-1 do begin
   obj_dissect,sclass[i],methods=sout,quiet=quiet,/super
   if trim(sout[0]) ne '' then begin
    if out[0] eq '' then out=sout else out=append_arr(out,sout,/no_copy)
   endif
  endfor
 endif
endif
methods=out

return & end
