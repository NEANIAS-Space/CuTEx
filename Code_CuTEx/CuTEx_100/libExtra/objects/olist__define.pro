;+
; Project     : HESSI
;
; Name        : OLIST__DEFINE
;
; Purpose     : Define a linked_list object that stores objects
;
; Category    : objects
;
; Syntax      : IDL> new=obj_new('olist')
;
; History     : Written 22 Apr 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;-------------------------------------------------------------------------

function olist::init

ret=self->linked_list::init()
if not ret then return,ret

return,1

end

;--------------------------------------------------------------------------
;-- clean up linked_list's stored objects
                     
pro olist::cleanup

dprint,'% OLIST::CLEANUP'
count=self->get_count()
for i=0,count-1 do begin
 elem=self->get_elem(i)
 free_var,elem
endfor

;-- don't forget superclass

self->linked_list::cleanup
                        
return & end                              

;--------------------------------------------------------------------------
;-- show methods

pro olist::methods,super=super

iprint,obj_methods(self,super=super)

return & end

;--------------------------------------------------------------------------

pro olist::size

message,'# of contained objects = '+trim(self->get_count()),/cont

return & end

;-------------------------------------------------------------------
;-- set list element properties
 
pro olist::set,index,_extra=extra,err=err

err=''
elem=self->get_elem(index)
if not obj_valid(elem) then return
 
elem->set,_extra=extra
self->set_elem,elem,index,err=err,/no_destroy
      
return & end

;-------------------------------------------------------------------
;-- get element from linked_list

function olist::get_elem,index,pointer,err=err

err=''
if not is_number(index) then index=0
count=self->get_count()
if (index lt 0) or (index ge count) then begin
 err='index out of range (0 -> '+trim(count-1)+')'
 message,err,/cont
 return,''
endif

pointer=self->get_item(index)
if not ptr_valid(pointer) then begin
 err='Invalid pointer at location '+trim(index)
 message,err,/cont 
 return,''
endif

if not exist(*pointer) then begin
 err='Empty pointer at location '+trim(index)
 message,err,/cont
 return,''
endif

if not obj_valid(*pointer) then begin
 err='Invalid object at location '+trim(index)
 message,err,/cont
 return,''
endif

return,*pointer

end

;--------------------------------------------------------------------
;-- insert element into linked_list

pro olist::set_elem,elem,index,err=err,_extra=extra

err=''
count=self->get_count()
if not is_number(index) then index=count 
if (index lt 0) or (index gt count) then begin
 err='index out of range (0 -> '+trim(count)+')'
 message,err,/count
 return
endif

if not obj_valid(elem) then return

;-- replacing or adding

if (count gt 0) and (index lt count) then $
 self->replace,elem,index,_extra=extra else $
  self->add,elem,index

return & end

;-------------------------------------------------------------------------

pro olist::replace,elem,index,no_destroy=no_destroy

if not is_number(index) then return
if not obj_valid(elem) then return

count=self->get_count()
if (count le 0) or (index ge count) then return

destroy=1-keyword_set(no_destroy)

old_elem=self->get_elem(index,pointer)
if not ptr_valid(pointer) then return
if destroy then free_var,old_elem
*pointer=elem

return & end

;-----------------------------------------------------------------------
; get property subroutine (uses pass by reference to get keyword values)
 
pro olist::get,index,_ref_extra=extra,err=err

elem=self->get_elem(index,err=err)
if obj_valid(elem) then elem->get,_extra=extra

return       
end

;--------------------------------------------------------------------------
;-- get property function

function olist::get,index,_extra=extra,err=err

elem=self->get_elem(index,err=err)

if obj_valid(elem) then return,elem->get(_extra=extra) else return,''

end

;---------------------------------------------------------------------------

pro olist__define                         

temp={olist, inherits linked_list}

return
end


