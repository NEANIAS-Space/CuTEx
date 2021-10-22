;+
; Project     : HESSI
;
; Name        : FIFO__DEFINE
;
; Purpose     : Define a FIFO object
;
; Category    : Objects
;
; Explanation : A first in first out FIFO object to store anything
;
; Syntax      : IDL> new=obj_new('fifo')
;              
; Inputs    :   MAX_SIZE = max # of elements to cache [def=10]. Once
;               exceeded, initial data will be overwritten.
;
; Methods     : ->SET,ID,INPUT                           ; store input
;               INPUT = any data type
;               ID = unique string identifier for INPUT
;
;               ->GET,ID,OUTPUT                        ; retrieve input   
;             
;               ->EMPTY   ;-- empty fifo
;               ->SHOW    ;-- show fifo contents
;
; History     : Version 1,  6-DEC-1999,  D.M. Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

;---------------------------------------------------------------------------
;-- constructor

function fifo::init,max_size,_extra=extra

if not is_number(max_size) then max_size=10
self.max_size=max_size

self->create

return,1
end

;----------------------------------------------------------------------------

pro fifo::create

self.pointer=ptr_new(/all)
max_size=self.max_size
if (self.max_size) eq 0 then begin
 max_size=10
 self.max_size=max_size
endif
struct={ipos:0,ids:strarr(max_size),ptrs:ptrarr(max_size,/all)}
*self.pointer=struct

return & end


;---------------------------------------------------------------------------
;-- destructor

pro fifo::cleanup

dprint,'% FIFO::CLEANUP'

heap_free,self.pointer

return & end

;----------------------------------------------------------------------------
;-- empty FIFO by freeing data from pointers

pro fifo::empty

heap_free,self.pointer
self->create

return & end
                        
;---------------------------------------------------------------------------

pro fifo::show

pointer=self.pointer
if not ptr_exist(pointer) then begin
 message,'Empty buffer',/cont
 return
endif

struct=*pointer
message,'current position: '+strtrim(struct.ipos,2),/cont
iprint,struct.ids
iprint,struct.ptrs
               
return & end

;---------------------------------------------------------------------------

pro fifo::set,id,value,status=status

status=0b
if (not is_string(id)) or (not exist(value)) then return

dprint,'% FIFO saving in cache - '+id
if not ptr_exist(self.pointer) then self->create
pointer=self.pointer
struct=*pointer
ipos=struct.ipos
ids=struct.ids
chk=where(strtrim(id,2) eq strtrim(ids,2),count)
no_dup=1b
if count gt 0 then begin
 dprint,'% FIFO overwriting cache - '+id
 ipos=chk[0]
 no_dup=0b
endif
max_len=n_elements(struct.ids)
if ipos ge max_len then ipos=0
struct.ids[ipos]=id
ptr=struct.ptrs[ipos]
if ptr_exist(ptr) then heap_free,*ptr
*ptr=value
struct.ptrs[ipos]=ptr
if no_dup then struct.ipos=ipos+1
*pointer=struct

status=1b
return & end

;-------------------------------------------------------------------------

pro fifo::delete,id

struct=*self.pointer

np=n_elements(id)
for i=0,np-1 do begin
 pos=self->get_pos(id[i])
 if pos ge 0 then begin
  dprint,'% FIFO freeing cache - '+id[i]
  heap_free,*((struct.ptrs)[pos])
  struct.ids[pos]=''
 endif
endfor

*self.pointer=struct

return & end

;-----------------------------------------------------------------------------

function fifo::get,id,status=status

self->get,id,value,status=status
return,value & end

;----------------------------------------------------------------------------

function fifo::get_pos,id

pos=-1
if (not is_number(id)) and (not is_string(id)) then return,pos

pointer=self.pointer
if not ptr_exist(pointer) then begin
 message,'Empty buffer',/cont
 return,pos
endif

struct=*pointer
np=n_elements(struct.ids)

if (is_string(id)) then begin
 chk=where(id eq struct.ids,count)
 if count gt 0 then pos=chk[0] 
;else message,'Input ID not found',/cont
endif else begin
 if (id gt -1) and (id lt np) then pos=id else begin
  message,'Out of range index',/cont
 endelse
endelse   

return,pos
end
  
;---------------------------------------------------------------------------

pro fifo::get,id,value,status=status

delvarx,value & status=0b
            
pos=self->get_pos(id)
if pos lt 0 then return

struct=*self.pointer

dprint,'% FIFO restoring from cache - '+trim(id)
ptr=(struct.ptrs)[pos]

if not ptr_valid(ptr) then begin
 message,'Invalid pointer',/cont & help,ptr 
 return
endif

if not exist(*ptr) then begin
 message,'Empty pointer',/cont & help,ptr 
 return
endif

value=*ptr
status=1b

return & end

;---------------------------------------------------------------------------
;-- define fifo structure

pro fifo__define

fifo_struct={fifo,max_size:0l,pointer:ptr_new()}

return & end
