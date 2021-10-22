;+
; Project     : HESSI
;
; Name        : MAP_LIST__DEFINE
;
; Purpose     : Define a map linkedlist
;
; Category    : imaging objects
;
; Syntax      : IDL> new=obj_new('map_list')
;
; History     : Written 22 Apr 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;-------------------------------------------------------------------------

function map_list::init,_ref_extra=extra

ret=self->olist::init(_extra=extra)
if not ret then return,ret

return,1

end

;--------------------------------------------------------------------------
;-- cleanup 
                     
pro map_list::cleanup

dprint,'% MAP_LIST::CLEANUP'

self->olist::cleanup
                        
return & end
                              
;--------------------------------------------------------------------------
;-- insert map object

pro map_list::add_map,map,index

if valid_omap(map) then self->set_elem,map,index

return & end

;---------------------------------------------------------------------------
;-- get map times

pro map_list::times,index,times,print=print

err=''
count=self->get_count()
if count eq 0 then return
if not is_number(index) then begin
 imax=self->get_count()-1 
 times=strarr(imax+1)
 for i=0,imax do times[i]=self->get(i,/time)
 if keyword_set(print) then iprint,times
endif else begin
 times=self->get(index,/time,err=err)
 if (err eq '') and keyword_set(print) then print,trim(index)+') '+times
endelse
if n_elements(times) eq 1 then times=times[0]

return & end

;----------------------------------------------------------------------------
;-- plot map 

pro map_list::plot,index,_extra=extra,status=status,err_msg=err_msg
                         
omap=self->getdata(index)
if valid_omap(omap) then omap->plot,_extra=extra,status=status,err_msg=err_msg
                         
return & end


;----------------------------------------------------------------------------
;-- rotate map 

pro map_list::rotate,angle,index,_extra=extra,status=status,err_msg=err_msg
                         
omap=self->getdata(index)
if valid_omap(omap) then omap->rotate,angle,_extra=extra,status=status,err_msg=err_msg
                         
return & end


;----------------------------------------------------------------------------
;-- L1 correct 

pro map_list::earth_view,index,_extra=extra,status=status,err_msg=err_msg
                         
omap=self->getdata(index)
if valid_omap(omap) then omap->earth_view,_extra=extra,status=status,err_msg=err_msg
                         
return & end


;----------------------------------------------------------------------------
;-- write map 

pro map_list::write,file,index,err=err
                         
omap=self->getdata(index)
if valid_omap(omap) then omap->write,file,err=err
                         
return & end

;--------------------------------------------------------------------------
;-- check if list has valid object map

function map_list::has_data,index

omap=self->getdata(index)
if not valid_omap(omap) then return,0b
return,(self->getdata())->has_map()

end

;---------------------------------------------------------------------------
;-- getdata function

function map_list::getdata,index,err=err

return,self->get_elem(index,err=err)

end

;---------------------------------------------------------------------------

pro map_list__define                         

temp={map_list,inherits olist}

return
end

