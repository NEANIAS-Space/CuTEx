;+
; Project     : HESSI
;
; Name        : INDEX2OMAP
;
; Purpose     : convert INDEX/DATA to object maps
;
; Category    : imaging, FITS, objects
;
; Syntax      : index2omap,index,data,omap
;
; Inputs      : INDEX = index structure array
;               DATA = data array
;               APPEND = append to existing list object
;
; Outputs     : OMAP = map object linkedlist 
;
; History     : Written, 22 April 2000, D.M. Zarro (SM&A/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro index2omap,index,data,omap,append=append,_extra=extra,err=err

err=''
np=n_elements(index)
if np eq 0 then begin
 err='input INDEX undefined'
 message,err,/cont & return
endif

ndata=data_chk(data,/nimage)
if np ne ndata then begin
 err='input DATA does not match INDEX
 message,err,/cont
 return
endif


;-- Start by making each map the old way as a structure. Store
;-- it in a temporary map object and then store the object in a linkedlist.
;-- Be careful not to destroy any pointers when cleaning up

new_omap=1-keyword_set(append)
if not valid_omap(omap) then new_omap=1
if new_omap then begin
 if max(obj_valid(omap)) then obj_destroy,omap 
 omap=obj_new('map_list')
endif


start_index=omap->get_count()
for i=0,np-1 do begin
 terr=''
 index2map,index[i],data[*,*,i],map,err=terr,_extra=extra 
 if terr eq '' then begin
  tmap=obj_new('map')
  tmap->set,map=map,/no_copy
  omap->add_map,tmap,i+start_index
 endif else err=err+trim(terr)
endfor

return
end

