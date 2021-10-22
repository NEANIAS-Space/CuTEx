;+
; Project     : STEREO
;
; Name        : mk_secchi_map
;
; Purpose     : make a SECCHI image map
;
; Category    : imaging, maps
;
; Syntax      : IDL> map=mk_secchi_map(file,omap)
;
; Inputs      : FILE = SECCHI FITS file
;               INDEX/DATA = index/data arrays from previous read 
;
; Outputs     : MAP = SECCHI map structure
;
; Keywords    : OMAP = optional object containing map
;
; History     : Written 4 October 2007 - Zarro (ADNET)
;
; Contact     : dzarro@solar.stanford.edu
;-

function mk_secchi_map,file,data,omap=omap,_ref_extra=extra,err=err,index=index

err=''
index_data=is_struct(file) and exist(data)
file_in=is_string(file)

if ~file_in and ~index_data then begin
 pr_syntax,'map = mk_secchi_map(fits_file_name)'
 pr_syntax,'map = mk_secchi_map(index,data)'
 return,-1
endif

;-- create a SECCHI object 

return_obj=arg_present(omap) 
if ~obj_valid(omap) then omap=obj_new('secchi',_extra=extra) else begin
 if obj_class(omap) ne 'SECCHI' then obj_destroy,omap
endelse

if ~obj_valid(omap) then return,-1

;-- read and/or process

if file_in then begin
 omap->read,file,index,data,_extra=extra,err=err
endif else begin
 omap->data2map,file,data,_extra=extra,err=err
endelse
 
if is_string(err) then begin
 obj_destroy,omap
 return,-1
endif

;-- extract map structure from object

map=omap->get(/map,_extra=extra)
if ~return_obj then obj_destroy,omap

return,map

end



