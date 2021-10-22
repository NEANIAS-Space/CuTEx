;+                                                                                     
; Project     : HESSI                                                                  
;                                                                                      
; Name        : PLOT_MAP_DEFINE                                                        
;                                                                                      
; Purpose     : Define object to store PLOT_MAP plot properties                        
;                                                                                      
; Category    : objects                                                                
;                                                                                      
; Syntax      : IDL> c=obj_new('plot_map')                                             
;                                                                                      
; History     : Written 5 Oct 2002, D. Zarro (LAC/GSFC)                                
;               Modified 9 May 2006, Zarro (L-3Com/GSFC) - added COLOR properties      
;                                                                                      
; Contact     : dzarro@solar.stanford.edu                                              
;-                                                                                     
;-----------------------------------------------------------------------------         
;-- init                                                                               
                                                                                       
function plot_map::init,_ref_extra=extra                                               
                                                                                       
dprint,'% PLOT_MAP::INIT                                                               
red=bindgen(!d.table_size)                                                             
green=red                                                                              
blue=red                                                                               
self->set,_extra=extra,red=red,green=green,blue=blue        
                                                                                       
return,1                                                                               
                                                                                       
end                                                                                    
                                                                                       
;-----------------------------------------------------------------------------         
                                                                                       
pro plot_map::cleanup                                                                  
                                                                                       
dprint,'% PLOT_MAP::CLEANUP'                                                           
                                                                                       
return & end                                                                           
                                                                                       
;----------------------------------------------------------------------------          
                                                                                       
pro plot_map::set,_extra=extra                                                         
                                                                                       
if is_struct(extra) then begin                                                         
 extra=fix_extra(extra,{plot_map})                                                     
 struct_assign,extra,self,/nozero                                                      
endif                                                                                  
                                                                                       
return & end                                                                           
                                                                                       
;-----------------------------------------------------------------------------         
                                                                                       
pro plot_map__define                                                                   
                                                                                       
red=bytarr(!d.table_size)                                                              
green=red                                                                              
blue=red                                                                               
                                                                                       
self={plot_map,log_scale:0b,grid_spacing:0.,limb_plot:0b,xrange:[0.,0],yrange:[0.,0.],$
      load_colors:0b,red:red,green:green,blue:blue,has_colors:0b, inherits gen}                                     
                                                                                       
return & end                                                                           
