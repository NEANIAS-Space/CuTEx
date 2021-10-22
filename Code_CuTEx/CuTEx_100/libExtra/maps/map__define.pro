;+                                                                                                
; Project     : HESSI                                                                             
;                                                                                                 
; Name        : MAP__DEFINE                                                                       
;                                                                                                 
; Purpose     : Define a MAP object                                                               
;                                                                                                 
; Category    : imaging objects                                                                   
;                                                                                                 
; Syntax      : IDL> new=obj_new('map')                                                           
;                                                                                                 
; History     : Written 22 Nov 1999, D. Zarro, SM&A/GSFC                                          
;             : Modified 18 Sept 2001, D. Zarro (EITI/GSFC) - improved                            
;               memory management                                                                 
;               Modified 5 Oct 2002, Zarro (LAC/GSFC) - added object                              
;               to store plot properties                                                          
;               Modified 2 Oct 2003, Zarro (GSI/GSFC) - added correction                          
;               for 180 degree rolled images                                                      
;               Modified 7 Feb 2004, Zarro (L-3Com/GSFC) - fixed bug with                         
;               plotting grid/limb                                                                
;               Modified 17 Mar 2004, Zarro (L-3Com/GSFC) - added FOV keyword                     
;               Modified 9 May 2006, Zarro (L-3Com/GSFC) 
;                - added COLOR support                    
;               Modified 23 November 2007, Zarro (ADNET) 
;                - preserve current value of DECOMP
;                                                                                                 
; Contact     : dzarro@solar.stanford.edu                                                         
;-                                                                                                
                                                                                                  
;-------------------------------------------------------------------------                        
                                                                                                  
function map::init,_extra=extra                                                                   
                                                                                                  
dprint,'% MAP::INIT'                                                                              
self.plot_type='image'                                                                            
self.created=!stime                                                                               
self.omap=obj_new('linked_list')        ;-- store map structures                                  
self.oindex=obj_new('linked_list')      ;-- store data index structures                           
self.oprop=obj_new('linked_list')       ;-- store plot property object                            
                                                                                                  
return,1                                                                                          
                                                                                                  
end                                                                                               
                                                                                                  
;-----------------------------------------------------------------------                          
;--destroy map object                                                                             
                                                                                                  
pro map::cleanup                                                                                  
                                                                                                  
dprint,'% MAP::CLEANUP'                                                                           
self->free_var                                                                                    
return                                                                                            
end                                                                                               
                                                                                                  
;-----------------------------------------------------------------------                          
;--empty map object                                                                               
                                                                                                  
pro map::empty                                                                                    
                                                                                                  
if not obj_valid(self.omap) then return                                                           
self.omap->delete,/all                                                                            
self.oindex->delete,/all                                                                          
self.oprop->delete,/all                                                                           
                                                                                                  
return & end                                                                                      
                                                                                                  
;-----------------------------------------------------------------------                          
;-- copy map object (function)                                                                    
                                                                                                  
function map::clone,k,_ref_extra=extra                                                            
                                                                                                  
self->clone,cobj,k,_extra=extra                                                                   
                                                                                                  
return,cobj                                                                                       
                                                                                                  
end                                                                                               
                                                                                                  
;-----------------------------------------------------------------------                          
;-- copy map object (procedure)                                                                   
                                                                                                  
                                                                                                  
pro map::clone,cobj,k,err=err,_extra=extra,all=all                                                
                                                                                                  
err=''                                                                                            
                                                                                                  
if keyword_set(all) then begin                                                                    
 obj_copy,self,cobj                                                                               
 return                                                                                           
endif                                                                                             
                                                                                                  
if not self->has_data(k,err=err) then begin                                                       
 message,err,/cont                                                                                
 cobj=-1                                                                                          
 return                                                                                           
endif                                                                                             
                                                                                                  
map=self->get(k,/map,_extra=extra)                                                                
index=self->get(k,/index,_extra=extra)                                                            
props=self->get(k,/props,_extra=extra)                                                            
                                                                                                  
class=obj_class(self)                                                                             
if obj_valid(cobj) then obj_destroy,cobj                                                          
cobj=call_function('obj_new',class)                                                               
                                                                                                  
cobj->set,map=map,index=index,props=props,_extra=extra                                            
                                                                                                  
return                                                                                            
                                                                                                  
end                                                                                               
                                                                                                  
;-------------------------------------------------------------------                              
;-- set map object properties                                                                     
                                                                                                  
pro map::set,k,map=map,index=index,_extra=extra,err=err,$                                         
               verbose_map=verbose_map,no_copy=no_copy,$                                          
               props=props                                                                        
                                                                                                  
if is_number(verbose_map) then self.verbose_map=0b > verbose_map < 1b                             
                                                                                                  
count=self->get(k,/count)                                                                         
                                                                                                  
replace=k lt count                                                                                
                                                                                                  
;-- save map structure in linked_list                                                             
                                                                                                  
if valid_map(map) or is_struct(index) then begin                                                  
 if valid_map(map) then begin                                                                     
  self.omap->add,map,k,err=err,replace=replace,no_copy=no_copy                                    
  if (not replace) then begin                                                                     
   oprop=obj_new('plot_map',_extra=extra)                                                         
   self.oprop->add,oprop,k,err=err                                                                
  endif                                                                                           
 endif                                                                                            
 if is_struct(index) then self.oindex->add,index,k,err=err,replace=replace                        
endif                                                                                             
                                                                                                  
;-- update plot properties                                                                        
                                                                                                  
if obj_valid(props) then begin                                                                    
 self.oprop->add,props,k,/replace                                                                 
endif                                                                                             
                                                                                                  
if is_struct(extra) then begin                                                                    
 oprop=self->get(k,/props,err=err)                                                                
 if err eq '' then begin                                                                          
  oprop->set,_extra=extra                                                                         
  self.oprop->add,oprop,k,/replace                                                                
 endif                                                                                            
endif                                                                                             
                                                                                                  
return                                                                                            
end                                                                                               
                                                                                                  
;--------------------------------------------------------------------------                       
;-- explicitly set map properties                                                                 
                                                                                                  
pro map::setmap,k,_extra=extra                                                                    
                                                                                                  
if is_struct(extra) then begin                                                                    
 smap=self->get(k,/map,/no_copy,err=err)                                                          
 if err eq '' then begin                                                                          
  extra=fix_extra(extra,tag_names(smap))                                                          
  struct_assign,extra,smap,/nozero                                                                
  self.omap->add,smap,k,/replace,/no_copy                                                         
 endif                                                                                            
endif                                                                                             
                                                                                                  
return & end                                                                                      
                                                                                                  
;-------------------------------------------------------------------------                        
;-- update index with map information                                                             
                                                                                                  
pro map::update_index,k,err=err                                                                   
                                                                                                  
err=''                                                                                            
                                                                                                  
if (not self->has_index(k,err=err)) or (not self->has_data(k,err=err)) then begin                 
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
index=self->get(k,/index,err=err)                                                                 
if err ne '' then return                                                                          
                                                                                                  
;-- make sure CRPIX/CRVAL and XCEN/YCEN are self-consistent                                       
                                                                                                  
nx=self->get(k,/nx)                                                                               
ny=self->get(k,/ny)                                                                               
dx=self->get(k,/dx)                                                                               
dy=self->get(k,/dy)                                                                               
xc=self->get(k,/xc)                                                                               
yc=self->get(k,/yc)                                                                               
roll=self->get(k,/roll_angle)                                                                     
rollc=self->get(k,/roll_center)                                                                   
                                                                                                  
index.naxis1=nx                                                                                   
index.naxis2=ny                                                                                   
                                                                                                  
index=rep_tag_value(index,0,'crval1')                                                             
index=rep_tag_value(index,0,'crval2')                                                             
                                                                                                  
index=rep_tag_value(index,dx,'cdelt1')                                                            
index=rep_tag_value(index,dy,'cdelt2')                                                            
                                                                                                  
crpix1=comp_fits_crpix(xc,dx,nx,0.)                                                               
crpix2=comp_fits_crpix(yc,dy,ny,0.)                                                               
                                                                                                  
index=rep_tag_value(index,crpix1,'crpix1')                                                        
index=rep_tag_value(index,crpix2,'crpix2')                                                        
                                                                                                  
index=rep_tag_value(index,xc,'xcen')                                                              
index=rep_tag_value(index,yc,'ycen')                                                              
                                                                                                  
index=rep_tag_value(index,roll,'crota')                                                           
                                                                                                  
index=rep_tag_value(index,rollc[0],'crotacn1')                                                    
index=rep_tag_value(index,rollc[1],'crotacn2')                                                    
                                                                                                  
if have_tag(index,'comment') then $                                                               
 index=rep_tag_value(index,rem_blanks(index.comment),'comment')                                   
                                                                                                  
if have_tag(index,'history') then $                                                               
 index=rep_tag_value(index,rem_blanks(index.history),'history')                                   
                                                                                                  
self->set,k,index=index,/replace                                                                  
                                                                                                  
return & end                                                                                      
                                                                                                  
;--------------------------------------------------------------------------                       
;-- check if FITS file written via MAPS                                                           
                                                                                                  
function map::fits_map,file                                                                       
                                                                                                  
mrd_head,file,header                                                                              
chk=stregex(header,'HISTORY.+WRITEFITS',/bool,/fold)                                              
ok=where(chk,count)                                                                               
return,count ne 0                                                                                 
                                                                                                  
end                                                                                               
                                                                                                  
;-------------------------------------------------------------------------                        
;-- write map to FITS file                                                                        
                                                                                                  
pro map::fitswrite,file,k,_ref_extra=extra                                                        
                                                                                                  
if not self->roll_corrected(k) then self->roll_correct,k                                          
                                                                                                  
self->write,file,k,_extra=extra                                                                   
                                                                                                  
return & end                                                                                      
;--------------------------------------------------------------------------                       
                                                                                                  
pro map::write,file,k,err=err,out_dir=out_dir,compress=compress,back=back                         
                                                                                                  
;-- validate output file name and directory                                                       
                                                                                                  
err=''                                                                                            
if is_blank(file) then begin                                                                      
 err='Invalid file name entered'                                                                  
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
break_file,file,dsk,dir,name,ext                                                                  
odir=trim(dsk+dir)                                                                                
if is_blank(odir) then odir=curdir()                                                              
if exist(out_dir) then odir=out_dir                                                               
if not write_dir(odir,err=err) then return                                                        
oname=trim(name+ext)                                                                              
                                                                                                  
;-- reconcile map and index before writing                                                        
                                                                                                  
if (not self->has_index(k,err=err)) or (not self->has_data(k,err=err)) then begin                 
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
map=self->get(k,/map,err=err)                                                                     
if err ne '' then return                                                                          
if not valid_map(map) then begin                                                                  
 err='Invalid map stored in object'                                                               
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
;-- update history                                                                                
                                                                                                  
self->update_history,'File written by WRITEFITS',k                                                
                                                                                                  
self->update_index,k                                                                              
index=self->get(k,/index)                                                                         
index.filename=oname                                                                              
head=struct2fitshead(index)                                                                       
                                                                                                  
ofile=concat_dir(odir,oname)                                                                      
if self->get(/verbose_map) then message,'Writing FITS file - '+ofile+'...',/cont                  
                                                                                                  
writefits,ofile,map.data,head                                                                     
                                                                                                  
chmod,ofile,/g_write,/g_read                                                                      
                                                                                                  
if keyword_set(compress) then espawn,'gzip -f '+ofile,/noshell,back=back                          
                                                                                                  
return & end                                                                                      
            
                                                                                      
;----------------------------------------------------------------------------                     
;-- get data method                                                                               
                                                                                                  
function map::getdata,k                                                                           
return, self-> get(k,/data)                                                                       
end                                                                                               

                                                                                      
;----------------------------------------------------------------------------                     
;-- get map method                                                                               
                                                                                                  
function map::getmap,k                                                                           
return, self-> get(k,/map)                                                                       
end                                                                                               
                                                                                                  
                                                                                                  
;-------------------------------------------------------------------                              
;-- get map object properties                                                                     
                                                                                                  
function map::get,k,map=map,type=type,created=created,_extra=extra,no_copy=no_copy,$              
              plot_type=plot_type,filename=filename,err=err,header=header,$                       
              index=index,count=count,verbose_map=verbose_map,$                                   
              props=props,all=all                                                                 
err=''                                                                                            
                                                                                                  
if not exist(k) then k=0                                                                          
                                                                                                  
;-- map properties                                                                                
                                                                                                  
if keyword_set(map) then begin                                                                    
 count=self.omap->get_count()                                                                     
 all=keyword_set(all)                                                                             
 if (n_elements(k) eq 1) and (not all) then return,self.omap->get_value(k,err=err,no_copy=no_copy)
 if all then k=indgen(count)                                                                      
 for i=0,n_elements(k)-1 do begin                                                                 
  j=k[i]                                                                                          
  if (j ge 0) and (j lt count) then begin                                                         
   err=''                                                                                         
   ptr=self.omap->get_item(j,err=err)                                                             
   if err eq '' then begin                                                                        
    if exist(aptr) then aptr=[aptr,ptr] else aptr=ptr                                             
   endif                                                                                          
  endif                                                                                           
 endfor                                                                                           
 if exist(aptr) then return,aptr else return,''                                                   
endif                                                                                             
                                                                                                  
;-- top level properties                                                                          
                                                                                                  
if keyword_set(created) then return,self.created                                                  
if keyword_set(type) then return,self.plot_type                                                   
if keyword_set(plot_type) then return,self.plot_type                                              
if keyword_set(verbose_map) then return,self.verbose_map                                          
                                                                                                  
                                                                                                  
if keyword_set(index) or keyword_set(header) then begin                                           
 if not self->has_index(k,err=err) then return,''                                                 
 index=self.oindex->get_value(k,err=err)                                                          
 if err ne '' then return,''                                                                      
 if keyword_set(header) then return,struct2fitshead(index) else return,index                      
endif                                                                                             
                                                                                                  
if keyword_set(count) then return,self.omap->get_count()                                          
if not self->has_data(k,err=err) then return,''                                                   
                                                                                                  
;-- index properties                                                                              
                                                                                                  
if keyword_set(filename) then begin                                                               
 if not self->has_index(k,err=err) then return,''                                                 
 return,(self.oindex->get_value(k,err=err)).filename                                              
endif                                                                                             
                                                                                                  
                                                                                                  
if keyword_set(props) then return,self.oprop->get_value(k,err=err)                                
                                                                                                  
if is_struct(extra) then begin                                                                    
 map_prop=self->get_map_prop(k,_extra=extra,err=err)                                              
 if err eq '' then return,map_prop                                                                
 oprop=self.oprop->get_value(k,err=err)                                                           
 plot_prop=oprop->getprop(_extra=extra,err=err)                                                   
 if err eq '' then return,plot_prop                                                               
endif                                                                                             
                                                                                                  
;-- if we get here then property is not supported                                                 
                                                                                                  
if self->get(/verbose_map) then begin                                                             
 message,err,/cont                                                                                
 help,/st,extra                                                                                   
endif                                                                                             
                                                                                                  
return,''                                                                                         
end                                                                                               
                                                                                                  
                                                                                                  
;--------------------------------------------------------------------------                       
;-- get properties of map structure                                                               
                                                                                                  
function map::get_map_prop,k,xc=xc,yc=yc,dx=dx,dy=dy,nx=nx,ny=ny,$                                
              roll_angle=roll_angle,roll_center=roll_center,$                                     
              xyoffset=xyoffset,xrange=xrange,yrange=yrange,drange=drange,$                       
              time=time,data=data,id=id,xunits=xunits,yunits=yunits,dur=dur,$                     
              xp=xp,yp=yp,pixel_size=pixel_size,soho=soho,$                                       
              err=err,_extra=extra                                                                
                                                                                                  
err=''                                                                                            
                                                                                                  
ptr=self.omap->get_item(k,err=err)                                                                
if err ne '' then return,''                                                                       
                                                                                                  
;-- basic properties                                                                              
                                                                                                  
if keyword_set(xc) then return,(*ptr).xc                                                          
if keyword_set(yc) then return,(*ptr).yc                                                          
if keyword_set(dx) then return,(*ptr).dx                                                          
if keyword_set(dy) then return,(*ptr).dy                                                          
if keyword_set(roll_angle) then return,(*ptr).roll_angle                                          
if keyword_set(roll_center) then return,(*ptr).roll_center                                        
if keyword_set(data) then return,(*ptr).data                                                      
                                                                                                  
;-- optional properties                                                                           
                                                                                                  
if keyword_set(soho) then begin                                                                   
 if have_tag(*ptr,'soho') then return,(*ptr).soho else return,0b                                  
endif                                                                                             
if keyword_set(xunits) then return,(*ptr).xunits                                                  
if keyword_set(yunits) then return,(*ptr).yunits                                                  
if keyword_set(dur) then return,(*ptr).dur                                                        
if keyword_set(id) then return,(*ptr).id                                                          
                                                                                                  
;-- derived properties                                                                            
                                                                                                  
if keyword_set(time) then begin                                                                   
 if have_tag(*ptr,'rtime') then return,anytim2utc((*ptr).rtime,/vms) else $                       
  return,anytim2utc((*ptr).time,/vms)                                                             
endif                                                                                             
                                                                                                  
if keyword_set(xyoffset) then return,[(*ptr).xc,(*ptr).yc]                                        
if keyword_set(pixel_size) then return,[(*ptr).dx,(*ptr).dy]                                      
if keyword_set(xp) then return,self->xp(k)                                                        
if keyword_set(yp) then return,self->yp(k)                                                        
if keyword_set(xrange) then return,self->xrange(k)                                                
if keyword_set(yrange) then return,self->yrange(k)                                                
if keyword_set(drange) then return,self->drange(k)                                                
if keyword_set(nx) then return,(size((*ptr).data))[1]                                             
if keyword_set(ny) then return,(size((*ptr).data))[2]                                             
                                                                                                  
err='No such map property'                                                                        
if self->get(/verbose_map) then begin                                                             
 message,err,/cont                                                                                
 help,/st,extra                                                                                   
endif                                                                                             
                                                                                                  
return,'' & end                                                                                   
                                                                                                  
;--------------------------------------------------------------------------                       
;-- create map structure and                                                                      
                                                                                                  
pro map::mk_map,index,data,k,no_copy=no_copy,out_size=out_size,$                                  
                err=err,filename=filename,_extra=extra                                            
                                                                                                  
err=''                                                                                            
                                                                                                  
idim=size(index,/n_dim)                                                                           
if idim ne 1 then begin                                                                           
 err='Input index cannot be 2D'                                                                   
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
;-- recover FITS parameters from INDEX                                                            
                                                                                                  
get_fits_par,index,xc,yc,dx,dy,time=time,roll=roll_angle,rcenter=roll_center,$                    
             id=id,soho=soho,/current,dur=dur                                                     
                                                                 
if err ne '' then return                                                                          
                                                                                                  
ndim=size(data,/n_dim)                                                                            
if (ndim ne 2) then begin                                                                         
 err='Input image not 2D'                                                                         
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
dim=size(data,/dim)                                                                               
nx=dim[0]                                                                                         
ny=dim[1]                                                                                         
                                                                                                  
if (nx ne index.naxis1) or (ny ne index.naxis2) then begin                                        
 err='Input index size does not match data size'                                                  
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
;-- check if rebinning                                                                            
                                                                                                  
if exist(out_size) then begin                                                                     
 msize=[out_size[0],out_size[n_elements(out_size)-1]]                                             
 if (nx ne msize[0]) or (ny ne msize[1]) then begin                                               
  dprint,'% MAP::MK_MAP rebinning to: ',msize[0],msize[1]                                         
  data=congrid(temporary(data),msize[0],msize[1])                                                 
  dx=dx*float(nx)/msize[0]                                                                        
  dy=dy*float(ny)/msize[1]                                                                        
 endif                                                                                            
endif                                                                                             
                                                                                                  
;-- create map structure                                                                          
                                                                                                  
map=make_map(data,xc=xc,yc=yc,dx=dx,dy=dy,time=time,id=id,soho=soho,dur=dur,$                     
             roll_angle=roll_angle,roll_center=roll_center,no_copy=no_copy,$
             _extra=extra)                                                                       
                                                                                                  
;-- insert map into pointer                                                                       
                                                                                                  
if not have_tag(index,'filename') then index=add_tag(index,'','filename')                         
if is_string(filename) then infile=file_break(filename)                                           
if is_string(index.filename) then infile=file_break(index.filename)                               
if is_string(infile) then index.filename=infile                                                   
                                                                                                  
value='Earth.*view'                                                                               
chk=where(stregex(trim2(index.history),value,/bool,/fold) gt 0,count)                             
if (count gt 0) and have_tag(map,'soho') then map.soho=0b                                         
                                                                                                  
self->set,k,map=map,index=index,filename=infile,/no_copy                                          
                                                                                                  
self->update_index,k                                                                              
                                                                                                  
return & end                                                                                      
                                                                                                  
;-----------------------------------------------------------------------                          
                                                                                                  
function map::xrange,k                                                                            
                                                                                                  
xc=self->get(k,/xc)                                                                               
nx=self->get(k,/nx)                                                                               
dx=self->get(k,/dx)                                                                               
                                                                                                  
xmin=min(xc-dx*(nx-1.)/2.)                                                                        
xmax=max(xc+dx*(nx-1.)/2.)                                                                        
return,[xmin,xmax]                                                                                
                                                                                                  
end                                                                                               
                                                                                                  
;-----------------------------------------------------------------------------                    
                                                                                                  
function map::yrange,k                                                                            
                                                                                                  
yc=self->get(k,/yc)                                                                               
ny=self->get(k,/ny)                                                                               
dy=self->get(k,/dy)                                                                               
                                                                                                  
ymin=min(yc-dy*(ny-1.)/2.)                                                                        
ymax=max(yc+dy*(ny-1.)/2.)                                                                        
return,[ymin,ymax]                                                                                
                                                                                                  
end                                                                                               
                                                                                                  
;-----------------------------------------------------------------------------                    
                                                                                                  
function map::drange,k                                                                            
                                                                                                  
if not self->has_data(k,err=err) then return,[0,0]                                                
                                                                                                  
item=self.omap->get_item(k,err=err)                                                               
dmin=min( (*item).data,max=dmax)                                                                  
return,[dmin,dmax]                                                                                
                                                                                                  
end                                                                                               
                                                                                                  
;-------------------------------------------------------------------------------                  
                                                                                                  
function map::xp,k,oned=oned                                                                      
                                                                                                  
xc=self->get(k,/xc)                                                                               
nx=self->get(k,/nx)                                                                               
ny=self->get(k,/ny)                                                                               
dx=self->get(k,/dx)                                                                               
                                                                                                  
if keyword_set(oned) then ny=1                                                                    
                                                                                                  
return,mk_map_xp(xc,dx,nx,ny)                                                                     
                                                                                                  
end                                                                                               
                                                                                                  
;-----------------------------------------------------------------------------------              
                                                                                                  
function map::yp,k,oned=oned                                                                      
                                                                                                  
yc=self->get(k,/yc)                                                                               
dy=self->get(k,/dy)                                                                               
nx=self->get(k,/nx)                                                                               
ny=self->get(k,/ny)                                                                               
                                                                                                  
if keyword_set(oned) then nx=1                                                                    
                                                                                                  
return,mk_map_yp(yc,dy,nx,ny)                                                                     
                                                                                                  
end                                                                                               
                                                                                                  
;----------------------------------------------------------------------------                     
;-- return solar radius (arcsec)                                                                  
                                                                                                  
function map::get_solrad,k,err=err                                                                
                                                                                                  
time=self->get(k,/time,err=err)                                                                   
soho=self->get(k,/soho)                                                                           
                                                                                                  
pb=pb0r(time,soho=soho,/arcsec)                                                                   
                                                                                                  
return,pb[2]                                                                                      
                                                                                                  
end                                                                                               
                                                                                                  
;--------------------------------------------------------------------------                       
;-- zero off disk pixels                                                                          
                                                                                                  
pro map::zero_limb,k,err=err                                                                      
                                                                                                  
if not self->has_data(k,err=err) then begin                                                       
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
xp=self->get(k,/xp)                                                                               
yp=self->get(k,/yp)                                                                               
rad=self->get_solrad(k)                                                                           
map=self->get(k,/map,/no_copy)                                                                    
                                                                                                  
off=where( (temporary(xp)^2+temporary(yp)^2) gt rad^2, count)                                     
if count gt 0 then map.data[off]=0                                                                
self->set,k,map=map,/no_copy,/replace                                                             
                                                                                                  
return & end                                                                                      
                                                                                                  
;----------------------------------------------------------------------------                     
;-- plot map object                                                                               
                                                                                                  
pro map::plot,k,_extra=extra,err_msg=err_msg,status=status,fov=fov
                                                                                                  
err_msg='' & status=0b                                                                            
                                                                                                  
if not self->has_data(k,err=err_msg) then begin                                                   
 message,err_msg,/cont                                                                            
 return                                                                                           
endif                                                                                             
                                                                                                  
;-- override default plotting properties with keyword input                                       
                                                                                                  
if is_struct(extra) then extra=fix_extra(extra,{plot_map})                                        
                                                                                                  
props=(self->get(k,/props))->getprop(/all_props)                                                  
extra=join_struct(extra,props)                                                                    
                                                                                                  
;-- load map color table                                                                                       
                 
if self->get(k,/load_colors) then begin
 device2,get_decomposed=decomp
 tvlct,r0,g0,b0,/get                                                                              
 self->load_ct,k                                                                                 
endif
                                                                                                  
if exist(fov) then fmap=fov                                                                       
if valid_omap(fov) then fmap=fov->get(/map,/no_copy)                                              
                                                                                                  
map=self->get(k,/map,/no_copy,err=err_msg)                                                        

if exist(decomp) then device,decomp=0
plot_map,map,_extra=extra,err_msg=err_msg,status=status,font=0,fov=fmap                           
if exist(decomp) then device,decomp=decomp
self->set,k,map=map,/no_copy,/replace                                                             
if valid_omap(fov) then fov->set,map=fmap,/no_copy,/replace                                       
                                                                                                  
;-- reset original colors                                                                                  
                                                                                                  
if exist(r0) then tvlct,r0,g0,b0                                                        
                                                                                                  
return & end                                                                                      
                                                                                                  
;----------------------------------------------------------------------------                     
;-- save current color table into map         
                                                                                         
pro map::save_ct,k                                                                              
                                                                                                  
tvlct,red,green,blue,/get                                                                         
self->set,k,red=red,green=green,blue=blue,/has_colors,/load_colors                                                           
                                                                                                  
return & end                                      
                                                                                                  
;-------------------------------------------------------------------------                        
;-- load map color table
                                                                                                  
pro map::load_ct,k                                                                     

if self->get(k,/has_colors) then begin
 red=self->get(k,/red) 
 green=self->get(k,/green)
 blue=self->get(k,/blue)
 tvlct,red,green,blue
endif
            
return                                                                                            
                                                                                                  
end                                                                                               
                                                                                                  
;--------------------------------------------------------------------------                       
;-- extract sub-region (function)                                                                 
                                                                                                  
function map::extract,k,_ref_extra=extra                                                          
                                                                                                  
self->clone,cobj,k,_extra=extra                                                                   
cobj->extract,_extra=extra                                                                        
return,cobj                                                                                       
end                                                                                               
                                                                                                  
;--------------------------------------------------------------------------                       
;-- extract sub-region (procedure)                                                                
                                                                                                  
pro map::extract,k,_extra=extra,err=err                                                           
                                                                                                  
err=''                                                                                            
if not self->has_data(k,err=err) then begin                                                       
 message,err,/cont                                                                                
 return                                                                                           
endif                                                                                             
                                                                                                  
map=self->get(k,/map,/no_copy)                                                                    
                                                                                                  
sub_map,map,smap,_extra=extra                                                                     
self->set,k,map=smap,/no_copy,/replace                                                            
self->update_index,k                                                                              
return & end                                                                                      
                                                                                                  
;----------------------------------------------------------------------------                     
;-- rotate map object (function)                                                                  
                                                                                                  
function map::rotate,angle,k,_ref_extra=extra                                                     
                                                                                                  
self->clone,cobj,k,_extra=extra                                                                   
cobj->rotate,angle,_extra=extra                                                                   
return,cobj                                                                                       
                                                                                                  
end                                                                                               
                                                                                                  
                                                                                                  
;----------------------------------------------------------------------------                     
;-- drotate map object (function)                                                                 
                                                                                                  
function map::drotate,duration,k,_ref_extra=extra                                                 
                                                                                                  
self->clone,cobj,k,_extra=extra                                                                   
cobj->drotate,duration,_extra=extra                                                               
return,cobj                                                                                       
                                                                                                  
end                                                                                               
                                                                                                  
;----------------------------------------------------------------------------                     
;-- rotate map object (procedure)                                                                 
                                                                                                  
pro map::rotate,angle,k,_extra=extra,err=err,all=all                                              
                                                                                                  
err=''                                                                                            
                                                                                                  
count=self->get(/count)                                                                           
if is_number(k) then m=k else m=0                                                                 
all=keyword_set(all)                                                                              
istart=m & iend=m                                                                                 
if all then begin                                                                                 
 istart=0 & iend=count-1                                                                          
endif                                                                                             
                                                                                                  
for i=istart,iend do begin                                                                        
                                                                                                  
 if not self->has_data(m,err=err) then begin                                                      
  message,err,/cont                                                                               
  continue                                                                                        
 endif                                                                                            
                                                                                                  
 map=self->get(m,/map,/no_copy)                                                                   
                                                                                                  
 rmap=rot_map(map,angle,_extra=extra,err=err,/full_size)                                          
 if err ne '' then begin                                                                          
  self->set,m,map=map,/no_copy,/replace                                                           
  continue                                                                                        
 endif                                                                                            
                                                                                                  
 self->set,m,map=rmap,/no_copy,/replace                                                           
                                                                                                  
 self->update_history,'Roll correction applied',m                                                 
 self->update_index,m                                                                             
                                                                                                  
endfor                                                                                            
                                                                                                  
status=1b                                                                                         
return & end                                                                                      
                                                                                                  
                                                                                                  
;----------------------------------------------------------------------------                     
;-- solar rotate map object                                                                       
                                                                                                  
pro map::drotate,duration,k,_extra=extra,err=err,all=all                                          
                                                                                                  
err=''                                                                                            
if (1-is_number(k)) then k=0                                                                      
do_all=keyword_set(all)                                                                           
count=self->get(/count)                                                                           
lind=indgen(count)                                                                                
                                                                                                  
;-- check if doing all or one                                                                     
                                                                                                  
if do_all then begin                                                                              
 chk=lind & dcount=count                                                                          
endif else begin                                                                                  
 chk=where(k eq lind,dcount)                                                                      
 if dcount eq 0 then begin                                                                        
  lind=0 & dcount=1                                                                               
 endif                                                                                            
endelse                                                                                           
                                                                                                  
for i=0,dcount-1 do begin                                                                         
 err=''                                                                                           
 j=lind[i]                                                                                        
 if not self->has_data(j,err=err) then begin                                                      
  message,err,/cont                                                                               
  continue                                                                                        
 endif                                                                                            
                                                                                                  
 map=self->get(j,/map,/no_copy)                                                                   
                                                                                                  
 rmap=drot_map(map,duration,_extra=extra,err=err)                                                 
 if err ne '' then begin                                                                          
  self->set,j,map=map,/no_copy,/replace                                                           
  continue                                                                                        
 endif                                                                                            
                                                                                                  
 self->set,j,map=rmap,/no_copy,/replace                                                           
 self->update_index,j                                                                             
endfor                                                                                            
                                                                                                  
return & end                                                                                      
                                                                                                  
;----------------------------------------------------------------------------                     
;-- correct for Earth-L1 distance offset                                                          
                                                                                                  
pro map::earth_correct,k,err=err                                                                  

err=''

;-- disable for now    

return
                                                                                              
count=1 & all=0b                                                                                  
if not is_number(k) then begin                                                                    
 count=self->get(/count)                                                                          
 all=1b                                                                                           
endif                                                                                             
                                                                                                  
verbose=self->get(/verbose_map)                                                                   
for i=0,count-1 do begin                                                                          
                                                                                                  
 if all then j=i else j=k                                                                         
 soho=self->get(j,/soho,err=err)                                                                  
 if err ne '' then continue                                                                       
                                                                                                  
 if not soho then begin                                                                           
  if verbose then message,'Earth-view correction already applied',/cont                           
  continue                                                                                        
 endif                                                                                            
                                                                                                  
 if verbose then message,'Applying Earth-view correction...',/cont                                
                                                                                                  
 map=self->get(j,/map,/no_copy)                                                                   
                                                                                                  
 rfac=soho_fac(map.time)                                                                          
 map.dx=map.dx/rfac                                                                               
 map.dy=map.dy/rfac                                                                               
 if have_tag(map,'soho') then map.soho=0b                                                         
                                                                                                  
 self->set,j,map=map,/no_copy,/replace                                                            
                                                                                                  
;-- update history                                                                                
                                                                                                  
 self->update_history,'Earth-view correction applied',j                                           
 self->update_index,j                                                                             
                                                                                                  
endfor                                                                                            
                                                                                                  
                                                                                                  
return & end                                                                                      
                                                                                                  
;----------------------------------------------------------------------------                     
;-- correct for 180 roll                                                                          
                                                                                                  
pro map::roll_correct,k,err=err,force=force                                                       
                                                                                                  
err=''                                                                                            
count=1 & all=0b                                                                                  
if not is_number(k) then begin                                                                    
 count=self->get(/count)                                                                          
 all=1b                                                                                           
endif                                                                                             
                                                                                                  
force=keyword_set(force)                                                                          
                                                                                                  
applied='180 degree roll correction already applied'                                              
verbose=self->get(/verbose_map)                                                                   
                                                                                                  
for i=0,count-1 do begin                                                                          
                                                                                                  
 if all then j=i else j=k                                                                         
                                                                                                  
 if self->roll_corrected(j) and not force then begin                                              
  if verbose then message,applied,/cont                                                           
  continue                                                                                        
 endif                                                                                            
                                                                                                  
 index=self->get(j,/index)                                                                        
 crota=0.                                                                                         
 if have_tag(index,'crot',pos,/exact) then crota=abs(index.(pos)) else $                          
  if have_tag(index,'crota',pos,/exact) then crota=abs(index.(pos)) else $                        
   if have_tag(index,'sc_roll',pos,/exact) then crota=abs(index.(pos))                            
                                                                                                  
;-- check SOHO roll database                                                                      
                                                                                                  
 soho=fix(self->get(/soho))                                                                       
; if (soho eq 1) then begin                                                                       
;  time=self->get(j,/time)                                                                        
;  soho_roll=get_soho_roll(time)                                                                  
;  correction_applied=(soho_roll eq crota) and (crota ne 180)                                     
; endif else correction_applied=(crota ne 180)                                                    
                                                                                                  
 correction_applied=(crota ne 180)                                                                
                                                                                                  
 if correction_applied then begin                                                                 
  if verbose then message,applied,/cont & continue                                                
 endif                                                                                            
                                                                                                  
 if verbose then begin                                                                            
  message,'Applying 180 degree roll correction...',/cont                                          
 endif                                                                                            
                                                                                                  
 map=self->get(j,/map,/no_copy)                                                                   
 data=rotate(temporary(map.data),2)                                                               
 index=rot_fits_head(index)                                                                       
 map.data=temporary(data)                                                                         
 map.roll_angle=0.                                                                                
 if have_tag(index,'xcen') then map.xc=index.xcen                                                 
 if have_tag(index,'ycen') then map.yc=index.ycen                                                 
 map.roll_center=[map.xc,map.yc]                                                                  
 self->set,j,map=map,index=index,/no_copy,/replace                                                
                                                                                                  
;-- update history                                                                                
                                                                                                  
 self->update_history,'180 degree roll correction applied',j                                      
                                                                                                  
endfor                                                                                            
                                                                                                  
return & end                                                                                      
                                                                                                  
;----------------------------------------------------------------                                 
;-- check for history value                                                                       
                                                                                                  
function map::has_history,value,k                                                                 
                                                                                                  
if is_blank(value) then return,0b                                                                 
                                                                                                  
if not self->has_index(k) then return,0b                                                          
index=self->get(k,/index,err=err)                                                                 
if err ne '' then return,0b                                                                       
if not have_tag(index,'history') then return,0b                                                   
                                                                                                  
chk=where(stregex(trim2(index.history),trim2(value),/bool,/fold) gt 0,count)                      
                                                                                                  
return,count gt 0                                                                                 
                                                                                                  
end                                                                                               
                                                                                                  
;-------------------------------------------------------------------------                        
;-- update history value                                                                          
                                                                                                  
pro map::update_history,value,k                                                                   
if is_blank(value) then return                                                                    
                                                                                                  
index=self->get(k,/index,err=err)                                                                 
if err ne '' then return                                                                          
                                                                                                  
if not have_tag(index,'history') then index=add_tag(index,'','history')                           
history=[rem_blanks(index.history),trim(value)]                                                   
nhist=n_elements(history)                                                                         
if nhist gt 2 then history=[history[0],get_uniq(history[1:nhist-1])]                              
                                                                                                  
index=rep_tag_value(index,history,'history')                                                      
self->set,k,index=index,/replace                                                                  
                                                                                                  
return & end                                                                                      
                                                                                                  
;---------------------------------------------------------------------------                      
;-- check if map is contained in object                                                           
                                                                                                  
function map::has_data,k,err=err                                                                  
                                                                                                  
err=''                                                                                            
if not is_number(k) then k=0                                                                      
ptr=self.omap->get_item(k)                                                                        
have_map=ptr_exist(ptr)                                                                           
if not have_map then err='No map currently saved in index '+trim(k)                               
                                                                                                  
return,have_map                                                                                   
end                                                                                               
                                                                                                  
;---------------------------------------------------------------------------                      
;-- check if index is contained in object                                                         
                                                                                                  
function map::has_index,k,err=err                                                                 
                                                                                                  
err=''                                                                                            
if not is_number(k) then k=0                                                                      
ptr=self.oindex->get_item(k,err=err)                                                              
have_index=ptr_exist(ptr)                                                                         
if not have_index then err='No index currently saved ('+trim(k)+')'                               
return,have_index                                                                                 
                                                                                                  
end                                                                                               
                                                                                                  
;--------------------------------------------------------------------------                       
;-- check if already roll-corrected                                                               
                                                                                                  
function map::roll_corrected,k                                                                    
                                                                                                  
return,self->has_history('Roll correction applied',k)                                             
                                                                                                  
end                                                                                               
                                                                                                  
;--------------------------------------------------------------------------                       
;-- check if written by writefits                                                                 
                                                                                                  
function map::fits_written,k                                                                      
                                                                                                  
return,self->has_history('File written by WRITEFITS',k)                                           
                                                                                                  
end                                                                                               

;------------------------------------------------------------------------
;- remote copy procedure

pro map::get,file,_ref_extra=extra

if is_blank(file) then begin
 message,'Missing string input filename',/cont
 return
endif

out_dir=curdir()
if ~write_dir(out_dir) then out_dir=get_temp_dir()
sock_copy,file,_extra=extra,out_dir=out_dir

return & end
                                                                                                  
;---------------------------------------------------------------------------                      
pro map__define                                                                                   
                                                                                                  
map={map, $                                                                                       
     plot_type:'',$                                                                               
     created:'',$                                                                                 
     verbose_map:0b,$                                                                             
     omap:obj_new(),$                                                                             
     oindex:obj_new(),$                                                                           
     oprop:obj_new(), inherits free_var}                                                                            

                                                                                                  
return                                                                                            
end                                                                                               
