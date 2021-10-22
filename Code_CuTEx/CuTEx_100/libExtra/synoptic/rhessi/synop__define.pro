;+                                                                                           
; Project     : HESSI                                                                        
;                                                                                            
; Name        : SYNOP_DEFINE                                                                 
;                                                                                            
; Purpose     : Define a SYNOP data object                                                   
;                                                                                            
; Category    : GBO Synoptic Objects                                                         
;                                                                                            
; Syntax      : IDL> c=obj_new('synop')                                                      
;                                                                                            
; History     : Written 5 March 2000, D. Zarro, SM&A/GSFC                                     
;               Modified 29 March 2006, Zarro (L-3Com/GSFC) 
;                - fixed remote file checking     
;               Modified 18 Nov 2006, Zarro (ADNET/GSFC) 
;                - removed HTTP as a property (caused persistence problems)
;                                                                                            
; Contact     : dzarro@solar.stanford.edu                                                    
;-                                                                                           
;-----------------------------------------------------------------------------               
;-- init                                                                                     
                                                                                             
function synop::init,_ref_extra=extra                                                        
                                                                                             
success=self->site::init(_extra=extra)                                                       
                                                                                             
if success then begin                                                                        
 user_synop_data=chklog('USER_SYNOP_DATA')                                                   
 if not is_dir(user_synop_data) then begin                                                   
  user_synop_data=curdir()                                                                   
  if not write_dir(user_synop_data) then user_synop_data=get_temp_dir()                      
 endif                                                                                       
 dprint,'% SYNOP::INIT -> USER_SYNOP_DATA: ',user_synop_data                                 
                                                                                             
 self->setprop,org='day',types='images',ext='',ldir=user_synop_data,/pair,$
  err=err,smode=1,mode=1,/bytes,$
  last_time=1b,rhost='beauty.nascom.nasa.gov',plotman=have_proc('plotman__define')                                                       
                                                                                             
 success=err eq ''                                                                           
endif                                                                                        
                                                                                             
if success eq 0 then return,0b                                                               
                                                                                             
success=self->synop_db::init()                                                               
                                                                                             
dprint,'% SYNOP::INIT ',success                                                              
                                                                                             
return,success                                                                               
                                                                                             
end                                                                                          
                                                                                             
;----------------------------------------------------------------------------                
                                                                                             
pro synop::cleanup                                                                           
                                                                                             
self->site::cleanup                                                                          
self->synop_db::cleanup                                                                      
                                                                                             
dprint,'% SYNOP::CLEANUP'                                                                    
                                                                                             
return & end                                                                                 
                                                                                             
;------------------------------------------------------------------------------              
;-- SET method                                                                               
                                                                                             
pro synop::setprop,types=types,last_count=last_count,site=site,err=err,$                     
                   wave=wave,mode=mode,synop_dir=synop_dir,$                                 
                   plotman=plotman,_extra=extra,current=current              
                                                                                             
err=''                                                                                       
                                                                                             
;-- set remote synop root directory name                                                     
                                                                                             
if is_string(synop_dir) then self.synop_dir=synop_dir                                        
                                                                                             
;-- set different search types                                                               
;   (comma delimited string: spectra, images, lightcurve)                                    
                                                                                             
self->set_synop_dirs,types                                                                   
                                                                                             
if is_string(wave) then self.wave=trim(wave)                                                 
if is_number(mode) then self.mode=(0 > mode < 3)                                             
if is_number(plotman) then self.plotman= -1 > plotman < 1                                    
if size(site,/tname) eq 'STRING' then self.site=trim(site)                                   
if is_number(last_count) then self.last_count=last_count                                     
if is_number(current) then self.current = 0b > current < 1b                                  
                                                                                             
;-- set the rest                                                                             
                                                                                             
if is_struct(extra) then self->site::setprop,_extra=extra,err=err                            
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- HTTP copy files                                                                          
                                                                                             
pro synop::hget,ofiles,count=count,err=err,_ref_extra=extra,cancelled=cancelled                  
                                                                                             
;-- ofiles = actual files copied                                                             
                                                                                             
err=''                                                                                       
count=0                                                                                      
ofiles=''                                                                                    
cancelled=0b                                                                                 

;-- check server

server=synop_server(network=network)                                               

if not network then begin
 err='Network connection currently unavailable'                           
 return
endif

http=obj_new('http')
if not obj_valid(http) then return
http->hset,server=server

lfile=self->getprop(/lfile)                                                                  
if is_blank(lfile) then return                                                               
rfile=self->getprop(/rfile)                                                                  
verbose=self->getprop(/verbose)                                                              
clobber=self->getprop(/clobber)                                                              
    
nfiles=n_elements(rfile)                                                                     
for i=0,nfiles-1 do begin                           
 http->copy,rfile[i],lfile[i],_extra=extra,err=err,/slurp,progress=2,/verbose,$            
                   clobber=clobber,status=status,cancelled=cancelled          
 if cancelled then break                                                                    
 if status gt 0 then tfiles=append_arr(tfiles,lfile[i],/no_copy)                                  
endfor                                                                                       
    
obj_destroy,http                                                                                         
count=n_elements(tfiles)                                                                     
if count gt 0 then ofiles=temporary(tfiles)                                                  
                                                                                             
return & end                                                                                 
                                                                                             
;---------------------------------------------------------------------------                 
;-- copy remote files                                                                        
                                                                                             
pro synop::rcopy,files,ofiles,count=count,err=err,cancelled=cancelled,$                      
                 _ref_extra=extra,lfiles=lfiles,bstruct=bstruct
                                                                                             
;-- files = files to copy                                                                    
;-- ofiles = actual files copied                                                             
;-- lfiles = local filenames (not remote)
;-- bstruct = structure with background file names and sizes

if is_struct(bstruct) then begin
 bfiles=bstruct.files
 bsizes=bstruct.sizes                                               
 nback=n_elements(bfiles)
endif else nback=0

ofiles='' & count=0 & cancelled=0b & lfiles='' & err=''                                      
rok=self->is_remote(files,count=rcount,/have_url)
          
if (rcount eq 0) then begin                                                                    
 lfiles=files & return                                                                       
endif

;-- extract remote filenames
                            
doit=where2(rok,rcount,complement=complement,ncomplement=ncomplement)                        
if ncomplement gt 0 then lfiles=files[complement]                                            
if rcount eq 0 then return
rfiles=files[doit]                                                                           

clobber=self->getprop(/clobber)                                                              
ldir=self->getprop(/ldir)                                                              
for i=0,rcount-1 do begin                                                                                                                           
 sock_copy,rfiles[i],out_dir=ldir,_extra=extra,err=err,copy=nfile,/need_size,$
             cancelled=cancelled,clobber=clobber,progress=(i eq 0),$                      
             status=status,/verbose,/nowait,limit=3,nback=nback,rsize=rsize
 if is_string(err) then status=0
 if cancelled then break
 if status gt 0 then begin                                       
  tfiles=append_arr(tfiles,nfile,/no_copy)
  if status eq 2 then begin
   bfiles=append_arr(bfiles,nfile,/no_copy)
   bsizes=append_arr(bsizes,rsize,/no_copy)
   nback=nback+1
  endif
 endif                                                                                       
endfor                                                                                       
                                                                                             
;-- clean-up                                                                                 

if is_string(bfiles) then begin
 bfiles=get_uniq(bfiles,sorder)                                                                                        
 bsizes=bsizes[sorder]
 bstruct={files:bfiles,sizes:bsizes}
endif

count=n_elements(tfiles)                                                                     
if count gt 0 then ofiles=temporary(tfiles)                                                  
                                                                                             
return & end                                                                                 
                                               
;-----------------------------------------------------------------------------               
;--copy specified file(s)                                                                    
                                                                                             
function synop::fetch,file,count=count,err=err,_ref_extra=extra                                  

count=0 & err=''                                                                             
if not is_string(file,cfile) then begin                                                      
 return,''                                                                                   
endif                                                                                        
                                                                                             
;-- check for remote site copies                                                             

self->rcopy,cfile,rfiles,_extra=extra,count=count,err=err,lfiles=lfiles  

if is_blank(lfiles) then return,rfiles                                                       
                                                                                             
;-- copy non-remote sites                                                                    
                                                                                             
rcount=count                                                                                 
self->fbreak,cfile,cdir,cname                                                                
chk=where(trim2(cdir) ne '',scount)                                                          
                                                                                             
if scount eq 0 then return,rfiles                                                            
sfiles=cfile[chk]                                                                            
self->setprop,rfile=sfiles                                                                   
self->copy_file,lfiles,_extra=extra,count=lcount,err=err                                     
                                                                                             
if rcount gt 0 then ofiles=append_arr(lfiles,rfiles,/no_copy) else ofiles=temporary(lfiles)  
                                                                                             
count=n_elements(ofiles)                                                                     
                                                                                             
return,ofiles                                                                                
              
end                                                                                          
                                                                                             
;---------------------------------------------------------------------------                 
;-- get search directories and patterns by:                                                  
;   site (mode=0), type (mode=1), or by sub-type (mode=2)
                                                                                             
function synop::get_search_dirs,patt=patt,err=err                                            

err=''
mode=self->getprop(/mode)
              
if mode eq 3 then begin
 patt='*'
 return,'/'
endif
                                                                               
def_patt=' '                                                                                 
def_dirs=self->list_synop_dirs(/lower)                                                       
count=n_elements(def_dirs)                                                                   
def_patt=replicate(def_patt,count)                                                           
                                                                                             
if (mode eq 1) then begin                                                                      
 sdirs=self->get_synop_dirs()                                                                
 ndirs=n_elements(sdirs)                                                                     
 patt=comdim2(replicate('*.f*t*',ndirs))                                                     
 return,sdirs                                                                                
endif                                                                                        

site_db=self->list_synop_sites()                                                             
if (mode eq 0) then begin                                                                      
 site=self->getprop(/site)                                                                   
 chk=where(site eq site_db.abbr,count)                                                       
 if count eq 0 then begin                                                                    
  err='Unsupported site: "'+site+'"'                                                         
  patt=''                                                                                    
  message,err,/cont                                                                          
  return,''                                                                                  
 endif                                                                                       
endif else begin                                                                             
 wave=self->getprop(/wave)                                                                   
 chk=where(wave eq site_db.wave,count)                                                       
 if count eq 0 then begin                                                                    
  err='Unsupported sub-type: "'+wave+'"'                                                     
  patt=''                                                                                    
  return,''                                                                                  
 endif                                                                                       
endelse                                                                                      
                                                                                             
chk=comdim2(chk)                                                                             
if count gt 0 then begin                                                                     
 output=(site_db.dir)[chk]                                                                   
 rpatt=(site_db.patt)[chk]                                                                   
 rabbr=(site_db.abbr)[chk]                                                                   
 nowild=where( stregex(rpatt,'\*+',/bool) eq 0,count)                                        
 if count gt 0 then rpatt[nowild]='*'+rpatt[nowild]                                          
 patt=rpatt                                                                                  
 chk=get_uniq(output+patt,sorder)                                                            
 sorder=comdim2(sorder)                                                                      
 patt=patt[sorder]                                                                           
 return,output[sorder]                                                                       
endif                                                                                        
                                                                                             
return,def_dirs                                                                              
                                                                                             
end                                                                                          
                                                                                             
;------------------------------------------------------------------------------              
;-- show properties                                                                          
                                                                                             
pro synop::show                                                                              
                                                                                             
self->site::show                                                                             
print,''                                                                                     
print,'SYNOP properties:'                                                                    
print,'-----------------'                                                                    
print,'% synop_dir: ',self.synop_dir                                                         
print,'% types: ',self.types                                                                 
print,'% site: ',self.site                                                                   
print,'% wave: ',self.wave                                                                   
print,'% last_count: ',self.last_count                                                       
print,'% mode: ',self.mode                                                                   
                                                                                             
return & end                                                                                 
                                                                                             
;------------------------------------------------------------------------------              
                                                                                             
pro synop::apply,event                                                                       
                                                                                             
xkill,event.top                                                                              
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- widget base setup                                                                        
                                                                                             
pro synop::config,group=group,no_types=no_types                                              
                                                                                             
current=self->getprop(/current)                                                              
clobber=self->getprop(/clobber)                                                              
cache=self->getprop(/cache)                                                                  
ldir=self->getprop(/ldir)                                                                    
types=self->getprop(/types)                                                                  
last_time=self->getprop(/last_time)                                                          
plotman=self->getprop(/plotman)                                                              
save_config={clobber:clobber,cache:cache,ldir:ldir,$                                         
             types:types,current:current,$                                                   
             last_time:last_time,plotman:plotman}                            
                                                                                             
mk_dfont,bfont=bfont,lfont=lfont                                                             
base=widget_mbase(/column,title="SHOW_SYNOP DOWNLOAD OPTIONS",/modal,group=group,uvalue=self)
                                                                                             
                                                                                             
;-- save time interval                                                                       
                                                                                             
base1=widget_base(base,/column,/frame)                                                       
                                                                                             
trow=widget_base(base1,/row)                                                                 
xmenu2,['Yes','No'],trow,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=tbuttons,uvalue=['yes_save','no_save'],$                                       
      title='Save last search time interval? ',lfont=lfont                                   
widget_control,tbuttons[1-last_time],/set_button                                             
                                     
;-- use caching                                                                              
                                                                                             
crow=widget_base(base1,/row)    
col1=widget_base(crow,/column)
                                                             
xmenu2,['Yes','No'],col1,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=sbuttons,uvalue=['yes_cache','no_cache'],$                                     
      title='Cache search results (recommended for speed)? ',lfont=lfont                     
widget_control,sbuttons[1-cache],/set_button                                                 
        
col2=widget_base(crow,/column)                                                                 
cbutt=widget_button(col2,value='Clear current cache',uvalue='clear',font=lfont)
                                                                                     
;-- choice of data type                                                                      
                                                                                             
if (1-keyword_set(no_types)) then begin                                                      
 supp_types=self->list_synop_dirs()                                                          
 row1=widget_base(base1,/row)                                                                
 xmenu2,supp_types,row1,/row,/exclusive,font=lfont,$                                         
      buttons=dbuttons,uvalue=strlowcase(supp_types),$                                       
      title='Data types to download: ',lfont=lfont                                           
 curr_types=self->get_synop_dirs()                                                           
 val=where_vector(curr_types,supp_types,count)                                               
 if count gt 0 then widget_control,dbuttons[val[0]],/set_button                              
endif                                                                                        
                                                                                             
;-- clobber data?                                                                            
                                                                                             
row3=widget_base(base1,/row)                                                                 
xmenu2,['Yes','No'],row3,/row,/exclusive,font=lfont,/no_rel,$                                
      buttons=cbuttons,uvalue=['yes_clobber','no_clobber'],$                                 
      title='Overwrite existing files when downloading? ',lfont=lfont                        
widget_control,cbuttons[1-clobber],/set_button                                               
                                                                                             
;-- use PLOTMAN?                                                                             
                                                                                             
if have_proc('plotman__define') and (self->getprop(/plotman) ne -1) then begin               
 row3=widget_base(base1,/row)                                                                
 xmenu2,['Yes','No'],row3,/row,/exclusive,font=lfont,/no_rel,$                               
       buttons=cbuttons,uvalue=['yes_plotman','no_plotman'],$                                
       title='Use PLOTMAN for plotting? ',lfont=lfont                                        
 plotman = 0b > plotman < 1b
 widget_control,cbuttons[1-plotman],/set_button                                              
endif                                                                                        
                                                                                             
;-- download directory                                                                       
                                                                                             
;row2=widget_base(base1,/row)                                                                 
;col21=widget_base(row2,/column)                                                              
;dlabel=widget_label(col21,value='Download directory: ',font=lfont,/align_left)               
;col22=widget_base(row2,/row)                                                                 
;dtext=widget_text(col22,value=ldir,xsize=25,/editable,uvalue='directory')                    
;dbutt=widget_button(col22,value='Browse',font=bfont,uvalue='browse')                         
                                                                                             
;-- write to current directory?                                                              
                                                                                             
;row3=widget_base(base1,/row)                                                                 
;xmenu2,['Yes','No'],row3,/row,/exclusive,font=lfont,/no_rel,$                                
;      buttons=cbuttons,uvalue=['yes_current','no_current'],$                                 
;      title='Always download to current working directory?',lfont=lfont                      
;widget_control,cbuttons[1-current],/set_button                                               
      
                                                                                       
row0=widget_base(base,/row,/align_center)                                                    
                                                                                             
doneb=widget_button(row0,value='Apply',uvalue='apply',$                                      
       font=bfont,/frame)                                                                    
cancb=widget_button(row0,value='Cancel',uvalue='cancel',$                                    
       font=bfont,/frame)                                                                    
                                                                                             
;-- share widget id's thru child's uvalue                                                    
                                                                                             
child=widget_info(base,/child)                                                               
;info={dtext:dtext,save_config:save_config}
widget_control,child,set_uvalue=save_config                                                         
                                                                                             
xrealize,base,/center                                                                        
                                                                                             
xmanager,'synop::config',base,event='synop_event'                                            
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- object event handler. Since actual methods cannot be event-handlers,                     
;   we shunt events thru this wrapper                                                        
                                                                                             
pro synop_event,event                                                                        
widget_control,event.top,get_uvalue=object                                                   
if obj_valid(object) then object->synop_event,event                                          
end                                                                                          
                                                                                             
;----------------------------------------------------------------------------                
                                                                                             
pro synop::synop_event,event                                                                 
                                                                                             
widget_control,event.id,get_uvalue=uvalue                                                    
widget_control,event.top,get_uvalue=self                                                     
child=widget_info(event.top,/child)                                                          
widget_control,child,get_uvalue=save_config                                                         
                                                                                             
uvalue=trim(uvalue[0])                                                                       
                     
;-- determine selected SYNOP types                                                           
                                                                                             
tvalue=trim(uvalue[0])                                                                       
self->set_synop_dirs,tvalue                                                                  
  
case tvalue of                                                                               
                                                                                             
 'yes_clobber': self->setprop,clobber=1                                                      
 'no_clobber': self->setprop,clobber=0                                                       
                                                                                             
 'yes_cache': self->setprop,cache=1                                                          
 'no_cache': self->setprop,cache=0                                                           
                                                                                             
 'yes_plotman': self->setprop,plotman=1                                                      
 'no_plotman': self->setprop,plotman=0                                                       
                                                                                             
 'yes_save': self->setprop,last_time=1                                                       
 'no_save': self->setprop,last_time=0                                                        
       
 'yes_copy': self->setprop,copy=1                                                            
 'no_copy':  self->setprop,copy=2                                                            
                                                                                             
 'apply':xkill,event.top                                                                     
      
 'clear': self->list_cache,/clear
                                                                                       
 'cancel': begin                                                                             
   struct_assign,save_config,self,/nozero                                               
   xkill,event.top                                                                           
  end                                                                                        
                                                                                             
 else: return                                                                                
endcase                                                                                      
                                                                                             
return & end                                                                                 
                                                                                             
;-----------------------------------------------------------------------------               
;-- list method (allows multiple SYNOP types)                                                
                                                                                             
pro synop::list,files,_ref_extra=extra,count=count,err=err                                   
         
count=0 & err=''                                                                             
sdirs=self->get_search_dirs(patt=patt,err=err)                                               
if err ne '' then return                                                                     
self->setprop,ftype=patt                                                                     
         
if is_string(sdirs) then begin                                                               
 synop_dir=self->getprop(/synop_dir)                                                         
 self->setprop,topdir=synop_dir+'/'+sdirs                                                    
 self->site::list,files,_extra=extra,count=count,err=err                                     
endif                                                                                        
         
self->setprop,last_count=count                                                               
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- create unique cache id for storing search results                                        
                                                                                             
function synop::get_cache_id                                                                 
                                                                                             
site=self->getprop(/site)                                                                    
wave=self->getprop(/wave)                                                                    
type=self->getprop(/type)                                                                    
mode=self->getprop(/mode)                                                                    
ftype=self->getprop(/ftype)
vso=''                                                                  
if mode eq 3 then begin
 vso='vso_'
 mode=0
endif
opts=[site,type,wave]                                                                        
cache_id=vso+opts[mode]+'_'+ftype                                                                
return, cache_id                                                                             
                                                                                             
end                                                                                          
                                                                                             
;----------------------------------------------------------------------------                
;-- set SYNOP search directories (images, spectra, lightcurves...)                           
                                                                                             
pro synop::set_synop_dirs,dirs                                                               
                                                                                             
in_dirs=self->valid_synop_dirs(dirs)                                                         
if in_dirs[0] eq '' then return                                                              
                                                                                             
self.types=arr2str(in_dirs)                                                                  
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- get SYNOP directories (images, spectra, lightcurves...)                                  
                                                                                             
function synop::get_synop_dirs,count=count                                                   
                                                                                             
sdirs=str2arr(self.types)                                                                    
count=n_elements(sdirs)                                                                      
return,sdirs                                                                                 
                                                                                             
end                                                                                          
                                                                                             
;-----------------------------------------------------------------------------               
;-- get file categories                                                                      
                                                                                             
function synop::get_cats,files                                                               
                                                                                             
if is_blank(files) then return,''                                                            
                                                                                             
nfiles=n_elements(files)                                                                     
cats=replicate('?',nfiles)                                                                   

;-- VSO case

if (self->getprop(/mode) eq 3) then begin
 site=self->getprop(/site)
 names=self->list_vso_names(abbr,cat)
 chk=where(site eq abbr,count)
 if count eq 1 then cats=replicate(cat[chk[0]],nfiles)
 return,cats
endif
                              
self->fbreak,files,dir,names                                                                 
sites=self->list_synop_sites()                                                               
         
for i=0,n_elements(sites)-1 do begin                                                         
 abbr=sites[i].abbr                                                                          
 patt=trim(str_replace(sites[i].patt,'*','.+'))                                              
 reg=abbr+'|'+patt                                                                           
 reg=patt
 chk=where(stregex(files,reg,/fold) gt -1,rcount)                                            
 if rcount gt 0 then cats[chk]=sites[i].wave+'/'+sites[i].dir                                
 left=where(cats eq '?',lcount)                                                              
 if lcount eq 0 then return,cats                                                             
endfor                                                                                       
                                                                                             
return,cats                                                                         
                                                                                             
end                                                                                          

;-------------------------------------------------------------------------
;-- check if remote site

function synop::is_remote_site

site=self->getprop(/site)                                                     
sites=self->list_synop_sites()    
chk=where( (site eq sites.abbr) and (sites.remote ne ''),dcount)       
return,dcount gt 0
end

;---------------------------------------------------------------------------                 
;-- HTTP listing                                                                             
                                                                                             
pro synop::hsearch,files,_ref_extra=extra        
    
if not allow_sockets(_extra=extra) then return

mode=self->getprop(/mode)
remote= ((mode eq 0) and (self->is_remote_site())) or (mode eq 3)
if remote then begin
 self->rsearch,files,_extra=extra                     
endif else begin
 self->lsearch,files,_extra=extra                 
endelse
                                                                                             
return & end                                                                                 
                                                                                             
;----------------------------------------------------------------------------                
;-- search local synoptic directories                                                        
                                                                                             
pro synop::lsearch,files,times=times,sizes=sizes,count=count,err=err
                                                                                             
err='' & files='' & count=0                                                                  
sizes='' & times=-1                                                                          
                                                                                             
;-- skip if searching a remote site                                                          

mode=self->getprop(/mode)                                                                    
remote= ((mode eq 0) and (self->is_remote_site())) or (mode eq 3)
if remote then return

;-- get server and path to data
      
server=synop_server(path=udir,network=network)

if not network then begin
 err='Network connection currently unavailable'                           
 return
endif

http=obj_new('http')
if not obj_valid(http) then return
http->hset,server=server

sdirs=self->get_sdir()                                                                       
rdirs=get_uniq(self->get_search_dirs(patt=rpatt))                                            
delim='/'                                                                                    
cache=self->getprop(/cache)                                                                  

;-- if date is before 15-Dec-04 then check new and old archives
;   if after, just check new.

change_date=anytim2tai('15-dec-04')
old_dir='/synoptic'
new_dir='/ancillary'

for i=0,n_elements(rdirs)-1 do begin                                                         
 for j=0,n_elements(sdirs)-1 do begin                                                        
  udate=ymd2date(sdirs[j],/tai)
  udir=new_dir
  rcount=0
  repeat begin
   rcount=rcount+1
   dprint,'checking '+udir
   done=udir eq old_dir
   root=udir+delim+rdirs[i]+delim+sdirs[j]+delim                                              
   http->extract_href,root,names,sizes=fsizes,count=fcount                    
   if (fcount gt 0) then begin                                                                 
    names=root+temporary(names)                                                               
    temp=append_arr(temp,names,/no_copy)                                                      
    stemp=append_arr(stemp,fsizes,/no_copy)                                                   
   endif
   if (udate gt change_date) or (fcount gt 0) then done=1b else udir=old_dir 
  endrep until done
 endfor                                                                                      
endfor                                                                                       
  
obj_destroy,http
        
count=n_elements(temp)
if count eq 0 then return
                                                                                   
;-- sort
             
files=temporary(temp)                                                                       
sizes=temporary(stemp)                                                                      
if rcount eq 2 then begin
 cfiles=file_break(files)
 sorder=uniq([cfiles],sort([cfiles]))
 files=files[sorder] & sizes=sizes[sorder]
 count=n_elements(files)
endif

;-- parse out patterns

if is_string(rpatt) then begin                                                              
 dprint,'% searching for '+rpatt
 reg=arr2str(rpatt,delim='|')                                                               
 reg=str_replace(reg,'.','\.')                                                              
 reg=str_replace(reg,'*','.*')                                                              
 chk=where(stregex(files,reg,/bool,/fold),count)                                            
 if count eq 0 then begin                                                                   
  files='' & sizes=''                                                                       
  return
 endif
 if count lt n_elements(files) then begin
  files=files[chk]                                                                          
  sizes=sizes[chk]                                                                          
 endif
endif                                                                                        
                                                                                             
;-- parse out times                                                                          
      
times=parse_time(files,count=count,err=err,ss=ss,/tai)                                           
if count eq 0 then begin                                                                    
 files='' & sizes='' & times=-1
 return
endif

if count lt n_elements(files) then begin
 files=files[ss]                                                                            
 times=times[ss]                                                                
 sizes=sizes[ss]    
endif                                                                                       
                                                                                             
return & end                                                                                 
                                                                                             
;-----------------------------------------------------------------------------               
;-- search remote sites                                                                      
                                                                                             
pro synop::rsearch,files,times=times,sizes=sizes,count=count,err=err
                                                                                             
files='' & sizes='' & count=0 & times=-1 & err=''                                            
       
smess='Remote searching not currently supported for this site'
error=0
catch,error
if error ne 0 then begin
 err=smess
 xack,err
 return
endif
 
mode=self->getprop(/mode)                                                                    
remote= ((mode eq 0) and (self->is_remote_site())) or (mode eq 3)
if (1-remote) then return
                                                                                             
tstart=self->getprop(/tstart)                                                               
tend=self->getprop(/tend)                                                                   
rsite=self->getprop(/site)

robj=obj_new(rsite,err=err)                                                                    
if (1-obj_valid(robj)) then begin
 if is_blank(err) then err=smess
 xack,err
 return
endif

if not have_method(robj,'search') then begin
 err=smess
 obj_destroy,robj
 xack,err
 return
endif

round=(mode ne 3)
files=robj->search(tstart,tend,times=times,sizes=sizes,count=count,$
            err=err,_extra=extra,/full,/tai,cache=0,round=round)   

obj_destroy,robj                                                                          
if is_string(err) then xack,err,/suppress                                                 
if count eq 0 then return
                                                                 
return & end                                                                                 
                                                                                             
;------------------------------------------------------------------------------              
;-- SYNOP site structure                                                                     
                                                                                             
pro synop__define                                                                            
                                                                                             
self={synop,synop_dir:'',last_count:0l,mode:0, site:'', wave:'', plotman:0,$                 
      types:'',current:0b,inherits site, inherits synop_db}      
                                                                                             
return & end                                                                                 
