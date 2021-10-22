;+                                                                              
; Project     : HESSI                                                           
;                                                                               
; Name        : SOCK_COPY                                                       
;                                                                               
; Purpose     : copy file via HTTP sockets                                      
;                                                                               
; Category    : utility system sockets                                          
;                                                                               
; Syntax      : IDL> sock_copy,url_file,outdir=outdir                           
;                                                                               
; Inputs      : URL_FILE = remote file name to copy with URL path               
;                                                                               
; Outputs     : None                                                            
;                                                                               
; Keywords    : OUT_DIR = output directory to copy file                         
;               ERR   = string error message                                    
;               NOWAIT = set to execute copy in background (>= IDL 6.3)         
;                                                                  
; History     : 27-Dec-2001,  D.M. Zarro (EITI/GSFC) - Written                  
;               23-Dec-2005, Zarro (L-3Com/GSFC) - removed COMMON               
;               26-Dec-2005, Zarro (L-3Com/GSFC) 
;                - added /HEAD_FIRST to HTTP->COPY to ensure checking for              
;                  file before copying                               
;               18-May-2006, Zarro (L-3Com/GSFC) 
;                - added IDL-IDL bridge capability for background copying                  
;               10-Nov-2006, Zarro (ADNET/GSFC) 
;                - replaced read_ftp call by ftp object
;                1-Feb-2007, Zarro (ADNET/GSFC)
;                - allowed for vector OUT_DIR
;                                                                               
; Contact     : DZARRO@SOLAR.STANFORD.EDU                                       
;-                                                                              
                                                                                
pro sock_copy,url,err=err,_ref_extra=extra,copy_file=copy_file,$
                nowait=nowait,out_dir=out_dir
                                                                                
err=''                                                                          
if is_blank(url) then begin                                                     
 err='missing input URL'                                                        
 pr_syntax,'sock_copy,url'                                                      
 return                                                                         
endif                                                                           
                                                                                
;-- handoff to IDL-IDL bridge version if /NOWAIT (temporarily disabled)

is_ftp=stregex(url,'ftp://',/bool)                                              
not_ftp=(1-is_ftp[0])
if is_string(extra) then extra=uniq_key(extra)                                                                                

;if keyword_set(nowait) and not_ftp then begin                                               
; sock_download,url,err=err,copy_file=copy_file,_extra=extra                     
; if is_string(err) then message,'reverting to regular SOCK_COPY..',/cont else return
;endif                                                                           
                                   
;-- check if using FTP                                                          
                                                                                
if is_ftp[0] then begin
 ftp=obj_new('ftp')
 m1=systime(/sec)
 ftp->fget,url,err=err,_extra=extra,copy_file=copy_file,out_dir=out_dir,nowait=nowait                                                         
; read_ftp,url,/file,err=err,_extra=extra,copy_file=copy_file 
 m2=systime(/sec)
 dprint,'% Download time (secs) = ',m2-m1
 obj_destroy,ftp                                       
 return                                                                         
endif                                                                           
                                                                                
http=obj_new('http',err=err)                                                    
if err ne '' then return                                                        
                                                                                
http->hset,_extra=extra                                                         
n_url=n_elements(url)                                                           
copy_file=strarr(n_url)    
if is_blank(out_dir) then out_dir=curdir()
if n_elements(out_dir) ne n_url then out_dir=replicate(out_dir[0],n_url)
for i=0,n_url-1 do begin                                                        
 http->copy,url[i],_extra=extra,err=err,copy_file=temp,/verbose,out_dir=out_dir[i]
 copy_file[i]=temp
endfor                                                 
                         
if n_url eq 1 then begin
 copy_file=copy_file[0]
 out_dir=out_dir[0]
endif
                                                                                
obj_destroy,http                                                                
                                                                                
return                                                                          
end                                                                             
