                                                                                         
; Project     : SOHO-CDS                                                                 
;                                                                                        
; Name        : GET_FITS_PAR                                                             
;                                                                                        
; Purpose     : get image parameters from FITS header                                    
;                                                                                        
; Category    : imaging                                                                  
;                                                                                        
; Syntax      : get_fits_par,header,xcen,ycen,dx,dy                                      
;                                                                                        
; Inputs      : HEADER = FITS header                                                     
;                                                                                        
; Outputs     : XCEN,YCEN = image FOV center                                             
;               DX,DY = image pixel spacings                                             
;               NX,NY = image dimensions                                                 
;                                                                                        
; Keywords    : ERR = error string                                                       
;               STC = output in structure format                                         
;               TIME = image time                                                        
;                                                                                        
;                                                                                        
; History     : Written 22 August 1997, D. Zarro, SAC/GSFC                               
;               Modified 16-Feb-2000, Zarro (SM&A/GSFC) - added                          
;               extra checks for non-SSW standard times                                  
;               Modified 20-Aug-2000, Zarro (EIT/GSFC) - moved                           
;               ROLL checks from INDEX2MAP to here                                       
;               Modified 4-Oct-2001, Zarro (LAC/GSFC) - fixed                            
;               potential bugs in roll calculation                                       
;               Modified 20-Dec-02, Zarro (EER/GSFC) - fixed another                     
;               roll issue. Why can't people learn to use FITS standards                 
;               after all these years.                                                   
;               Modified 4-Nov-03, Zarro (L-3/GSFC) - added axis unit check              
;               Modified 21-Mar-04, Zarro (L-3Com/GSFC) - cleaned up                     
;               Modified 29-Mar-05, Zarro (L-3Com/GSFC) - improved check for CROTA       
;               Modified,31-Mar-05, Thompson (L-3Com/GSFC) - added cdelt1,cdelt2 tags if not found
;               Modified 7-Oct-05, R.Bentley (UCL) - cdelt1/cdelt2 to handle
;                case where plate scale is in "solar radii/pixel" ("solrad" - VSM)
;               Modified 29-Mar-06, Zarro (L-3Com/GSFC) - further
;                                                         improved roll check    
;               Modified 3-Sep-06, Zarro (ADNET/GSFC) - added WAVE to ID
;               Modified 28-Mar-07, Zarro (ADNET) - check for valid CRPIXi
;                                                                                        
; Contact     : dzarro@solar.stanford.edu                                                
;-                                                                                       
                                                                                         
pro get_fits_par,header,xcen,ycen,dx,dy,err=err,time=time,stc=stc,nx=nx,$                
                ny=ny,roll=roll,current=current,rcenter=rcenter,id=id,$                  
                dur=dur,soho=soho                                                        
                                                                                         
err=''                                                                                   
                                                                                         
dtype=''                                                                                 
if is_struct(header) then dtype='STC'                                                    
if is_string(header) then dtype='STR'                                                    
if (dtype ne 'STC') and (dtype ne 'STR') then begin                                      
 err='input argument error'                                                              
 pr_syntax,'get_fits_par,header,xcen,ycen,dx,dy'                                         
 return                                                                                  
endif                                                                                    
                                                                                         
;-- check whether FITS header or index structure was input                               
                                                                                         
if dtype eq 'STR' then begin                                                             
 stc=fitshead2struct(header)                                                             
 if err ne '' then return                                                                
 dtype='STC'                                                                             
endif else stc=header                                                                    
                                                                                         
;-- determine OBS time                                                                   
                                                                                         
nimg=n_elements(stc)                                                                     
time=strarr(nimg)                                                                        
for i=0,nimg-1 do begin                                                                  
 get_fits_time,stc[i],dtime,/current                                                     
 time[i]=dtime                                                                           
endfor                                                                                   
if nimg eq 1 then time=time[0]                                                           
                                                                                         
;-- get roll                                                                             
;-- if roll correction was applied, rely on CROTA field for roll                         
;-- if not, check angle keywords                                                         
                                                                                         
delvarx,roll                                                                             
count=0                                                                                  
if have_tag(stc,'history',cindex,/exact) then begin                                      
 history=strlowcase(comdim2(stc.(cindex)))                                               
 chk=where(strpos(history,'roll correction applied') gt -1,count)                        
endif                                                                                    
                                                                                         
if count eq 0 then begin                                                                 
 choices=['sc_roll','p_angle','angle','solar_p0']                                        
 nchoices=n_elements(choices)                                                            
 i=-1                                                                                    
 repeat begin                                                                            
  i=i+1                                                                                  
  if have_tag(stc,choices[i],pindex,/exact) then roll=-stc.(pindex)                      
 endrep until ((i eq (nchoices-1)) or exist(roll))                                       
endif                                                                                    
                                                                                         
;-- check if roll-correction was applied to CROTA                                        
                                                                                         
if exist(roll) then begin                                                                
 have_crot=have_tag(stc,'crota',rindex)                                                  
 if have_crot then begin                                                                 
  roll_c=stc.(rindex[0])                                                                 
  chk=where(roll_c eq 0,count)                                                           
  if count gt 0 then roll[chk]=0.                                                        
 endif                                                                                   
endif                                                                                    
                                                                                         
;-- make FITS/SSW compliant                                                              
                                                                                         
if have_proc('struct2ssw') and (dtype eq 'STC') then begin                               
 if not have_tag(stc,'naxis1',/exact) then stc=call_function('struct2ssw',stc)           
endif                                                                                    
                                                                                         
if not exist(roll) then begin                                                            
 have_crot=have_tag(stc,'crota',rindex)                                                  
 if have_crot then begin                                                                 
  roll=stc.(rindex[0])                                                                   
  dprint,'% GET_FITS_PAR: using CROTA field for roll value'                              
 endif else roll=replicate(0.,nimg)                                                      
endif                                                                                    
roll=comdim2(roll)                                                                       
                                                                                         
;-- determine FITS scaling                                                               
                                                                                         
get_fits_cdelt,stc,dx,dy,time=time,err=err                                               
                                                                                         
if err ne '' then begin                                                                  
 message,err,/cont                                                                       
 err=''                                                                                  
endif                                                                                    
                                                                                         
;-- compute image center                                                                 
                                                                                         
get_fits_cen,stc,xcen,ycen,time=time,err=err                                             
                                                                                         
;-- take care of roll center (def to image center)                                       
                                                                                         
if not exist(rcenter) then begin                                                         
 rcenter=[[xcen],[ycen]]                                                                 
 found_roll_center=0b                                                                    
 roll_center_x=gt_tagval(stc,/crotacn1,found=found_roll_center_x)                        
 roll_center_y=gt_tagval(stc,/crotacn2,found=found_roll_center_y)                        
 found_roll_center=found_roll_center_x and found_roll_center_y                           
 if found_roll_center then rcenter=float([[roll_center_x],[roll_center_y]])              
endif                                                                                    
                                                                                         
;-- determine image dimensions                                                           
                                                                                         
nx=stc.naxis1                                                                            
ny=stc.naxis2                                                                            
                                                                                         
;-- update scaling                                                                       
                                                                                         
if have_tag(stc,'xcen',/exact) then stc.xcen=xcen                                        
if have_tag(stc,'ycen',/exact) then stc.ycen=ycen                                        
                                                                                         
if have_tag(stc,'cdelt1',/exact) then stc.cdelt1=dx else $                               
  stc = add_tag(stc,dx,'cdelt1',/top_level)                                              
if have_tag(stc,'cdelt2',/exact) then stc.cdelt2=dy else $                               
  stc = add_tag(stc,dy,'cdelt2',/top_level)                                              
                                                                                         
if have_tag(stc,'cunit1',/exact) then begin                                     
 cunit1=strtrim(strlowcase(stc(0).cunit1),2)
 if strpos(strlowcase(stc(0).cunit1),'deg') gt -1 then stc.cdelt1=stc.cdelt1*3600.       
 if strpos(strlowcase(stc(0).cunit1),'rad') gt -1 then stc.cdelt1=!radeg*stc.cdelt1*3600.
 if (strpos(cunit1,'solrad') eq 0) and have_tag(stc,'eph_r0') then stc.cdelt1=stc.cdelt1*stc.eph_r0
 stc.cunit1='arcsecs' & dx=stc.cdelt1                                                    
endif                                                                                    
                                                                                         
if have_tag(stc,'cunit2',/exact) then begin                                              
 cunit2=strtrim(strlowcase(stc(0).cunit2),2)
 if strpos(strlowcase(stc(0).cunit2),'deg') gt -1 then stc.cdelt2=stc.cdelt2*3600.       
 if strpos(strlowcase(stc(0).cunit2),'rad') gt -1 then stc.cdelt2=!radeg*stc.cdelt2*3600.
 if (strpos(cunit2,'solrad') eq 0) and have_tag(stc,'eph_r0') then stc.cdelt2=stc.cdelt2*stc.eph_r0
 stc.cunit2='arcsecs' & dy=stc.cdelt2                                                    
endif                                                                                    
                 
if have_tag(stc,'crpix1',/exact) then crpix1=stc.crpix1 else begin
 crpix1=(nx-1)/2.
 stc = add_tag(stc,crpix1,'crpix1',/top_level)                                              
endelse
 
if have_tag(stc,'crpix2',/exact) then crpix2=stc.crpix2 else begin
 crpix2=(ny-1)/2.
 stc = add_tag(stc,crpix2,'crpix2',/top_level)
endelse

if have_tag(stc,'crval1',/exact) then stc.crval1=comp_fits_crval(xcen,dx,nx,stc.crpix1)  
if have_tag(stc,'crval2',/exact) then stc.crval2=comp_fits_crval(ycen,dy,ny,stc.crpix2)  
                                                                                         
;-- check for miscellaneous stuff                                                        
                                                                                         
blank=comdim2(replicate('',nimg))                                                        
                                                                                         
;-- L1-Earth correction for SOHO images                                                  
                                                                                         
soho=comdim2(replicate(0b,nimg))                                                         
if is_string(history) then begin                                                         
 chk=where(strpos(history,'earth') gt -1,count)                                          
 if count eq 0 then begin                                                                
  if tag_exist(stc,'telescop') then soho=(strpos(stc.telescop,'SOHO') gt -1)             
 endif                                                                                   
endif                                                                                    
                                                                                         
;-- exposure time                                                                        
                                                                                         
dur=comdim2(replicate(0.,nimg))                                                          
if tag_exist(stc,'exptime') then dur=float(stc.exptime) else $                           
 if tag_exist(stc,'sht_mdur') then dur=float(stc.sht_mdur)                               
if total(dur) eq 0. then begin                                                           
 if tag_exist(stc,'date_obs') and tag_exist(stc,'date_end') then $                       
  dur=anytim2tai(stc.date_end)-anytim2tai(stc.date_obs)                                  
endif                                                                                    
                                                                                         
if tag_exist(stc,'ORIGIN') then org=trim(stc.origin) else org=blank                      
if tag_exist(stc,'INSTRUME') then inst=trim(stc.instrume) else inst=blank                
if tag_exist(stc,'DETECTOR') then det=trim(stc.detector)  else det=blank                 
if tag_exist(stc,'type') then type=trim(stc.type) else type=blank                        
if tag_exist(stc,'wavelnth') then wave=trim(stc.wavelnth) else wave=blank                
if tag_exist(stc,'wave_len') then wave=trim(stc.wave_len)                                
                                                                                         
temp=inst                                                                                
nob=where(inst ne '',count)                                                              
if count eq 0 then temp=det

if is_string(type) then begin
 nowave=where(wave eq '',count)
 if count gt 0 then wave[nowave]=type[nowave]                                                              
endif

id=strtrim(strcompress(org+' '+temp+' '+wave),2)                                           


                                                                                         
return & end                                                                             
