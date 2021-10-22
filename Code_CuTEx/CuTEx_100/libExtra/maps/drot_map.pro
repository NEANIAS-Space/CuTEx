;+
; Project     : SOHO-CDS
;
; Name        : DROT_MAP
;
; Purpose     : Differentially rotate image contained within structure created by MAKE_MAP
;               Can also project image to different orbital angles
; Category    : imaging
;
; Syntax      : rmap=drot_map(map,duration)
;
; Inputs      : MAP = map structure
;               DURATION = hours to rotate by
;
; Outputs     : RMAP = map with projected coordinates
;
; Keywords    : SECONDS = duration units in seconds
;               TIME = time to rotate image to (can be a MAP or INDEX stc)
;               NO_REMAP = don't remap image data (just compute
;               project coords)
;               NO_DATA = same as NO_REMAP, but just return without DATA
;               DAYS = specify duration units in days
;               TAG_ID = tag name or index to project
;               RCENTER = [rxc,ryc] = new rotation center of output
;               image (if rolling)
;               PROJ_RCENTER = project roll center 
;               (used mainly for partial images rolled about center) 
;               TRANS = [xs,ys] = translation of output image [+N, +W]
;               SPACE/RESOLUTION = [dx,dy] = spacing of output image
;               OUTSIZE  = [nx,ny] = size of output image
;               ROLL  = new roll of output image
;               NOLIMB = exclude off limb points
;               MISSING = values to set unrotatable (e.g. off limb) points
;               CENTER = [xc,yc] = new center of output image coords
;               REF_MAP = reference image to map to. 
;                         If entered, then TIME, SPACING, ROLL are inherited
;               FAST = set when one wants to solely shift the entire FOV 
;                      without correcting for the latitudinal dependence of 
;                      the solar differential rotation function.
;                      USE WITH CARE! - results are only meaningful for small
;                      rotation intervals of a few hours for images within
;                      10-20 degrees of the equator.
;               UNROTATED = indicies of unprojected pixels
;               KEEP_CENTER = keep center of output image same as input
;                             [def is to shift to new center]
;               NO_DROTATE = skip diff. rotation part
;               NO_PROJECT = slip projection part
;               RIGID = rotate as rigid body
;               DEGREES = duration is in units of degrees
;               TRACK_CENTER = set to track center of the output image to where the 
;                         center of the input image is rotated to. 
;                         The default is to center the output image on the 
;                         center of the rotated field of view.
;                         This makes a non-subtle difference for full and 
;                         partial FOV images.
;               PRESERVE_AREA = set to increase (or decrease) number of 
;                         output image pixels to preserve input image 
;                         area. Default is to keep original input number
;                         of pixels.
;               B0, L0, RSUN = solar B angle (deg), 
;                         heliographic angle (deg) offset, and
;                         solar radius (arcsecs) of output image
;
; History     : Written 22 November 1997, D. Zarro, SAC/GSFC
;               Modified 27 Sept 1999, Zarro (SM&A/GSFC) -- removed
;               SOHO view correction (now handled by MAP2SOHO)
;               9-Feb-2000, Zarro (SM&A/GSFC) -- added correction
;               for solar rotation of roll center when in fov
;               9 Jan 2001, Kim Tolbert - made onx and ony longwords so onx*ony 
;                 won't overflow
;               20 Aug 2001, Zarro (EITI/GSFC) - added /keep_center and optimized
;               memory
;               21 Aug 2001, Zarro (EITI/GSFC) - added ability to retain 
;               limb pixels for good buddy GLS
;               22 Feb 2003, Zarro (EER/GSFC) - added /RIGID
;               11 Mar 2003, Zarro (EER/GSFC) - added /DEGREES
;               23 Nov 2003, Zarro (L-3/GSFC) - added check for roll values
;                less than 1 degree. Treat these as effectively 0 degree roll.
;               18 May 2004, Zarro (L-3Com/GSFC) - added /TRACK_CENTER, /PRESERVE_AREA
;                8 Jun 2004, Zarro (L-3Com/GSFC) - added XRANGE/YRANGE
;                8 Jan 2005, Zarro (L-3Com/GSFC) - added B0
;                9 Mar 2005, Zarro (L-3Com/GSFC) - added check for integer data
;               10 Oct 2007, Zarro (ADNET) 
;                - added B0, L0, and RSUN to support STEREO
;               20 Nov 2007, Zarro (ADNET)
;                - added PROJ_RCENTER and made /KEEP_CENTER not the default
;
; Contact     : dzarro@solar.stanford.edu
;-

function drot_map,map,duration,_extra=extra,proj_rcenter=proj_rcenter,$
                  time=time,no_remap=no_remap,err=err,ilimb=ilimb,$
                  verbose=verbose,trans=trans,no_project=no_project,$
                  tag_id=tag_id,space=space,resolution=resolution,roll=roll,fast=fast,$
                  rcenter=rcenter,center=center,nolimb=nolimb,$
                  unrotated=unrotated,no_drotate=no_drotate,$
                  ref_map=ref_map,outsize=outsize,keep_center=keep_center,$
                  degrees=degrees,track_center=track_center,$
                  preserve_area=preserve_area,xrange=xrange,yrange=yrange,$
                  b0=b0,l0=l0,rsun=rsun,xp=xp,yp=yp,no_data=no_data,olimb=olimb

if not valid_map(map) then begin
 pr_syntax,'rmap=drot_map(map,duration,[time=time])'
 return,-1
endif

deg_per_day=diff_rot(1,0,/synod,_extra=extra)
sec_per_day=24.*3600.
sec_per_deg=sec_per_day/deg_per_day

if keyword_set(degrees) then begin
 if not exist(duration) then begin
  message,'Enter amount of rotation in degrees',/cont
  dur='' & read,'-> ',dur
  duration=float(dur)
 endif
 tdur=24.*duration/deg_per_day
endif else begin
 if exist(duration) then tdur=duration
endelse

;-- FAST option

if keyword_set(fast) then $
 return,drot_map_fast(map,tdur,_extra=extra)

;-- check keywords

nolimb=keyword_set(nolimb)
verbose=keyword_set(verbose)
proj_rcenter=keyword_set(proj_rcenter)
keep_center=keyword_set(keep_center)
no_drotate=keyword_set(no_drotate)
preserve_area=keyword_set(preserve_area)
track_center=keyword_set(track_center)
if preserve_area then track_center=0b
remap=(1-keyword_set(no_remap))
no_data=keyword_set(no_data)

;-- if REF_MAP entered then use it's TIME, SPACING, CENTER, ROLL, and
;    DIMENSIONS

case 1 of
 valid_map(ref_map): begin
  etime=get_map_time(ref_map[0],/tai)
  unpack_map,ref_map[0],xc=xc,yc=yc,dx=dx,dy=dy,$
   nx=nx,ny=ny,roll_center=droll_center,roll_angle=droll,/no_data
  dspace=[dx,dy]
  dcenter=[xc,yc]
  dsize=[nx,ny]
 end
 valid_time(time): etime=anytim2tai(time)
 valid_map(time): etime=get_map_time(time,/tai)
 else:continue=1
endcase

;-- get solar rotation duration

dtime=get_drot_dur(map,tdur,time=etime,_extra=extra)
cur_time=get_map_time(map,/tai)
 
;-- translate after rotation?

xs=0. & ys=0.
do_trans=n_elements(trans) eq 2
if do_trans then begin
 xs=trans[0] & ys=trans[1]
 do_trans=(xs ne 0.) or (ys ne 0.)
endif

tags=tag_names(map) & ntags=n_elements(tags) & nmp=n_elements(map)
have_roll_center=have_tag(map,'roll_center')
have_view=have_tag(map,'soho')
ntime=n_elements(dtime)

sub_range=valid_range(xrange) and valid_range(yrange)

;-- input data type is less than float, then make it float for better
;   precision

mk_float=size(map.data,/type) lt 4
for i=0,nmp-1 do begin
 pmap=map[i]
 if sub_range then begin
  err=''
  sub_map,map[i],pmap,xrange=xrange,yrange=yrange,err=err
  if is_string(err) then continue
 endif 
 unpack_map,pmap,err=err,dx=dx,dy=dy,$
          nx=nx,ny=ny,roll_angle=curr_roll,xc=xc,yc=yc,roll_center=curr_rcenter,$
          xrange=pxrange,yrange=pyrange,/no_data
 xp=get_map_xp(pmap)
 yp=get_map_yp(pmap)
 
 curr_view=get_map_prop(pmap,/soho,def=0b,/quiet)

;-- check if differentially rotating

 cdur=dtime[i < (ntime-1)]
 if no_drotate then cdur=0.
 dprint,'% duration (sec): ',cdur
 new_time=cur_time[i]+cdur
 do_drot=(new_time ne cur_time[i])

 if do_drot and (cdur gt 180*sec_per_deg) then begin
  message,'Warning, most of Sun will rotate over limb',/cont
 endif

;-- if b0, l0, or rsun are entered, then we are reprojecting solar angles

 do_proj=is_number(b0) or is_number(l0) or is_number(rsun)
 if keyword_set(no_project) then do_proj=0b
 pstart=-1 & pend=-1
 if do_proj or do_drot then begin
  ang=float(pb0r(cur_time[i],soho=curr_view,/arcsec))
  pstart={b0:ang[1],l0:0.,rsun:ang[2]}
  struct_assign,pmap,pstart,/nozero
  if do_drot then begin
   ang=float(pb0r(new_time,soho=curr_view,/arcsec))
   pend={b0:ang[1],l0:0.,rsun:ang[2]}
  endif else pend=pstart
  if is_number(b0) then pend.b0=b0
  if is_number(l0) then pend.l0=l0
  if is_number(rsun) then pend.rsun=rsun
  do_proj=1-match_struct(pstart,pend) 
 endif

;-- check if rolling

 have_roll=((curr_roll mod 360.0) ne 0.) and (abs(curr_roll) gt 1.)
 new_roll=curr_roll

 if exist(droll) then new_roll=droll
 if exist(roll) then new_roll=float(roll)
 roll_diff=new_roll-curr_roll

 do_roll=((roll_diff mod 360.) ne 0.) and (abs(roll_diff) gt 1.)
 new_roll=curr_roll+roll_diff

; if verbose and (i eq 0) then begin
;  if have_roll then $
;   message,'correcting old '+trim(string(curr_roll))+' degree roll',/cont
;  if do_roll then $
;   message,'applying new '+trim(string(new_roll))+' degree roll',/cont
; endif

;-- check if new roll center

 new_rcenter=curr_rcenter
 if n_elements(droll_center) eq 2 then new_rcenter=float(droll_center)
 if n_elements(rcenter) eq 2 then new_rcenter=float(rcenter)
 do_rcenter=0b
 if (do_roll or have_roll) then $
  do_rcenter=((new_rcenter[0] ne curr_rcenter[0]) or $
              (new_rcenter[1] ne curr_rcenter[1]))

;-- check if recentering 
;  (if an array of images, then track relative to first)

 do_center=0
 if (i eq 0) and (nmp gt 1) then new_center=float([xc,yc])
 if n_elements(dcenter) eq 2 then new_center=float(dcenter)
 if n_elements(center) eq 2 then new_center=float(center)
 if exist(new_center) then $
  do_center=(new_center[0] ne xc) or (new_center[1] ne yc)
  
;-- check if rebinning

 if i eq 0 then new_space=[dx,dy]
 if n_elements(dspace) eq 2 then new_space=float(dspace)
 if n_elements(space) eq 2 then new_space=float(space)
 if n_elements(resolution) eq 2 then new_space=float(resolution)
 do_rebin=(new_space[0] ne dx) or (new_space[1] ne dy)

;-- check if resizing

 if i eq 0 then new_size=float([nx,ny])
 if n_elements(dsize) eq 2 then new_size=dsize
 if n_elements(outsize) eq 2 then new_size=float(outsize)
 do_resize=(new_size[0] ne nx) or (new_size[1] ne ny) or preserve_area

 dprint,'%do_drot,do_proj,do_rebin,do_roll,do_center,do_trans,do_rcenter,do_resize: ',$
 do_drot,do_proj,do_rebin,do_roll,do_center,do_trans,do_rcenter,do_resize

 nmap=temporary(pmap)
 
 onx=long(new_size[0]) & ony=long(new_size[1])
 if not have_tag(nmap,'roll_angle') then nmap=add_tag(nmap,0.,'roll_angle',index='id')
 if not have_roll_center then nmap=add_tag(nmap,[xc,yc],'roll_center',index='roll_angle')
 if not have_view then nmap=add_tag(nmap,curr_view,'soho')

 if verbose then begin
  help,/st,anytim2utc(cur_time[i],/vms),pstart
  help,/st,anytim2utc(new_time,/vms),pend
  help,curr_roll,new_roll
  print,'CURR_RCENTER',curr_rcenter
  print,'NEW_RCENTER',new_rcenter
 endif

 if (1-do_proj) and (1-do_drot) and $
    (1-do_rcenter) and (1-do_trans) and $
    (1-do_rebin) and (1-do_roll) and $
    (1-do_center) and (1-do_resize) then begin
  if no_data then return,-1
  message,'Nothing to project',/cont
  goto,done
 endif

;-- get the before and after solar radii since we will need these to
;   flag offlimb points when rotating or projecting.

 if do_proj or do_drot then begin
  sol_rad1=pstart.rsun
  sol_rad2=pend.rsun
 endif

 if do_roll then begin
  nmap.roll_angle=new_roll
  nmap.roll_center=new_rcenter
 endif else begin
  if have_roll then begin
   nmap.roll_angle=curr_roll

;-- if image is rolled and roll-center is within image, then
;   project roll-center if /proj_rcenter

   roll_in_image= ((curr_rcenter[0] le max(pxrange)) and $
                   (curr_rcenter[0] ge min(pxrange))) or $
                  ((curr_rcenter[1] le max(pyrange)) and $
                   (curr_rcenter[1] ge min(pyrange)))
   if roll_in_image and (do_proj or do_drot) and proj_rcenter then begin
    if verbose and (i eq 0)  then message,'projecting roll center',/cont
    temp=rot_xy(curr_rcenter[0],curr_rcenter[1],tstart=cur_time[i],$
         tend=new_time,_extra=extra,/sphere,$
         vstart=pstart,vend=pend)
    drot_rcenter=reform(temp)
   endif else drot_rcenter=curr_rcenter
   nmap.roll_center=drot_rcenter
  endif
 endelse

;-- correct current roll before projecting

 xr=xp & yr=yp
 if have_roll then roll_xy,xr,yr,-curr_roll,xr,yr,center=curr_rcenter
 
;-- flag offlimb pixels

 icount=0 & ocount=0 & olimb=-1 & fsize=long(nx)*long(ny) & fcount=fsize
 ilimb=-1
 if do_proj or do_drot then begin
  rad1=sqrt(xr^2+yr^2)
  ilimb=where(rad1 gt sol_rad1,icount)
  if icount eq fcount then begin
   err='All points off limb, cannot project'
   message,err,/cont
   if no_data then return,-1 
   goto,done
  endif
 
;-- apply solar rotation/projection

  xr=reform(temporary(xr),fsize)
  yr=reform(temporary(yr),fsize)
  rcor=rot_xy(xr,yr,tstart=cur_time[i],tend=new_time,$
              _extra=extra,/sphere,vstart=pstart,vend=pend,err=err)
  if is_string(err) then begin
   message,err,/cont
   if no_data then return,-1 
   goto,done
  endif
  sz=size(rcor)
  if sz[2] eq 2 then rcor=transpose(temporary(rcor))
 
  xr=reform(rcor[0,*],nx,ny)
  yr=reform(rcor[1,*],nx,ny)

;-- flag pixels that projected over limb

  rad2=sqrt(xr^2+yr^2)
  olimb=where(rad2 gt sol_rad2,ocount)
  if ocount eq fsize then begin
   err='All points projected off limb'
   message,err,/cont
   if no_data then return,-1 
   goto,done
  endif

;-- determine valid pixels still on disk

  fov=where( ((rad1 le sol_rad1) and (rad2 le sol_rad2)),fcount)
   
  if fcount eq 0 then begin
   err='All points projected outside original FOV'
   message,err,/cont
   if no_data then return,-1 
   goto,done
  endif

 endif

;-- apply translation

 xr=temporary(xr)+xs
 yr=temporary(yr)+ys

;-- apply roll
  
 if do_roll then roll_xy,xr,yr,new_roll,xr,yr,center=new_rcenter else $
  if have_roll then roll_xy,xr,yr,curr_roll,xr,yr,center=drot_rcenter

;-- return if just need coordinates
 
 if no_data then begin
  xp=temporary(xr)
  yp=temporary(yr)
  return,-1
 endif

;-- update map properties 
;   (if not remapping pixels, save in old format to preserve irregular 
;    coordinates)

 if not remap then nmap=mk_map_old(nmap)
 nmap=repack_map(nmap,xr,yr,/no_copy)
  
;-- remap image

 if remap then begin

;-- first make a regularized grid using only pixels that are still in fov
;   (i.e. limb pixels and disk pixels that haven't projected over limb)

  if keep_center then new_center=get_map_prop(map[i],/center)

;-- track FOV center 

  if track_center and (do_proj or do_drot) then begin
   xcen=xc  & ycen=yc
   if have_roll then roll_xy,xcen,ycen,-curr_roll,xcen,ycen,center=curr_rcenter
   ncenter=rot_xy(xcen,ycen,tstart=cur_time[i],tend=new_time,$
           _extra=extra,/sphere,$
           vstart=pstart,vend=pend)
   ncenter=reform(ncenter)
   xcen=ncenter[0] & ycen=ncenter[1]
   if do_roll then roll_xy,xcen,ycen,new_roll,xcen,ycen,center=new_rcenter else $
    if have_roll then roll_xy,xcen,ycen,curr_roll,xcen,ycen,center=drot_rcenter
   new_center=[xcen,ycen]
  endif

  if (fcount lt fsize) and exist(fov) then begin
   xr=xr[fov] & yr=yr[fov]
  endif
  
  grid_xy,xr,yr,gx,gy,space=new_space,center=new_center,size=new_size,$
                      preserve_area=preserve_area
  onx=new_size[0] & ony=new_size[1]
  do_resize=(onx ne nx) or (ony ne ny)

;-- project grid points back to find where each point came from

  nmap=repack_map(nmap,gx,gy,/no_copy)
  xr=temporary(gx)
  yr=temporary(gy)

;-- roll back 

  if do_roll then roll_xy,xr,yr,-new_roll,xr,yr,center=new_rcenter else $
   if have_roll then roll_xy,xr,yr,-curr_roll,xr,yr,center=drot_rcenter

;-- shift back

  xr=temporary(xr)-xs
  yr=temporary(yr)-ys

;-- project backwards

  icount2=0 & ocount2=0

  if do_proj or do_drot then begin

;-- flag projected limb pixels 

   rad2=sqrt(xr^2+yr^2)
   olimb2=where(rad2 gt sol_rad2,ocount2)

   xr=reform(temporary(xr),onx*ony)
   yr=reform(temporary(yr),onx*ony)
   rcor=rot_xy(xr,yr,tstart=new_time,tend=cur_time[i],$
               _extra=extra,/sphere,vstart=pend,vend=pstart,err=err)
   if is_string(err) then begin
    message,err,/cont
    goto,done
   endif
   sz=size(rcor)
   if sz[2] eq 2 then rcor=transpose(temporary(rcor))
   xr=reform(rcor[0,*],onx,ony)
   yr=reform(rcor[1,*],onx,ony)

  endif

;-- roll back to initial roll

  if have_roll then roll_xy,xr,yr,curr_roll,xr,yr,center=curr_rcenter

;-- remap here

  if mk_float then nmap=rep_tag_value(nmap,float(nmap.data),'data')

  temp=interp2d(nmap.data,xp,yp,xr,yr,_extra=extra,missing=0.)  
  if do_resize then nmap=rep_tag_value(nmap,temp,'data',/no_copy) else $
   nmap.data=temporary(temp)

  if do_proj or do_drot then begin
   dmin=min(nmap.data,max=dmax)
   if (dmin eq dmax) and (dmin eq 0.) then begin
    err='Image projected out of field of view'
    message,err,/cont
   endif
  endif
 endif

done:

 if do_drot then nmap=rep_tag_value(nmap,anytim2utc(new_time,/vms),'rtime')
 if is_struct(pend) then begin
  if have_tag(pend,'b0') then nmap=rep_tag_value(nmap,pend.b0,'b0')
  if have_tag(pend,'l0') then nmap=rep_tag_value(nmap,pend.l0,'l0')
  if have_tag(pend,'rsun') then nmap=rep_tag_value(nmap,pend.rsun,'rsun')
 endif

 if not exist(rmap) then rmap=temporary(nmap) else begin
  c1=valid_map(rmap,old=rold)
  c2=valid_map(nmap,old=nold)
  if nold ne rold then begin
   if rold then nmap=mk_map_old(nmap) else nmap=mk_map_new(nmap)
  endif
  rmap=merge_struct(rmap,temporary(nmap))
 endelse
endfor

if exist(olimb2) then olimb=temporary(olimb2)
delvarx,xp,yp,xr,yr,gx,gy,rad2,rad1
delvarx,ilimb2,nmap

if not valid_map(rmap) then rmap=-1

return,rmap & end
