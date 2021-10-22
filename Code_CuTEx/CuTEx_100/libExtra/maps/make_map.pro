;+
; Project     : SOHO-CDS
;
; Name        : MAKE_MAP
;
; Purpose     : Make an image map 
;
; Category    : imaging
;
; Syntax      : map=make_map(data)
;
; Inputs      : DATA = 2d data array
;
; Outputs     : 
;               MAP ={data:data,xc:xc,yc:yc,dx:dx,dy:dy,time:time}
;               where,
;               DATA  = 2d image array
;
;               or old format (which is more memory intensive),
;
;               MAP ={data:data,xp:xp,yp:yp,time:time}
;               where,
;               XP,YP = 2d cartesian coordinate arrays (old format)
;
;               OLD_FORMAT is used if XC and YC
;               are 2d arrays of coordinates for the center of each pixel.
;
; Opt. Outputs: None
;
; Keywords    : XC,YC = center of image
;               DX,DY = pixel spacing in X and Y directions
;               TIME = image time
;               SUB   = [x1,x2,y1,y2] = indicies of sub-array to extract
;               DATA_UNITS = units of subarray in data units [def=pixel number]
;               OLD_FORMAT = use old .xp, .yp format
;               ROLL_ANGLE = image roll (deg clockwise from N)
;               ROLL_CENTER = roll center
;               FOV = same as SUB, but with /DATA_UNITS
;               NO_COPY= set to not make new copy of data
;               BYTE_SCALE = bytescale data
;
; History     : Written 22 October 1996, D. Zarro, ARC/GSFC
;               Modified 3 May 1999, Zarro (SM&A/GSFC)
;                - fixed roll angle definition. Roll angle measures 
;                  degrees clockwise that the image is rolled from 
;                  solar North.
;               Modified 10 August 1999, Zarro (SM&A/GSFC)
;                - included ROLL and ROLL_CENTER properties, even
;                  if roll was zero.
;               Modified 12 Sept 2001, Zarro (EITI/GSFC)
;                - added /NO_COPY and replaced call to JOIN_STRUCT
;                  with faster call to PAIR_STRUCT
;
; Contact     : dzarro@solar.stanford.edu
;-

function make_map,data,xc=xcen,yc=ycen,dx=dx,dy=dy,time=time,soho=soho,$
                  sub=sub,roll_angle=roll_angle,old_format=old_format,id=id,$
                  data_units=data_units,err=err,roll_center=roll_center,$
                  _extra=extra,fov=fov,xunits=xunits,yunits=yunits,$
                  dur=dur,no_copy=no_copy,byte_scale=byte_scale

;-- check input image

map=0
err=''
sz=size(data)
if sz(0) ne 2 then begin
 err='Input image must be 2-d'
 message,err,/cont
 return,0
endif

if n_params() ne 1 then begin
 pr_syntax,'map=make_map(data,xc=xcen,yc=ycen,dx=dx,dy=dy)'
 return,0
endif

;-- check time

stime=anytim2utc(time,/vms,err=err)
if err ne '' then get_utc,stime,/vms

err=''

;-- some defaults (origin center, unit pixel scale)

if not exist(xcen) then xcen=0.
if not exist(ycen) then ycen=0
if not exist(dx) then dx=1.
if not exist(dy) then dy=dx
if not exist(id) then id=''
id=comdim2(id)
if not exist(dur) then dur=0.
if is_blank(xunits) then xunits='arcsecs'  
if is_blank(yunits) then yunits='arcsecs'  
if is_number(soho) then soho=0b > soho < 1b else soho=0b

nx=sz(1) & ny=sz(2)
dx=float(dx(0)) & dy=float(dy(0))
if dx eq 0. then dx=1.
if dy eq 0. then dy=1.
dx=abs(dx) & dy=abs(dy)

;-- extract subarray?

data_units=keyword_set(data_units)
old_format=keyword_set(old_format)
if n_elements(fov) eq 4 then begin
 subreg=fov & data_units=1
endif else begin
 if n_elements(sub) eq 4 then subreg=sub
endelse

sx=size(xcen) & sy=size(ycen)
old_input_format= (sx(0) eq 2) and (sy(0) eq 2) and $
                  (sx(1) eq nx) and (sy(1) eq nx) and $
                  (sx(2) eq ny) and (sy(2) eq ny)

do_sub=n_elements(subreg) eq 4

if old_input_format then begin
 xp=xcen                
 yp=ycen
 xc=get_arr_center(xp,dx=dx)
 yc=get_arr_center(yp,dy=dy)
endif else begin
 xc=float(xcen(0)) & yc=float(ycen(0))
 if do_sub then begin
  xp=mk_map_xp(xc,dx,nx,ny)
  yp=mk_map_yp(yc,dy,nx,ny)
 endif
endelse

;-- start building map structure

no_copy=keyword_set(no_copy)

bscale=keyword_set(byte_scale)
if bscale then if datatype(bdata) ne 'BYT' then $
 bdata=cscale(data,no_copy=no_copy)

dprint,'% MAKE_MAP: no_copy, bscale ',no_copy,bscale
if bscale then begin
 prefix='map={data:temporary(bdata)'
endif else begin
 if keyword_set(no_copy) then prefix='map={data:temporary(data)' else $
  prefix='map={data:data'
endelse

if do_sub then begin
 if data_units then begin
  sub=get_map_region(xp,yp,subreg,count=count,err=err)
  if (count gt 0) then begin
   if ( (sub(1)-sub(0)) lt 2) or ((sub(3)-sub(2)) lt 2) then begin
    err='Insufficient data points to produce map'
    return,0
   endif
   xp=temporary(xp(sub(0):sub(1),sub(2):sub(3)))
   yp=temporary(yp(sub(0):sub(1),sub(2):sub(3)))
  endif else begin
   err='Zero data points within selected subregion'
   message,err,/cont
   return,0
  endelse
  if bscale then sub_data=bdata(sub(0):sub(1),sub(2):sub(3)) else $
   sub_data=data(sub(0):sub(1),sub(2):sub(3))
 endif else begin
  if min(subreg) lt 0 then begin
   err='Negative index values -> use /data for such values'
   message,err,/cont
   return,0
  endif
  x1=subreg(0) < (nx-1) & x2=subreg(1) < (nx-1)
  y1=subreg(2) < (ny-1) & y2=subreg(3) < (ny-1)
  xp=temporary(xp(x1:x2,y1:y2))
  yp=temporary(yp(x1:x2,y1:y2))
  if bscale then sub_data=bdata(x1:x2,y1:y2) else $
   sub_data=data(x1:x2,y1:y2)
 endelse
 xc=get_arr_center(xp)
 yc=get_arr_center(yp)
 prefix='map={data:temporary(sub_data)'
endif

;-- add properties

if old_format then begin
 prefix=prefix+',xp:xp,yp:yp'
endif else begin
 prefix=prefix+',xc:xc,yc:yc,dx:dx,dy:dy'
endelse

state=prefix+',time:stime,id:id'

;-- treat roll

if not exist(roll_angle) then roll_angle=0.
if (n_elements(roll_center) ne 2) then roll_center=[xc,yc]  

state=state+',roll_angle:float(roll_angle mod 360.),roll_center:reform(float(roll_center))'

state=state+',dur:dur,xunits:xunits,yunits:yunits,soho:soho'

if datatype(extra) eq 'STC' then begin
 state=state+','+pair_struct(extra,'extra',/colon)
endif
state=state+'}'

s=execute(state)
return,map

end



