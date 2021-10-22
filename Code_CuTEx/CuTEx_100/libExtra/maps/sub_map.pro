;+
; Project     : SOHO-CDS
;
; Name        : SUB_MAP
;
; Purpose     : get subimage of an image map
;
; Category    : imaging
;
; Explanation : Calling this procedure with /plot will invoke PLOT_MAP
;               and sub region is selected with a cursor.
;               In this case, XRANGE and YRANGE are returned as outputs.
;               Alternatively, XRANGE and YRANGE can be input variables
;               and will be used for extraction.
;               If REF_MAP is a valid map, then its XRANGE, YRANGE are used
;               for extraction
;
; Syntax      : sub_map,map
;
; Inputs      : MAP = map structure created by MAKE_MAP
;
; Opt. Inputs : None
;
; Outputs     : SMAP = subimage of map
;
; Opt. Outputs: None
;
; Keywords    : NOPLOT = don't invoke PLOT_MAP for graphical selection
;               XRANGE = [x1,x2] = min/max x-coord's (data units)
;               YRANGE = [y1,y2] = min/max y-coord's
;               IRANGE =[x1,x2,y1,y2] = output subarray of indicies (pixel units)
;               INDEX = map index for multiple maps (if plotting)
;               REF_MAP = reference map for inferring XRANGE, YRANGE
;               PRESERVE = output dimensions of SMAP same as REF_MAP
;               PIXEL = XRANGE/YRANGE are in pixel units
;               INIT = start all over again
;
; History     : Written 22 November 1997, D. Zarro, SAC/GSFC
;               Modified 10 June 2003, Zarro (EER/GSFC) - changed SUB keyword
;               to IRANGE
;
; Contact     : dzarro@solar.stanford.edu
;-

pro sub_map,map,smap,xrange=xrange,yrange=yrange,ref_map=ref_map,preserve=preserve,$
            noplot=noplot,err=err,irange=irange,fov=fov,pixel=pixel,$
            rubber=rubber,index=index,_extra=extra,size=gsize,init=init,$
            sub=sub

err=''

if not valid_map(map,err=err) then begin
 pr_syntax,'sub_map,map,smap,[xrange=xrange,yrange=yrange])'
 return
endif

if (not valid_map(ref_map)) and valid_map(fov) then ref_map=fov

use_cursor=1
do_plot=(1-keyword_set(noplot))
use_range=( valid_range(xrange) and valid_range(yrange) ) or $
            valid_map(ref_map) or keyword_set(pixel)

if keyword_set(init) then use_range=0

if use_range then begin
 do_plot=0 & use_cursor=0
endif

;-- check for open window

if use_cursor and (not do_plot) then begin
 device,window=ow
 open=where(ow ne 0,count)
 if count eq 0 then do_plot=1 else wshow
endif

;-- get data coordinates to extract

nmap=n_elements(map)
if not exist(index) then index=0 else index=index < (nmap-1)

if do_plot then begin
 plot_map,map(index),err=err,_extra=extra
 if err ne '' then return
endif

if use_cursor then begin
 region=get_sub_region(rubber=rubber)
 xrange=[region(0),region(1)]
 yrange=[region(2),region(3)]
endif else begin
 if valid_map(ref_map) then begin
  xrange=get_map_prop(ref_map(0),/xr)
  yrange=get_map_prop(ref_map(0),/yr)
 endif
endelse

;-- get pixel indicies

if (1-keyword_set(pixel)) then begin

 pic=get_map_sub(map(index),xrange=xrange,yrange=yrange,arange=arange,$
                irange=irange,count=count,err=err)

 sub=irange
 if count le 2 then begin
  err='Insufficient points in sub-region'
  message,err,/cont
  return
 endif

 x1=irange(0)
 x2=irange(1)
 y1=irange(2)
 y2=irange(3)

dprint,'% arange: ',arange

endif else begin
 if (not valid_range(xrange)) and (not valid_range(yrange)) then begin
  smap=map
  return
 endif
 nx=data_chk(map(0).data,/nx)
 ny=data_chk(map(0).data,/ny)
 if (not valid_range(xrange)) then xrange=[0,nx-1]
 if (not valid_range(yrange)) then yrange=[0,ny-1]
 x1=min(xrange,max=x2) > 0l
 x2=x2 < (nx-1)
 y1=min(yrange,max=y2) > 0l
 y2=y2 < (ny-1)
endelse

;-- keep same pixel dimensions as REF_MAP map

if valid_map(ref_map) and keyword_set(preserve) then begin
 nx=get_map_prop(ref_map,/nx)
 ny=get_map_prop(ref_map,/ny)
 fspace=get_map_prop(ref_map,/space)
 dspace=get_map_prop(map(index),/space)
 d1=dspace(0)-fspace(0)
 d2=dspace(1)-fspace(1)
 if (abs(d1) ge dspace(0)) or (abs(d2) ge dspace(1)) then $
  message,'warning, input image and reference image have different pixel spacings',/cont
 x2=x1+nx-1
 y2=y1+ny-1
endif

delvarx,smap

for i=0,nmap-1 do begin
 tmap=rep_tag_value(map(i),(map(i).data)(x1:x2,y1:y2),'data')
 xp=get_map_prop(map(i),/xp) & yp=get_map_prop(map(i),/yp)
 tmap=repack_map(tmap,xp(x1:x2,y1:y2),yp(x1:x2,y1:y2),/no_copy)
 xc=get_map_prop(tmap,/xc)
 yc=get_map_prop(tmap,/yc)
 if map(i).roll_angle ne 0. then begin
  tmap.roll_angle=map(i).roll_angle
  tmap.roll_center=map(i).roll_center
 endif else begin
  tmap.roll_center=[xc,yc]
 endelse
 smap=merge_struct(smap,tmap)
endfor

return
end

