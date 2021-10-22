;+
; Project     : SOHO-CDS
;
; Name        : REBIN_MAP
;
; Purpose     : Rebin an image map to new dimensions
;
; Category    : imaging
;
; Explanation : Rebin a map to user-specified dimensions and
;               compute new output pixel spacings
;
; Syntax      : gmap=rebin_map(map,gx,gy)
;
; Inputs      : MAP = image map structure
;               GX,GY = new dimensions
;
; Outputs     : GMAP = rebinned map
;
; History     : Written 22 August 1997, D. Zarro, ARC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function rebin_map,map,gx,gy,err=err,_extra=extra

on_error,1

;-- check inputs (valid map & dimensions)

if (not valid_map(map,old=old)) or (not exist(gx)) then begin
 pr_syntax,'gmap=rebin_map(map,gx,gy)'
 if exist(map) then return,map else return,-1
endif
if not exist(gy) then gy=gx

for i=0,n_elements(map)-1 do begin
 err=''
 unpack_map,map(i),dx=dx,dy=dy,nx=nx,ny=ny,xc=xc,yc=yc
 if (gx eq nx) and (gy eq ny) then begin
  message,'no rebinning necessary',/cont
  gmap=merge_struct(gmap,map(i))
 endif else begin
  tmap=rep_tag_value(map(i),congrid(map(i).data,gx,gy,_extra=extra),'data',/no_copy)
  dx=dx*nx/gx
  dy=dy*ny/gy
  xp=mk_map_xp(xc,dx,gx,gy)
  yp=mk_map_yp(yc,dy,gx,gy)
  tmap=repack_map(tmap,xp,yp)
  gmap=merge_struct(gmap,tmap)
 endelse
endfor

return,gmap & end

