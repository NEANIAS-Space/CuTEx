;+
; Project     : SOHO-CDS
;
; Name        : SHIFT_MAP
;
; Purpose     : shift an image map 
;
; Category    : imaging
;
; Explanation : translate a map in x- and y-directions by moving
;               it's centroid
;
; Syntax      : smap=shift_map(map,sx,sy)
;;
; Inputs      : MAP = image map structure
;               SX,SY = shift values in x- and y- directions (+W, +N)
;
; Outputs     : SMAP = shifted map
;
; Opt. Outputs: None
;
; Keywords    : XC = new X-center (ignored if SX entered)
;               YC = new Y-center (ignored if SY entered)
;
; History     : Written 12 May 1998, D. Zarro, SAC/GSFC
;               Modified 22 March 2000, Zarro (SM&A/GSFC) -- added
;               check for ROLL_CENTER
;
; Contact     : dzarro@solar.stanford.edu
;-

function shift_map,map,sx,sy,err=err,xc=xc,yc=yc,no_copy=no_copy

on_error,1
err=''

;-- check inputs 

if not valid_map(map,old=old) or (not exist(sx) and not exist(xc)) then begin
 pr_syntax,'smap=shift_map(map,sx,sy,[xc=xc,yc=yc])'
 if exist(map) then return,map else return,-1
endif

err=''
if keyword_set(no_copy) then tmap=copy_var(map) else tmap=map

pxc=get_map_prop(tmap,/xc)
pyc=get_map_prop(tmap,/yc)

xshift=0.
if exist(sx) then xshift=float(sx) else $
 if exist(xc) then xshift=xc-pxc

yshift=0.
if exist(sy) then yshift=float(sy) else $
 if exist(yc) then yshift=yc-pyc

if (xshift ne 0.) or (yshift ne 0.) then begin
 if old then begin
  tmap.xp=temporary(tmap.xp)+xshift
  tmap.yp=temporary(tmap.yp)+yshift
 endif else begin
  tmap.xc=tmap.xc+xshift
  tmap.yc=tmap.yc+yshift
 endelse
 if have_tag(tmap,'roll_center') then begin  
  tmap.roll_center=tmap.roll_center+[xshift,yshift]
 endif
endif

return,tmap
end
