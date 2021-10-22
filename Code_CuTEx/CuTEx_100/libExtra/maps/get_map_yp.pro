;+
; Project     : SOHO-CDS
;
; Name        : GET_MAP_YP
;
; Purpose     : extract Y-coordinate arrays of map
;
; Category    : imaging
;
; Syntax      : yp=get_map_yp(map)
;
; Inputs      : MAP = image map
;
; Outputs     : YP = 2d X-coordinate array
;
; Keywords    : ERR = error string
;               DY  = y-pixel spacing
;               YC  = y-pixel center coordinate
;               ONED = extract as 1-d
;
; History     : Written 16 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_map_yp,map,dy=dy,yc=yc,err=err,oned=oned,ny=ny

err=''
if not valid_map(map,err=err) then return,-1
if n_elements(map) ne 1 then begin
 err='cannot handle more than one map'
 message,err,/cont
 return,-1
endif

sz=size(map.data)
nx=sz(1) & ny=sz(2)
dy=map.dy
yc=map.yc
if keyword_set(oned) then nx=1
yp=mk_map_yp(yc,dy,nx,ny)

return,yp

end
