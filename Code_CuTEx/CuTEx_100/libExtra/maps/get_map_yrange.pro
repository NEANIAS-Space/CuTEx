;+
; Project     : SOHO-CDS
;
; Name        : GET_MAP_YRANGE
;
; Purpose     : extract min/max Y-coordinate of map
;
; Category    : imaging
;
; Syntax      : yrange=get_map_yrange(map)
;
; Inputs      : MAP = image map
;
; Opt. Inputs : None
;
; Outputs     : YRANGE = [ymin,ymax]
;
; Opt. Outputs: None
;
; Keywords    : ERR = error string
;
; History     : Written 16 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_map_yrange,map,err=err

on_error,1

err=''
if not valid_map(map,err=err) then return,-1

sz=size(map.data)
ny=sz(2)
dy=map.dy & yc=map.yc
ymin=min(yc-dy*(ny-1.)/2.)
ymax=max(yc+dy*(ny-1.)/2.)
yrange=[ymin,ymax]

return,yrange

end
