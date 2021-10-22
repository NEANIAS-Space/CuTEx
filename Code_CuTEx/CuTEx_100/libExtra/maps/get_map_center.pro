;+
; Project     : SOHO-CDS
;
; Name        : GET_MAP_SPACE
;
; Purpose     : extract xc,yc center from map
;
; Category    : imaging
;
; Explanation : 
;
; Syntax      : center=get_map_center(map)
;
; Examples    :
;
; Inputs      : MAP = image map
;
; Opt. Inputs : None
;
; Outputs     : CENTER = [xc,yc]
;
; Opt. Outputs: None
;
; Keywords    : ERR = error string
;
; Common      : None
;
; Restrictions: None
;
; Side effects: None
;
; History     : Written 16 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_map_center,map,err=err

on_error,1

err=''
if not valid_map(map,err=err,old=old) then return,-1

if old then begin
 xc=get_arr_center(map.xp,dx=dx)
 yc=get_arr_center(map.yp,dy=dy)
 boost_array,center,[xc,yc]
endif else begin
 center=reform(transpose([[map.xc],[map.yc]]))
endelse

return,center

end
