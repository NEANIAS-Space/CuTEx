;+
; Project     : SOHO-CDS
;
; Name        : GET_MAP_FOV
;
; Purpose     : compute map FOV 
;
; Category    : imaging
;
; Explanation : 
;
; Syntax      : fov=get_map_fov(map)
;
; Examples    :
;
; Inputs      : MAP = image map
;
; Opt. Inputs : None
;
; Outputs     : FOV = [fovx,fovy] = arcsec fov in X and Y directions
;
; Opt. Outputs: None
;
; Keywords    : ERR = error string
;               ARCMIN = return in arcmin units
;               ROUND = round to nearest integer
;
; Common      : None
;
; Restrictions: None
;
; Side effects: None
;
; History     : Written 26 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-


function get_map_fov,map,arcmin=arcmin,err=err,round_=round_

on_error,1

if not valid_map(map) then begin
 pr_syntax,'fov=get_map_fov(map,[arcmin=arcmin])'
 return,-1
endif

xrange=get_map_prop(map,/xr)
yrange=get_map_prop(map,/yr)

fovx=max(xrange)-min(xrange)
fovy=max(yrange)-min(yrange)

fov=[fovx,fovy]

if keyword_set(arcmin) then fov=fov/60.
if keyword_set(round_) then fov=float(nint(fov))
if keyword_set(arcmin) then begin
 if fov(0) eq 0 then fov(0)=1.
 if fov(1) eq 0 then fov(1)=1.
endif else begin
 if fov(0) eq 0 then fov(0)=60.
 if fov(1) eq 0 then fov(1)=60.
endelse 


return,fov

end
