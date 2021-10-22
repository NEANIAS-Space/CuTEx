;+
; Project     : SOHO-CDS
;
; Name        : GET_MAP_XRANGE
;
; Purpose     : extract min/max X-coordinate of map
;
; Category    : imaging
;
; Syntax      : xrange=get_map_xrange(map)
;
; Inputs      : MAP = image map
;
; Opt. Inputs : None
;
; Outputs     : XRANGE = [xmin,xmax]
;
; Keywords    : ERR = error string
;
; History     : Written 16 Feb 1998, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function get_map_xrange,map,err=err

on_error,1

err=''
if not valid_map(map,err=err) then return,-1

sz=size(map.data)
nx=sz(1)
dx=map.dx & xc=map.xc
xmin=min(xc-dx*(nx-1.)/2.)
xmax=max(xc+dx*(nx-1.)/2.)
xrange=[xmin,xmax]

return,xrange

end
