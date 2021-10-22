;+
; Project     : SOHO-CDS
;
; Name        : MK_MAP_XP
;
; Purpose     : compute X-coordinate arrays from center and spacing
;
; Category    : imaging
;
; Explanation : 
;
; Syntax      : xp=mk_map_xp(xc,dx,nx,ny)
;
; Examples    :
;
; Inputs      : XC = x-coord image center (arcsecs)
;               DX = pixel spacing in x-direc (arcsecs)
;               NX,NY = output dimensions
;
; Opt. Inputs : None
;
; Outputs     : XP = 2d X-coordinate array
;
; Opt. Outputs: None
;
; Keywords    : None
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


function mk_map_xp,xc,dx,nx,ny
dumx = nx*dx/2.

;dprint,'% mk_map_xp: being called by '+get_caller()
xp=(findgen(nx)+0.5)*dx - dumx + xc
if not exist(ny) then ny=1
if ny gt 1 then return,rebin(temporary(xp),nx,ny,/sample) else return,xp
end

