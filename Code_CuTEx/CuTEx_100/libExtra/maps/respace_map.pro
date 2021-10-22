;+
; Project     : SOHO-CDS
;
; Name        : RESPACE_MAP
;
; Purpose     : Rebin an image map to new pixel spacing  
;
; Category    : imaging
;
; Explanation : Rebin a map to user-specified spacings and
;               compute new output dimensions
;
; Syntax      : gmap=respace_map(map,sx,sy)
;
; Inputs      : MAP = image map structure
;               SX,SY = new (x,y) spacing
;
; Outputs     : GMAP = rebinned map
;
; History     : Written 22 March 1998, D. Zarro, SAC/GSFC
;		P. Saint-Hilaire, 2007/01/04: added keyword INTERACTIVE (if not set, program will no longer prompt user)
;
; Contact     : dzarro@solar.stanford.edu
;-

function respace_map,map,sx,sy,err=err,_extra=extra,max_dim=max_dim, INTERACTIVE=INTERACTIVE

on_error,1
err=''

;-- check inputs (valid map & spacings)

if (not valid_map(map,old=old)) or (not exist(sx)) then begin
 pr_syntax,'gmap=respace_map(map,sx,sy)'
 if exist(map) then return,map else return,-1
endif
if not exist(sy) then sy=sx

IF KEYWORD_SET(INTERACTIVE) THEN asked=0 ELSE asked=1

for i=0,n_elements(map)-1 do begin
 err=''
 unpack_map,map(i),dx=dx,dy=dy,nx=nx,ny=ny
 if (sx eq dx) and (sy eq dy) then begin
  message,'no rebinning necessary',/cont
  gmap=merge_struct(gmap,map(i))
 endif else begin

;-- compute and check new output dimensions

  gx=nint(dx*nx/sx)
  gy=nint(dy*ny/sy)
  if not asked then begin
   if (gx gt 1024) or (gy gt 1024) then begin
    message,'excessive new output dimensions - '+trim(string(gx))+','+trim(string(gy)),/cont
    ans='' & read,'* continue [def=n]?',ans
    ans=strmid(strupcase(ans),0,1)
    if ans ne 'Y' then begin
     err='Aborted' & message,err,/cont
     return,map
    endif
    asked=0
   endif
  endif
  tmap=rep_tag_value(map(i),congrid(map(i).data,gx,gy,_extra=extra),'data',/no_copy)
  tmap.dx=sx
  tmap.dy=sy
  gmap=merge_struct(gmap,tmap)
 endelse
endfor

return,gmap & end
