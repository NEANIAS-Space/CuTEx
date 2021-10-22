;+
; Project     : SOHO-CDS
;
; Name        : MAP2L1
;
; Purpose     : convert EARTH-view image map to L1-view
;
; Category    : imaging
;
; Syntax      : lmap=map2l1(map)
;
; Inputs      : MAP = image map structure
;
; Outputs     : LMAP = remapped structure 
;
; Keywords    : INVERSE = set to map SOHO to EARTH-view
;               PARTIAL = set to correct offset pointing for partial frame 
;                         images
;
; History     : Written 17 October 1999, D. Zarro, SM&A/GSFC
;               Modified 23 February 2005, Zarro (L-3Com/GSFC) - added /partial
;
; Contact     : dzarro@solar.stanford.edu
;-


function map2l1,map,inverse=inverse,partial=partial

partial=keyword_set(partial)
if not valid_map(map,old=old_format) then begin
 message,'Invalid input map',/cont       
 if exist(map) then return,map else return,''
endif
if (old_format) then begin
 message,'Old format not supported',/cont
 if exist(map) then return,map else return,''
endif

inverse=keyword_set(inverse)

lmap=map
nmap=n_elements(map)
for i=0,nmap-1 do begin
 dx=get_map_prop(map(i),/dx)
 dy=get_map_prop(map(i),/dy)

 if partial then begin
  xc=get_map_prop(map(i),/xc)
  yc=get_map_prop(map(i),/yc)
 endif

 soho=get_map_prop(map(i),/soho,def=0b,/quiet)
 time=get_map_prop(map(i),/time)
 dfac=1. & dsoho=soho
 if (1-inverse) then begin
  if (not soho) then begin
   dfac=soho_fac(time)
   dsoho=1b
  endif
 endif else begin
  if (soho) then begin
   dfac=1./soho_fac(time)
   dsoho=0b
  endif
 endelse
 dprint,'% MAP2L1: ',dfac
 if dfac eq 1 then begin
  message,'Input map already has required view.',/cont
 endif else begin
  lmap(i).dx=dfac*dx
  lmap(i).dy=dfac*dy
  if partial then begin
   lmap(i).xc=dfac*xc
   lmap(i).yc=dfac*yc
  endif   
  lmap(i).soho=dsoho
 endelse
endfor


return,lmap & end


