;+
; Project     : SOHO-CDS
;
; Name        : roll_xy
;
; Purpose     : rotate image coordinates
;
; Category    : imaging
;
; Syntax      : roll_xy,xarr,yarr,angle,rx,ry
;
; Inputs      : XARR,YARR = image (X,Y) coordinates
;               ANGLE = angle in degrees (+ for clockwise)
;
; Outputs     : RX,RY = rotated coordinates
;
; Keywords    : CENTER= [XC,YC] = center of rotation [def = center of image]
;
; History     : Written 22 November 1996, D. Zarro, ARC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro roll_xy,xarr,yarr,angle,rx,ry,center=center,verbose=verbose

if n_elements(angle) eq 0 then begin
 repeat begin
  angle='' & read,'* enter angle [deg] by which to rotate image [+ clockwise]: ',angle
 endrep until angle ne ''
 angle=float(angle)
endif

angle=float(angle)

if (angle mod 360.) eq 0 then begin
 rx=xarr & ry=yarr
 return
endif

theta=angle*!dtor
costh=cos(theta) & sinth=sin(theta)

;-- rotate pixel arrays about requested center 
;-- (if input coords are 2d and center not specified, then use image center) 

verbose=keyword_set(verbose)
xc=0. & yc=0.
sangle=num2str(angle,format='(f5.1)')
if n_elements(center) eq 2 then begin
 xc=float(center(0)) & yc=float(center(1))
 if verbose then begin
  sxc=num2str(xc,format='(f7.1)')
  syc=num2str(yc,format='(f7.1)')
  message,'rotating '+sangle+' about supplied center: '+sxc+', '+syc,/cont
 endif
endif else begin
 if data_chk(xarr,/ndim) eq 2 then begin
  min_x=min(xarr,max=max_x)
  min_y=min(yarr,max=max_y)
  xc=(min_x+max_x)/2. &  yc=(min_y+max_y)/2.
  if verbose then begin
   sxc=num2str(xc,format='(f7.1)')
   syc=num2str(yc,format='(f7.1)')
   message,'rotating '+sangle+' about computed center: '+sxc+', '+syc,/cont
  endif
 endif
endelse

trx=xc+costh*(xarr-xc)+sinth*(yarr-yc)
try=yc-sinth*(xarr-xc)+costh*(yarr-yc)

rx=temporary(trx)
ry=temporary(try)

return & end

