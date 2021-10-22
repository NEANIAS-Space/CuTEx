function mpfit_plane,im , rms1 = rms1, quiet=quiet;,dmax,windowcenter

par=replicate({back:0.d0, backx:0.d0, backy:0.d0, chi2:0.d0, plane:fltarr(n_elements(im(*,0)),n_elements(im(0,*)))},1)

;;;; IMPORTANT!!! for a reason I could not figure out the centering of sources in 1 Gaussian fits may be off unless I force windocenter to be an integer. It would be nice to understand one day

;windowcenter=nearint(windowcenter)
;
;xwin1=max([nearint(windowcenter(0))-dmax(0),1])
;xwin2=min([nearint(windowcenter(0))+dmax(0),n_elements(im(*,0))-1])
;ywin1=max([nearint(windowcenter(1))-dmax(1),1])
;ywin2=min([nearint(windowcenter(1))+dmax(1),n_elements(im(0,*))-1])
;
;im1=im(xwin1:xwin2,ywin1:ywin2)
;;rms1=rms(windowcenter(0)-dmax:windowcenter(0)+dmax,windowcenter(1)-dmax:windowcenter(1)+dmax)
;  
	im1 = im
if NOT keyword_set(rms1) then begin

  	rms1=im1
  	rms1(*,*)=1.

endif

; build arrays needed for MPFIT
  x=im1 & for k=0,n_elements(im1(*,0))-1 do x(k,*)=float(k)
  y=im1 & for k=0,n_elements(im1(0,*))-1 do y(*,k)=float(k)

  parinfo=replicate({fixed:0., limited:[0,0], limits:[0.,0.], value:0., mpside:2, relstep:0., mpminstep:0., mpmaxstep:0.},3)

 
p=mpfit2dfun('plane_funct',x,y,im1,rms1,parinfo=parinfo,maxiter=500,nprint=0,YFIT=YFIT,/QUIET)

; NO +1 offset in fitted X0,Y0 gaussian center should be added here since everything is internal to IDL, and the offset will be inserted explicitely in pixel coordinates at the time of output 

  par(0).back=p(0)
  par(0).backx=p(1)
  par(0).backy=p(2)
	
  par(0).plane = yfit	
;  surface, im1,zrange=[min(im1),max(im1)]
;  surface, yfit, /NOERASE,zrange=[min(im1),max(im1)]
  stats = moment(im1-yfit, sdev=sdev)

if NOT keyword_set(quiet)  then  print, "R.M.S. residuals = ", sdev
  



if (keyword_set(model_subtract)) then begin
; now build the corresponding gaussian on an array bigger than the extent of the gaussian
  xdim=(2.*abs(ceil(((par(0).sx*2.354)*4.)*cos(par(0).pa/180.*!pi))))+1
  ydim=max([(2.*abs(ceil(((par(0).sx*2.354)*4.)*sin(par(0).pa/180.*!pi)))),2.*abs(ceil(((par(0).sy*2.354)*4.)))])+1
  ptmp=par
  for i=0,0 do begin
    ptmp(i).x0=ptmp(i).x0-1-windowcenter(0)+((xdim-1)/2)
    ptmp(i).y0=ptmp(i).y0-1-windowcenter(1)+((ydim-1)/2)
  endfor
  f=gaussian2d(xdim,ydim,ptmp)
  mkhdr,hf,f
  writefits,'gaussian_mpfit1.fits',f,hf

; now subtracts the model form the data and writes a file with the source subtracted
;computes the shift to bring the array with the gaussian modl to overlap on the right portion of the input image
  blc=[windowcenter(0)-((xdim-1)/2),windowcenter(1)-((ydim-1)/2)]
  trc=[windowcenter(0)+((xdim-1)/2),windowcenter(1)+((ydim-1)/2)]
;now check we did not went off the input image
  if (blc(0) lt 0) then begin
    f=f(abs(blc(0)):xdim-1,*)
    blc(0)=0
  endif
  if (blc(1) lt 0) then begin
    f=f(*,abs(blc(1)):ydim-1)
    blc(1)=0
  endif
  if (trc(0) gt n_elements(im(*,0))-1) then begin
    f=f(0:xdim-1-(trc(0)-(n_elements(im(*,0))-1)),*)
    trc(0)=xdim-1
  endif
  if (trc(1) gt n_elements(im(0,*))-1) then begin
    f=f(*,0:ydim-1-(trc(1)-(n_elements(im(0,*))-1)))
    trc(1)=ydim-1
  endif
  imsub=im
  imsub(blc(0):trc(0),blc(1):trc(1))=im(blc(0):trc(0),blc(1):trc(1))-f
  writefits,'image_subtr_mpfit1.fits',imsub,h
endif

return,par

end
