function mpfit_plane_2ord,im , rms1 = rms1, quiet=quiet;,dmax,windowcenter

par=replicate({back:0.d0, backx:0.d0, backy:0.d0, backxy:0.d0, backx2:0.d0, backy2:0.d0, chi2:0.d0, plane:fltarr(n_elements(im(*,0)),n_elements(im(0,*)))},1)


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

  parinfo=replicate({fixed:0., limited:[0,0], limits:[0.,0.], value:0., mpside:2, relstep:0., mpminstep:0., mpmaxstep:0.},6)

 
p=mpfit2dfun('quadric_funct',x,y,im1,rms1,parinfo=parinfo,maxiter=500,nprint=0,YFIT=YFIT, /QUIET)

; NO +1 offset in fitted X0,Y0 gaussian center should be added here since everything is internal to IDL, and the offset will be inserted explicitely in pixel coordinates at the time of output 

  par(0).back = p(0)
  par(0).backx = p(1)
  par(0).backy = p(2)
  par(0).backxy = p(3)
  par(0).backx2 = p(4)
  par(0).backy2 = p(5)
  	
  par(0).plane = yfit	

;  surface, im1,zrange=[min(im1),max(im1)]
;  surface, yfit, /NOERASE,zrange=[min(im1),max(im1)]
  stats = moment(im1-yfit, sdev=sdev)

if NOT keyword_set(quiet)  then  print, "R.M.S. residuals = ", sdev
  


return,par

end
