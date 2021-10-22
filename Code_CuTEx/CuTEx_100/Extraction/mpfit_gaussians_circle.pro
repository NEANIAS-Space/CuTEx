function mpfit_gaussians, im, dmax, drange, windowcenter, center, ngaussians, source_par, psf_lim, $
posback=posback, weight=weight, backgfit=backgfit, model_subtract=model_subtract, psf=psf, mask=mask, $
sources=sources

; ADDED KEYWORD FOR DIFFERENT ORDER OF POLYMONIAL FOR BACKGROUND FITTING
;
; COVAR = covar_ -> Covariance matrix
; YFIT  = prova  -> Fitted Function. 
; BESTNORM = chisum -> Square sum of residuals
; PERROR = perror -> Errors on single parameters
; DOF = dof -> Degrees of freedom
; NO +1 offset in fitted X0,Y0 gaussian center should be added here since everything is internal to IDL, and the offset will be inserted explicitely in pixel coordinates at the time of output 
; Multiple Gaussian fitting. Fit N gaussians, where N is defined by parameter ngaussians, to the image im
; using the guess parameters contained in the source_par structure p

;
; THIS IS UNWEIGHTED FIT.
;
; Added: Putting the starting values of the parameters for the background plane as zero brings problems
; in fitting. The intial values for the [A,B,C] parameters of the plane are now defined through a rought
; plane fitting to the subimage. Thus the initial value of the central peak flux of the gaussian is given 
; by the value of the subimage at the center position minus the value of the plane at the center position
;
; Added: The background is now computed as the value of the final fitted plane at the center position.
; Since the plane is fitted in the subimage the center position should take count of the different 
; zero axis position. background = a0+ b0*(center(0) - xwin1) + c0*(center(1) - ywin1)
;
; Added: it returns also the fitting function as a numerical matrix. It is stored in the par(0).fit. 
; Matrix dimensions are equal to the fitted region (xwin1:xwin2, ywin1:ywin2) defined by windowcenter +- dmax
;
; Added: It returns also the covariance matrix between the fitted parameters. Covariance matrix stored in the par(0).cov_matrix
;
; Added a keyword WEIGHT. If setted it is assumed that we are in Photon Noise regime and at each pixel it associates the error equal to square root of signal.
; At moment the WEIGHT is equal to 1/abs(<flux in pixel)
;
; If the subima im1 has its minimum less than zero then add a small constant value to have all positive values
;
; PA unconstrained when fitted. -> to be fixed to proper values.
;
; Added: Keyword Backfit to control the polynomial degree of the background. If it is active it use a second-order polynomial function
;
; Added: Keyword Perr to control if error on fitting should be computed or not
;
; Added: Keyword to save the fitted background only 
;
; KEYWORD :	Backgfit	-	Define the order of the polynomial used to fit the background
;		Model_Subtract	-	(OLD) 
;		Weight		-	
;		Posback		-	
;		Perr		-
; ISSUES:
;
; COVARIANCE MATRIX SAVED ONLY IN FIRST PAR (?)
xdmax=dmax(0)
ydmax=dmax(1)

windowcenter=nearint(windowcenter)

xwin1=max([nearint(windowcenter(0))-xdmax,1])
xwin2=min([nearint(windowcenter(0))+xdmax,n_elements(im(*,0))-1])
ywin1=max([nearint(windowcenter(1))-ydmax,1])
ywin2=min([nearint(windowcenter(1))+ydmax,n_elements(im(0,*))-1])

im1 = im(xwin1:xwin2,ywin1:ywin2)


if NOT keyword_SET(backgfit) then begin 
	;print, "Fitting sources adopting a plane approximation for the background"
	bck = 0
	par=replicate({ f0:0.d0, x0:0.d0, y0:0.d0, sx:0.d0, sy:0.d0, pa:0.d0, $
	error_x0:0.d0, error_y0:0.d0, error_f0:0.d0, error_sx:0.d0, error_sy:0.d0, error_pa:0.d0,$
	back:0.d0, backx:0.d0, backy:0.d0, $
	error_back:0.d0, error_backx:0.d0, error_backy:0.d0, $
	chi2:0.d0, dof:0.d0,  status:0.d0, $
	fit:fltarr(n_elements(im1(*,0)),n_elements(im1(0,*))), backgr:fltarr(n_elements(im1(*,0)),n_elements(im1(0,*))), cov_matrix:dblarr(3+6*ngaussians,3+6*ngaussians)},ngaussians)
endif else begin
	;print, "Fitting sources adopting a second order polynomial approximation for the background"
	bck = 3
	par=replicate({ f0:0.d0, x0:0.d0, y0:0.d0, sx:0.d0, sy:0.d0, pa:0.d0, $
	error_x0:0.d0, error_y0:0.d0, error_f0:0.d0, error_sx:0.d0, error_sy:0.d0, error_pa:0.d0, $
	back:0.d0, backx:0.d0, backy:0.d0, backxy:0.d0, backx2:0.d0, backy2:0.d0, $
	error_back:0.d0, error_backx:0.d0, error_backy:0.d0, error_backxy:0.d0, error_backx2:0.d0, error_backy2:0.d0, $
	chi2:0.d0, dof:0.d0, status:0.d0, $
	fit:fltarr(n_elements(im1(*,0)),n_elements(im1(0,*))),backgr:fltarr(n_elements(im1(*,0)),n_elements(im1(0,*))), cov_matrix:dblarr(6+6*ngaussians,6+6*ngaussians)},ngaussians)
endelse

parinfo=replicate({fixed:0., limited:[0,0], limits:[0.,0.], tied:'', value:0., mpside:2, relstep:0., mpminstep:0., mpmaxstep:0.}, 3 + bck + 6*(ngaussians))

if KEYWORD_SET(posback) then begin
	parinfo(0).limited=[1,0]
    	parinfo(0).limits(0)=0.
endif

x = im1 & for k=0,n_elements(im1(*,0))-1 do x(k,*)=float(k)
y = im1 & for k=0,n_elements(im1(0,*))-1 do y(*,k)=float(k)

if keyword_set(mask) then begin
	rms_err = mask.mask 
endif else begin 
	rms_err = fltarr(1+2*xdmax,1+2*ydmax)
	rms_err(*,*) = 1.
endelse

;print, "Dmax", dmax
;print, "Drange", drange
;print, "Windowcenter", windowcenter
;print, "Center", center
;print, "Source_par", source_par

offset = 0.
if min(im1) le 0. then begin 
	offset = min(im1)
	im1 = im1 + ABS(min(im1)) + 1E-4
endif
;print, min(im1)

im1_ = im1
;indices = where(im1 le median(im1))
;indices = where(im1 lt mean(im1)/2.)
indices=where(mask.maskbck eq 1e5, cout)

;CHECK THIS. Putted a cicle if

rms2 = im1
rms2(*,*) = 10000. 

if cout ne 0 then begin

	wheretomulti, im1_, indices, row, colu
	im1_(row, colu) = !VALUES.F_NAN
	rms2(row,colu) = 1. 

endif
;

;stop
if KEYWORD_SET(backgfit) then begin

 ; p_im = mpfit_plane_2ord(im1_, rms1=rms2, /QUIET)
   p_im = mpfit_plane_points(im1_, /HIGH, /QUIET)

;endif else p_im = mpfit_plane(im1_, rms1=rms2, /QUIET)
;endif else p_im = mpfit_plane(im1_, /QUIET)
endif else p_im = mpfit_plane_points(im1_, /QUIET)



for ng=1,ngaussians do begin

	parinfo(3 + bck + 6*(ng-1)).limited=[1,0] 	; LIMITING THE PEAK FLUX FOR EACH GAUSSIAN TO NOT BE NEGATIVE
	parinfo(3 + bck + 6*(ng-1)).limits(0)=0.
	

	if (ngaussians eq 1) then begin

  		parinfo(4 + bck).limited=[1,1]	;	LIMITING THE OFFSET FOR THE CENTER OF THE GAUSSIAN
  		parinfo(5 + bck).limited=[1,1]	;
					
		parinfo(6 + bck).limited=[source_par(3),source_par(3)]	;LIMITING SIZE VARIATION USING THE PSF_LIM VALUES
		parinfo(7 + bck).limited=[source_par(3),source_par(3)]		
		parinfo(6 + bck).limits=[max([source_par(0)*psf_lim(0), 0.95*psf]),source_par(0)*psf_lim(1)]/2.354
		parinfo(7 + bck).limits=[max([source_par(1)*psf_lim(0), 0.95*psf]),source_par(1)*psf_lim(1)]/2.354
			
			;parinfo(6 + bck).limits=source_par(0)*psf_lim/2.354
			;parinfo(7 + bck).limits=source_par(1)*psf_lim/2.354
			
		center_ = center
						

		parinfo(4 + bck).limits=[center_(0)-(windowcenter(0)-xdmax)-drange,center_(0)-(windowcenter(0)-xdmax)+drange]
		parinfo(5 + bck).limits=[center_(1)-(windowcenter(1)-ydmax)-drange,center_(1)-(windowcenter(1)-ydmax)+drange]

		parinfo(0:2).value = [p_im.back,p_im.backx,p_im.backy]
		
		if KEYWORD_SET(backgfit) then begin

			backgr_peak = p_im.back + p_im.backx*(center_(0) - (windowcenter(0) - xdmax)) + p_im.backy*(center_(1) - (windowcenter(1) - ydmax)) + $
				      p_im.backxy*(center_(0) - (windowcenter(0) - xdmax))*(center_(1) - (windowcenter(1) - ydmax)) + $
				      p_im.backy2*(center_(1) - (windowcenter(1) - ydmax))^2 + p_im.backx2*(center_(0) - (windowcenter(0) - xdmax))^2
		
			parinfo(3:5).value = [p_im.backxy, p_im.backx2, p_im.backy2]
		
		endif else begin
		
			backgr_peak = p_im.back + p_im.backx*(center_(0) - (windowcenter(0) - xdmax)) + p_im.backy*(center_(1) - (windowcenter(1) - ydmax))
		
		endelse
		
		parinfo(3 + bck: 8 + bck).value =[im1(nearint(center_(0) - (windowcenter(0) - xdmax)),nearint(center_(1)- (windowcenter(1) - ydmax))) - backgr_peak,center_(0)-(windowcenter(0)-(xdmax)),center_(1)-(windowcenter(1)-(ydmax)),source_par(0)/2.354,source_par(1)/2.354,source_par(2)]

		;print, "BCKGR_PEAK ", backgr_peak		
		
	endif else begin
		
		center_ = reform(center(0, *))
		center__ = reform(center(ng - 1,*))
		;____________________________________
		; TO DEBUG
		;print, "CENTER_ SOURCE 0" , center_
		;print, "CENTER__  SOURCE ", ng , center__
		;____________________________________
		parinfo(4 + 6*(ng - 1) + bck).limited=[1,1]
  		parinfo(5 + 6*(ng - 1) + bck).limited=[1,1]
		
		parinfo(6 + 6*(ng - 1) + bck).limited=[source_par(ng - 1,3),source_par(ng - 1,3)]		;LIMITING SIZE VARIATION USING THE PSF_LIM VALUES (MULTIPLE SOURCES CASE)
		parinfo(7 + 6*(ng - 1) + bck).limited=[source_par(ng - 1,3),source_par(ng - 1,3)]


		parinfo(4 + 6*(ng - 1) + bck).limits=[center__(0)-(windowcenter(0)-xdmax)-drange,center__(0)-(windowcenter(0)-xdmax)+drange]
		parinfo(5 + 6*(ng - 1) + bck).limits=[center__(1)-(windowcenter(1)-ydmax)-drange,center__(1)-(windowcenter(1)-ydmax)+drange]

		parinfo(6 + 6*(ng - 1) + bck).limits=[max([source_par(ng - 1,0)*psf_lim(0), 0.95*psf]),source_par(ng - 1, 0)*psf_lim(1)]/2.354
		parinfo(7 + 6*(ng - 1) + bck).limits=[max([source_par(ng - 1,1)*psf_lim(0), 0.95*psf]),source_par(ng - 1, 1)*psf_lim(1)]/2.354
;		parinfo(6 + 6*(ng - 1) + bck).limits=source_par(ng - 1,0)*psf_lim/2.354
;		parinfo(7 + 6*(ng - 1) + bck).limits=source_par(ng - 1,1)*psf_lim/2.354

		parinfo(0:2).value = [p_im.back,p_im.backx,p_im.backy]
		
		if KEYWORD_SET(backgfit) then begin
		
			backgr_peak = p_im.back + p_im.backx*(center__(0) - (windowcenter(0) - xdmax)) + p_im.backy*(center__(1) - (windowcenter(1) - ydmax)) + $
			p_im.backxy*(center__(0) - (windowcenter(0) - xdmax))*(center__(1) - (windowcenter(1) - ydmax)) + $
			p_im.backy2*(center__(1) - (windowcenter(1) - ydmax))^2 + p_im.backx2*(center__(0) - (windowcenter(0) - xdmax))^2
		
			parinfo(3:5).value = [0., 0., 0.]
		
		endif else begin

			backgr_peak = p_im.back + p_im.backx*(center__(0) - (windowcenter(0) - xdmax)) + p_im.backy*(center__(1) - (windowcenter(1) - xdmax))
		
		endelse
		
		parinfo((3 + bck + 6*(ng -1) ):(2 + bck + 6*ng)).value = [im1(nearint(center__(0) - (windowcenter(0) - xdmax)),nearint(center__(1) - (windowcenter(1) - ydmax))) - backgr_peak, center__(0)-(windowcenter(0)-xdmax), center__(1)-(windowcenter(1)-ydmax), source_par(ng-1,0)/2.354, source_par(ng-1,1)/2.354, source_par(ng-1,2)]

		;print, "BCKGR_PEAK ", backgr_peak		
		;		
		
;		for ng_ = 1,ng-1 do begin
		 	
;			center__ = reform(center((ng_ ),*))
 		
  	        if ng gt 1 then begin
	      
		        parinfo(4 + 6*(ng -1) + bck).tied="p["+strcompress(string(4+ bck,FORMAT='(I3)'),/REMOVE_ALL)+"]+("+strcompress(string(center__(0)-center_(0)),/remove_all)+")"
		        parinfo(5 + 6*(ng -1)+ bck).tied="p["+strcompress(string(5+ bck,FORMAT='(I3)'),/REMOVE_ALL)+"]+("+strcompress(string(center__(1)-center_(1)),/remove_all)+")"
	      
	        endif
		  	
;		endfor	

	endelse
	
endfor

;____________________________________________________________________________________________
; TO DEBUG
;help, PARINFO, /STRUCT
;for i = 3,8 do print, "PARINFO", i, " " , parinfo(i).value
;if n_elements(parinfo.value) gt 9 then for i = 9,14 do print, "PARINFO", i, " " , parinfo(i).value
;if n_elements(parinfo.value) gt 15 then for i = 15,20 do print, "PARINFO", i, " " , parinfo(i).value
;if n_elements(parinfo.value) gt 21 then for i = 21,26 do print, "PARINFO", i, " " , parinfo(i).value
;print, "BACK ", bck
;____________________________________________________________________________________________
if KEYWORD_SET(backgfit) then  funct_ = 'multi_funct_back' else funct_ = 'multi_funct'

if keyword_set(WEIGHT) then begin

	p=mpfit2dfun(funct_,x,y,im1,rms_err,parinfo=parinfo,maxiter=500,nprint=0, $
	FUNCTARGS={ngauss:ngaussians}, COVAR=covar, BESTNORM=bestnorm, PERROR=perror, DOF=dof, $
	YFIT=YFIT, WEIGHT=1./ABS(im1), STATUS=status, /QUIET) 

endif else begin 
	
if KEYWORD_SET(PERR) then begin

	p=mpfit2dfun(funct_,x,y,im1,rms_err,parinfo=parinfo,maxiter=500,nprint=0,$
	FUNCTARGS={ngauss:ngaussians}, COVAR=covar, BESTNORM=bestnorm, PERROR=perror, DOF=dof,$
	YFIT=YFIT, STATUS=status, /QUIET)
endif else $
	p=mpfit2dfun(funct_,x,y,im1,rms_err,parinfo=parinfo,maxiter=500,nprint=0,$
	FUNCTARGS={ngauss:ngaussians}, COVAR=covar, BESTNORM=bestnorm, DOF=dof,$
	YFIT=YFIT, STATUS=status, /QUIET)
endelse

for ng=0,ngaussians-1 do begin
	
  	par(ng).back  = p(0) + offset - 1E-4
  	par(ng).backx = p(1)
  	par(ng).backy = p(2)
	
	if KEYWORD_SET(backgfit) then begin
		
		par(ng).backxy = p(3)
  		par(ng).backx2 = p(4)
	  	par(ng).backy2 = p(5)
		
	endif
	;print, "X0 ",  p(4 + bck + 6*ng)
	;print, "Y0 ",  p(5 + bck + 6*ng)
	
	par(ng).x0 = p(4 + bck + 6*ng) + windowcenter(0) - xdmax
  	par(ng).y0 = p(5 + bck + 6*ng) + windowcenter(1) - ydmax  
	par(ng).f0 = p(3 + bck + 6*ng)
	par(ng).sx = p(6 + bck + 6*ng)
	par(ng).sy = p(7 + bck + 6*ng)
	par(ng).pa = p(8 + bck + 6*ng)
	
	if keyword_set(PERR) eq 1 then begin
	
		par(ng).error_x0 = perror(4 + bck)
		par(ng).error_y0 = perror(5 + bck)
		par(ng).error_f0 = perror(3 + bck)
		par(ng).error_sx = perror(6 + bck)
		par(ng).error_sy = perror(7 + bck)
		par(ng).error_pa = perror(8 + bck)

 		par(ng).error_back = perror(0)
  		par(ng).error_backx = perror(1)
  		par(ng).error_backy = perror(2)

		if KEYWORD_SET(backgfit) then begin
		
			par(ng).error_backxy = perror(3)
  			par(ng).error_backx2 = perror(4)
	  		par(ng).error_backy2 = perror(5)
	
		endif


	endif else begin

		par(ng).error_x0 = -1.0
		par(ng).error_y0 = -1.0
		par(ng).error_f0 = -1.0
		par(ng).error_sx = -1.0
		par(ng).error_sy = -1.0
		par(ng).error_pa = -1.0
		
		par(ng).error_back = -1.0
  		par(ng).error_backx = -1.0
  		par(ng).error_backy = -1.0

		if KEYWORD_SET(backgfit) then begin
		
			par(ng).error_backxy = -1.0
  			par(ng).error_backx2 = -1.0
	  		par(ng).error_backy2 = -1.0
	
		endif
 
 	endelse
	
	par(ng).chi2 = bestnorm
	par(ng).dof = dof
	par(ng).status = status

endfor

;print, " A = ", par(0).back
;print, " B = ", par(0).backx
;print, " C = ", par(0).backy
;print, " F0 = ", par(0).f0
;print, " Sx = ", par(0).sx
;print, " Sy = ", par(0).sy

par(0).fit = yfit
par(0).cov_matrix = covar

; COMPUTING ONLY FITTED BACKGROUND

background = im1 
background(*,*) = 0.

sz = size(im1)
for i = 0,sz(1)-1 do begin
	
	for j=0,sz(2)-1 do begin
	
		if keyword_set(backgfit) then begin
			background(i,j) = par(0).back + par(0).backx*x(i,j) + par(0).backy*y(i,j) + $
			(par(0).backx2*x(i,j)^2 + par(0).backy2*y(i,j)^2 + par(0).backxy*x(i,j)*y(i,j))
		endif else background(i,j) = par(0).back + par(0).backx*x(i,j) + par(0).backy*y(i,j)
			
	endfor
endfor
par(0).backgr = background

if (keyword_set(model_subtract)) then begin
; now build the corresponding gaussian on an array bigger than the extent of the gaussian
  xdim=(2.*abs(ceil(((par(0).sx*2.354)*10.)*cos(par(0).pa/180.*!pi))))+1
  ydim=max([(2.*abs(ceil(((par(0).sx*2.354)*10.)*sin(par(0).pa/180.*!pi)))),2.*abs(ceil(((par(0).sy*2.354)*10.)))])+1
  ptmp=par
  for i=0,ngaussians-1 do begin
    ptmp(i).x0=ptmp(i).x0-windowcenter(0)+((xdim-1)/2)
    ptmp(i).y0=ptmp(i).y0-windowcenter(1)+((ydim-1)/2)
  endfor
  f=gaussian2d(xdim,ydim,ptmp)
  mkhdr,hf,f
  writefits,'gaussian_mpfit.fits',f,hf


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

if keyword_set(sources) then if sources(0) eq 44 then stop
return,par



end
