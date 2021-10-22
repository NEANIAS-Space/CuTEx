; Function RES_PHOTO
;
;	FEW CHANGES:
;	the NEWSDEV2 and NEWSDEV4 now compute the sdev not flagging pixels anymore with the initial guess mask from the DETECTION routine (through the CHECK_MASK.pro procedure)
;	instead now we compute an auxiliary mask from the F0 and the size of the fitted source. The auxiliary mask is computed as all the pixels that are inside 1 FWHM (in any direction)
;	It is easy to identify those pixels since they are the one where the flux of the fitted gaussian is above EXP(-4*alog(2))*f0. I called this mask ONSOURCE_MASK
;	
;	The mask has to be built for single source and multiple sources. If there are multiple sources the only pixels I am masking are the pixels of the current souce if we are using the 
;	/CLOSEST_NEIGH in the Extract_photo. This is recognized by /EACHSOUR keyword
;
;; 	To be implemented: Fix for multiple sources but not with the CLOSEST_NEIGH option active
;
;	
; 	Using to subtract the fitted source from the ima and compute the RMS
;
;
;	SDEV3:	Standard Deviation Residuals after source subtraction and then fit of a planar background on pixels not belonging to the other sources.
;	SDEV1:	Standard Deviation Residuals after source subtraction and then fit of a planar background on pixels not belonging to any source.
;	SDEV4:	Standard Deviation Residuals after fit subtraction on pixels not belonging to the other sources in the subfield.
;	SDEV2:	Standard Deviation Residuals after fit subtraction on pixels not belonging to any (fitted) source in the subfield.
;	SDEV4_ONSOU:	Standard Deviation Residuals after fit subtraction on pixels belonging to THE source in the subfield. ; BUG FOR MULTIPLE SOURCES NOT WITH /CLOSEST_NEIGH
;
;	NEWSDEV4:	Standard Deviation Residuals after fit subtraction on pixels not belonging to sources in the subfield. The mask for the actual source is wide as 2 FWHM.
;	NEWSDEV4_ONSOU:	Standard Deviation Residuals after fit subtraction on pixels belonging to THE source in the subfield. The mask for the actual source is wide as 2 FWHM.
;	NEWSDEV5_ONSOU:	Standard Deviation Residuals after fit subtraction on pixels belonging to THE source in the subfield. The mask for the actual source is wide as 1 FWHM.
;
;	WARNING if we are not using the CLOSEST_NEIGH keyword then the output of NEWSDEV_ONSOU is a vector depending on how many sources are fitted.

function res_photo, im,dmax, windowcenter, f0, center_fit_x0, center_fit_y0, xsigma, ysigma, pa, yfit, fact=fact, multiple=multiple, eachsour=eachsour, mask=mask

minpix = 5	; MINIMUM NUMBER OF PIXELS TO MAKE THE STATISTIC

if keyword_set(fact) then fact = fact else fact = 1.
	
  xwin1=max([nearint(windowcenter(0)) - fact*dmax(0),1])
  xwin2=min([nearint(windowcenter(0)) + fact*dmax(0),n_elements(im(*,0))-1])
  ywin1=max([nearint(windowcenter(1)) - fact*dmax(1),1])
  ywin2=min([nearint(windowcenter(1)) + fact*dmax(1),n_elements(im(0,*))-1])

  
  
  im1 = im(xwin1:xwin2,ywin1:ywin2)
  sources = im1 
  sources(*,*) = 0.
  
  if keyword_set(multiple) and NOT keyword_set(eachsour) then begin
	    onsource_mask = fltarr(n_elements(sources(*,0)), n_elements(sources(0,*)), n_elements(f0)) 
	    onsource_mask2 = fltarr(n_elements(sources(*,0)), n_elements(sources(0,*)), n_elements(f0)) 
	    sdev4_onsou_ = fltarr(n_elements(f0))
	    newsdev4_onsou_ = fltarr(n_elements(f0))
	    newsdev5_onsou_ = fltarr(n_elements(f0))
	    
  endif else begin
  	
	onsource_mask = sources
	onsource_mask2 = sources
  
  endelse
  
  
  if NOT keyword_set(multiple) then begin 
; 	  x0 = center_fit_x0  - windowcenter(0) + fact*dmax(0)
;	  y0 = center_fit_y0  - windowcenter(1) + fact*dmax(1)
 	  x0 = center_fit_x0  - xwin1
	  y0 = center_fit_y0  - ywin1
	
	  gaussian = make_gauss(im1, f0, x0, y0, xsigma, ysigma, pa)
	  sources = gaussian
	  
	  onsou_pixels = where(gaussian gt f0*exp(-4*alog(2)), nonsou_pixels, complement=offsou_pixels, ncomplement=noffsou_pixels)
	  onsou_pixels2 = where(gaussian gt f0*exp(-2*alog(2)), nonsou_pixels2, complement=offsou_pixels2, ncomplement=noffsou_pixels2)
	  if nonsou_pixels gt 0 then onsource_mask[onsou_pixels] = 1.
	  if nonsou_pixels2 gt 0 then onsource_mask2[onsou_pixels2] = 1.
	  	  	
  endif else begin
  
  for j = 0, n_elements(center_fit_x0)-1 do begin

 ; 	     	       	x0 = center_fit_x0(j) - windowcenter(0,min(ii)) + fact*dmax(0,min(ii))
 ;       	       	y0 = center_fit_y0(j) - windowcenter(1,min(ii)) + fact*dmax(1,min(ii))
		 	  x0 = center_fit_x0(j)  - xwin1
			  y0 = center_fit_y0(j)  - ywin1

        	       	gaussian = make_gauss(im1, f0(j), x0, y0, xsigma(j), ysigma(j), pa(j))
        	       	sources = sources + gaussian
			
			if NOT keyword_set(eachsour) then begin 
			
				  onsou_pixels = where(gaussian gt f0[j]*exp(-4*alog(2)), nonsou_pixels, complement=offsou_pixels, ncomplement=noffsou_pixels)
				  if nonsou_pixels gt 0 then onsource_mask[onsou_pixels, j] = j+1.
	 			 
				  onsou_pixels2 = where(gaussian gt f0*exp(-2*alog(2)), nonsou_pixels2, complement=offsou_pixels2, ncomplement=noffsou_pixels2)
				  if nonsou_pixels2 gt 0 then onsource_mask2[onsou_pixels2] = j+1.
			
			endif 	else begin
			
					if j eq 0 then begin
			
					  onsou_pixels = where(gaussian gt f0[j]*exp(-4*alog(2)), nonsou_pixels, complement=offsou_pixels, ncomplement=noffsou_pixels)
				 	  if nonsou_pixels gt 0 then onsource_mask[onsou_pixels] = j+1.
			
					  onsou_pixels2 = where(gaussian gt f0[j]*exp(-2*alog(2)), nonsou_pixels2, complement=offsou_pixels2, ncomplement=noffsou_pixels2)
					  if nonsou_pixels2 gt 0 then onsource_mask2[onsou_pixels2] = j+1.
			
					endif

			endelse
			
  endfor
  
	checkoffsou = where(onsource_mask eq 0, ncheckoffsou)
	;print, ncheckoffsou
	
 ENDELSE  
 
 if keyword_set(eachsour) then begin
	
	xwin1_ = max([nearint(windowcenter(0)) - eachsour(0) - xwin1,0])
	xwin2_ = min([nearint(windowcenter(0)) + eachsour(0) - xwin1,n_elements(im1(*,0))-1])
	ywin1_ = max([nearint(windowcenter(1)) - eachsour(1) - ywin1,0])
	ywin2_ = min([nearint(windowcenter(1)) + eachsour(1) - ywin1,n_elements(im1(0,*))-1])
	
	;print, " WIN SIZE EACH SOUR", xwin1_, xwin2_, ywin1_, ywin2_
	
 endif else begin
 	xwin1_ = 0
	xwin2_ = n_elements(im1(*,0))-1
	ywin1_ = 0
	ywin2_ = n_elements(im1(0,*))-1
 	;print, " WIN SIZE", xwin1_, xwin2_, ywin1_, ywin2_

 endelse
 
 
  resid1 = im1 - sources
  pl = mpfit_plane(resid1,/QUIET)
  
  residual_image = resid1 - pl.plane
  
  subresidual_image = residual_image(xwin1_:xwin2_,ywin1_:ywin2_)
  
  if keyword_set(mask) then begin
  	
	newmask = mask.mask(xwin1_:xwin2_,ywin1_:ywin2_)
	newmask_back = mask.maskbck(xwin1_:xwin2_,ywin1_:ywin2_)
  	
	indx = where(newmask eq 1E5,complement=iindx, ncomplement=niindx)	
	indx_back = where(newmask_back eq 1E5,complement=iindx_back, ncomplement=niindx_back)	

	indx = where(newmask eq 1E5 and finite(subresidual_image) eq 0,complement=iindx, ncomplement=niindx)	
	indx_back = where(newmask_back eq 1E5 and finite(subresidual_image) eq 0,complement=iindx_back, ncomplement=niindx_back)	

	mom_ = moment(subresidual_image(iindx) , sdev=sdev1)

	if niindx gt minpix then mom_ = moment(subresidual_image(iindx) , sdev=sdev1) else sdev1 = !VALUES.F_NAN
	if niindx_back gt minpix then mom_ = moment(subresidual_image(iindx_back), sdev=sdev3) else sdev3 = !VALUES.F_NAN
		
  endif else	mom_ = moment(subresidual_image , sdev=sdev1)
 
  
  resid2 = im1 - yfit
  subresidual_image_fit = resid2(xwin1_:xwin2_,ywin1_:ywin2_)
  
  if keyword_set(mask) then begin

	newmask = mask.mask(xwin1_:xwin2_,ywin1_:ywin2_)
  	newmask_back = mask.maskbck(xwin1_:xwin2_,ywin1_:ywin2_)
	
;	indx = where(newmask eq 1E5,complement=iindx, ncomplement=niindx)	
;	indx_back = where(newmask_back eq 1E5,complement=iindx_back, ncomplement=niindx_back)	

	indx = where(newmask eq 1E5 or finite(subresidual_image_fit) eq 0,complement=iindx, ncomplement=niindx)	
	indx_back = where(newmask_back eq 1E5 or finite(subresidual_image_fit) eq 0,complement=iindx_back, ncomplement=niindx_back)	
	newiindx_back = where((newmask_back - newmask) eq 99999., nnewiindx_back)	
	
	; Other mask
	
	if not keyword_set(multiple) or keyword_set(eachsour) then otherindx_back = where(newmask_back eq 1E5 or finite(subresidual_image_fit) eq 0 or onsource_mask ge 1, complement=otheriindx_back, ncomplement=notheriindx_back)
	if not keyword_set(multiple) or keyword_set(eachsour) then newotheriindx_back = where(onsource_mask eq 1, nnewotheriindx_back)
	
	if not keyword_set(multiple) or keyword_set(eachsour) then otherindx_back2 = where(newmask_back eq 1E5 or finite(subresidual_image_fit) eq 0 or onsource_mask2 ge 1, complement=otheriindx_back2, ncomplement=notheriindx_back2)
	if not keyword_set(multiple) or keyword_set(eachsour) then newotheriindx_back2 = where(onsource_mask2 eq 1, nnewotheriindx_back2)

;	mom_ = moment(subresidual_image_fit(iindx) , sdev=sdev2) 
	
	
	if niindx gt minpix then begin
	
		mom_ = moment(subresidual_image_fit(iindx) , sdev=sdev2) 
		flagin = 0
		
	endif else begin 
		
		sdev2 = !VALUES.F_NAN
		flagin = 1
		
	endelse

	
	if niindx_back gt minpix then  begin
	
		mom_ = moment(subresidual_image_fit(iindx_back), sdev=sdev4) 
		if nnewiindx_back gt 0 then mom_onsou = moment(subresidual_image_fit(newiindx_back), sdev=sdev4_onsou) else sdev4_onsou = !VALUES.F_NAN
		flagout = 0
		
	endif else  begin
		
		sdev4 = !VALUES.F_NAN
		sdev4_onsou =!VALUES.F_NAN
		flagout = 1

	endelse	
	
	if keyword_set(multiple) and NOT keyword_set(eachsour) then begin
		
		for p = 0, n_elements(f0)-1 do begin
		
			if p eq 0 then totonsource_mask =  onsource_mask[*,*,0] else totonsource_mask = totonsource_mask + onsource_mask[*,*,p]
			 newotheriindx_back = where(onsource_mask eq p+1,nnewotheriindx_back)
	
			if p eq 0 then totonsource_mask2 =  onsource_mask2[*,*,0] else totonsource_mask2 = totonsource_mask2 + onsource_mask2[*,*,p]
			 newotheriindx_back2 = where(onsource_mask2 eq p+1,nnewotheriindx_back2)
	
			 if nnewotheriindx_back gt minpix then mom_onsou = moment(subresidual_image_fit(newotheriindx_back), sdev=newsdev4_onsou) else newsdev4_onsou = !VALUES.F_NAN
			 if nnewotheriindx_back2 gt minpix then mom_onsou2 = moment(subresidual_image_fit(newotheriindx_back2), sdev=newsdev5_onsou) else newsdev5_onsou = !VALUES.F_NAN
			 newsdev4_onsou_[p] = newsdev4_onsou
			 newsdev5_onsou_[p] = newsdev5_onsou
			 
		endfor
		otherindx_back = where(newmask_back eq 1E5 or finite(subresidual_image_fit) eq 0 or totonsource_mask  ge 1, complement=otheriindx_back, ncomplement=notheriindx_back)
		otherindx_back2 = where(newmask_back eq 1E5 or finite(subresidual_image_fit) eq 0 or totonsource_mask2  ge 1, complement=otheriindx_back2, ncomplement=notheriindx_back2)
		
		if notheriindx_back gt minpix then  begin
	
			mom_ = moment(subresidual_image_fit(otheriindx_back), sdev=newsdev4) 
		
		endif else  begin
	
			newsdev4 = !VALUES.F_NAN
	
		endelse	
		
			
	endif else begin
	
		if notheriindx_back gt minpix then  begin
	
			mom_ = moment(subresidual_image_fit(otheriindx_back), sdev=newsdev4) 
			if nnewotheriindx_back gt minpix then mom_onsou = moment(subresidual_image_fit(newotheriindx_back), sdev=newsdev4_onsou) else newsdev4_onsou = !VALUES.F_NAN
		
		endif else  begin
	
			newsdev4 = !VALUES.F_NAN
			newsdev4_onsou =!VALUES.F_NAN

		endelse	
	
		if notheriindx_back2 gt minpix then  begin
	
			if nnewotheriindx_back2 gt minpix then mom_onsou2 = moment(subresidual_image_fit(newotheriindx_back2), sdev=newsdev5_onsou) else newsdev5_onsou = !VALUES.F_NAN
		
		endif else  begin
	
			newsdev5_onsou = !VALUES.F_NAN
		endelse	

	endelse	
	
	
  endif else begin
  
  		mom_ = moment(subresidual_image_fit , sdev=sdev2)
  
  endelse
  
  if NOT keyword_set(multiple) or keyword_set(eachsour) then begin
  	
  	sdev4_onsou_ = sdev4_onsou
  	newsdev4_onsou_ = newsdev4_onsou
  	newsdev5_onsou_ = newsdev5_onsou
  	
  endif
  
; DEBUGGING PURPOSE

; window, 1, xpos=0, ypos=500, xs=400, ys=400, title='Subframe'
; surface, im1
; z_range = !z.crange
; window, 2, xpos=400, ypos=500, xs=400, ys=400, title='Sources'
; surface, sources
; window, 3, xpos=800, ypos=500, xs=400, ys=400, title='Residual'
; surface, residual_image
; window,4, xpos=000, ypos=0, xs=400, ys=400 , title='Fit'
; surface, yfit
; window,5, xpos=400, ypos=0, xs=400, ys=400 , title='Residual 2' 
; surface, subresidual_image_fit, ZR=z_range
; window,5, xpos=400, ypos=0, xs=400, ys=400 , title='Residual 2 MASK' 
 ;if n_elements(indx) gt 1 then subresidual_image_fit(indx) = !VALUES.F_NAN
 ;surface, subresidual_image_fit, ZR=z_range 
;surface, newmask_back 

; window,6, xpos=800, ypos=0, xs=400, ys=400 , title='Residual 2 MASK BCK' 
 ;if n_elements(indx) gt 1 then subresidual_image_fit(indx_back) = !VALUES.F_NAN
 ;surface, subresidual_image_fit, ZR=z_range 
 
; surface, onsource_mask
 
;print, "STANDARD DEVIATIONS ", sdev1, sdev2, sdev3, sdev4, sdev4_onsou, newsdev4, newsdev4_onsou
;stop

 return, {sdev1:sdev1, sdev2:sdev2, sdev3:sdev3,sdev4:sdev4, sdev4_onsou:sdev4_onsou_, newsdev4:newsdev4,  newsdev4_onsou: newsdev4_onsou_, newsdev5_onsou: newsdev5_onsou_, flagin:flagin, flagout:flagout}

end
  
  
