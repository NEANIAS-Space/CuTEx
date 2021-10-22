function check_mask, im_mask, windowcenter, dmax, sources, verbose=verbose, backgfit=backgfit

; Cut the same subimage that will be used for the mpfit_gaussians

somedata = 1.
if keyword_set(backgfit) then minpix = 8 else minpix = 5

while somedata eq 1. do begin 

	xdmax=dmax(0)
	ydmax=dmax(1)

	windowcenter=nearint(windowcenter)

	xwin1=max([nearint(windowcenter(0))-xdmax,1])
	xwin2=min([nearint(windowcenter(0))+xdmax,n_elements(im_mask(*,0))-1])
	ywin1=max([nearint(windowcenter(1))-ydmax,1])
	ywin2=min([nearint(windowcenter(1))+ydmax,n_elements(im_mask(0,*))-1])

	subima = im_mask(xwin1:xwin2,ywin1:ywin2)

	if KEYWORD_SET(verbose) then begin

		print, "Checking "+strcompress(string(n_elements(sources)),/REMOVE_ALL)+" sources"
		print, "Sources ID:"
		print, sources

	endif

	mask = subima		; This is the mask that exclude other sources
	mask(*,*) = 1.		
	maskbck = subima	; This is the mask used for background estimates	
	maskbck(*,*) = 1.
	
	; INDX 	=	all the pixels not masked by detection
	; NINDX =	all the pixels masked by detection
	; NOZERONINDX 	number of pixels masked 
	
	
	indx = where(subima eq 0, counts, complement=nindx, NCOMPLEMENT=nozeronindx)	
	
	if nozeronindx ne 0 then begin
		
		; Identify the sources that are present in the subframe
	
		vtmp = subima(nindx(sort(subima(nindx))))
		valsour = vtmp(uniq(vtmp))

		;print, "VAL SOURCE ", valsour
		;print, "SOURCES ", sources
		if n_elements(valsour) ne n_elements(sources) then nsouinframe = n_elements(valsour) - n_elements(sources) else nsouinframe = 0. 
		;CHECK IF EMPTY STOP. Putting an IF
	
		; If there are masked sources in the subframe they have to be masked
		; MASKBCK should have masked all the pixels belonging to any source
		; MASK instead should have mask all the pixels belonging to all the sources except the one currently fitted
	
		if counts gt minpix then begin
	
			
			
			maskbck(nindx) = 1E5
			
			; Identify the pixels belonging to the fitted sources that are in the input variable.
			; INDX_ 	= 	are the pixels corresponding to the input sources
			; NNINDX	= 	are the pixels that do not correspond to the input sources
			; NNNINDX	=	are the number of pixels not corresponding to the input sources
			
			for j=0, n_elements(sources) - 1 do begin
			
				indx_ = where(subima(nindx) eq sources(j), counts_, COMPLEMENT=nnindx, NCOMPLEMENT=nnnindx)
				
				; Update the list of pixels not belonging to the input sources

				if (counts gt 0 and nnnindx gt 0) then nindx = nindx(nnindx)	

			endfor 

			;mask(nindx) = 1E5 
			;if NOT (nnnindx eq 0 and n_elements(valsour) eq 1) then mask(nindx) = 1E5 
		
			; The MASK image should be produced if there are pixels to be masked not belonging to the sources
			; currently fitted. This is true if after the for cycle the NNNINDX is greater than 0 or
			; there are more sources (VALSOUR) than SOURCES
		
			if NOT (nnnindx eq 0 and n_elements(valsour) eq n_elements(sources)) then mask(nindx) = 1E5 

		
			somedata = 0.
	
		endif else begin
			
			print, "Warning! No enough pixels for background guess for source/sources number : ", sources
			print, "Enlarging fitting window by 2 pixel"
			
			dmax(0) = dmax(0) + 1
			dmax(1) = dmax(1) + 1
		endelse
	
	endif else begin
		
			print, "Error! No masked pixels found in the image for the source/sources number ", sources
			somedata = -1
			;return, -1
			nsouinframe = 0.
			
	endelse
					
endwhile		


return, {mask:mask, maskbck:maskbck, dmax_new:dmax, nsou:nsouinframe}

end
