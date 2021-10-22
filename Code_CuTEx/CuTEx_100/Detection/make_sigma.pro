function make_sigma, ima, window=window, zero=zero, complete=complete, step=step
	
	; UPDATED version:	
	;
	; Do not compute anymore the local median absolute deviation (MAD) for each point of the image 
	; As default it computes an approximate local MAD sampling the image every 30 pixels (the default WINDOW size).
	; Such MAD value is used for all the pixels in the surrounding (half WINDOW size square) of the sampled pixel.
	;
	; Keywords:	WINDOW		-	Set the half-size of the window size where to compute the local MAD - Default 30 pixels (local square 61x61 pixels)
	;		ZERO		- 	Take into account the pixels with zero values when computing the MAD - Default not take into account.
	;		COMPLETE 	- 	Ignore the restriction on the sampling for the local MAD image and compute of every pixels - Warning long computational time required
	;		STEP		-	The number of pixels defining the length to skip when sampling the approximate local MAD - Default as WINDOW. If keyword COMPLETE is setted this keyword is skipped.
		
	sz = size(ima)
	
	if sz(0) eq 0 then begin
		print, 'Reading the image file'
		ima = readfits(ima,h)
		sz = size(ima)
	endif
	
	if NOT keyword_set(window) then window = 30.
		
	win_sz = size(window)

	if win_sz(0) eq 0 then begin
		window = [window, window]
	endif	
	
	if keyword_set(step) then begin 
		if step gt window(0) and NOT keyword_set(complete) then begin
			print, "Warning step size for sampling smaller than window - Setting it to the window size"
			step = window(0)
		endif else step = step 
	endif else step = window(0)
	
	if keyword_set(complete) then step = 1
						
	sigmaframe = ima
	sigmaframe(*,*) = 0.
	medianframe = sigmaframe
	
	print, 'Evaluating standard deviation and absolute median deviation'
	print, 'This could take a while depending on the size of the map'
	print, 'Map size eq ', sz(1), ' times ', sz(2), ' pixels'
	print, 'Sampling on a step of '+strcompress(string(step), /REMOVE_ALL)+' pixels'
	
	if step gt 1 then begin 
		
		x_start	= nearint(sz(1) mod step)/2.
		y_start	= nearint(sz(2) mod step)/2.
		
		x_end = sz(1) - 1 - (sz(1)-1 mod step)
		y_end = sz(2) - 1 - (sz(2)-1 mod step)
		
	endif else begin
		
		x_start = 0
		y_start = 0

	endelse	
	
	
	
	for i = x_start, sz(1), step do begin
		for j = y_start, sz(2), step do begin
			
			xwin1 = max([i-window(0),0])
			xwin2 = min([i+window(0),sz(1)-1])
			ywin1 = max([j-window(1),0])
			ywin2 = min([j+window(1),sz(2)-1])
			
			subframe = ima(xwin1:xwin2,ywin1:ywin2)
			
			if keyword_set(zero) then begin
				
				dummy = moment(subframe, sdev = sdev)
				dummy_med = median(subframe)
				
				if step gt 1 then leng = max([nearint(step/2.),1]) else leng = 0
				
				
				;print, 'LENGTH = ',leng
					
				;sigmaframe((i - leng):(i + leng),(j - leng):(j + leng)) = sdev
				;medianframe((i - leng):(i + leng),(j - leng):(j + leng)) = median(SQRT((subframe - median(subframe))^2))
				
				sigmaframe(max([(min([i,sz(1)-1]) - leng),0]):min([(i + leng), sz(1)-1]),max([(min([j,sz(2)-1]) - leng),0]):min([(j + leng), sz(2)-1])) = sdev						 
				medianframe(max([(min([i,sz(1)-1]) - leng),0]):min([(i + leng), sz(1)-1]),max([(min([j,sz(2)-1]) - leng),0]):min([(j + leng), sz(2)-1])) = median(SQRT((subframe - median(subframe))^2)) 
					
			endif else begin
			
				pnts = where(subframe ne 0,cout)
				
				if step gt 1 then leng = max([nearint(step/2.),1]) else leng = 0.
			
			
				
				if cout gt 3 then begin 
				
					dummy = moment(subframe(pnts), sdev = sdev)
					dummy_med = median(subframe(pnts))
					
			
					sigmaframe(max([(min([i,sz(1)-1]) - leng),0]):min([(i + leng), sz(1)-1]),max([(min([j,sz(2)-1]) - leng),0]):min([(j + leng), sz(2)-1])) = sdev
					medianframe(max([(min([i,sz(1)-1]) - leng),0]):min([(i + leng), sz(1)-1]),max([(min([j,sz(2)-1]) - leng),0]):min([(j + leng), sz(2)-1])) = median(SQRT((subframe(pnts) - median(subframe(pnts)))^2))
				
				endif else begin
				
					;sigmaframe((i - leng):(i + leng),(j - leng):(j + leng)) = 0.
					;medianframe((i - leng):(i + leng),(j - leng):(j + leng)) = 0.
					
					sigmaframe(max([(min([i,sz(1)-1]) - leng),0]):min([(i + leng), sz(1)-1]),max([(min([j,sz(2)-1]) - leng),0]):min([(j + leng), sz(2)-1])) = 0.
					medianframe(max([(min([i,sz(1)-1]) - leng),0]):min([(i + leng), sz(1)-1]),max([(min([j,sz(2)-1]) - leng),0]):min([(j + leng), sz(2)-1])) = 0.
				endelse

			endelse
			
		endfor
	endfor

return, {sigmaframe:sigmaframe, medianframe:medianframe}

end	
			
	
