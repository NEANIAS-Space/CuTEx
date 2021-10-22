function check_nan, ima, limit_fract=limit_fract
	
	if NOT keyword_set(limit_fract) then limit_fract = 0.1 else limit_fract = limit_fract
	
	dummy = ima & dummy(*) = 0.
	indx = where(FINITE(ima) eq 0, nindx)

	if (nindx gt 0.) then begin
		
		dummy(indx) = 1
		;dummy_mask = label_region(dummy, /ULONG)
		dummy_mask = test_label_region(dummy)
		dummy_mask = FLOAT(dummy_mask)
		
		H = HISTOGRAM(dummy_mask, min=0, bin=1, location=X, REVERSE_INDICES=rr)
				
		; THERE SHOULD NOT BE ANY SKIPPED LABEL
		
		checkindx = where( H / FLOAT(N_ELEMENTS(DUMMY)) lt LIMIT_FRACT and X ne 0, ncheckindx, COMPLEMENT=nncheckindx, NCOMPLEMENT=nnncheckindx)
		
		if ncheckindx gt 0 then begin
		
			for i = 0, ncheckindx - 1 do begin
			
				nanpos = rr(rr(checkindx(i)):rr(checkindx(i)+1)-1)
				wheretomulti, dummy, nanpos, xnanpos, ynanpos

				if i eq 0 then xnanpixel = xnanpos else xnanpixel = [xnanpixel, xnanpos]
				if i eq 0 then ynanpixel = ynanpos else ynanpixel = [ynanpixel, ynanpos]
			
			
			endfor

			nanpixel = [[xnanpixel],[ynanpixel]]
		
		endif else begin
		
			nanpixel = 0.

		endelse	
		
	
		for j = 0, nnncheckindx-1 do begin
			
			dummy_mask(rr(rr(nncheckindx(j)):rr(nncheckindx(j)+1)-1)) = -nncheckindx(j)
		
		endfor
		
		
		inside_nans = where(dummy_mask gt 0, ninside)
		outside_nans = where(dummy_mask lt 0, noutside)
		
		if ninside gt 0 then dummy_mask(inside_nans) = 1.
		if noutside gt 0 then dummy_mask(outside_nans) = -1.
		
		check = 1
		
	endif else begin
	
		print, "No NaN found on the map"
		nanpixel = 0
		dummy_mask = 0
		check = 0
	
	endelse
	
	return, {nanpixels:nanpixel, nanmask:dummy_mask, check:check}
	
end
