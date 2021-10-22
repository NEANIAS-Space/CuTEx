function remove_regions_new, mask, cut=cut, ALL_NEIGHBOURS=ALL_NEIGHBOURS

	if keyword_set(cut) eq 0 then cut = 3
		
	mask = mask/max(mask)
	;if KEYWORD_SET(ALL_NEIGHBOURS) then mask_label = label_region(mask, /ALL_NEIGHBORS, /ULONG) else mask_label = label_region(mask, /ULONG)
	if KEYWORD_SET(ALL_NEIGHBOURS) then mask_label = test_label_region(mask) else mask_label = test_label_region(mask)
        
	in = where(mask_label gt 0, cnt)
	
	if cnt gt 0 then begin 
		print, "Cutting all the region smaller than "+string(cut,FORMAT='(I3)')+" pixels"
		
		H = HISTOGRAM(mask_label, min = 1, BINSIZE=1, REVERSE_INDICES=revind)
		
		; plot, indgen(n_elements(H))+1, H, PSYM=10
		; plots, [1,n_elements(H)], [cut, cut], line=2
		
		selregion = where(H gt cut, nselregion, complement=noselregion, ncomplement=nnoselregion)
		
		if nselregion eq 0 then begin
		
			print, "No region bigger than "+string(cut, FORMAT='(I3)')+" pixels"
			return, -1
		endif
		
		maskfilter = mask
		
		for i = 0L, nnoselregion-1L do begin 
		
			indx = revind(revind(noselregion(i)):revind(noselregion(i)+1)-1)
			wheretomulti, maskfilter, indx, c, r
			maskfilter(c,r) = 0
			
		endfor 
		
	
;		print, "Found "+string(TOTAL(H(selregion)),format='(i10)')+" pixel groups"
		print, "Found "+string(nselregion,format='(i10)')+" pixel groups"

	endif else begin
		
		print, "Found no pixel groups "
	
	endelse	
	
	return, maskfilter
	
end	
				
		
		
