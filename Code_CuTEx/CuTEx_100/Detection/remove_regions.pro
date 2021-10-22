function remove_regions, mask, cut=cut, ALL_NEIGHBOURS=ALL_NEIGHBOURS

	if keyword_set(cut) eq 0 then cut = 2
	
	in_ = where(mask eq 0,count)

	if count gt 0 then begin 
		wheretomulti, mask, in_, c, r
		mask(c,r) = 0.
	endif
	
	mask = mask/max(mask)
	;if KEYWORD_SET(ALL_NEIGHBOURS) then mask_label = label_region(mask, /ALL_NEIGHBOURS, /ULONG) else mask_label = label_region(mask, /ULONG)
	if KEYWORD_SET(ALL_NEIGHBOURS) then mask_label = test_label_region(mask) else mask_label = test_label_region(mask)
        
	in = where(mask_label gt 0, cnt)
	
	if cnt gt 0 then begin 
	
		mask_sort = mask_label(in(sort(mask_label(in))))
		mask_ind = mask_sort(uniq(mask_sort))
	
		dim_region = intarr(n_elements(mask_ind))
	
	; I have to get rid of first element of mask_label
	
;	stop
		for i = 0L, n_elements(mask_ind)-1 do dim_region(i) = n_elements(where(mask_label eq mask_ind(i)))
	
		rem_reg = where(dim_region le cut, cout)
		print, "Found ", n_elements(mask_ind) - cout, " pixel groups"
	
		for j = 0L, cout-1 do begin
		
			region_id = mask_ind(rem_reg(j))
			ind_ = where(mask_label eq region_id)
			wheretomulti, mask, ind_, c_, r_
			mask(c_,r_) = 0.
	
		endfor
	
	endif else begin
		
		print, "Found no pixel groups "
	
	endelse	
	
	return, mask
	
end	
				
		
		
