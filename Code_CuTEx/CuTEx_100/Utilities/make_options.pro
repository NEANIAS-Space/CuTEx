function make_options, optionfile, comment=comment

	if not keyword_set(comment) then comment = '#'

	nlines = NUMLINES(optionfile)
	
	openr, lun, optionfile, /GET_LUN
	
	checkallcomments = intarr(nlines)
	dummystr = ''
	
	for I = 0, nlines-1 do begin
		
		readf, lun, dummystr

		sz = size(parameters)
		
		if sz(1) eq 0 then parameters = [dummystr] else $
				parameters = [parameters,[dummystr]]

		check = strmid(dummystr,0,1)

;		print, i , dummystr, check

		if check eq '#' then checkcomment =  1 else checkcomment = 0
		checkallcomments[i] = checkcomment
		

		 
	endfor	
	
; 	For GDL I cannot use the label_region to identify the regions
;       indx = test_label_region(checkallcomments -1)
	
	lab = 0
	indx = checkallcomments*0.

	for l = 0, n_elements(checkallcomments) -1 do begin
		if checkallcomments[l] eq 1 then lab = lab + 1
		if checkallcomments[l] eq 0 then indx[l] = lab		
	endfor	
	
	im_ = parameters(where(indx eq 1))
	thr_ = parameters(where(indx eq 2))
	parstrdet_ = parameters(where(indx eq 3))
	parstrextr_ = parameters(where(indx eq 4))
	
	
	parstrdet = ''
	parstrextr = ''

	voidstr = strsplit(im_, ' ', /EXTRACT)
        imageStr = voidstr(2)	

	voidstr = strsplit(thr_, ' ', /EXTRACT)
	thr = voidstr(2)
	
	for k1 = 0, n_elements(parstrdet_)-1 do begin
		
		voidstr = strsplit(parstrdet_(k1), ' ', /EXTRACT)
		
		if strcompress(voidstr(2),/REMOVE_ALL) ne '0' then parstrdet = parstrdet+','+strcompress(voidstr(0),/REMOVE_ALL)+'='+$
			strcompress(voidstr(2),/REMOVE_ALL)
	
	
	endfor
	
	for k2 = 0, n_elements(parstrextr_)-1 do begin
		
		voidstr = strsplit(parstrextr_(k2), ' ', /EXTRACT)
		
		if strcompress(voidstr(2),/REMOVE_ALL) ne '0' then parstrextr = parstrextr+','+strcompress(voidstr(0),/REMOVE_ALL)+'='+$
			strcompress(voidstr(2),/REMOVE_ALL)
	
	endfor
	print,'dbg: ','image ', imageStr
	print,'dbg: ','thr ', thr
	print,'dbg: ','parstrdet ', parstrdet
	print,'dbg: ','parstrextr ', parstrextr
	return, {parstrdet:parstrdet, parstrextr:parstrextr, thr:thr, image:imageStr}


end 
