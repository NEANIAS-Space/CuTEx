pro readima_pipeline, fitsfile, outfile=outfile
	
	RDFITS_STRUCT, fitsfile, struct
	tags = tag_names(struct)
	
	indx = where(strmid(tags,0,3) eq 'HDR',cout)
	
	if cout eq 0 then begin
		
		print, "Error FITS file not recognize!"
		goto, fine

	endif else begin	
	
		i = 0
		
		REPEAT BEGIN

			i = i + 1
			if i gt cout then begin
				print, "Image not found in the FITS file!"
				goto, fine
			endif			
			
			header = struct.(indx(i))
			check = sxpar(header,'EXTNAME')
		ENDREP UNTIL strmid(check,0,5) eq 'image' or i gt cout 
		
		
		pos = where(strmid(tags,0,3) eq 'IM'+STRCOMPRESS(STRING(I,format='(I)'),/REMOVE_ALL))
		
		print, strmid(check,0,5)
		print, i
		image = struct.(pos)
		
		if NOT KEYWORD_SET(outfile) then writefits, strmid(fitsfile,0,strpos(fitsfile,'.fits'))+'_ext.fits', image, header else $
		writefits, outfile, image, header
	endelse			

	fine:
end
