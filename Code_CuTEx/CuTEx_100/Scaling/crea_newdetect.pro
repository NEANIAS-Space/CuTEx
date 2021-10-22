pro crea_newdetect, indir, infile, bandin, headerin, band, headerout, outdir, outfile, photall=photall

wave = [70, 160, 250, 350, 500]
pixsize = [3.2, 4.5, 6.0, 8.0, 11.5]

bandbeam = [5., 12.14, 17.86, 25., 35.8]

; FROM DETECTION SOURCE FILE
if NOT KEYWORD_SET(photall) then begin

	readcol, indir+infile, id, x, y, xa, ya, pa, fitflag, ra, dec, format ='I,I,I,F,F,F,F,D,D'
	print, 'Source file produced on '+strcompress(string(bandin,format='(I3)'),/REMOVE_ALL)+' band map'
	
	indx = where(wave eq bandin,cout) 
	if cout eq 0 then begin
		print, 'Error - Input Band not recognized'
		goto, last
	endif

	if indx eq 4 then begin
	 	print, 'Error - Input source file is relative to longest wavelength (500 micron)'
		goto, last
	endif
	
	pix2asec = pixsize(indx(0))
	
	fwhm_x = xa*pix2asec(0)
	fwhm_y = ya*pix2asec(0)
	
;	xyxy, headerin, headerout, x - 1, y - 1, xout, yout
	euler, ra, dec, ra_l, dec_l, 1
	adxy, headerin, ra_l, dec_l, x_in, y_in
	xyxy, headerin, headerout, x_in, y_in, xout, yout
	
;	adxy, headerout, ra_l, dec_l, xout, yout
	print, 'Input Band Beam', bandbeam(indx)
	;____________________________	
	; THIS IS FOR DS 9 FORMAT!!!!
	;____________________________	
	
	xout = xout + 1
	yout = yout + 1 
		
	j = where(wave eq band)
	j = j(0)
	
	print, 'Working on band', wave(j)
	print, 'Output band beam', bandbeam(j)
	resize = (bandbeam(j)^2 - bandbeam(indx)^2)

	print, 'Pix 2 Asec = ', pix2asec
	print, 'New Pix size = ', pixsize(j)
	print, 'Resize factor = ', resize

	check_zero_x = where(fwhm_x eq 0, nzx)
	check_zero_y = where(fwhm_y eq 0, nzy)
	
	fwhm_xout = SQRT(fwhm_x^2 + resize(0))
	fwhm_yout = SQRT(fwhm_y^2 + resize(0))
	
	fwhm_xout_pix = fwhm_xout/pixsize(j)
	fwhm_yout_pix = fwhm_yout/pixsize(j)

;	check_zero_x = where(FINITE(fwhm_xout_pix) eq 0, nzx)
;	check_zero_y = where(FINITE(fwhm_yout_pix) eq 0, nzy)

	if nzx gt 0 then fwhm_xout_pix(check_zero_x) = 0.
	if nzy gt 0 then fwhm_yout_pix(check_zero_y) = 0.
			
	hours = convert_ardec(ra, dec)
	
	outfile = strmid(infile,0,strpos(infile,'.dat'))+'_band_'+strcompress(string(wave(j),format='(I3)'),/REMOVE_ALL)+'.dat'
	
	openw, unit, outdir+outfile, /get_lun
	openw, unit2, outdir+strmid(outfile,0,strpos(outfile,'.dat'))+'.reg', /GET_LUN
	openw, unit3, outdir+strmid(outfile,0,strpos(outfile,'.dat'))+'_wcs.reg', /GET_LUN

	;PRINT SOURCE FILE
	
	printf,unit,'# File produced by detection.pro - Adapted from the '+strcompress(string(bandin,format='(I3)'),/REMOVE_ALL)+' photometry'
	printf,unit,'# field :  
	printf,unit,'# Coordinates of detected sources'
	printf,unit,'# '
	printf,unit,'# sourceID      X      Y	  Xaxis    Yaxis    PA   FitFlag   RA	     DEC	   RA		 DEC   ';	Back	   Rms  '
	printf,unit,'#  	    pxl    pxl   pxl	 pxl	 Rad		Deg	   Deg         h:m:s	      d:m:s  ';     MJy/sr    MJy/sr'
	printf,unit,'# ============================================================================';======================='
	
	for kp=0L, n_elements(id)-1 do printf,unit,format='(2x,i5,2(2x,f9.4),1x,4(2x,f9.4),3x,f10.6,2x,f10.6,2x,a12,2x,a12,2x,f7.2,2x,f7.2,2x,i1)',id(kp), xout(kp), yout(kp), fwhm_xout_pix(kp), fwhm_yout_pix(kp), pa(kp), fitflag(kp), ra(kp), dec(kp), hours.ras(kp),hours.decs(kp)

	
	
	;PRINT RELATIVE REGION FILE	
	printf,unit2,'# File produced by detection.pro - Adapted from the '+strcompress(string(bandin,format='(I3)'),/REMOVE_ALL)+' photometry'
	printf,unit2,'# field :  
	printf,unit2,'# Coordinates of detected sources'
	printf,unit2,'global color=red font='+'"helvetica 12 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0'
	printf,unit2,'image'
	
	for kp=0L,n_elements(id)-1 do begin
  		printf,unit2,'point ', xout(kp), yout(kp)
		printf,unit2,'ellipse('+strcompress(string(xout(kp)),/REMOVE_ALL)+','+strcompress(string(yout(kp)),/REMOVE_ALL)+','+strcompress(string(fwhm_xout_pix(kp)/2.),/remove_all)+','+strcompress(string(fwhm_yout_pix(kp)/2.),/remove_all)+','+strcompress(string(pa(kp)/!pi*(180. mod 360.)),/remove_all)+')'
	        printf,unit2,'# text('+strcompress(string(xout(kp)),/REMOVE_ALL)+','+strcompress(string(yout(kp)),/REMOVE_ALL)+') text={'+strcompress(string(id(kp)),/remove_all)+'}'
	endfor
	
	
	;PRINT RELATIVE REGION FILE WCS	
	printf,unit3,'# File produced by detection.pro - Adapted from the '+strcompress(string(bandin,format='(I3)'),/REMOVE_ALL)+' photometry'
	printf,unit3,'# field :  
	printf,unit3,'# Coordinates of detected sources'
	printf,unit3,'global color=red font='+'"helvetica 12 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0'
	printf,unit3,'fk5'
	
	st='j2000; point'
	for kp=0L,n_elements(id)-1 do begin
  		printf,unit3,format='(a12,a12,1x,a12)',st,strmid(hours.ras(kp),0,12),strmid(hours.decs(kp),0,12)
		printf,unit3,'ellipse('+strmid(hours.ras(kp),0,12)+','+strmid(hours.decs(kp),0,12)+','+strcompress(string(fwhm_xout_pix(kp)/2.),/remove_all)+'i,'+strcompress(string(fwhm_yout_pix(kp)/2.),/remove_all)+'i,'+strcompress(string(pa(kp)/!pi*(180. mod 360.)),/remove_all)+')'
	        printf,unit3,'# text('+strmid(hours.ras(kp),0,12)+','+strmid(hours.decs(kp),0,12)+') text={'+strcompress(string(id(kp)),/remove_all)+'}'
	endfor

	close, unit
	close, unit2
	close, unit3
	free_lun, unit
	free_lun, unit2
	free_lun, unit3
	
	 	 
	 	 
	


; FROM PHOTOMETRY SOURCE FILE
endif else begin
	print, 'To be implemented'
	
endelse

last:

end
	


