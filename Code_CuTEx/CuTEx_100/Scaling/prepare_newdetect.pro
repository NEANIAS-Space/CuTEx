function prepare_newdetect, workdir, map, sourceslist, fact=fact,  psfpix=psfpix, outfile=outfile

if not keyword_set(fact) then fact = 2.
if not keyword_set(outfile) then outfile = 'newdetectlist.dat'

readcol, sourceslist, ra , dec, format='d,d'

im = readfits(map, h)
sz = size(im)

distlen = 2

; BUILD DETECTION LIST
openw,unit, workdir+'/'+outfile,/get_lun, width=1000

tags = ['ID', 'X', 'Y', 'X_AXIS', 'Y_AXIS', 'PA', 'FIT_FLAG', 'RA', 'DEC', 'RA_STRING', 'DEC_STRING','BORDER_FLAG', 'CLUMP_FLAG', 'DIST_FLAG', 'GUESS_FLAG', 'NAN_FLAG',$
'DER2X', 'DER2Y', 'DER2X45', 'DER2Y45', 'DER2X_THR', 'DER2Y_THR', 'DER2X45_THR', 'DER2Y45_THR' ]


tagsfmt = ['INT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'DOUBLE', 'DOUBLE', 'CHAR', 'CHAR', 'INT', 'INT', 'FLOAT', 'INT', 'INT', $
'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT']

tagsunit = [' ', 'pixel', 'pixel', 'pixel', 'pixel', 'rad', ' ', 'degrees', 'degrees', ' ', ' ', ' ' , ' ', 'pixel', ' ',' ', $
'MJy/sr/pix^2', 'MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2']


field_len = max([strlen( [tags, tagsfmt,tagsunit]),15])

printstr= '|'
for r = 0, n_elements(tags)-1 do printstr = printstr+pad_parameter(tags(r),' ', field_len)+'|'
printf, unit, printstr

printstr= '|'
for r = 0, n_elements(tagsfmt)-1 do printstr = printstr+pad_parameter(tagsfmt(r),' ', field_len)+'|'
printf, unit, printstr

printstr= '|'
for r = 0, n_elements(tagsunit)-1 do printstr = printstr+pad_parameter(tagsunit(r),' ', field_len)+'|'
printf, unit, printstr

id = indgen(n_elements(ra))+1

checkcoor = sxpar(h,'CTYPE1')

if strmid(checkcoor,0,4) eq 'GLON' then begin

	print, 'Galactic coordinate header - switching to Equatorial coordinates'
	;euler, ra, dec, lat, lon, 2
	heuler, h, /CELESTIAL
	unitcoord = 'galactic'
	
endif 	else unitcoord = 'fk5'

adxy, h, ra, dec, xpos, ypos

pixsize = sxpar(h, 'CDELT1')

if NOT keyword_set(psfpix) then psfpix = 3.0

fwhm_x  = fltarr(n_elements(ra))
fwhm_y  = fltarr(n_elements(dec))

fwhm_x(*) = psfpix
fwhm_y(*) = psfpix

pa = fltarr(n_elements(ra))
pa(*) = 0.

fflag = fltarr(n_elements(ra))
fflag(*) = 1.


ras=strarr(n_elements(ra))
decs=strarr(n_elements(ra))


for jj=0L,n_elements(ra)-1 do begin
  	
	dummy_ra = sixty(ra(jj)/15.)
	dummy_dec = sixty(dec(jj))
	
	dummy_hour = string(dummy_ra(0),format='(I2)')
	if dummy_dec(0) lt 0 then signstring = '-' else signstring = ' '
	dummy_degree = signstring+string(ABS(dummy_dec(0)),format='(I2)')
	if dummy_ra(1) lt 1. then dummy_min = '00' else if dummy_ra(1) lt 10. then dummy_min = '0'+strcompress(string(dummy_ra(1),format='(I1)'),/REMOVE_ALL) else dummy_min = strcompress(string(dummy_ra(1),format='(I2)'),/REMOVE_ALL)
	if dummy_ra(2) lt 1. then dummy_sec = '0'+strcompress(string(dummy_ra(2),format='(F4.2)'),/REMOVE_ALL) else if dummy_ra(2) lt 10. then dummy_sec = '0'+strcompress(string(dummy_ra(2),format='(F4.2)'),/REMOVE_ALL) else dummy_sec = strcompress(string(dummy_ra(2),format='(F5.2)'),/REMOVE_ALL)
	
	if dummy_dec(1) lt 1. then dummy_amin = '00' else if dummy_dec(1) lt 10. then dummy_amin = '0'+strcompress(string(dummy_dec(1),format='(I1)'),/REMOVE_ALL) else dummy_amin = strcompress(string(dummy_dec(1),format='(I2)'),/REMOVE_ALL)
	if dummy_dec(2) lt 1. then dummy_asec = '0'+strcompress(string(dummy_dec(2),format='(F4.2)'),/REMOVE_ALL) else if dummy_dec(2) lt 10. then dummy_asec = '0'+strcompress(string(dummy_dec(2),format='(F4.2)'),/REMOVE_ALL) else dummy_asec = strcompress(string(dummy_dec(2),format='(F5.2)'),/REMOVE_ALL)

	
	ras(jj) = strcompress(dummy_hour+':'+dummy_min+':'+dummy_sec,/REMOVE_ALL)
	decs(jj) = strcompress(dummy_degree+':'+dummy_amin+':'+dummy_asec,/REMOVE_ALL)
	
endfor

bflag = fltarr(n_Elements(ra))
cflag = fltarr(n_elements(ra))
dflag = fltarr(n_elements(ra))
gflag = fltarr(n_elements(ra))
nflag = fltarr(n_elements(ra))

xd2_val = dblarr(n_elements(ra))
yd2_val = dblarr(n_elements(ra))
x45d2_val = dblarr(n_elements(ra))
y45d2_val = dblarr(n_elements(ra))
xd2m_val = dblarr(n_elements(ra))
yd2m_val = dblarr(n_elements(ra))
x45d2m_val = dblarr(n_elements(ra))
y45d2m_val = dblarr(n_elements(ra))

bflag(*) = 0.
cflag(*) = 0.

gflag(*) = 4.
nflag(*) = 4


xd2 = readfits(workdir+'der2x_'+map,ha1,/silent)
yd2 = readfits(workdir+'der2y_'+map,ha1,/silent)
x45d2 = readfits(workdir+'der2x45_'+map,ha1,/silent)
y45d2 = readfits(workdir+'der2y45_'+map,ha1,/silent)

xd2m = readfits(workdir+'der2x_'+strmid(map,0,strpos(map,'.fits'))+'_median.fits',ha1,/silent)
yd2m = readfits(workdir+'der2y_'+strmid(map,0,strpos(map,'.fits'))+'_median.fits',ha1,/silent)
x45d2m = readfits(workdir+'der2x45_'+strmid(map,0,strpos(map,'.fits'))+'_median.fits',ha1,/silent)
y45d2m = readfits(workdir+'der2y45_'+strmid(map,0,strpos(map,'.fits'))+'_median.fits',ha1,/silent)

szxd2 = size(xd2)
szyd2 = size(yd2)
szx45d2 = size(x45d2)
szy45d2 = size(y45d2)

szxd2m = size(xd2m)
szyd2m = size(yd2m)
szx45d2m = size(x45d2m)
szy45d2m = size(y45d2m)

for k = 0, n_elements(ra)-1 do begin
	
	if szxd2(0) eq 2 then begin
		xd2_val(k) = xd2(xpos(k),ypos(k))
	 
	endif else xd2_val(k) = 0.

	if szxd2m(0) eq 2 then begin
		xd2m_val(k) = xd2m(xpos(k),ypos(k))
	 
	endif else xd2m_val(k) = 0.
	
	if szyd2(0) eq 2 then begin
		yd2_val(k) = yd2(xpos(k),ypos(k))
	
	endif else yd2_val(k) = 0.

	if szyd2m(0) eq 2 then begin
		yd2m_val(k) = yd2m(xpos(k),ypos(k))
	 
	endif else yd2m_val(k) = 0.
	
	if szx45d2(0) eq 2 then begin
		x45d2_val(k) = x45d2(xpos(k),ypos(k))
	
	endif  else x45d2_val(k) = 0.

	if szx45d2m(0) eq 2 then begin
		x45d2m_val(k) = x45d2m(xpos(k),ypos(k))
	 
	endif else x45d2m_val(k) = 0.
	
	if szy45d2(0) eq 2 then begin
		y45d2_val(k) = y45d2(xpos(k),ypos(k))
	
	endif  else y45d2_val(k) = 0.
	if szy45d2m(0) eq 2 then begin
		y45d2m_val(k) = y45d2m(xpos(k),ypos(k))
	 
	endif else y45d2m_val(k) = 0.
	
	
	checksurr =im(xpos(k)-distlen:xpos(k)+distlen, ypos(k)-distlen:ypos(k)+distlen)
	maxflux = max(checksurr,maxpos)
	wheretomulti,checksurr, maxpos, cmax, rmax

	;maxfluxpos = norm([cmax, rmax] - [xpos(k) - nearint(xpos(k)-distlen),ypos(k) - nearint(ypos(k)-distlen)])
		maxfluxpos = norm([cmax, rmax] - [distlen,distlen])
							
	
	dflag(k) = maxfluxpos
endfor		

;help, id, xpos, ypos, fwhm_x, fwhm_y, $
;pa, fflag, ra,dec, ras,decs, bflag, cflag, dflag, nflag

for kp = 0L, n_elements(ra)-1 do begin
	printstr = ' '+ pad_parameter(id(kp), ' ', field_len)+ ' '+ pad_parameter(xpos(kp)+1, ' ', field_len)+ ' '+ pad_parameter(ypos(kp)+1, ' ', field_len)+$
		' '+ pad_parameter(fwhm_x(kp), ' ', field_len)+ ' '+ pad_parameter(fwhm_y(kp), ' ', field_len)+ $
		' '+ pad_parameter(pa(kp), ' ', field_len)+ ' '+ pad_parameter(fflag(kp), ' ', field_len)+ $
		' '+ pad_parameter(ra(kp), ' ', field_len)+ ' '+ pad_parameter(dec(kp), ' ', field_len)+ $
		' '+ pad_parameter(ras(kp), ' ', field_len)+ ' '+ pad_parameter(decs(kp), ' ', field_len)+ $
		' '+ pad_parameter(bflag(kp), ' ', field_len)+ ' '+ pad_parameter(cflag(kp), ' ', field_len)+ ' '+ pad_parameter(dflag(kp), ' ', field_len)+ ' '+ pad_parameter(gflag(kp), ' ', field_len)+' '+ pad_parameter(nflag(kp), ' ', field_len)+$
		' '+ pad_parameter(xd2_val(kp), ' ', field_len)+ ' '+ pad_parameter(yd2_val(kp), ' ', field_len)+$
		' '+ pad_parameter(x45d2_val(kp), ' ', field_len)+ ' '+ pad_parameter(y45d2_val(kp), ' ', field_len)+$
		' '+ pad_parameter(xd2m_val(kp), ' ', field_len)+ ' '+ pad_parameter(yd2m_val(kp), ' ', field_len)+$
		' '+ pad_parameter(x45d2m_val(kp), ' ', field_len)+ ' '+ pad_parameter(y45d2m_val(kp), ' ', field_len)
		
	printf, unit, printstr

endfor

free_lun, unit
; BUILD SOURCEMASK

sourcesmask = fltarr(sz(1),sz(2))

qcompact = indgen(n_elements(ra))
nqcompact = n_elements(ra)

for i = 0, nqcompact-1 do begin

	x_vert=fltarr(8) 
	y_vert=fltarr(8)
   	
	for vert=0,7 do begin

   		x_vert(vert)=xpos(qcompact(i))+fact*fwhm_x(qcompact(i))/2.*(cos(pa(qcompact(i)))*cos(vert*!pi/4.))- fact*fwhm_y(qcompact(i))/2.*(sin(pa(qcompact(i)))*sin(vert*!pi/4.)) ; The positions in the detection file are
   		y_vert(vert)=ypos(qcompact(i))+fact*fwhm_x(qcompact(i))/2.*(sin(pa(qcompact(i)))*cos(vert*!pi/4.))+ fact*fwhm_y(qcompact(i))/2.*(cos(pa(qcompact(i)))*sin(vert*!pi/4.)) ; shifted by 1 pixel in x and y direction
 	
	endfor

	roi=obj_new('IDLanROI',x_vert,y_vert)
	tmp_mask=roi->ComputeMask(dimensions=[sz(1),sz(2)])
	tmp_mask=tmp_mask/255.*(id(qcompact(i))+1)  ;put mask value equal to source ID number
	
	sourcesmask=sourcesmask+tmp_mask
	qsum=where(sourcesmask gt id(qcompact(i))+1,nqsum)
	
	if (nqsum gt 0) then sourcesmask(qsum) = id(qcompact(i))+1
	obj_destroy,roi

endfor

if FILE_EXIST(workdir+'/'+strmid(map,0,strpos(map,'.fits'))+'_sourcesmask.fits') eq 1 then begin

	print, 'Existing SourceMask found - copy the old one in the file'
	print, 'Original   ', workdir+'/'+strmid(map,0,strpos(map,'.fits'))+'_sourcesmask.fits'
	print, 'Backup   ', workdir+'/'+strmid(map,0,strpos(map,'.fits'))+'_backup.fits'
	
	spawn, 'cp '+workdir+'/'+strmid(map,0,strpos(map,'.fits'))+'_sourcesmask.fits '+ workdir+'/'+strmid(map,0,strpos(map,'.fits'))+'_backup.fits'
endif
	
writefits, workdir+'/'+strmid(map,0,strpos(map,'.fits'))+'_sourcesmask.fits', sourcesmask, h 


source = [TRANSPOSE(xpos),TRANSPOSE(ypos)]
source_ellipse = [TRANSPOSE(fwhm_x),TRANSPOSE(fwhm_y), TRANSPOSE(pa), TRANSPOSE(fflag)]

; COORDINATE UNIT

if unitcoord eq 'fk5' then begin

	coord = [transpose(ras),transpose(decs)]

endif else begin
	
	if unitcoord eq 'galactic' then begin
		  
		  	
		xyad,h,xpos,ypos,lll,bbb,/galactic
		  
  		  if n_elements(lll) gt 1 then begin 
		   
			  coord = [transpose(STRCOMPRESS(string(lll, FORMAT='(F15.6)'),/REMOVE_ALL)), transpose(STRCOMPRESS(string(bbb, FORMAT='(F15.6)'),/REMOVE_ALL))]
		   
		  endif else  coord = [STRCOMPRESS(string(lll, FORMAT='(F15.6)'),/REMOVE_ALL),STRCOMPRESS(string(bbb, FORMAT='(F15.6)'),/REMOVE_ALL)]

	endif

endelse



save_ = save_reg(source, source_ellipse, 'blue', map, workdir, '1.0', nanflag=nflag)
save_ = save_reg(source, source_ellipse, 'red', map, workdir, '1.0', coord_system=coord,unitcoord=unitcoord, nanflag=nflag)

save, outfile, filename='detection.sav'

return, outfile

end
