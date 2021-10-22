function build_sourcemask, workdir, filename,sourceslist, fact, thr, psfpix, outfile=outfile

readcol, workdir+sourceslist, id, x, y, fw_x, fw_y, pa, inf, ra, dec,  format='I,I,I,F,F,F,F,F,F,F'

;if strmid(sourceslist,9,1) eq '.' then  begin
;	filelist = file_search(workdir+strmid(sourceslist, 17,strpos(sourceslist,'.dat')-17)+'*.fits',count=cnt)
;	filename = filelist(0)
;	
;	if cnt ge 1 then begin 
;		im = readfits(filename, h) 
;	endif
;
;endif else begin
;	
;	pos1 = strpos(sourceslist,'_')
;	pos_end = strpos(sourceslist,'.dat')
;
;	dummy = strmid(sourceslist, pos1 +1, pos_end - pos1 + 3)
;	filename = strmid(dummy, strpos(dummy, '_')+1, strpos(dummy,'.dat') - strpos(dummy, '_') - 1)+'.fits'
;	im = readfits(filename, h)
;	
;endelse		

im = readfits(filename, h)

sz = size(im)
sourcesmask = fltarr(sz(1),sz(2))

qcompact=where((fw_x le thr*psfpix or fw_y le thr*psfpix) and inf ne 0,nqcompact)

if (nqcompact eq 0) then begin
  print,' No source gfound with input criteria...no mask produced'
  return,-1
endif

for i = 0, nqcompact-1 do begin

	x_vert=fltarr(8) 
	y_vert=fltarr(8)
   	
	for vert=0,7 do begin

   		x_vert(vert)=x(qcompact(i))+fact*fw_x(qcompact(i))/2.*(cos(pa(qcompact(i)))*cos(vert*!pi/4.))- fact*fw_y(qcompact(i))/2.*(sin(pa(qcompact(i)))*sin(vert*!pi/4.)) - 1 ; The positions in the detection file are
   		y_vert(vert)=y(qcompact(i))+fact*fw_x(qcompact(i))/2.*(sin(pa(qcompact(i)))*cos(vert*!pi/4.))+ fact*fw_y(qcompact(i))/2.*(cos(pa(qcompact(i)))*sin(vert*!pi/4.)) - 1 ; shifted by 1 pixel in x and y direction
 	
	endfor

	roi=obj_new('IDLanROI',x_vert,y_vert)
	tmp_mask=roi->ComputeMask(dimensions=[sz(1),sz(2)])
	tmp_mask=tmp_mask/255.*(id(qcompact(i)))  ;put mask value equal to source ID number
	
	sourcesmask=sourcesmask+tmp_mask
	qsum=where(sourcesmask gt id(qcompact(i)),nqsum)
	
	if (nqsum gt 0) then sourcesmask(qsum) = id(qcompact(i))
	obj_destroy,roi

endfor

if not keyword_set(outfile) then  writefits, 'sourcesmask_'+filename, sourcesmask, h else writefits, outfile, sourcesmask, h

return, sourcesmask

end
