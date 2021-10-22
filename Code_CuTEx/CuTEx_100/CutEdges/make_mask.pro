;! Function to remove the irregular borders of Hi-GAL maps
;! Can work on mapfile or on IDL array contaning the map
;! 
;! USAGE:
;!
;! outmap = make_mask(filename, region_coord)
;! 
;! or
;!
;! outmap = make_mask(array, region_coord, header=header)
;!
;!--------------------------------------------------------
;!
;! INPUT VARIABLES:
;! 
;! filename = string containing the name of map file
;!
;! 	   = alternatively array variable containing the map
;! 
;! region_coord = vertex coordinates of the region to be cutted 
;!		 syntax = [x1, y1, x2, y2, etc..] 
;!
;! KEYWORDS:
;!
;! header = (to be used only when working on the array variable as input) header file 
;!
;! OUTPUT VARIABLES:
;!
;! outmap = variable containing the new array of the map embedded into a zero-matrix
;! 
;! FITS files:
;!
;! In case it uses the mapfile as input the FITS files in the same path of the input map 
;! it creates a file with the same root of the input file with a suffix '_mask.fits'
;!
;! Otherwise it create a file 'masked.fits'
;!

function make_mask, ima, region, header=header, NAN=NAN

ch = size(ima)

if ch(0) eq 0 then im = readfits(ima,header) else begin
	
	if ch(0) gt 0 and NOT KEYWORD_SET(header) then begin
		print, "Error no header set - Continuing without header"
		header = ''
	endif
	
		
endelse		

nelem = n_elements(region)

if nelem eq 0 or (nelem and 1)  then begin
	print, 'Wrong Region Array'
	return, -1
endif	

indx = 2*indgen(fix(nelem/2))

ra  = region(indx) 
dec = region(indx+1)	

print, ra
print, dec	
	
adxy, header, ra,dec , xpos, ypos

;mask = im  
;mask(*,*) = 0.
;for i = 0,n_elements(mask(*,0))-1 do for j=0, n_elements(mask(0,*))-1 do mask(i,j) = inside( i, j, xpos, ypos)

roi = obj_new('IDLanROI', xpos, ypos)	
mask = roi->ComputeMask(dimensions=[n_elements(im(*,0)),n_elements(im(0,*))])
mask = FLOAT(mask)/MAX(mask)

if keyword_set(NAN) then begin
	
	dummy = where(mask eq 0,ndummy)
	if ndummy gt 0 then mask(dummy) = !VALUES.F_NAN	

endif 
	
im_mask = im*mask
if ch(0) eq 0 then writefits, strmid(ima, 0 , strpos(ima, '.fits'))+'_mask.fits', im_mask, header else writefits, 'masked.fits', im_mask, header

return, im_mask

end
