;! Function to read the input region coordinates file and build the array 
;! form for make_mask function
;!
;! USAGE:
;! 
;! region = read_region(regfile)
;!
;! INPUT VARIABLES:
;! 
;! regfile = string containing the region file created by DS9
;! 
;!------------------------------------------------------------------------

function read_region, regfile


openr, 1, regfile

dummy = strarr(1)

;Skip header

readf, 1, dummy
dummy_check = strmid(dummy, 0, 7)

while dummy_check ne 'polygon' do begin
	
	readf, 1, dummy
	dummy_check = strmid(dummy, 0, 7)

endwhile
close,1

region_str = strmid(dummy, strpos(dummy,'(')+1, strpos(dummy,')')-strpos(dummy,'(')-1)
region_str = strsplit(region_str,',',/EXTRACT)
region = FLOAT(region_str)


return, region

End
