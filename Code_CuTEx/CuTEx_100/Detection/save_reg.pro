function save_reg, source, source_ellipse, color, a1, outdir, stsig, coord_system=coord_, unitcoord=unitcoord,nanflag=nanflag

		
	if (color ne 'white') and (color ne 'black') and (color ne 'red') and (color ne 'blue') and (color ne 'green') and (color ne 'yellow') and (color ne 'cyan') and (color ne 'magenta') then begin
		print, "Wrong color for region file"
		return, -1
	endif
	if (color eq 'cyan') then print, 'Wrong color. Cyan already used for sources nearby NaN pixels"
	
	if keyword_set(mask) then begin
	
		prefix = 'selected_'
		good = where(mask eq 1, n)
	
	endif else begin
	
		prefix = ''
		n = n_elements(source(0,*))
		good = lindgen(n)
	endelse
			
if keyword_set(coord_)  then begin
	if NOT keyword_set(unitcoord) then suffix = 'fk5' else  suffix = unitcoord
	if unitcoord eq 'galactic' then begin
		st = 'point(' 
		fmt = '(a6,a12,1x,a12)'
		endif else begin
		st = 'j2000; point('
		fmt = '(a12,a12,1x,a12)'
	endelse
	
	infix = 'i'	
	
	coord = strarr(2,n)
	coord(0,*) = strmid(coord_(0,good(*)),0,12)
	coord(1,*) = strmid(coord_(1,good(*)),0,12)
	

	
endif else begin
	suffix ='image'
	st = 'point('
	stend = ')'
	infix = ''
	fmt = '(a6,a5,1x,a5)'
	
	coord = strarr(2,n)
	coord(0,*) = strcompress(string(source(0,good(*)) + 1), /remove_all)
	coord(1,*) = strcompress(string(source(1,good(*)) + 1), /remove_all)

endelse

	openw,unit1,outdir+'/sources_'+prefix+suffix+'_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'.reg',/get_lun
	
	printf,unit1,'# File produced by detection.pro'
	printf,unit1,'# field :  '+a1
	printf,unit1,'# Coordinates of detected sources'
	printf,unit1,'global color='+color+' font='+'"helvetica 12 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0'
	printf,unit1, suffix

for kp = 0L, n-1 do begin
	
	if keyword_set(nanflag) then if nanflag(kp) lt 4 then supplystr = ' # color=cyan' else supplystr= ' '
	printf,unit1,  st+strcompress(pad_parameter(coord(0,kp),' ', 20)+','+pad_parameter(coord(1,kp),' ', 20), /REMOVE_ALL)+')'+' # point=cross '+strmid(supplystr,strpos(supplystr,'#')+1)
	printf,unit1, 'ellipse('+coord(0,kp)+','+coord(1,kp)+','+strcompress(string(source_ellipse(0,kp)/2.),/remove_all)+infix+','+$
	strcompress(string(source_ellipse(1,kp)/2.),/remove_all)+infix+','+strcompress(string(source_ellipse(2,kp)/!pi*(180. mod 360.)),/remove_all)+')'+supplystr
        printf,unit1,'# text('+coord(0,kp)+','+coord(1,kp)+') text={'+strcompress(string(long(kp)+1),/remove_all)+'}'+strmid(supplystr,strpos(supplystr,'#')+1)

endfor


close,unit1
free_lun,unit1

return, 0

END
