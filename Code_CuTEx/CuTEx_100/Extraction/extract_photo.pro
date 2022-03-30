pro extract_photo, 	indir, mapfile, workdir, fabiana_sourcelist, $
			outfile=outfile, margin=margin, maskfile=maskfile,$
			max_dist_fac=max_dist_fac, psflim=psflim, $
			adaptive_group=adaptive_group, closest_neigh=closest_neigh, $
			adaptive_window=adaptive_window, dmax_factor=dmax_factor, $
			smoothing=smoothing, posback=posback, $
			correl=correl, weight=weight, noforcefit=noforcefit, $
			dthr=dthr, backgfit=backgfit, centering=centering, $
			psfpix=psfpix, border=border, quiet = quiet, time=time, single=single


forward_function mpfit_gaussians
; 				ADD THE BACKGROUND FORWARD FUNCTION

; 
; KEYWORD:	
;				Smoothing 	-	Smooth the input image.		Smoothing is always to an odd number				-	(IMPLEMENTED!)
;				noforcefit	-	Not force the fitting dipendently from the fitflag flag in the input sourcelist			-	(IMPLEMENTED!)
;				Weight		-	Assume Photon Noise Regime for the map (Error equal to the Square Root of the Signal)		-	(IMPLEMENTED!)
;				Correl		-	Assume that the width and peak flux of the fitted sources are correlated in the error compute	-	(IMPLEMENTED!)
;				psflim		-	Define the interval variability from the initial guess for the source size (default 30%)	-	(IMPLEMENTED!)	
;				adaptive_window -	Assume variable size for the fitting window from the guessed size of each source		-	(IMPLEMENTED!)  
;				adaptive_group  -	Groups sources on basis of an adaptive length depending from the guessed size			-	(IMPLEMENTED!)
;				closest_neigh   -	Activate to select only the closest neighbour (from dmax_factor) sources in fitting process	-	(IMPLEMENETD!)
;				dthr		-	Threshold distance to define the closest neighbours (in pixel). Default use instead dmax_factor	-	(IMPLEMENTED!) 
;				dmax_factor	-	Fitting window magnitude size in PSF units (default) or in source sizes (with adaptive_window)  -	(IMPLEMENTED!)  
;				max_dist_fac	-	Distance Threshold for the grouping in PSF units (default) or in guessed sizes (with adpt._gr). -	(IMPLEMENTED!)
;				background	-	Selective to adopt a plane or a second order polynomial approximation for the background	-	(IMPLEMENETD!) 
; 				posback		-	Enable the possibility that the offset value for the background level is limited to be positive -	(IMPLEMENTED!)
;												
; -------------------------------------------------------------------------------------------------------
; Troubles: In case of multiple groups the reg file has always color = 'cyan'. Need to be manually edited.
;
;
; in_flag setted all at 1 to force the fitting with the guess.
;!!!!!!!!!!!!!!ATTENTION!!!!!!!!!!!!!!!
; in_flag setted all equal at 1 in this way it will force the fitting of sources with a psf even if the guess ellipse fit has not converged
;
; Now Group Sources to find Clusters to be fitted simultaneously - PARAMETER max_dist_fac
;
;_________________________________________________________________________________
; Read Information from header: 1) Pixel Size in Arcsec 2) Pixel Coordinates type 
;_________________________________________________________________________________

; it may be (e.g. in GLIMPSE) that the header contains the CD matrix and not the CDELT keyowards, so we have to fix this
; check header and set it to RA-DEC if found in GALACTIC (e.g. ATLASGAL), otherwise the conversion from pixel coordinate will not work ok
; Check if Errors from the fitting are positive. If one of the three errors over f_peak, sigma_x or sigma_y is negative then error on integrate flux 
; is assigned to be -999.
;
; Problems if the sources mask file is built with thresholds 
; greater or equal than 10

print, "------------------------------------------------------------------"
print, "Extraction Routine of CuTEx - ver 1.000 - Released 29 Sep 2016"
print, "------------------------------------------------------------------"
print, "------------------------------------------------------------------"


if keyword_set(quiet) then quiet = 1. else quiet = 0

if keyword_set(TIME) then  timer, /START

im=readfits(indir+mapfile,h,/silent)
extast, h, astr
;im_mask=writefits,indir+'/'+'sourcemask_'+stsig+'_'+a1,im_mask,ha1


;_________________________________________	
; READ THE SOURCES MASK FILE

;if strmid(fabiana_sourcelist,9,1) eq '.' then  begin
;	masklist = file_search(workdir+strmid(fabiana_sourcelist, 17,strpos(fabiana_sourcelist,'.dat')-17)+'*sourcesmask.fits',count=cnt)
	
;	if cnt eq 1 then begin 
;		print, "Reading the Sources Mask file"
;		im_mask=readfits(workdir+masklist(0),hmsk) 
;	endif else begin
    maskfile=strmid(mapfile,0,strpos(mapfile,'.fits'))+'_sourcesmask.fits'
	im_mask = readfits(workdir+maskfile,hmsk,/silent)
;		if keyword_set(maskfile) then begin
;			print, "Reading the Sources Mask file"
;			im_mask = readfits(workdir+maskfile,hmsk)
;		endif else print, "Please there is ambiguity on the source mask file. Please use MASKFILE keyword"
;	endelse
;
;endif else if keyword_SET(maskfile) then begin
;			print, "Reading the Sources Mask file"
		
;			in_mask = readfits(workdir+maskfile,hmsk)
;	endif else begin
;	   	print, "Please there is ambiguity on the source mask file. Please use MASKFILE keyword"
;		goto, fine
;endelse		
;_________________________________________	
	
;im_mask=readfits(workdir+'strmid(fabiana_sourcelist,strpos(fabiana_sourcelist,'_', /REVERSE_SEARCH),strmid(fabiana_sourcelist,'.fits'))+'_sourcesmask.fits',hmsk)



pos=strpos(mapfile,'.fits')
prefix=strmid(mapfile,0,pos)

if (keyword_set(outfile)) then begin
	regfile=workdir+outfile+'.reg'
endif else begin
	regfile=workdir+prefix+'_list_memb_*.reg'
endelse

checkfile = file_search(regfile,count=count)
;print, "File ", file_search(regfile)

if count gt 0 then begin
	print, "Attention Reg Files already exist. Cancelling old files"
	for kk = 0, count-1 do file_delete,checkfile(kk) 
	;print, "File ", file_search(regfile)
endif


asec2rad=1./3600./180.*!pi

knownband = ['PACS 70', 'PACS 160', 'SPIRE 250', 'SPIRE 350', 'SPIRE 500']
knownp2a = [3.2, 4.5, 6.0, 8.0, 11.5]
knownwave = [70., 160., 250., 350., 500.]
convfact = [4154.81, 2101.00, 117.72, 60.08, 29.30] ; Conversion Factors from Jy/pix or Jy/Beam to MJy/sr Assuming that the Beam is Diffraction Shaped and the ROMAGAL pixscale

;--------------------------------------------------------
;! Checking  Map info
;--------------------------------------------------------

print, "Checking Photometry Band of the Map "


	pix2asec=abs(sxpar(h,'CDELT1'))*3600.
	if (pix2asec eq 0) then pix2asec=abs(sxpar(h,'CD1_1'))*3600.
	band = sxpar(h,'BAND')			; Checks info on the band

if NOT keyword_set(psfpix) then begin 

	indxband = where(ABS(knownp2a - pix2asec) lt 1E-1)
	if indxband eq -1 and band eq 0 then begin
		print, "Band not Recognized - Not an Herschel - Hi-GAL Map"
	endif else begin

		if band ne 0 then print, "Band ", band, " Identified by Header" 
		if indxband ne -1 then print, "Band ", knownband(indxband), " Identified by pixel scale "
		print, " Each pixel is ", STRING(pix2asec,FORMAT='(F9.3)'), " arcsec wide"
	endelse	

;_____________________________________________
;
; INSERT HERE PARAMETERS FOR THE INSTRUMENT 
;
;_____________________________________________

	if indxband ne -1 then begin
		Diam = 350.	; Herschel Telescope diameter
		psf = 25.*knownwave(indxband(0))/Diam
		beam=2*!pi/(8.*alog(2.))*((psf/3600./180.*!pi)^2.)
		psf = psf/pix2asec
;		print, "PSF in pixel", psf
		if indxband eq 0 then begin 
			print, 'Attention - Blue Band detected - Assumed a scan speed 60"/s'
			print, 'Fixing the PSF size to 9.8" x 8.8" - Valid for fast scan speed' 	
			psf = 2.7 ; Assuming a minimum of PSFPIX = 2.7 for consistency with detection. Real number ~ 2.9 from circularization
			beam = 2*!pi/(8.*alog(2.))*(9.8*8.8)
		endif				
	endif  else begin
		print, "Error no PSF pix setted"
		goto, fine
	endelse		
endif else begin
	indxband = 0. 
	psf = psfpix
	convfact = 1.
	knownwave = sxpar( h, "WAVE")
	knownband = string(sxpar( h, "WAVE"))
endelse


if NOT keyword_set(psflim) then begin
	print, "Variation Intervals size not set "
	print, "Assuming 30% Variation respect the guessed size from the detection"
	psflim = [0.7,1.3]

endif 
	
if keyword_set(dmax_factor) then dmax_factor = dmax_factor else dmax_factor = 2.









;___________________________________________
; Check the Coordinate System used:
; In case it is Galactic System the conversion doesn't work
; Thus transform all to Equatorial System.


;k1=sxpar(h,'CTYPE1')
;k2=sxpar(h,'CTYPE2')


; Changing this to work whatever Projection is used.

checkctype1 = strmid(astr.ctype[0],0,strpos(astr.ctype[0],'-'))
checkctype2 = strmid(astr.ctype[1],0,strpos(astr.ctype[1],'-'))

;if ((astr.ctype[0] eq 'GLON-TAN') and (astr.ctype[1]  eq 'GLAT-TAN')) or ((astr.ctype[0] eq 'GLON-CAR') and (astr.ctype[1]  eq 'GLAT-CAR'))  then begin

if (checkctype1 eq 'GLON') and (checkctype2 eq 'GLAT') then begin

	heuler,h,/celestial
	print, "Map has Galactic Coordinates"
	unitcoord = 'galactic'
	
endif else begin

;	if (astr.ctype[0] eq 'RA---TAN') and (astr.ctype[1]  eq 'DEC--TAN') then begin

	if (checkctype1 eq 'RA') and (checkctype2 eq 'DEC') then begin
		print, "Map has Equatorial Coordinates"
		unitcoord = 'fk5'
	endif else begin
		print, "Map Coordinates not Recognize - There could be an issue with region files in wcs system'
		print, "Warning if your map is not in Equatorial coordinate (any projection) there will be problems!"
		unitcoord = 'wcs'
	endelse	
endelse	

print, "Checking Map Header to determine the map units"

mapunit1 = sxpar(h,'UNIT') 		; Check Map Units
mapunit2 = sxpar(h,'BUNIT')

m1 = size(mapunit1,/TYPE)
m2 = size(mapunit2,/TYPE)


if  m1 eq 2 and m2 eq 2 then begin 

	print, "No Definition of Unit type in Map Header - Assuming that maps are in MJy/sr" 
	intflux_corr = 1E6     

endif else begin
	
	if m1 eq 7 then mapunit=strcompress(mapunit1,/REMOVE_ALL) else mapunit=strcompress(mapunit2,/REMOVE_ALL)
	
	if mapunit eq 'Jy/pix' or mapunit eq 'Jy/Beam' or mapunit eq 'Jy/beam' then begin
	
		if mapunit eq 'Jy/pix' then begin
			omegapxl=((pix2asec/3600./180.*!pi)^2.) 
		
			intflux_corr = 1./omegapxl(0)  
		endif else begin 
		
			if n_elements(beam) eq 0 then begin
				beam = 2*!pi/(8.*alog(2.))*((psf*pix2asec/3600./180.*!pi)^2.)
				print, "Beam area computed from PSF assuming the Diffraction limit ", beam*((3600.*180)/!pi)^2, " arcsec^2"	
				print, "Warning this might be wrong if you have a PACS 70 micron map scanned in fast mode and the Band is not correctely identified!
			endif 
			
			intflux_corr = 1./beam(0)

		endelse

	endif else if mapunit eq 'MJy/sr' then begin 
		
		print, "Map Unit are recognized to be MJy/sr"
		intflux_corr = 1E6
				
	endif else begin
		print, " Unit not recognized - Assuming that maps are in MJy/sr "
		intflux_corr = 1E6 ; THIS HAS TO CHANGE !!!!!!!!!!! 1E6
		
	endelse
	
endelse 











;_________________________________________________________________
; Building up the definition for Photometry Table in IPAC format
;_________________________________________________________________

; Photometry File output

print, " Preparing IPAC table format for output files "


field_len=15

;col_name = ['N','group','ngroup','x','y', 'ra','dec','Wavelength','f_peak', 'fwhm_x','fwhm_y', 'pa','f_int','back','chi2_dof','rms', 'chi2',  'Status']
;col_type = ['int','int','int', 'float','float',  'double', 'double','double','double','double','double','float','double','double','double','double','double', 'int']
;col_unit = [' ', ' ', ' ', 'pixel', 'pixel' ,'degree' ,'degree' ,'micron' ,'MJy/sr' ,'arcsec' ,'arcsec' ,'degree ','Jy','MJy/sr',' ','MJy/sr',' ', ' ']

col_name = ['N','X','Y', 'RA','DEC', 'GLON', 'GLAT', 'WAVELENGTH','F_PEAK', 'FWHM_X','FWHM_Y', 'PA','F_INT','ERR_F_INT', 'BACK_ONPEAK','RMS', 'RMS_ON', 'RMS_OFF', 'RES2SUM', 'DOF',  'CHI2', 'CHI2OPP', $
	 'SIZE_FLAG', 'GROUP_FLAG', 'GAUSS_FLAG', 'CLUMP_FLAG', 'SHIFT_FIT', 'GUESS_FLAG', 'NCONTAM', 'STATUS', 'FIT_FLAG', 'RMS_ON_FLAG', 'RMS_OFF_FLAG', $
	 'D2XDET', 'D2YDET', 'D2X45DET', 'D2Y45DET', 'D2XTHR', 'D2YTHR', 'D2X45THR',  'D2Y45THR',$
	 'D2XFITDET', 'D2YFITDET', 'D2X45FITDET',  'D2Y45FITDET', 'D2XFITBCKDET', 'D2YFITBCKDET', 'D2X45FITBCKDET', 'D2Y45FITBCKDET', $
	 'D2XFITPEAK', 'D2YFITPEAK', 'D2X45FITPEAK', 'D2Y45FITPEAK','D2XFITBCKPEAK', 'D2YFITBCKPEAK', 'D2X45FITBCKPEAK', 'D2Y45FITBCKPEAK']
	 
col_type = ['int', 'float','float',  'double', 'double',  'double', 'double','double','double','double','double','double','float','double','double','double','double','double','double', 'int', 'double', 'double', $
	'CHAR', 'LONG','int','int',  'FLOAT', 'int',  'int', 'float', 'float', 'int',  'int',$
	'double', 'double','double', 'double', 'double', 'double','double', 'double', $
	'double', 'double','double', 'double', 'double', 'double','double', 'double', $
	'double', 'double','double', 'double', 'double', 'double','double', 'double']
	
col_unit = [' ',  'pixel', 'pixel' ,'degree' ,'degree' ,'degree' ,'degree' , 'micron' ,'MJy/sr' ,'arcsec' ,'arcsec' ,'degree ','Jy', 'Jy', 'MJy/sr', 'MJy/sr','MJy/sr', 'MJy/sr', ' ', ' ', ' ',$
	' ', ' ', ' ', ' ', ' ', 'pixel', ' ', ' ', ' ', ' ', ' ', ' ', $
	'MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2',$
	'MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2',$
	'MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2']

n_par=n_elements(col_name)

s1='|'
for j=0,n_par-1 do  begin
	if col_name(j) eq 'SIZE_FLAG' then begin
		jmark = j
		s1=s1+pad_parameter(col_name(j),' ',25)+'|' 
	endif else s1=s1+pad_parameter(col_name(j),' ',field_len)+'|'
	
endfor

s2='|'
for j=0,n_par-1 do  begin
	if j eq jmark then s2=s2+pad_parameter(col_type(j),' ',25)+'|' else s2=s2+pad_parameter(col_type(j),' ',field_len)+'|'
endfor

s3='|'
for j=0,n_par-1 do  begin
	if j eq jmark then s3=s3+pad_parameter(col_unit(j),' ',25)+'|' else s3=s3+pad_parameter(col_unit(j),' ',field_len)+'|'
endfor

; Photometry Errors File output

;col_name_err =['n','group','ngroup','x','y', 'ra','dec','Wavelength', 'err_f_peak', 'err_fwhm_x', 'err_fwhm_y','err_pa','err_f_int','chi2_dof','rms', 'rms_on', 'chi2',  'Status']
col_name_err = ['N','GROUP', 'NGROUP','X','Y', 'RA', 'DEC', 'GLON', 'GLAT', 'WAVELENGTH', 'ERR_F0','ERR_X0', 'ERR_Y0', 'ERR_FWHM_X', 'ERR_FWHM_Y','ERR_PA','ERR_F_INT', 'chi2_dof','rms', 'chi2',  'Status']

col_type_err =['int','int','int','float','float',  'double', 'double', 'double', 'double','double','double','double','double','double','double','float','double','double','double','double','double', 'int']
col_unit_err =[' ', ' ', ' ', 'pixel', 'pixel' ,'degree' ,'degree' ,'degree' ,'degree' , 'micron' , 'MJy/sr','arcsec', 'arcsec', 'arcsec' ,'arcsec','degree ','Jy',' ','MJy/sr','MJy/sr','MJy/sr',  ' ']

n_par=n_elements(col_name_err)

s4='|'
for j=0,n_par-1 do  s4 = s4 + pad_parameter(col_name_err(j),' ',field_len)+'|'
s5='|'
for j=0,n_par-1 do  s5 = s5 + pad_parameter(col_type_err(j),' ',field_len)+'|'
s6='|'
for j=0,n_par-1 do  s6 = s6 + pad_parameter(col_unit_err(j),' ',field_len)+'|'

; Parameter FILE output


col_name_par = ['N', 'GROUP', 'NGROUP', 'X_CENTER', 'Y_CENTER', 'X_WINDOW', 'Y_WINDOW', 'X_SOURCE', 'Y_SOURCE', 'GUESS_X_FWHM', 'GUESS_Y_FWHM', 'GUESS_PA', 'FIT_FLAG', 'NNANPIX'] 
col_type_par = ['int', 'int', 'int', 'int', 'int', 'int', 'int', 'float', 'float', 'double', 'double', 'double', 'double', 'int']
col_unit_par = [' ', ' ', ' ', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'rad', ' ', ' ']

n_par=n_elements(col_name_par)

s7='|'
for j=0,n_par-1 do  s7=s7+pad_parameter(col_name_par(j),' ',field_len)+'|'
s8='|'
for j=0,n_par-1 do  s8=s8+pad_parameter(col_type_par(j),' ',field_len)+'|'
s9='|'
for j=0,n_par-1 do  s9=s9+pad_parameter(col_unit_par(j),' ',field_len)+'|'


sdevcol = ['ID', 'SDEV1', 'SDEV2', 'SDEV3',  'SDEV4', 'SDEV4_ONSOU', 'SDEV4_NM', 'SDEV4_ONSOU_NM', 'SDEV5_ONSOU_NM']
sss1 = '|'
for j=0,n_elements(sdevcol)-1 do sss1=sss1+pad_parameter(sdevcol(j),' ',field_len)+'|'

sdevcoltype = ['int', 'float', 'float', 'float', 'float',  'float', 'float', 'float', 'float']
sss2 = '|'
for j=0,n_elements(sdevcoltype)-1 do sss2=sss2+pad_parameter(sdevcoltype(j),' ',field_len)+'|'

sdevcolunit = [' ', ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ']
sss3= '|'
for j=0,n_elements(sdevcolunit)-1 do sss3=sss3+pad_parameter(sdevcolunit(j),' ',field_len)+'|'

backcol = ['ID', 'A', 'B_X', 'C_Y']
backcoltype = ['int', 'float', 'float', 'float']
backcolunit = [' ', ' ', ' ', ' ']

if keyword_Set(BACKGFIT) then begin 
	backcol = [backcol, 'D_X2', 'E_Y2', 'F_XY']
	backcoltype = [backcoltype, 'float', 'float', 'float']
	backcolunit = [backcolunit , ' ', ' ', ' ']
	
endif	

bbb1 = '|'
for j=0,n_elements(backcol)-1 do bbb1=bbb1+pad_parameter(backcol(j),' ',field_len)+'|'
bbb2 = '|'
for j=0,n_elements(backcoltype)-1 do bbb2=bbb2+pad_parameter(backcoltype(j),' ',field_len)+'|'
bbb3 = '|'
for j=0,n_elements(backcolunit)-1 do bbb3=bbb3+pad_parameter(backcolunit(j),' ',field_len)+'|'

;-----------------------------------------------------------------------------------------------------------------
; do the smoothing if required. remember that the smoothing does not conserve the peak flux (it decreases), but it conserves the integrated flux of a source. 
; clearly the PSF will be larger by about sqrt(smooth_width), and the limits for gaussian width fitting and maximum distance for blended sources will have to 
; take that into account. IDL SMOOTH takes odd number as window width or rounds up to the upper odd if an even number is provided. 
; PLEASE make sure you provide an odd number as rescaling factors for the fitting parameter are based on the provided number <----

if (keyword_set(smoothing)) then  begin
	
	if smoothing eq 1 then begin
		print, "Warning! Smoothing parameter setted equal 1. No smoothing applied"
	endif else begin
		print, "Smoothing the Map. Attention the Peak Flux will decrease, but the Integrated Flux is conserved"
		smooth = smoothing mod 2	
		if smooth eq 0 then begin
			print, "Smoothing parameter is even. Rounding to the higher odd value"
			print, "Smoothing parameter equal to ", smoothing+1
			im = smooth(im,smoothing+1,/edge_truncate,/nan) 
		endif else begin
			print, "Smoothing parameter equal to ", smooothing
			im = smooth(im,smoothing,/edge_truncate,/nan) 
		endelse 
	endelse		 
	
endif









;______________________________________________________________________________________________________________________________
; WORK ON THE SOURCE LIST 
;	get the results from detection.pro (fabiana_sourcelist keyword) not a keyword anymore
;______________________________________________________________________________________________________________________________

print, "Reading the Input Source List"

; OLD METHOD NOW THE OUTPUT FORMAT FROM THE DETECTION IS DIFFERENT
;readcol,workdir+fabiana_sourcelist,nx,xorig,yorig,in_dx,in_dy,in_pa,in_flag,ra,dec,ra1,dec1, /SILENT

read_ipac_table, workdir+fabiana_sourcelist, detlist

nx = detlist.id

BORDERLIST = detlist.border_flag
checkNBorderList = where(borderlist eq 1, ncheckNBorderList)

if ncheckNBorderList eq n_elements(borderlist) then print, 'WARNING! Your Input list is composed all by sources nearby border! - Uncontrolled results may occur.'

;if NOT keyword_set(noforcefit) then begin
;	print, "Forcing the fitting around the PSF for the sources for which was not possible to determine a good guess"
;	in_flag(*) = 1	; This Force the fitting of PSF to the sources detected but for which it was not possible to determine a guess ellipse fit.  
;endif

; in case the input source list is not drawn from this image but is imported from another image,
; the set of pixel coordinates have to be recomputed from the RA-DEC pairs based on the header 
; parameteres on the current image (which is the one where we are going to perform photometry).
; I DO NOT have to shift them back one unit to make them consistent with IDL array indexing 
; because the IDL routine ADXY is taking care of that internally.

n=lindgen(n_elements(NX))+1

print, "The Source List is composed of ", strtrim(n_elements(nx),2), " objects"

if n_elements(nx) eq 0 then begin
	
	print, "Source list is Empty. No fitting is performed"
	goto, fine

endif

if NOT keyword_set(border) then begin
	;help, detlist
	bordercheck = where(borderlist eq 1, COMPLEMENT=nnbordercheck, NCOMPLEMENT=nnnbordercheck, nBordercheck)
	print, "Number of objects close to the border that will not be fitted ", strtrim(nbordercheck,2)
	if nnnBordercheck gt 0 then detlist = detlist(nnbordercheck)
	;help, detlist
endif

nx = detlist.id
Xorig = detlist.X
Yorig = detlist.Y
In_Dx = detlist.X_AXIS
In_Dy = detlist.Y_AXIS
In_PA = detlist.PA
In_FLAG = detlist.FIT_FLAG

IN_FLAG2 = IN_FLAG

RA = detlist.ra
DEC = detlist.dec
RA1 = detlist.ra_string
DEC1 = detlist.dec_string
IN_CLUMP = detlist.clump_flag
IN_DIST = detlist.DIST_FLAG
IN_GUESS = detlist.GUESS_FLAG

IN_D2X = detlist.DER2X
IN_D2Y = detlist.DER2Y
IN_D2X45 = detlist.DER2X45
IN_D2Y45 = detlist.DER2Y45

IN_D2X_THR = detlist.DER2X_THR
IN_D2Y_THR = detlist.DER2Y_THR
IN_D2X45_THR = detlist.DER2X45_THR
IN_D2Y45_THR = detlist.DER2Y45_THR

; I am finding strange problems with the conversion from COORDINATES to IMAGE POSITIONS when using the ASTROLIB routines. 
; Inconsistencies that I do not know where they come from, so I decide to not recompute the IMAGE positions from the CELESTIAL coordinates

adxy,h,ra,dec,x,y
;; Fixed an issue checked on ALMA images
;; x = xorig
;; y = yorig

;__________________________________________
;RESERVED FOR THE RMS MAPS
rms1=fltarr(n_elements(nx)) 
rms1(*)=-999.

;

;__________________________________________
; Removing of Border Pixels
;__________________________________________

; to avoid border problems avoid sources within a fitting box from the margin
if (keyword_set(margin)) then begin
	sz_marg = size(margin)
	if sz_marg(0) eq 0 then begin
		print, "Using same threshold for the distance to the border in both directions"
		print, "Removing sources closer to the image borders than ", nearint(margin), " pixels"
		xmargin = margin
		ymargin = margin
		
	endif else begin
		print, "Removing sources closer to the image borders "
		print, "in x direction ", nearint(margin(0)), " pixels and in y direction ", nearint(margin(1))		
		xmargin = margin(0)
	  	ymargin = margin(1)
	endelse

endif else begin	
	print, "No margin Keyword setted - trying with the full input list selected from BORDER_FLAG"
	xmargin = 0.
	ymargin = 0.
endelse	

qmarg=where((x gt 0 + xmargin) and (x lt n_elements(im(*,0)) - xmargin) and (y gt 0 + ymargin) and (y lt n_elements(im(0,*)) - ymargin))

print, "Number of sources withing the margin limit ", strtrim(n_elements(qmarg),2)
qth=lindgen(n_elements(qmarg))

if keyword_set(TIME) THEN begin
	print, "Estimating Time for Auxiliary stuffs (remove sources too close border, recognize map inputs,etc.)"
 	timer, /STOP, /PRINT, dt
	timer, /START
endif
;________________________________________
; SOURCE GROUPING
;________________________________________

;; nino 2014/01/29 - variable renaming for fast array access.

; Now Group Sources to find Clusters to be fitted simultaneously - PARAMETER max_dist_fac
;
; FOR ADAPTIVE GROUPING
;  dmax = [nearint(dmax_factor*max([in_dx(qmarg[qth[qsingle[i]]]),in_dy(qmarg[qth[qsingle[i]]])])),nearint(dmax_factor*max([in_dx(qmarg[qth[qsingle[i]]]),in_dy(qmarg[qth[qsingle[i]]])]))]
;


print, "Start defining the Source Groups "

; INSERT HERE CODE FOR ADAPTIVE GROUPPING

if NOT keyword_set(adaptive_group) then  begin
	print, "Grouping Sources using Fixed Distances"
	
	if keyword_SET(max_dist_fac) then begin
		print, "Using distance to group together sources closer than ", max_dist_fac, " times the PSF"
		max_dist=max_dist_fac*psf
	
	endif else begin

		print, "Using default distance for grouping together sources closer than of two times the PSF"
		max_dist_fac = 2.
		max_dist = max_dist_fac*psf

	endelse
	
	print, "Threshold distance for Grouping "+string(max_dist*pix2asec,FORMAT='(F9.3)')+" arcsec "

	if (n_elements(qth) ge 2) then begin
  	
		group=group_sources(x(qmarg(qth)),y(qmarg(qth)),max_dist,workdir+prefix)
	endif else if (n_elements(qth) eq 1) then begin
  	
		group=0
	
	endif else begin
  	
		print,'No source found!!'
  		goto, fine
	
	endelse

endif else begin
	print, "Grouping Sources using Adaptive Distances based on the Guessed Sizes"

	if keyword_SET(max_dist_fac) then begin
		print, "Using distance to group together sources closer than ", string(max_dist_fac,format='(F9.3)'), " times the Guessed Size"
			
	endif else begin

		print, "Using default distance to group together sources closer than of two times the Guessed Size"
		max_dist_fac = 2.
		max_dist = max_dist_fac*psf

	endelse
	if (n_elements(qth) ge 2) then begin
  		dd_ =dblarr(n_elements(x(qmarg(qth))))
  		for j=0,n_elements(x(qmarg(qth)))-1 do dd_(j) = max([in_dx(j),in_dy(j)])	; Evaluating the maximum axis of the sources

  		group=grp_src(x(qmarg(qth)),y(qmarg(qth)),max_dist_fac,workdir+prefix, distances=dd_)

	endif else if (n_elements(qth) eq 1) then begin
  	
		group=0
	
	endif else begin
  	
		print,'No source found!!'
  		goto, fine
	
	endelse

endelse	











;;______________________________________________________________________________
;;______________________________________________________________________________
;;______________________________________________________________________________
;;______________________________________________________________________________
;;______________________________________________________________________________

;_____________________________________________________
; OPENING OUTPUT FILES IN IPAC TABLE FORMAT
;_____________________________________________________

; PHOTOMETRY FILE . . . . - photometry file	- lun_photfile	
; PHOTOMETRY FILE . . . . - background data file	- lun_photfile_bak	
; SDEV FILE	. . . . . . . - sdev file		- lun_phot_dev
; ERROR PHOTOMETRY FILE . - error file		- lun_phot_err
; PARAMETERS FILE . . . . - parameter file	- lun_phot_par
; REGION FILE ALL . . . . - all objects file 	- lun_region
; REGION FILE ALL . . . . - all objects file wcs	- lun_region_wcs
; REGION FILE EACH GROUP. - single group file	- lung_group_region
; REGION FILE EACH GROUP. - single group file wcs	- lung_group_region_wcs

if (keyword_set(outfile)) then begin

  	photfile = workdir+outfile+'.dat'
  	photfilebck = workdir+outfile+'_backpar.dat'
	devfile = workdir+outfile+'_sdev.dat'
	errfile = workdir+outfile+'_err.dat'
	paramfile = workdir+outfile+'_parameters.dat'
 	regoutfile=workdir+outfile+'_allmemb.reg'
	regoutfilewcs = workdir+outfile+'_list_allmemb_wcs.reg'
	regfiles = workdir+outfile+'_list_memb_'
	
endif else begin
 
  	photfile = workdir+prefix+'_photall.dat'
  	photfilebck = workdir+prefix+'_backpar.dat'
	devfile = workdir+prefix+'_sdev.dat'
  	errfile = workdir+prefix+'_photall_err.dat'
	paramfile = workdir+prefix+'_parameters.dat'
  	regoutfile= workdir+prefix+'_list_allmemb.reg'
	regoutfilewcs = workdir+prefix+'_list_allmemb_wcs.reg'
	regfiles = workdir+prefix+'_list_memb_'

endelse

openw, lun_photfile, photfile, /GET_LUN, width=2000
openw, lun_photfile_bak, photfilebck, /GET_LUN, width=2000
openw, lun_phot_err, errfile, /GET_LUN, width=2000
openw, lun_phot_dev, devfile, /GET_LUN, width=2000
openw, lun_phot_par, paramfile,  /get_lun, width=2000
openw, lun_region, regoutfile, /get_lun
openw, lun_region_wcs, regoutfilewcs, /get_lun


;_________________________________________________________

launchstring = "extract_photo, '"+string(indir)+"',"+"'"+string(mapfile)+"','"+string(workdir)+"','"+string(fabiana_sourcelist)+"'"

if keyword_set(outfile) then launchstring = launchstring+", outfile='"+string(outfile)+"'"
if keyword_set(margin) then launchstring = launchstring+', margin=['+string(xmargin)+','+string(ymargin)+']'
if keyword_set(maskfile) then launchstring = launchstring+", maskfile='"+string(maskfile)+"'"
if keyword_set(max_dist_fac) then launchstring = launchstring+', max_dist_fac='+string(max_dist_fac)
if keyword_set(psflim) then launchstring = launchstring+', psflim=['+string(psflim(0))+','+string(psflim(1))+']'
if keyword_set(adaptive_group) then launchstring = launchstring+', adaptive_group='+string(adaptive_group)
if keyword_set(closest_neigh) then launchstring = launchstring+', closest_neigh='+string(closest_neigh)
if keyword_set(adaptive_window) then launchstring = launchstring+', adaptive_window='+string(adaptive_window)
if keyword_set(dmax_factor) then launchstring = launchstring+', dmax_factor='+string(dmax_factor)
if keyword_set(posback) then launchstring = launchstring+', posback='+string(posback)
if keyword_set(border) then launchstring = launchstring+', border='+string(border)
if keyword_set(correl) then launchstring = launchstring+', correl='+string(correl)
if keyword_set(weight) then launchstring = launchstring+', weight='+string(weight)
if keyword_set(noforcefit) then launchstring = launchstring+', noforcefit='+string(noforcefit)
if keyword_set(dthr) then launchstring = launchstring+', dthr='+string(dthr)
if keyword_set(backgfit) then launchstring = launchstring+', backgfit='+string(backgfit)
if keyword_set(centering) then launchstring = launchstring+', centering='+string(centering)
if keyword_set(psfpix) then launchstring = launchstring+', psfpix='+string(psfpix)
if keyword_set(quiet) then launchstring = launchstring+', quiet='+string(quiet)
if keyword_set(TIME) then launchstring = launchstring+', TIME='+string(TIME)

launchstring = strcompress(launchstring,/REMOVE_ALL)

printf,lun_photfile,'\ # File produced by extract_photo.pro - New Version 0.99! 
printf,lun_photfile,'\ # field :  '+mapfile
printf,lun_photfile,'\ # String used to launch extraction '

if strlen(launchstring) lt 500 then printf,lun_photfile,'\ '+launchstring else begin
	
	splitstr = strsplit(launchstring,',', /EXTRACT)
	npieces = n_elements(splitstr)

	startstr = '\'
	for p = 0, FLOOR(npieces/3) do startstr = startstr+' '+ splitstr(p)+','
	printf, lun_photfile, startstr+'$'
	
	startstr = '\'
	for p = FLOOR(npieces/3) + 1, FLOOR(2*npieces/3)  do startstr = startstr+' '+ splitstr(p)+','
	printf, lun_photfile, startstr+'$'
	
	startstr = '\'
	for p = FLOOR(2*npieces/3) + 1, npieces-1  do startstr = startstr+' '+ splitstr(p)+','
	printf, lun_photfile, strmid(startstr, 0, strpos(startstr, ',', /REVERSE_SEARCH))

endelse		

;_________________________________________________________


;_________________________________________
;PRINT HEADERS 
;_________________________________________

printf,lun_photfile,s1		;Printing Header Photometry File
printf,lun_photfile,s2
printf,lun_photfile,s3
printf,lun_photfile_bak,bbb1		;Printing Header Photometry File
printf,lun_photfile_bak,bbb2
printf,lun_photfile_bak,bbb3

printf,lun_phot_err,s4	;Printing Header Error Photometry File
printf,lun_phot_err,s5
printf,lun_phot_err,s6
printf, lun_phot_par, s7	;Printing Header Parameter File
printf, lun_phot_par, s8
printf, lun_phot_par, s9
printf, lun_phot_dev, sss1	;Printing Header Parameter File
printf, lun_phot_dev, sss2
printf, lun_phot_dev, sss3

color='white'
printf,lun_region,'# Region file format: DS9 version 4.0'
printf,lun_region,'# Filename: ./'+workdir+mapfile+'.fits  -  List of All detected sources'
printf,lun_region,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
printf,lun_region,'image'

color='white'
printf,lun_region_wcs,'# Region file format: DS9 version 4.0'
printf,lun_region_wcs,'# Filename: ./'+workdir+mapfile+'.fits  -  List of All detected sources'
printf,lun_region_wcs,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
printf,lun_region_wcs, unitcoord

if keyword_set(TIME) then begin
	print, "Estimating Time for Estimating Source Groups"
	timer, /STOP, /PRINT, ft
	timer, /START
endif
;__________________________________________
; FITTING SOURCES STARTS HERE!
;__________________________________________

if keyword_set(CORREL) then print, "Computing the Integrated Flux errors assuming correlation between gaussian parameters"
if keyword_set(adaptive_window) then print, "Adaptive Windowing around Sources to determine the local region used for the fit" else $
print, "Fixed Window around sources to determine the local region used for the fit: Size equal at ", STRING(dmax_factor,FORMAT='(F9.3)'), " times the PSF " 
if keyword_set(backgfit) then print, "Fitting sources adopting a second order polynomial approximation for the background" else $
print, "Fitting sources adopting a plane approximation for the background"
		
;____________________________________________________________________
; Fitting Single Sources
;____________________________________________________________________

; Defining the DMAX as default for the cases that no single sources are found it is setted to proper values during the routine
dmax = [nearint(dmax_factor*psf),nearint(dmax_factor*psf)]

print, "Start fitting process - Going for single sources "

if NOT keyword_set(closest_neigh) then begin
	ngrpsfit = intarr(n_elements(group))
	ngrpsindx = where(group eq 0, nngrps)
	if nngrps gt 0 then ngrpsfit(ngrpsindx) = 1.
	dummysort = group(sort(group))
	grpsindx = dummysort(uniq(dummysort))
	for k = 1, n_elements(grpsindx)-1 do ngrpsfit(where(group eq grpsindx(k))) = n_elements(where(group eq grpsindx(k)))
endif else begin
	ngrpsfit = intarr(n_elements(group))
	ngrpsindx = where(group eq 0, nngrps)
	if nngrps gt 0 then ngrpsfit(ngrpsindx) = 1.
endelse

qsingle=where(group eq 0,nsingle, complement=qmulti, ncomplement=nmulti)

group_flag = 0.

if (nsingle gt 0) then begin

	ngauss = 1
	regfile_ = regfiles+strcompress(string(ngauss,format='(I)'),/remove_all)+'_wcs.reg'
	regfile  = regfiles+strcompress(string(ngauss,format='(I)'),/remove_all)+'.reg'
	
	print, "__________________________________________________"
	print, "Fitting Single Sources "
	print, "Total number = ", nsingle 
	print, "__________________________________________________"
	
	openw, lung_group_region, regfile, /get_lun
	openw, lung_group_region_wcs, regfile_, /get_lun
	
	color = 'green'
	printf,lung_group_region,'# Region file format: DS9 version 4.0'
	printf,lung_group_region,'# Filename: ./'+workdir+mapfile+'.fits  -  List of single sources'
	printf,lung_group_region,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
	printf,lung_group_region,'image'

	printf,lung_group_region_wcs,'# Region file format: DS9 version 4.0'
	printf,lung_group_region_wcs,'# Filename: ./'+workdir+mapfile+'.fits  -  List of single sources'
	printf,lung_group_region_wcs,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
	printf,lung_group_region_wcs,unitcoord
	
	nquiet = nsingle/20.
	counter = 0

	for i=0L,nsingle-1 do begin
	
		if quiet eq 0 then begin
			if (i ne 0) and (i ge counter*nquiet)  then begin
				print, "Already analyzed "+strcompress(string(LONG(counter*5)),/REMOVE_ALL)+" % of "+strcompress(string(LONG(nsingle)),/REMOVE_ALL)+" single sources"
				counter = counter+1
			endif  
			if (i eq nsingle -1L) then begin
				print, "Already analyzed "+strcompress(string(100),/REMOVE_ALL)+" % of "+strcompress(string(LONG(nsingle)),/REMOVE_ALL)+" single sources"
			endif
		endif

	
	
;_____________________________________
; DMAX IS WIDTH OF THE FITTING REGION
;_____________________________________

  		center1=[x(qmarg[qth[qsingle[i]]]),y(qmarg[qth[qsingle[i]]])]
  		windowcenter=center1
  
  		if keyword_set(adaptive_window) then begin
			 dmax = [nearint(dmax_factor*max([in_dx(qmarg[qth[qsingle[i]]]),in_dy(qmarg[qth[qsingle[i]]])])),nearint(dmax_factor*max([in_dx(qmarg[qth[qsingle[i]]]),in_dy(qmarg[qth[qsingle[i]]])]))]
  		endif else dmax = [nearint(dmax_factor*psf),nearint(dmax_factor*psf)]
		
  		source_par=[in_dx(qmarg[qth[qsingle[i]]]),in_dy(qmarg[qth[qsingle[i]]]),in_pa(qmarg[qth[qsingle[i]]]),in_flag2(qmarg[qth[qsingle[i]]])]
; modifica del 13 settembre 2013 per evitare il fit liberi in mpfit_gaussians (parinfo(6 + bck).limited=[max([source_par(3),0]),max([source_par(3),0])])
                if (source_par[3] le 0) then source_par[3] = 1
; fine modifica
			
		; PERFORM THE REAL GAUSSIAN FITTING USING THE MPFIT_GAUSSIAN
		
		; possource, posback, -> Possource = 1, posback = 0
;________________________________________
; TO DEBUG		
;print, "DMAX = ", dmax
;print, "DRANGE = ", drange
;print, "WINDOWCENTER = ", windowcenter
;print, "CENTER1 = ", center1
;print, "NGAUSS = ", ngauss
;print, "SOURCE_PAR = ", source_par
;print, "PSF LIM = ", psflim
;________________________________________
		
		err_mask = check_mask(im_mask, windowcenter, dmax, nx(qmarg[qth[qsingle[i]]]), backgfit=backgfit)
		;stop
		; print, "SOURCE", nx(qmarg[qth[qsingle[i]]])		
		if dmax(0) ne err_mask.dmax_new(0) then print, "Fitting window for Source "+strcompress(string(nx(qmarg[qth[qsingle[i]]])),/REMOVE_ALL)+ " enlarged "
		
		dmax = err_mask.dmax_new
		
		;print, "SOURCE ID: ", nx(qmarg[qth[qsingle[i]]]) ; DEBUG MODE
		
		drange = IN_DIST(qmarg[qth[qsingle[i]]]) + 0.5
		
		; Check for border sources - 
		
		if borderlist[qmarg[qth[qsingle[i]]]] eq 1 then begin
			
			print, 'Fitting source nearby border - ', ' SOURCE Number ', nx(qmarg[qth[qsingle[i]]]), ' Adopting a PSF for guess  size ' 
			source_par = [DOUBLE(psf), DOUBLE(psf), 0. , 1.]
			
		endif

		par=mpfit_gaussians(im,dmax,drange,windowcenter,center1,ngauss,source_par,  psflim,  posback=posback, weight=weight, backgfit=backgfit, psf=psf, mask=err_mask, /PERR)
		;stop
		shift_fit = norm([center1(0),center1(1)]-[par.x0,par.y0])
		
		xyad,h,par.x0,par.y0,ra1,dec1,/celestial ;Recomputing the Equatorial Coordinates on the newly fit gaussian peak position
		xyad,h,par.x0,par.y0,glon1,glat1,/galactic 

  		int_flux=par.f0*2.*!pi*((par.sx)*pix2asec*asec2rad)*((par.sy)*pix2asec*asec2rad)*intflux_corr ; Computing the Integrate Flux

		check1 = (par.error_f0 gt 0.)	; Check if Errors from the fitting are positive. If one of the three errors on f_peak, sigma_x  
		check2 = (par.error_sx gt 0.)	; or sigma_y is negative then error on integrate flux is assigned to be -999.
		check3 = (par.error_sy gt 0.)

		check4 = (check1) or (check2) or (check3)

		if check4 eq 0 then err_int_flux = -999. else begin

			if NOT keyword_set(CORREL) then begin
				err_int_flux = int_flux*(nint(check2)*(par.error_sx/par.sx)^2 + $
					nint(check3)*(par.error_sy/par.sy)^2 + $
					nint(check1)*(par.error_f0/par.f0)^2)^0.5
			endif else begin
				err_int_flux = int_flux*(nint(check2)*(par.error_sx/par.sx)^2 + $
					nint(check3)*(par.error_sy/par.sy)^2 + $
					nint(check1)*(par.error_f0/par.f0)^2 + $
					2*(nint(check2 and check3))*par(0).cov_matrix(3,4)/(par.sx*par.sy) +$
					2*(nint(check1 and check2))*par(0).cov_matrix(0,3)/(par.f0*par.sx) +$
					2*(nint(check1 and check3))*par(0).cov_matrix(0,4)/(par.f0*par.sy))^0.5
			endelse
		endelse	

 		; Computing the Background Value at Peak Position
		 
  		
		if keyword_set(backgfit) then begin
			background = par.back + (par.x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par.backx + (par.y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par.backy + $
			(par.x0 - max([nearint(windowcenter(0))-dmax(0),1]))^2 *par.backx2 + (par.y0 - max([nearint(windowcenter(1))-dmax(1),1]))^2*par.backy2 + $
			(par.x0 - max([nearint(windowcenter(0))-dmax(0),1]))*(par.y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par.backxy
		endif else begin
			background = par.back + (par.x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par.backx + (par.y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par.backy 
    		endelse
		
		chi2_dof = par.chi2/par.dof
		
		if unitcoord eq 'fk5' then begin 
			xyad,h,par.x0,par.y0,xreg,yreg,/celestial ;Recomputing the Equatorial Coordinates on the newly fit gaussian peak position
		endif else begin
			xyad,h,par.x0,par.y0,xreg,yreg,/galactic ;Recomputing the Galactic Coordinates on the newly fit gaussian peak position
		endelse
 
; Compute the Standard Deviation of the Residuals in two different ways: 
;		As STD of residuals after plane subtraction, where the plane is fitted on the residuals of source subtraction - > noise.sdev1
;	 	As STD of residuals after subtraction of source+plane fit simultaneously					      - > noise.sdev2
		
		noise = res_photo(im, dmax, windowcenter, par.f0, par.x0, par.y0, par.sx, par.sy, (par.pa/!pi*180. mod 360.), par(0).fit, mask=err_mask)
		
		if ngauss eq 1 then group_flag = 0

;  note in the output below that I am adding 1 to X and Y (see above comments)
;
;----------------------------------------------------
; SAVING RESULTS FOR SINGLE SOURCES
;----------------------------------------------------
;		printf,lun_photfile, ' ', pad_parameter(n(qmarg[qth[qsingle[i]]]),' ',field_len), ' ', pad_parameter(group(qsingle[i]),' ',field_len), ' ', $
;		pad_parameter(ngrpsfit(qsingle[i]), ' ', field_len), ' ', $
;		pad_parameter(par.x0+1,' ',field_len), ' ', pad_parameter(par.y0+1,' ',field_len), ' ' , $
;		pad_parameter(ra1,' ',field_len), ' ',pad_parameter(dec1,' ',field_len), ' ', $
;		pad_parameter(glon1,' ',field_len), ' ',pad_parameter(glat1,' ',field_len), ' ', $
;		pad_parameter(knownwave(indxband(0)),' ',field_len), ' ', $
;		pad_parameter(par.f0,' ',field_len), ' ',  $ 
;		pad_parameter((par.sx)*2.354*pix2asec,' ',field_len), ' ', pad_parameter((par.sy)*2.354*pix2asec,' ',field_len), ' ', $
;		pad_parameter(par.pa/!pi*180. mod 360.,' ',field_len), ' ', pad_parameter(int_flux,' ',field_len), ' ', $ 
;		pad_parameter(background,' ', field_len), ' ', pad_parameter(chi2_dof,' ',field_len), ' ',$
;		pad_parameter(noise.sdev2,' ',field_len), ' ', pad_parameter(par.chi2, ' ', field_len), ' ',$
;		pad_parameter(par.status, ' ',  field_len)

	        stringToPrint =  ' '+ pad_parameter(n(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ $
	        pad_parameter(par.x0+1,' ',field_len)+ ' '+ pad_parameter(par.y0+1,' ',field_len)+ ' ' + $
	        pad_parameter(ra1,' ',field_len)+ ' '+pad_parameter(dec1,' ',field_len)+ ' '+ $
	        pad_parameter(glon1,' ',field_len)+ ' '+pad_parameter(glat1,' ',field_len)+ ' '+ $
	        pad_parameter(knownwave(indxband(0)),' ',field_len)+ ' ' 
			
			stringToPrint = stringToPrint + pad_parameter(par.f0,' ',field_len)+ ' '+  $ 
	        pad_parameter((par.sx)*2.354*pix2asec,' ',field_len)+ ' '+ pad_parameter((par.sy)*2.354*pix2asec,' ',field_len)+ ' '+ $
	        pad_parameter(par.pa/!pi*180. mod 360.,' ',field_len)+ ' '+ pad_parameter(int_flux,' ',field_len)+ ' '+ $ 
	        pad_parameter(err_int_flux,' ',field_len)+ ' '+$
			pad_parameter(background,' ', field_len)+ ' '+ pad_parameter(noise.sdev2,' ',field_len)+ ' '+pad_parameter(noise.newsdev4_onsou,' ',field_len)+ ' '+$
	        pad_parameter(noise.sdev4,' ',field_len)+ ' '+ pad_parameter(par.sum2res, ' ', field_len)+ ' '+$
	        pad_parameter(par.dof, ' ', field_len)+ ' '+ pad_parameter(par.chi2, ' ', field_len )+ ' '+ pad_parameter(par.chi2opp, ' ', field_len )+ ' '+$
			pad_parameter(par.size_flag, ' ', 25)+ ' '+ pad_parameter(group_flag, ' ', field_len)+ ' '
			
			stringToPrint = stringToPrint + pad_parameter(ngauss, ' ', field_len)+ ' '+ pad_parameter(IN_CLUMP(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+$
 ;		pad_parameter(IN_DIST(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ pad_parameter(IN_GUESS(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ $
			pad_parameter(shift_fit,' ',field_len)+ ' '+ pad_parameter(IN_GUESS(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ $
			pad_parameter(STRING(err_mask.nsou, format='(I3)'), ' ',  field_len)+ ' '+$
			pad_parameter(STRING(par.status, format='(F4.1)'), ' ',  field_len)+ ' '+  pad_parameter(IN_FLAG(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+$
			pad_parameter(noise.flagin, ' ',  field_len)+ ' '+  pad_parameter(noise.flagout,' ',field_len)+ ' '
			
			stringToPrint = stringToPrint + pad_parameter(IN_D2X(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ pad_parameter(IN_D2Y(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ $
			pad_parameter(IN_D2X45(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ pad_parameter(IN_D2Y45(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ $
			pad_parameter(IN_D2X_THR(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ pad_parameter(IN_D2Y_THR(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ $
			pad_parameter(IN_D2X45_THR(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ pad_parameter(IN_D2Y45_THR(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '
			
			stringToPrint = stringToPrint + pad_parameter(strtrim(string(par.xd2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.yd2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.x45d2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.y45d2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
			pad_parameter(strtrim(string(par.xd2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.yd2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.x45d2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.y45d2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
			pad_parameter(strtrim(string(par.xd2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.yd2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.x45d2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.y45d2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
			pad_parameter(strtrim(string(par.xd2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.yd2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.x45d2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par.y45d2photbck , FORMAT='(F15.5)'),2),' ',field_len)
		
			printf,lun_photfile, stringToPrint
		
		str2printbck =  ' '+ pad_parameter(n(qmarg[qth[qsingle[i]]]),' ',field_len)+ ' '+ pad_parameter(par.back,' ',field_len)+ ' '+ pad_parameter(par.backx,' ',field_len)+ ' '+ pad_parameter(par.backy,' ',field_len)+ ' '
		if keyword_set(BACKGFIT) then str2printbck = str2printbck+ pad_parameter(par.backx2,' ',field_len)+ ' '+ pad_parameter(par.backy2,' ',field_len)+ ' '+pad_parameter(par.backxy,' ',field_len)+' '
		printf, lun_photfile_bak, str2printbck
		
		printf,lun_phot_dev, ' ', pad_parameter(n(qmarg[qth[qsingle[i]]]),' ', field_len), ' ', pad_parameter(noise.sdev1,' ', field_len), ' ', $
		pad_parameter(noise.sdev2,' ', field_len), ' ', pad_parameter(noise.sdev3,' ', field_len), ' ', pad_parameter(noise.sdev4, ' ', field_len), ' ', $
		pad_parameter(noise.sdev4_onsou,' ', field_len), ' ', pad_parameter(noise.newsdev4,' ', field_len), ' ', pad_parameter(noise.newsdev4_onsou, ' ', field_len),  ' ', pad_parameter(noise.newsdev5_onsou, ' ', field_len)

		
		printf,lun_phot_err,' ', pad_parameter(n(qmarg[qth[qsingle[i]]]),' ',field_len), ' ', pad_parameter(group(qsingle[i]),' ',field_len), ' ', $
		pad_parameter(ngrpsfit(qsingle[i]), ' ', field_len), ' ',$
		pad_parameter(par.x0+1,' ',field_len), ' ', pad_parameter(par.y0+1,' ',field_len), ' ' , $
		pad_parameter(ra1,' ',field_len), ' ',pad_parameter(dec1,' ',field_len), ' ', $
		pad_parameter(glon1,' ',field_len), ' ',pad_parameter(glat1,' ',field_len), ' ', $
		pad_parameter(knownwave(indxband(0)),' ',field_len), ' ', $	
		pad_parameter(strtrim(par.error_f0,2),' ',field_len), ' ', $			
		pad_parameter(strtrim(par.error_x0*pix2asec,2),' ',field_len), ' ', pad_parameter(strtrim(par.error_y0*pix2asec,2),' ',field_len), ' ', $
		pad_parameter(strtrim((par.error_sx)*2.354*pix2asec,2),' ',field_len), ' ', pad_parameter(strtrim((par.error_sy)*2.354*pix2asec,2),' ',field_len), ' ', $
		pad_parameter(strtrim(par.error_pa/!pi*180. mod 360.,2),' ',field_len), ' ',pad_parameter(strtrim(err_int_flux,2),' ',field_len), ' ',$
		pad_parameter(strtrim(chi2_dof,2),' ',field_len), ' ',pad_parameter(strtrim(noise.sdev2,2),' ', field_len), ' ',pad_parameter(strtrim(par.chi2,2), ' ', field_len), ' ',$
		pad_parameter(par.status, ' ',  field_len)

 		printf,lung_group_region,'ellipse('+strcompress(string(par.x0+1.),/remove_all)+','+strcompress(string(par.y0+1.),/remove_all)+','+strcompress(string(abs(par.sx)*2.354/2.),/remove_all)+','+strcompress(string(abs(par.sy)*2.354/2.),/remove_all)+','+strcompress(string(par.pa/!pi*180. mod 360.),/remove_all)+')'
  		printf,lung_group_region,'# text('+strcompress(string(par.x0+1.),/remove_all)+','+strcompress(string(par.y0+1.),/remove_all)+') text={'+strcompress(string(fix(n(qmarg[qth[qsingle[i]]]))),/remove_all)+'}'
  		
		printf,lung_group_region_wcs,'ellipse('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+','+strcompress(string(abs(par.sx)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(abs(par.sy)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(par.pa/!pi*180. mod 360.),/remove_all)+')'
  		printf,lung_group_region_wcs,'# text('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+') text={'+strcompress(string(fix(n(qmarg[qth[qsingle[i]]]))),/remove_all)+'}'
  		
  		printf,lun_phot_par, ' ', pad_parameter(n(qmarg[qth[qsingle[i]]]),' ',field_len), ' ', pad_parameter(group(qsingle[i]),' ',field_len), ' ', $
		pad_parameter(ngrpsfit(qsingle[i]), ' ', field_len), ' ', $
		pad_parameter(windowcenter(0),' ',field_len), ' ', pad_parameter(windowcenter(1),' ',field_len), ' ' , $	
		pad_parameter(dmax(0),' ',field_len), ' ', pad_parameter(dmax(1),' ',field_len), ' ' , $	
		pad_parameter(center1(0),' ',field_len), ' ', pad_parameter(center1(1),' ',field_len), ' ' , $
		pad_parameter(source_par(0),' ',field_len), ' ', pad_parameter(source_par(1),' ',field_len), ' ' , pad_parameter(source_par(2) ,' ',field_len), ' ', pad_parameter(source_par(3),' ',field_len), ' ', pad_parameter(par(0).nnan,' ',field_len)
		
 		printf,lun_region,'ellipse('+strcompress(string(par.x0+1.),/remove_all)+','+strcompress(string(par.y0+1.),/remove_all)+','+strcompress(string(abs(par.sx)*2.354/2.),/remove_all)+','+strcompress(string(abs(par.sy)*2.354/2.),/remove_all)+','+strcompress(string(par.pa/!pi*180. mod 360.),/remove_all)+')'
  		printf,lun_region,'# text('+strcompress(string(par.x0+1.),/remove_all)+','+strcompress(string(par.y0+1.),/remove_all)+') text={'+strcompress(string(fix(n(qmarg[qth[qsingle[i]]]))),/remove_all)+'}'

 		
		printf,lun_region_wcs,'ellipse('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+','+strcompress(string(abs(par.sx)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(abs(par.sy)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(par.pa/!pi*180. mod 360.),/remove_all)+')'
  		printf,lun_region_wcs,'# text('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+') text={'+strcompress(string(fix(n(qmarg[qth[qsingle[i]]]))),/remove_all)+'}'
	endfor

	free_lun,lung_group_region
	free_lun,lung_group_region_wcs

endif

dmax_single = dmax

print, "Single Sources fitting process complete "
if keyword_set(TIME) Then begin
	print, "Estimating Time for Fitting the Single Sources"
	timer, /STOP, /PRINT, dt
	timer, /START
endif

;_____________________________________________________________
; now understand how many groups we have and their consistency
;_____________________________________________________________


if keyword_set(closest_neigh) then begin
	print, "Fitting Source using only the closest neighbours"
	if KEYWORD_SET(dthr) then print, "Distance defining the closest neighbours (constant for everyone) = ", dthr else $
	print, "Distance defining the closest neighbours (largest ellipse axis) = ", dmax_factor
endif

print, "Number of sources grouped = ", nmulti

g = where(group ne 0, nq0)
if keyword_set(single) then nq0 = 0 ; Stefano 11 settembre 2013, per fittare solo sorgenti singole

;Fabiana: se non ci sono sorgenti in gruppo qui si blocca allora inserisco un if

if (nq0 gt 0) then begin
	
	print, "Starting Multiple Sources Simultaneous Fitting"
  	
	s=sort(group(g))
  	u=uniq(group(g(s)))
  	ng=n_elements(u)
  	group_id=intarr(ng,2)  ; this array holds the group ID and the number of members for each group as it comes out from GROUP_SOURCES
  	
	for k=0,ng-1 do begin
    		id=group(g(s(u(k))))
    		m=where(group eq id, members)
    		group_id(k,0)=id
    		group_id(k,1)=members
  	endfor


;------------------------------------------------------------------------------------


	memb_ = group_id(*,1)

	ngauss = memb_(sort(memb_))
	ngauss = ngauss(uniq(ngauss))			
	print, "Largest group found is composed by "+string(ngauss(n_elements(ngauss)-1),'(I)')+" Sources"

;____________________________________________________________________
; Fitting Multiple Sources
;____________________________________________________________________

	
	if NOT keyword_set(closest_neigh) then begin
	
		
		for n_m = 0L,n_elements(ngauss)-1 do begin						; N_M cycle on the number of gaussians

			q = where(group_id(*,1) eq ngauss(n_m), nq_)					; q identify the groups composed by n_m gaussians

			print, "__________________________________________________"
			print, "Fitting Multiple Sources - Case of "+string(ngauss(n_m),'(I)')+" Sources"
			print, "Total number = ", nq_ 
			print, "__________________________________________________"
			
			if (nq_ gt 0) then begin									; N cases with n_m gaussians
				
				regfile=regfiles+strcompress(strtrim(string(ngauss(n_m),'(I)')),/remove_all)+'.reg'
				regfile_=regfiles+strcompress(strtrim(string(ngauss(n_m),'(I)')),/remove_all)+'_wcs.reg'
	
	  			openw,lung_group_region,regfile,/get_lun
	  			openw,lung_group_region_wcs,regfile_,/get_lun
	
  				color='cyan'
	  			printf,lung_group_region,'# Region file format: DS9 version 4.0'
	  			printf,lung_group_region,'# Filename: ./'+workdir+mapfile+'.fits  -  List of Multiple sources - Case of '+strcompress(string(ngauss(n_m),'(I)'), /REMOVE_ALL)+' Sources'
  				printf,lung_group_region,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  				printf,lung_group_region,'image'

	  			printf,lung_group_region_wcs,'# Region file format: DS9 version 4.0'
	  			printf,lung_group_region_wcs,'# Filename: ./'+workdir+mapfile+'.fits  -  List of Multiple sources - Case of '+strcompress(string(ngauss(n_m),'(I)'), /REMOVE_ALL)+' Sources'
  				printf,lung_group_region_wcs,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  				printf,lung_group_region_wcs, unitcoord

				nquiet = nq_/20.
				counter = 0
				
				for k_=0L,nq_-1L do begin									
					
					group_flag = group_flag + k_
					
    				if quiet eq 0 then begin
    					if (k_ ne 0) and (k_ ge counter*nquiet)  then begin
							print, "Already analyzed "+strcompress(string(LONG(counter*5)),/REMOVE_ALL)+" % of "+strcompress(string(LONG(nq_)),/REMOVE_ALL)+" groups with "+string(ngauss(n_m),'(I)')+" sources "
							counter = counter+1
						endif  
						if (k_ eq nq_ -1) then print, "Already analyzed "+strcompress(string(100),/REMOVE_ALL)+" % of "+strcompress(string(LONG(nq_)),/REMOVE_ALL)+" groups with "+string(ngauss(n_m),'(I)')+" sources "
					endif
	
					
					
					center=fltarr(ngauss(n_m),2)   ; To hold the coordinates of the source centers
    					windowcenter=fltarr(2)
    					dmax=intarr(2)
    
			    		qgroup=where(group eq group_id(q(k_),0))
	    				center(*,0)=x(qmarg(qth(qgroup)))
    					center(*,1)=y(qmarg(qth(qgroup)))

					; Center of Fitting Window identified by the Baricenter of the sources
				
						windowcenter(0) = 0.
    					windowcenter(1) = 0.
				
					for i_ = 0,ngauss(n_m)-1 do begin 
    						windowcenter(0)=(windowcenter(0)+center(i_,0))
    						windowcenter(1)=(windowcenter(1)+center(i_,1))
					endfor
		
					windowcenter(0) = windowcenter(0)/(ngauss(n_m))
					windowcenter(1) = windowcenter(1)/(ngauss(n_m))


		
			 		if keyword_set(adaptive_window) then begin
			 		
						dmax = [nearint(dmax_factor*max([in_dx(qmarg(qth(qgroup(i)))),in_dy(qmarg(qth(qgroup(i))))])),nearint(dmax_factor*max([in_dx(qmarg(qth(qgroup(i)))),in_dy(qmarg(qth(qgroup(i))))]))]
  					
					endif else begin
					
						dmax(0) = nearint(dmax_factor*psf + (abs(max(center(*,0))-min(center(*,0)))/2.) + 0.01) + 1
    						dmax(1) = nearint(dmax_factor*psf + (abs(max(center(*,1))-min(center(*,1)))/2.) + 0.01) + 1
    					endelse
    				
					; Build up the initial guess parameters for the sources of the group
				
						source_par=fltarr(ngauss(n_m),4) ;this is for groups of n_m sources
    					for s1=0,ngauss(n_m)-1 do source_par(s1,*) = [in_dx(qmarg(qth(qgroup(s1)))),in_dy(qmarg(qth(qgroup(s1)))),in_pa(qmarg(qth(qgroup(s1)))),in_flag2(qmarg(qth(qgroup(s1))))]
; modifica del 13 settembre 2013 per evitare il fit liberi in mpfit_gaussians (parinfo(6 + bck).limited=[max([source_par(3),0]),max([source_par(3),0])])
                                        badFlags = where(source_par[*,3] le 0,count)
                                        if (count GT 0) then source_par[badFlags,3] = 1
; fine modifica

;DMAX = [5,4]
;______________________________________
; TO DEBUG
;print, "DMAX = ", dmax
;print, "DRANGE = ", drange
;print, "WINDOWCENTER = ", windowcenter
;print, "CENTER = ", center
;print, "NGAUSS = ", ngauss
;print, "SOURCE_PAR = ", source_par
;print, "PSF LIM = ", psflim
;______________________________________

; TESTING REASONS

;test = nx(qmarg(qth(qgroup)))
;print, "Analysing sources", nx(qmarg(qth(qgroup)))

;-------------------------------------------------------------------------------------------------------------------------

					
					err_mask = check_mask(im_mask, windowcenter, dmax, nx(qmarg(qth(qgroup))), backgfit=backgfit)
					
					if dmax(0) ne err_mask.dmax_new(0) then print, "Fitting window for Source "+strcompress(string(nx(qmarg(qth(qgroup)))),/REMOVE_ALL)+ " enlarged "
		
					dmax = err_mask.dmax_new
			
					drange = IN_DIST(qmarg(qth(qgroup))) + 0.5
			
					; Check for border sources - 
					; Could not test this here 
						
						for sborder = 0, ngauss(n_m)-1 do begin
							
							if borderlist[qmarg(qth(qgroup(sborder)))] eq 1 then begin
			
								print, 'Fitting source nearby border - ', ' SOURCE Number ', nx[qmarg(qth(qgroup(sborder)))], ' Adopting a PSF for guess  size ' 
								source_par[sborder,*] = [DOUBLE(psf), DOUBLE(psf), 0. , 1.]
				
							endif
						endfor 
					;print, "SOURCE GROUPED ID:", nx(qmarg(qth(qgroup)))
					par=mpfit_gaussians(im,dmax, drange, windowcenter, center, ngauss(n_m), source_par, psflim,  posback=posback, weight=weight, backgfit=backgfit, psf=psf, mask=err_mask, /PERR)

;					help, par
;					;print, "PAR" , par
					f0_ = dblarr(ngauss(n_m))			
					x0_ = dblarr(ngauss(n_m))			
					y0_ = dblarr(ngauss(n_m))			
					sx_ = dblarr(ngauss(n_m))			
					sy_ = dblarr(ngauss(n_m))			
					pa_ = dblarr(ngauss(n_m))			
			
				;; nino verificare se il seguente ciclo diventa: o qualcosa di simile
				;; f0__ =  par[*].f0
				;; x0__ =  par[*].x0
				;; y0__ =  par[*].y0
				;; sx__ =  par[*].sx
				;; sy__ =  par[*].sy
				;; pa__ =  par[*].pa/!pi*180. mod 360.
				;;
					for _anIdx_ = 0,ngauss(n_m)-1 do begin
						f0_(_anIdx_) = par(_anIdx_).f0
						x0_(_anIdx_) = par(_anIdx_).x0
						y0_(_anIdx_) = par(_anIdx_).y0
						sx_(_anIdx_) = par(_anIdx_).sx
						sy_(_anIdx_) = par(_anIdx_).sy
						pa_(_anIdx_) = (par(_anIdx_).pa/!pi*180. mod 360.)
					endfor
								
			       	
			       	noise = res_photo(im,dmax, windowcenter, f0_, x0_, y0_, sx_, sy_, pa_, par(0).fit, /MULTIPLE, mask=err_mask)
	
					; now evaluate both sources and discard funny sources
					
					
					for memb=0L,ngauss(n_m)-1 do begin

						; I have to recompute RA and DEC based on the newly fit gaussian peak position to go into the output photometry file
      					
						xyad, h, par(memb).x0, par(memb).y0, ra1, dec1, /celestial
      					xyad, h, par(memb).x0, par(memb).y0, glon1, glat1, /galactic
      				
						shift_fit = norm([center(memb,0),center(memb,1)]-[par(memb).x0,par(memb).y0])
							
						int_flux=par(memb).f0*2.*!pi*((par(memb).sx)*pix2asec*asec2rad)*((par(memb).sy)*pix2asec*asec2rad)*intflux_corr
					

						check1 = (par(memb).error_f0 gt 0.)
						check2 = (par(memb).error_sx gt 0.)
						check3 = (par(memb).error_sy gt 0.)
	
						check4 = (check1) or (check2) or (check3)


						if check4 eq 0 then err_int_flux = -999. else begin 
					
							if NOT keyword_set(CORREL) then begin
								err_int_flux = int_flux*(nint(check2)*(par(memb).error_sx/par(memb).sx)^2 + $
						 		nint(check3)*(par(memb).error_sy/par(memb).sy)^2 + $
								nint(check1)*(par(memb).error_f0/par(memb).f0)^2)^0.5
							endif else begin
								if memb eq 0 then begin
									err_int_flux = int_flux*(nint(check2)*(par(memb).error_sx/par(memb).sx)^2 +$
					 				nint(check3)*(par(memb).error_sy/par(memb).sy)^2 +$
									nint(check1)*(par(memb).error_f0/par(memb).f0)^2 +$
									2*(nint(check2 and check3))*par(0).cov_matrix(3,4)/(par(memb).sx*par(memb).sy) + $
							  		2*(nint(check1 and check2))*par(0).cov_matrix(0,3)/(par(memb).f0*par(memb).sx) + $
							 	 	2*(nint(check1 and check3))*par(0).cov_matrix(0,4)/(par(memb).f0*par(memb).sy))^0.5
								
								endif else err_int_flux = int_flux*(nint(check2)*(par(memb).error_sx/par(memb).sx)^2 + $
										nint(check3)*(par(memb).error_sy/par(memb).sy)^2 + nint(check1)*(par(memb).error_f0/par(memb).f0)^2 +$
										2*(nint(check2 and check3))*par(0).cov_matrix(3 + (3 + 6*memb),4 + (3 + 6*memb))/(par(memb).sx*par(memb).sy) + $
										2*(nint(check1 and check2))*par(0).cov_matrix(0 + (3 + 6*memb),3 + (3 + 6*memb))/(par(memb).f0*par(memb).sx) + $
										2*(nint(check1 and check3))*par(0).cov_matrix(0 + (3 + 6*memb),4 + (3 + 6*memb))/(par(memb).f0*par(memb).sy))^0.5
						
								endelse
						endelse			
								
						if keyword_set(backgfit) then begin
						
							background = par(memb).back + $
											(par(memb).x0 - max([nearint(windowcenter(0))-dmax(0),1]))   *  par(memb).backx + $
											(par(memb).y0 - max([nearint(windowcenter(1))-dmax(1),1]))   *  par(memb).backy + $
											(par(memb).x0 - max([nearint(windowcenter(0))-dmax(0),1]))^2 *  par(memb).backx2 + $
											(par(memb).y0 - max([nearint(windowcenter(1))-dmax(1),1]))^2 *  par(memb).backy2 + $
											(par(memb).x0 - max([nearint(windowcenter(0))-dmax(0),1]))   * (par(memb).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(memb).backxy
						endif else begin
							background = par(memb).back + (par(memb).x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par(memb).backx + (par(memb).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(memb).backy 
    					endelse
		

						
						;background = par(memb).back + (par(memb).x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par(memb).backx + (par(memb).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(memb).backy

						chi2_dof = par(memb).chi2/par(memb).dof
										
						
						if unitcoord eq 'fk5' then begin 
							xyad,h,par(memb).x0,par(memb).y0,xreg,yreg,/celestial ;Recomputing the Equatorial Coordinates on the newly fit gaussian peak position
						endif else begin
							xyad,h,par(memb).x0,par(memb).y0,xreg,yreg,/galactic ;Recomputing the Galactic Coordinates on the newly fit gaussian peak position
						endelse
 
						
						
						 
						; note in the output below that I am adding 1 to X and Y (see above comments)
      			
;						printf,lun_photfile, ' ', pad_parameter(n(qmarg(qth(qgroup(memb)))),' ',field_len), ' ', pad_parameter(group(qgroup(memb)),' ',field_len), ' ', $
;						pad_parameter(ngauss(n_m), ' ', field_len), ' ',$
;						pad_parameter(par(memb).x0+1,' ',field_len), ' ', pad_parameter(par(memb).y0+1,' ',field_len), ' ', $
;						pad_parameter(ra1,' ',field_len), ' ',pad_parameter(dec1,' ',field_len), ' ',$
;						pad_parameter(glon1,' ',field_len), ' ',pad_parameter(glat1,' ',field_len), ' ',$
;						pad_parameter(knownwave(indxband(0)),' ',field_len), ' ', $
;						pad_parameter(par(memb).f0,' ',field_len), ' ', $
;						pad_parameter((par(memb).sx)*2.354*pix2asec,' ',field_len), ' ', pad_parameter((par(memb).sy)*2.354*pix2asec,' ',field_len), ' ', $
;						pad_parameter(par(memb).pa/!pi*180. mod 360.,' ',field_len), ' ', $
;						pad_parameter(int_flux,' ',field_len), ' ', $ 
;						pad_parameter(background,' ', field_len), ' ', pad_parameter(chi2_dof,' ',field_len), ' ',$
;						pad_parameter(noise.sdev2, ' ',field_len), ' ', pad_parameter(par(memb).chi2, ' ', field_len), ' ',$
;						pad_parameter(par(memb).status, ' ',  field_len)

	        				stringToPrint =  ' '+ pad_parameter(n(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
	       					pad_parameter(par(memb).x0+1,' ',field_len)+ ' '+ pad_parameter(par(memb).y0+1,' ',field_len)+ ' ' + $
	        				pad_parameter(ra1,' ',field_len)+ ' '+pad_parameter(dec1,' ',field_len)+ ' '+ $
	        				pad_parameter(glon1,' ',field_len)+ ' '+pad_parameter(glat1,' ',field_len)+ ' '+ $
	        				pad_parameter(knownwave(indxband(0)),' ',field_len)+ ' '
							
							stringToPrint = stringToPrint + pad_parameter(par(memb).f0,' ',field_len)+ ' '+  $ 
	        				pad_parameter((par(memb).sx)*2.354*pix2asec,' ',field_len)+ ' '+ pad_parameter((par(memb).sy)*2.354*pix2asec,' ',field_len)+ ' '+ $
	        				pad_parameter(par(memb).pa/!pi*180. mod 360.,' ',field_len)+ ' '+ pad_parameter(int_flux,' ',field_len)+ ' '+ $ 
	        				pad_parameter(err_int_flux,' ',field_len)+ ' '+$
						pad_parameter(background,' ', field_len)+ ' '+ pad_parameter(noise.sdev2,' ',field_len)+ ' '+pad_parameter(noise.newsdev4_onsou[memb],' ',field_len)+ ' '+$
	        				pad_parameter(noise.sdev4,' ',field_len)+ ' '+ pad_parameter(par(memb).sum2res, ' ', field_len)+ ' '+$
	        				pad_parameter(par(memb).dof, ' ', field_len)+ ' '+ pad_parameter(par(memb).chi2, ' ', field_len )+ ' '+ pad_parameter(par(memb).chi2opp, ' ', field_len )+ ' '+$
							pad_parameter(par(memb).size_flag, ' ', 25)+ ' '+ pad_parameter(group_flag, ' ', field_len)+ ' '
							
							stringToPrint = stringToPrint + pad_parameter(ngauss(n_m), ' ', field_len)+ ' '+ pad_parameter(IN_CLUMP(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+$
; 							pad_parameter(IN_DIST(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ pad_parameter(IN_GUESS(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
	 						pad_parameter(shift_fit,' ',field_len)+ ' '+ pad_parameter(IN_GUESS(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
							pad_parameter(STRING(err_mask.nsou, format='(I3)'), ' ',  field_len)+ ' '+$
							pad_parameter(STRING(par(memb).status, format='(F4.1)'), ' ',  field_len)+ ' '+ pad_parameter(IN_FLAG(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
							pad_parameter(noise.flagin, ' ',  field_len)+ ' '+  pad_parameter(noise.flagout,' ',field_len)+ ' '
							
							stringToPrint = stringToPrint + pad_parameter(IN_D2X(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
							pad_parameter(IN_D2X45(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y45(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
							pad_parameter(IN_D2X_THR(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y_THR(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ $
							pad_parameter(IN_D2X45_THR(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y45_THR(qmarg(qth(qgroup(memb)))),' ',field_len)+ ' '
							
							stringToPrint = stringToPrint + pad_parameter(strtrim(string(par(memb).xd2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).yd2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).x45d2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+pad_parameter(strtrim(string(par(memb).y45d2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+$
							pad_parameter(strtrim(string(par(memb).xd2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).yd2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).x45d2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).y45d2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
							pad_parameter(strtrim(string(par(memb).xd2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).yd2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).x45d2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).y45d2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
							pad_parameter(strtrim(string(par(memb).xd2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).yd2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).x45d2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(memb).y45d2photbck , FORMAT='(F15.5)'),2),' ',field_len)
					
						
							printf,lun_photfile, stringToPrint
		
						str2printbck =  ' '+ pad_parameter(n(qmarg(qth(qgroup(memb)))),' ',field_len)+' '+ pad_parameter(par(0).back,' ',field_len)+ ' '+ pad_parameter(par(0).backx,' ',field_len)+ ' '+ pad_parameter(par(0).backy,' ',field_len)+ ' '
						if keyword_set(BACKGFIT) then str2printbck = str2printbck+ pad_parameter(par(0).backx2,' ',field_len)+ ' '+ pad_parameter(par(0).backy2,' ',field_len)+ ' '+pad_parameter(par(0).backxy,' ',field_len)+' '
						printf, lun_photfile_bak, str2printbck
						
						printf,lun_phot_dev, ' ', pad_parameter(n(qmarg(qth(qgroup(memb)))),' ', field_len), ' ', pad_parameter(noise.sdev1,' ', field_len), ' ', $
						pad_parameter(noise.sdev2,' ', field_len), ' ', pad_parameter(noise.sdev3,' ', field_len), ' ', pad_parameter(noise.sdev4, ' ', field_len), ' ', $
						pad_parameter((noise.sdev4_onsou)[memb],' ', field_len), ' ', pad_parameter(noise.newsdev4,' ', field_len), ' ', pad_parameter((noise.newsdev4_onsou)[memb], ' ', field_len), ' ', pad_parameter((noise.newsdev5_onsou)[memb], ' ', field_len)

						
						printf,lun_phot_err,' ', pad_parameter(n(qmarg(qth(qgroup(memb)))),' ',field_len), ' ', pad_parameter(group(qgroup(memb)),' ',field_len), ' ', $
						pad_parameter(ngauss(n_m), ' ', field_len), ' ',$
						pad_parameter(par(memb).x0+1,' ',field_len), ' ', pad_parameter(par(memb).y0+1,' ',field_len), ' ' , $
						pad_parameter(ra1,' ',field_len), ' ',pad_parameter(dec1,' ',field_len), ' ',$
						pad_parameter(glon1,' ',field_len), ' ',pad_parameter(glat1,' ',field_len), ' ',$
						pad_parameter(knownwave(indxband(0)),' ',field_len), ' ', $				
						pad_parameter(strtrim(par(memb).error_f0,2),' ',field_len), ' ', $			
						pad_parameter(strtrim(par(memb).error_x0*pix2asec,2),' ',field_len), ' ', pad_parameter(strtrim(par(memb).error_y0*pix2asec,2),' ',field_len), ' ', $
						pad_parameter(strtrim((par(memb).error_sx)*2.354*pix2asec,2),' ',field_len), ' ', pad_parameter(strtrim((par(memb).error_sy)*2.354*pix2asec,2),' ',field_len), ' ', $
						pad_parameter(strtrim(par(memb).error_pa/!pi*180. mod 360.,2),' ',field_len), ' ',pad_parameter(strtrim(err_int_flux,2),' ',field_len), ' ',$
						pad_parameter(strtrim(chi2_dof,2),' ',field_len), ' ',pad_parameter(strtrim(noise.sdev2,2),' ', field_len),  ' ',pad_parameter(strtrim(par(memb).chi2,2), ' ', field_len), ' ',$
						pad_parameter(par(memb).status, ' ',  field_len)

    						printf,lung_group_region,'ellipse('+strcompress(string(par(memb).x0+1.),/remove_all)+','+strcompress(string(par(memb).y0+1.),/remove_all)+','+strcompress(string(abs(par(memb).sx)*2.354/2.),/remove_all)+','+strcompress(string(abs(par(memb).sy)*2.354/2.),/remove_all)+','+strcompress(string(par(memb).pa/!pi*180. mod 360.),/remove_all)+')'
	      					printf,lung_group_region,'# text('+strcompress(string(par(memb).x0+1.),/remove_all)+','+strcompress(string(par(memb).y0+1.),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(qgroup(memb)))))),/remove_all)+'}'
    					
						printf,lung_group_region_wcs,'ellipse('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+','+strcompress(string(abs(par(memb).sx)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(abs(par(memb).sy)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(par(memb).pa/!pi*180. mod 360.),/remove_all)+')'
  						printf,lung_group_region_wcs,'# text('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(qgroup(memb)))))),/remove_all)+'}'
  		
						printf,lun_phot_par, ' ', pad_parameter(n(qmarg(qth(qgroup(memb)))),' ',field_len), ' ', pad_parameter(group(qgroup(memb)),' ',field_len), ' ', $
						pad_parameter(ngauss(n_m), ' ', field_len), ' ',$
						pad_parameter(windowcenter(0),' ',field_len), ' ', pad_parameter(windowcenter(1),' ',field_len), ' ' , $	
						pad_parameter(dmax(0),' ',field_len), ' ', pad_parameter(dmax(1),' ',field_len), ' ' , $
						pad_parameter(center(memb,0),' ',field_len), ' ', pad_parameter(center(memb,1),' ',field_len), ' ' , $
						pad_parameter(source_par(memb,0),' ',field_len), ' ', pad_parameter(source_par(memb,1),' ',field_len), ' ' , pad_parameter(source_par(memb,2) ,' ',field_len), ' ', pad_parameter(source_par(memb,3),' ',field_len), ' ', pad_parameter(par(0).nnan,' ',field_len)

    						printf,lun_region,'ellipse('+strcompress(string(par(memb).x0+1.),/remove_all)+','+strcompress(string(par(memb).y0+1.),/remove_all)+','+strcompress(string(abs(par(memb).sx)*2.354/2.),/remove_all)+','+strcompress(string(abs(par(memb).sy)*2.354/2.),/remove_all)+','+strcompress(string(par(memb).pa/!pi*180. mod 360.),/remove_all)+')'
      						printf,lun_region,'# text('+strcompress(string(par(memb).x0+1.),/remove_all)+','+strcompress(string(par(memb).y0+1.),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(qgroup(memb)))))),/remove_all)+'}'
					
	   					printf,lun_region_wcs,'ellipse('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+','+strcompress(string(abs(par(memb).sx)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(abs(par(memb).sy)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(par(memb).pa/!pi*180. mod 360.),/remove_all)+')'
      						printf,lun_region_wcs,'# text('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(qgroup(memb)))))),/remove_all)+'}'
		
			
						endfor

				
				endfor
		
		  	free_lun,lung_group_region
			free_lun,lung_group_region_wcs

  			endif
		if keyword_set(TIME) then begin
			print, "Estimating Time for Multiple Sources ( Groups of "+string(ngauss(n_m),format='(I3)')+" sources"
			timer, /STOP, /PRINT, dt
			timer, /START
		endif
		
		endfor
	
			
;________________________
;CLOSEST MEMBER STUFF		
;________________________
	endif else begin
		dthr = 0.
		counter = 0
		nquiet = nq0/20.
		
		for n_m = 0L,nq0-1 do begin 
		
		       group_flag = group_flag + n_m
		       
		       if quiet eq 0 then if (n_m ne 0) and (n_m ge counter*nquiet)  then begin
						print, "Already analyzed "+strcompress(string(LONG(counter*5)),/REMOVE_ALL)+" % of "+strcompress(string(LONG(nq0)),/REMOVE_ALL)+" of multiple source groups"
						counter = counter+1
			endif  
			
			if (n_m eq nq0 -1) then print, "Already analyzed "+strcompress(string(100),/REMOVE_ALL)+" % of "+strcompress(string(LONG(nq0)),/REMOVE_ALL)+" of multiple source groups"
	
		       
		       m = where(group eq group(g(n_m)), members)      
			
		       ;Decide if the groups elements are close enough
		       xpos_ = x(qmarg(qth(g(n_m))))
		       ypos_ = y(qmarg(qth(g(n_m))))
	
		       xsel =  x(qmarg(qth(m)))
		       ysel =  y(qmarg(qth(m)))
	
		       dd = sqrt((xsel - xpos_)^2 + (ysel - ypos_)^2)
		       dd_ = sort(dd)  ; I want to consider also the source itself. This way the first element will be always the source itself
		
		       if (dthr eq 0) then dthr = max_dist_fac* max([in_dx(qmarg(qth(g(n_m)))),in_dy(qmarg(qth(g(n_m))))])
		       
		       print, "Threshold distance for closer-neighbours is : "+ strcompress(string(dthr,format='(F9.3)'), /REMOVE_ALL)+" Pixels"
		
		       selsource_ = where(dd(dd_) le dthr, memb_sel)   
		       selsource = m(dd_(selsource_)) 
		       n_gauss = memb_sel			       ; Fix the number of gaussians to be fitted
		       print, "Selected ", FIX(n_gauss -1), " closer-neighbours to fit. Full group membership is of ", FIX(members), " sources"
			

		       ;Prepare the sub image to be fitted
	
		       center=fltarr(n_gauss,2)   ; to hold the coordinates of the sources center
		       windowcenter=fltarr(2)
		       dmax=intarr(2)

		       ; Window Centered on the source we want to consider     
		       IF KEYWORD_SET(centering) then begin
       
				for _aMembIdx_ = 0L,memb_sel-1 do begin 
					    windowcenter(0)=(windowcenter(0)+x(qmarg(qth(selsource(_aMembIdx_)))))
		        		windowcenter(1)=(windowcenter(1)+y(qmarg(qth(selsource(_aMembIdx_)))))
		        endfor

		        	windowcenter(0) = windowcenter(0)/(memb_sel)
	 				windowcenter(1) = windowcenter(1)/(memb_sel)
 			endif else      windowcenter = [xpos_, ypos_]   ; In variable center holds the centers of all the sources selected

				center(*,0)=x(qmarg(qth(selsource)))  
    			center(*,1)=y(qmarg(qth(selsource)))
	
			;ADAPTIVE WINDOW
			if keyword_set(adaptive_window) then begin
			 		
				dmax = [nearint(dmax_factor*max([in_dx(qmarg(qth(selsource(i_)))),in_dy(qmarg(qth(selsource(i_))))])),nearint(dmax_factor*max([in_dx(qmarg(qth(selsource(i_)))),in_dy(qmarg(qth(selsource(i_))))]))]
  			; INSERT THE CENTERING ALSO HERE 		
			endif else begin
				
						dmax(0) = nearint(dmax_factor*psf + (abs(max(center(*,0))-min(center(*,0)))/2.) + 0.01) + 1
    					dmax(1) = nearint(dmax_factor*psf + (abs(max(center(*,1))-min(center(*,1)))/2.) + 0.01) + 1
    			    	
			
;		       NEEDED ? Seems to slow down the process

			       if keyword_set(centering) then begin
				       
				       dmax(0) = nearint(dmax_factor*psf + (abs(max(x(qmarg(qth(selsource))))-min(x(qmarg(qth(selsource)))))/2.) + 0.01) + 1
				       dmax(1) = nearint(dmax_factor*psf + (abs(max(y(qmarg(qth(selsource))))-min(y(qmarg(qth(selsource)))))/2.) + 0.01) + 1
			       
			       
			       endif else begin
				       
				       dmax(0) = nearint(dmax_factor*psf + (abs(max(center(*,0))-min(center(*,0)))/2.) + 0.01) + 1
				       dmax(1) = nearint(dmax_factor*psf + (abs(max(center(*,1))-min(center(*,1)))/2.) + 0.01) + 1
			       
			       endelse

    			endelse
				
			; Prepare the dummy variable containing the guess for the sources to be fitted
    			source_par=fltarr(n_gauss,4) 

			for s1=0L,n_gauss-1 do $
			source_par(s1,*)=[in_dx(qmarg(qth(selsource(s1)))),in_dy(qmarg(qth(selsource(s1)))),in_pa(qmarg(qth(selsource(s1)))),in_flag2(qmarg(qth(selsource(s1))))]
; modifica del 13 settembre 2013 per evitare il fit liberi in mpfit_gaussians (parinfo(6 + bck).limited=[max([source_par(3),0]),max([source_par(3),0])])
                        badFlags = where(source_par[*,3] le 0,count)
                        if (count GT 0) then source_par[badFlags,3] = 1
; fine modifica

			; Fit the Subimage
			; The first element should be the source we want to fit. We ignore the others	
			

			err_mask = check_mask(im_mask, windowcenter, dmax, nx(qmarg(qth(selsource))), backgfit=backgfit)
			
			if dmax(0) ne err_mask.dmax_new(0) then print, "Fitting window for Source "+strcompress(string(nx(qmarg(qth(selsource)))),/REMOVE_ALL)+ " enlarged "

			dmax = err_mask.dmax_new

			drange = IN_DIST(qmarg(qth(selsource))) + 0.5
			for sborder = 0, n_gauss-1 do begin
							
						if borderlist[qmarg(qth(selsource(sborder)))] eq 1 then begin
			
								print, 'Fitting source nearby border - ', ' SOURCE Number ', nx[qmarg(qth(qgroup(sborder)))], ' Adopting a PSF for guess  size ' 
								source_par[sborder,*] = [DOUBLE(psf), DOUBLE(psf), 0. , 1.]
				
						endif
			endfor 
					
			;print, "SOURCE GROUPED ID: ", nx(qmarg(qth(selsource)))
			par=mpfit_gaussians(im,dmax,drange,windowcenter, center, n_gauss,source_par,psflim,posback=posback,weight=weight, backgfit=backgfit, psf=psf, mask=err_mask, /PERR)
				
			; The source we want to fit should be the first element 
			f0_ = par(0).f0
			x0_ = par(0).x0
			y0_ = par(0).y0
			sx_ = par(0).sx
			sy_ = par(0).sy
			pa_ = (par(0).pa/!pi*180. mod 360.)
			
			f0__ = dblarr(n_gauss)
			x0__ = dblarr(n_gauss)
			y0__ = dblarr(n_gauss)
			sx__ = dblarr(n_gauss)
			sy__ = dblarr(n_gauss)
			pa__ = dblarr(n_gauss)
	 		
				;; nino verificare se il seguente ciclo diventa:
				;; f0__ =  par[*].f0
				;; x0__ =  par[*].x0
				;; y0__ =  par[*].y0
				;; sx__ =  par[*].sx
				;; sy__ =  par[*].sy
				;; pa__ =  par[*].pa/!pi*180. mod 360.
				;;
		       for _nGaussIdx_ = 0L,n_gauss-1 do begin
		               f0__[_nGaussIdx_] =  par[_nGaussIdx_].f0
		               x0__[_nGaussIdx_] =  par[_nGaussIdx_].x0
		               y0__[_nGaussIdx_] =  par[_nGaussIdx_].y0
		               sx__[_nGaussIdx_] =  par[_nGaussIdx_].sx
		               sy__[_nGaussIdx_] =  par[_nGaussIdx_].sy
		               pa__[_nGaussIdx_] = (par[_nGaussIdx_].pa/!pi*180. mod 360.)
				endfor
							
			noise = res_photo(im,dmax, windowcenter, f0__, x0__, y0__, sx__, sy__, pa__, par(0).fit, /MULTIPLE, eachsour=dmax_single, mask=err_mask)
	
			; Compute the integrate flux
	
			xyad,h,par(0).x0,par(0).y0,ra1,dec1,/celestial
      		xyad,h,par(0).x0,par(0).y0,glon1,glat1,/galactic
      		
			shift_fit = norm([center(0,0),center(0,1)]-[par(0).x0,par(0).y0])
			
			int_flux=par(0).f0*2.*!pi*((par(0).sx)*pix2asec*asec2rad)*((par(0).sy)*pix2asec*asec2rad)*intflux_corr
        
			check1 = (par(0).error_f0 gt 0.)
        		check2 = (par(0).error_sx gt 0.)
        		check3 = (par(0).error_sy gt 0.)

			check4 = (check1) or (check2) or (check3)


			if check4 eq 0 then err_int_flux = -999. else begin 
					
				if NOT keyword_set(CORREL) then begin
					err_int_flux = 	int_flux*(nint(check2)*(par(0).error_sx/par(0).sx)^2 + $
							nint(check3)*(par(0).error_sy/par(0).sy)^2 + $
							nint(check1)*(par(0).error_f0/par(0).f0)^2)^0.5
				endif else begin
	
					err_int_flux = 	int_flux*(nint(check2)*(par(0).error_sx/par(0).sx)^2 +$
							nint(check3)*(par(0).error_sy/par(0).sy)^2 +$
							nint(check1)*(par(0).error_f0/par(0).f0)^2 +$
							2*(nint(check2 and check3))*par(0).cov_matrix(3,4)/(par(0).sx*par(0).sy) + $
							2*(nint(check1 and check2))*par(0).cov_matrix(0,3)/(par(0).f0*par(0).sx) + $
							2*(nint(check1 and check3))*par(0).cov_matrix(0,4)/(par(0).f0*par(0).sy))^0.5
				endelse
			endelse			
	
			if keyword_set(backgfit) then begin
				background = par(0).back + (par(0).x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par(0).backx + (par(0).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(0).backy + $
				(par(0).x0 - max([nearint(windowcenter(0))-dmax(0),1]))^2 *par(0).backx2 + (par(0).y0 - max([nearint(windowcenter(1))-dmax(1),1]))^2*par(0).backy2 + $
				(par(0).x0 - max([nearint(windowcenter(0))-dmax(0),1]))*(par(0).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(0).backxy
			endif else begin
				background = par(0).back + (par(0).x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par(0).backx + (par(0).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(0).backy 
    			endelse
		

	
	
			;background = 	par(0).back + (par(0).x0 - max([nearint(windowcenter(0))-dmax(0),1])) *par(0).backx + $
			;		(par(0).y0 - max([nearint(windowcenter(1))-dmax(1),1]))*par(0).backy

			chi2_dof = par(0).chi2/par(0).dof
			
			if unitcoord eq 'fk5' then begin 
				xyad,h,par(0).x0,par(0).y0,xreg,yreg,/celestial ;Recomputing the Equatorial Coordinates on the newly fit gaussian peak position
			endif else begin
				xyad,h,par(0).x0,par(0).y0,xreg,yreg,/galactic ;Recomputing the Galactic Coordinates on the newly fit gaussian peak position
			endelse
 							
	
			; 	SAVE THE RESULT!
				 
			; note in the output below that I am adding 1 to X and Y (see above comments)
;      			printf,lun_photfile, ' ', pad_parameter(n(qmarg(qth(selsource(0)))),' ',field_len), ' ', pad_parameter(group(selsource(0)),' ',field_len), ' ', $
;			pad_parameter(n_gauss,' ',field_len), ' ', $
;			pad_parameter(par(0).x0+1,' ',field_len), ' ', pad_parameter(par(0).y0+1,' ',field_len), ' ', $
;			pad_parameter(ra1,' ',field_len), ' ',pad_parameter(dec1,' ',field_len), ' ', $
;			pad_parameter(glon1,' ',field_len), ' ',pad_parameter(glat1,' ',field_len), ' ',$
;			pad_parameter(knownwave(indxband(0)),' ',field_len), ' ', $
;			pad_parameter(par(0).f0,' ',field_len), ' ' , $
;			pad_parameter((par(0).sx)*2.354*pix2asec,' ',field_len), ' ', pad_parameter((par(0).sy)*2.354*pix2asec,' ',field_len), ' ', $
;			pad_parameter(par(0).pa/!pi*180. mod 360.,' ',field_len), ' ', $ 
;			pad_parameter(int_flux,' ',field_len), ' ', $
;			pad_parameter(background,' ', field_len), ' ', pad_parameter(chi2_dof,' ',field_len), ' ',$
;			pad_parameter(noise.sdev2,' ', field_len), ' ', pad_parameter(par(0).chi2, ' ', field_len), ' ',$
;			pad_parameter(par(0).status, ' ',  field_len)
 
 
 
	        	stringToPrint =  ' '+ pad_parameter(n(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ $
	        	pad_parameter(par(0).x0+1,' ',field_len)+ ' '+ pad_parameter(par(0).y0+1,' ',field_len)+ ' ' + $
	        	pad_parameter(ra1,' ',field_len)+ ' '+pad_parameter(dec1,' ',field_len)+ ' '+ $
	        	pad_parameter(glon1,' ',field_len)+ ' '+pad_parameter(glat1,' ',field_len)+ ' '+ $
	        	pad_parameter(knownwave(indxband(0)),' ',field_len)+ ' '
				
				stringToPrint =  stringToPrint + pad_parameter(par(0).f0,' ',field_len)+ ' '+  $ 
	        	pad_parameter((par(0).sx)*2.354*pix2asec,' ',field_len)+ ' '+ pad_parameter((par(0).sy)*2.354*pix2asec,' ',field_len)+ ' '+ $
	        	pad_parameter(par(0).pa/!pi*180. mod 360.,' ',field_len)+ ' '+ pad_parameter(int_flux,' ',field_len)+ ' '+ $ 
	        	pad_parameter(err_int_flux,' ',field_len)+ ' '+$
			pad_parameter(background,' ', field_len)+ ' '+ pad_parameter(noise.sdev2,' ',field_len)+ ' '+ pad_parameter(noise.newsdev4_onsou,' ',field_len)+ ' '+$
	        	pad_parameter(noise.sdev4,' ',field_len)+ ' '+ pad_parameter(par(0).sum2res, ' ', field_len)+ ' '+$
	        	pad_parameter(par(0).dof, ' ', field_len)+ ' '+ pad_parameter(par(0).chi2, ' ', field_len )+ ' '+ pad_parameter(par(0).chi2opp, ' ', field_len )+ ' '+$
				pad_parameter(par(0).size_flag, ' ', 25)+ ' '+ pad_parameter(group_flag, ' ', field_len)+ ' '
				
				stringToPrint =  stringToPrint + pad_parameter(n_gauss, ' ', field_len)+ ' '+ pad_parameter(IN_CLUMP(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+$
;	 			pad_parameter(IN_DIST(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ pad_parameter(IN_GUESS(qmarg(qth(selsource(0))
	 			pad_parameter(shift_fit,' ',field_len)+ ' '+ pad_parameter(IN_GUESS(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ $
				pad_parameter(STRING(err_mask.nsou, format='(I3)'), ' ',  field_len)+ ' '+$
				pad_parameter(STRING(par(0).status, format='(F4.1)'), ' ',  field_len)+ ' '+ pad_parameter(IN_FLAG(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ $
				pad_parameter(noise.flagin, ' ',  field_len)+ ' '+  pad_parameter(noise.flagout,' ',field_len)+ ' '
				
				stringToPrint =  stringToPrint + pad_parameter(IN_D2X(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ $	
				pad_parameter(IN_D2X45(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y45(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ $
				pad_parameter(IN_D2X_THR(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y_THR(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ $	
				pad_parameter(IN_D2X45_THR(qmarg(qth(selsource(0)))),' ',field_len)+ ' '+ pad_parameter(IN_D2Y45_THR(qmarg(qth(selsource(0)))),' ',field_len)+ ' '
				
				stringToPrint =  stringToPrint + pad_parameter(strtrim(string(par(0).xd2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).yd2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).x45d2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).y45d2det , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
				pad_parameter(strtrim(string(par(0).xd2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).yd2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).x45d2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).y45d2detbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
				pad_parameter(strtrim(string(par(0).xd2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).yd2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).x45d2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).y45d2phot , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ $
				pad_parameter(strtrim(string(par(0).xd2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).yd2photbck , FORMAT='(F15.5)'),2),' ',field_len) +' '+ pad_parameter(strtrim(string(par(0).x45d2photbck , FORMAT='(F15.5)'),2),' ',field_len)+ ' '+ pad_parameter(strtrim(string(par(0).y45d2photbck , FORMAT='(F15.5)'),2),' ',field_len)

 				printf,lun_photfile,stringToPrint
				
			str2printbck =  ' '+ pad_parameter(n(qmarg(qth(selsource(0)))),' ',field_len)+' '+ pad_parameter(par(0).back,' ',field_len)+ ' '+ pad_parameter(par(0).backx,' ',field_len)+ ' '+ pad_parameter(par(0).backy,' ',field_len)+ ' '
			if keyword_set(BACKGFIT) then str2printbck = str2printbck+ pad_parameter(par(0).backx2,' ',field_len)+ ' '+ pad_parameter(par(0).backy2,' ',field_len)+ ' '+pad_parameter(par(0).backxy,' ',field_len)+' '
			printf, lun_photfile_bak, str2printbck

 
			printf,lun_phot_dev, ' ', pad_parameter(n(qmarg(qth(selsource(0)))),' ', field_len), ' ', pad_parameter(noise.sdev1,' ', field_len), ' ', $
			pad_parameter(noise.sdev2,' ', field_len), ' ', pad_parameter(noise.sdev3,' ', field_len), ' ', pad_parameter(noise.sdev4, ' ', field_len), ' ', $
			pad_parameter((noise.sdev4_onsou)[0],' ', field_len), ' ', pad_parameter(noise.newsdev4,' ', field_len), ' ', pad_parameter((noise.newsdev4_onsou)[0], ' ', field_len), ' ', pad_parameter((noise.newsdev5_onsou)[0], ' ', field_len)


 
 			printf,lun_phot_err,' ', pad_parameter(n(qmarg(qth(selsource(0)))),' ',field_len), ' ', pad_parameter(group(selsource(0)),' ',field_len), ' ', $
			pad_parameter(n_gauss, ' ', field_len), ' ', $
			pad_parameter(par(0).x0+1,' ',field_len), ' ', pad_parameter(par(0).y0+1,' ',field_len), ' ' , $
			pad_parameter(ra1,' ',field_len), ' ',pad_parameter(dec1,' ',field_len), ' ',$
			pad_parameter(glon1,' ',field_len), ' ',pad_parameter(glat1,' ',field_len), ' ',$
			pad_parameter(knownwave(indxband(0)),' ',field_len), ' ', $				
			pad_parameter(strtrim(par(0).error_f0,2),' ',field_len), ' ',$
			pad_parameter(strtrim(par(0).error_x0*pix2asec,2),' ',field_len), ' ', pad_parameter(strtrim(par(0).error_y0*pix2asec,2),' ',field_len), ' ', $
			pad_parameter(strtrim((par(0).error_sx)*2.354*pix2asec,2),' ',field_len), ' ', pad_parameter(strtrim((par(0).error_sy)*2.354*pix2asec,2),' ',field_len), ' ', $
			pad_parameter(strtrim(par(0).error_pa/!pi*180. mod 360.,2),' ',field_len), ' ',pad_parameter(strtrim(err_int_flux,2),' ',field_len), ' ',$
			pad_parameter(strtrim(chi2_dof,2),' ',field_len), ' ',pad_parameter(strtrim(noise.sdev2,2),' ', field_len), ' ', pad_parameter(strtrim(par(0).chi2,2), ' ', field_len), ' ',$
			pad_parameter(par(0).status, ' ',  field_len)

			printf,lun_phot_par, ' ', pad_parameter(n(qmarg(qth(selsource(0)))),' ',field_len), ' ', pad_parameter(group(selsource(0)),' ',field_len), ' ', $
			pad_parameter(n_gauss, ' ', field_len), ' ', $
			pad_parameter(windowcenter(0),' ',field_len), ' ', pad_parameter(windowcenter(1),' ',field_len), ' ' , $	
			pad_parameter(dmax(0),' ',field_len), ' ', pad_parameter(dmax(1),' ',field_len), ' ' , $
			pad_parameter(center(0,0),' ',field_len), ' ', pad_parameter(center(0,1),' ',field_len), ' ' , $
			pad_parameter(source_par(0,0),' ',field_len), ' ', pad_parameter(source_par(0,1),' ',field_len), ' ' , pad_parameter(source_par(0,2) ,' ',field_len), ' ', pad_parameter(source_par(0,3),' ',field_len), ' ', pad_parameter(par(0).nnan,' ',field_len)
		
    			printf,lun_region,'ellipse('+strcompress(string(par(0).x0+1.),/remove_all)+','+strcompress(string(par(0).y0+1.),/remove_all)+','+strcompress(string(abs(par(0).sx)*2.354/2.),/remove_all)+','+strcompress(string(abs(par(0).sy)*2.354/2.),/remove_all)+','+strcompress(string(par(0).pa/!pi*180. mod 360.),/remove_all)+')'
      			printf,lun_region,'# text('+strcompress(string(par(0).x0+1.),/remove_all)+','+strcompress(string(par(0).y0+1.),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(selsource(0)))))),/remove_all)+'}'
			
	   		printf,lun_region_wcs,'ellipse('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+','+strcompress(string(abs(par(0).sx)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(abs(par(0).sy)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(par(0).pa/!pi*180. mod 360.),/remove_all)+')'
      			printf,lun_region_wcs,'# text('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(selsource(0)))))),/remove_all)+'}'
		
		
		
			; SAVE THE REG FILE

			if (keyword_set(outfile)) then begin
				regfile=workdir+outfile+'_list_memb_'+strcompress(strtrim(string(n_gauss,'(I)')),/remove_all)+'.reg'
			endif else begin
				regfile=workdir+prefix+'_list_memb_'+strcompress(strtrim(string(n_gauss,'(I)')),/remove_all)+'.reg'
			endelse

			regfile_=regfiles+strcompress(strtrim(string(n_gauss,'(I)')),/remove_all)+'_wcs.reg'
	
			checkfile = file_search(regfile,count=count)
		  	if count eq 0 then begin
				openw,lung_group_region,regfile,/get_lun 
				openw,lung_group_region_wcs,regfile_,/get_lun 
			endif else begin
				openu, lung_group_region, regfile, /GET_LUN, /APPEND
				openu, lung_group_region_wcs, regfile_, /GET_LUN, /APPEND
			endelse
			
   			printf,lung_group_region,'ellipse('+strcompress(string(par(0).x0+1.),/remove_all)+','+strcompress(string(par(0).y0+1.),/remove_all)+','+strcompress(string(abs(par(0).sx)*2.354/2.),/remove_all)+','+strcompress(string(abs(par(0).sy)*2.354/2.),/remove_all)+','+strcompress(string(par(0).pa/!pi*180. mod 360.),/remove_all)+')'
	      		printf,lung_group_region,'# text('+strcompress(string(par(0).x0+1.),/remove_all)+','+strcompress(string(par(0).y0+1.),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(selsource(0)))))),/remove_all)+'}'
    			
			printf,lung_group_region_wcs,'ellipse('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+','+strcompress(string(abs(par(0).sx)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(abs(par(0).sy)*2.354*pix2asec/2.),/remove_all)+'",'+strcompress(string(par(0).pa/!pi*180. mod 360.),/remove_all)+')'
  			printf,lung_group_region_wcs,'# text('+strcompress(string(xreg),/remove_all)+','+strcompress(string(yreg),/remove_all)+') text={'+strcompress(string(fix(n(qmarg(qth(selsource(0)))))),/remove_all)+'}'
 
			free_lun, lung_group_region	
			free_lun, lung_group_region_wcs		
		

	
		endfor
	endelse

	if keyword_set(TIME) then begin
		print, "Estimating Time for Multiple Sources sources (with Closest Neighbours)"
		timer, /STOP, /PRINT, dt
		timer, /START
	endif
endif  

free_lun,lun_region
free_lun,lun_region_wcs

free_lun,lun_photfile
free_lun,lun_photfile_bak
free_lun,lun_phot_dev
free_lun,lun_phot_par
free_lun,lun_phot_err


fine:
if keyword_SET(TIME) then timer, /STOP

end

;; ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- 
;; ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- 
;; 
;; Last Developer       : $Author: hipe $
;; Revision             : $Revision: 1.4 $
;; Checkout Tag         : $Name:  $
;; Last Modification    : $Date: 2014/01/29 15:16:18 $
;; Location             : $RCSfile: extract_photo.pro,v $
;; CVSID                : $Header: //home/higal_repository/CVS_ROOT/cutex/Extraction/extract_photo.pro,v 1.4 2014/01/29 15:16:18 hipe Exp $
;; 
;; Commitments History :
;; As reported in Main cvs Documentation
;; The Modification Log has been posted at End Of File.
;;
;; Commitments History :
;;
;; $Log: extract_photo.pro,v $
;; Revision 1.4  2014/01/29 15:16:18  hipe
;; nino 2014/01/29 adding history log
;;
;; 
;; ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- 
