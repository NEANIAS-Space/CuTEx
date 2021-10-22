; This program identifies sources detected using analysis of the second derivatives of the images. 
;
; inputDir  : input file directory (image files)
; a1        : name of file
; nsigma  : detection threshold
; outputDir  : where the output will be written
; outfile: IDL variable containing the name of the file containing the list of detected source
;
;

;  ATTENZIONE AL PSFPIX!!!!!!!!!!!!!!!!!!!
; 
;psfpix = 3.  ; this assumes that the image is such that the PSF FWHM is covered by three pixels. 
;             ; it is needed for the sanity check of the source ellipse guess which is done further down
; 
;psfpix = 2.7 ; for PACS Blue
;psfpix = 2.7 ; for PACS Red
;psfpix = 5.6 ; for MIPSGAL24
; 
;Default is 3 pixel if keyword is not setted.
;
;
;	USED Functions :im_deriv.pro		-	Compute image derivatives
;					im_threshold.pro	-	Compute Mask map given the Threshold
;					deriv_5.pro		-	5-points Derivate function  - along x and y axis
;					deriv_45.pro		-	5-points Derivate function  - along diagonals
;					remove_regions.pro	- 	Remove pixels group less than NPIXEL
;
; 	KEYWORDS :		PSFPIX			-	Property of the map. Define the number of pixels that sample the instrumental PSF.
;					NPIXMASK		-	Parameter selecting the minimum number of closest-neighbours above the threshold to be found to define a source candidate.
;					ALLOW_SINGLE	- 	Allow single pixels above the thresholds to be considered as source center candidates
;					SMOOTHING		-	Activate internal smoothing of the image usign a boxcar. The width can be setted and has to be an odd number.
;					NO_OVERWRITE	-	If Setted it will check if the derivate images are already computed. If they are not computed the code compute them, otherwise simply read the images.
;					ALL_NEIGHBOURS	- 	Consider pixels along diagonals while checking closest-neighbours. (Default no set)
;					DERIVATE		- 	Set this to have the standard 3-points derivative. (Default not set 5-points derivate)
;					THRESH			- 	Define the criteria to detect pixels as source center candidates. (Default not set)
;					ABSCURV			- 	Convert input threshold level as absolute curvature value
;					LOCAL_THRESH	-	Activate the local threshold method. The value of the local threshold define the window size for local thresholding
;					RANGE 			-	Number of pixels away from the center where to check second derivative minima
; SUPER_RESOLUTION is a keyord to resolve sources in curvature pixel clusters: if it is set then each cluster pixel is checked with respecto to its neighbours considering ewach derivative direction separately, otherwise the check is done using the average derivative

function 	detection,inputDir,a1,nsigma,outputDir,outfile, $
		smoothing=smoothing,npixmask=npixmask,psfpix=psfpix,derivate=derivate, $
		no_overwrite=no_overwrite, allow_single=allow_single, all_neighbours=all_neighbours, $
		thresh=thresh, range=range, super_resolution=super_resolution, $
		abscurv=abscurv, local_thresh=local_thresh, fact_mask=fact_mask, checkhits=checkhits, quiet=quiet, TIME=TIME

;_________________________________________________________
; Initial settings
;_________________________________________________________

COMMON DERIV, xd2, yd2, x45d2, y45d2
COMMON DERIVTHRESH, xd2m, yd2m, x45d2m, y45d2m
COMMON NANMAP, nanmask, nanmasksurround, xd2nan, yd2nan, x45d2nan, y45d2nan, nanpixels

print, "-----------------------------------------------------------------"
print, "Detection Routine of CuTEx - ver 1.000 - Released 29 Sep 2016  "
print, "-----------------------------------------------------------------"

if keyword_SET(TIME) then  timer, /START

border=2L

if keyword_set(quiet) then quiet = 1. else quiet = 0

if (keyword_set(super_resolution)) then superres_key=',/super_resolution' else superres_key=''

nsigma=float(nsigma)
stsig=pad_parameter(nsigma,'0',5)

; this assumes that the image is such that the PSF FWHM is covered by
; three pixels. it is needed for the sanity check of the source
; ellipse guess which is done further down 

if NOT keyword_set(psfpix) then psfpix = 3. else psfpix = psfpix
if NOT keyword_set(range) then range = 8. else range=range	; Number of pixels away from the center where to check second derivative minima				

if (keyword_set(npixmask)    eq 0) then npixmask=4
if (keyword_set(allow_single)    ) then npixmask=1 
print, "Using assumption that PSF on image is sampled by", psfpix, " pixels"
print, "Using setup for source detection of ", npixmask, " neighbour pixels"

;_________________________________________________________
; Reading the image
;_________________________________________________________

print, 'Reading the Input Image'
a=readfits(inputDir+a1,ha1,/silent)

;_________________________________________________________
; Check for NaN
;_________________________________________________________

print, 'Checking for pixels flagged as NaN'

anan = a
anan(*)= 0
anan(border:n_elements(a(*,0)) - (border +1),border:n_elements(a(0,*)) - (border +1)) $
   = $
   a(border:n_elements(a(*,0)) - (border +1),border:n_elements(a(0,*)) - (border +1))

nans = check_nan(anan)
if nans.check ne 0 then begin 
		nanmask = nans.nanmask
		nanpixels = nans.nanpixels 
endif else begin 
	
	nanmask = 0.
	nanpixels = 0.
	
endelse

if keyword_set(CHECKHITS) then begin
      	
	frac_hits = 0.6
      	dbox = 5
      	knownp2a = [3.2, 4.5, 6.0, 8.0, 11.5]
      	pix2asec=abs(sxpar(ha1,'CDELT1'))*3600.
      	indxband = where(ABS(knownp2a - pix2asec) lt 1E-1)
      	knownband=['blue','red','PSW','PMW','PLW']
      	hitfiles=file_search(inputDir+'/hits*'+knownband(indxband)+'*.fits')
      	;  ahit = readfits(inputDir+checkhits, /SILENT)
      	ahit = readfits(inputDir+'/'+hitfiles(0), /SILENT)
      	ahit = smooth(ahit,dbox)
      	okpix = where(ahit gt frac_hits*max(ahit), complement=excludepix,ncomplement=nexcludepix)
	if (nexcludepix gt 0) then a(excludepix) = 0.

endif

extast, ha1, astr
print, 'Checking the Map Coordinate System'

if ((astr.ctype[0] eq 'GLON-TAN') and (astr.ctype[1]  eq 'GLAT-TAN')) or ((astr.ctype[0] eq 'GLON-CAR') and (astr.ctype[1]  eq 'GLAT-CAR')) then begin

	print, "Map has Galactic Coordinates"
	unitcoord = 'galactic'

endif else begin
	if ((astr.ctype[0] eq 'RA---TAN') and (astr.ctype[1]  eq 'DEC--TAN'))  or ((astr.ctype[0] eq 'RA---CAR') and (astr.ctype[1]  eq 'DEC--CAR')) then begin
		print, "Map has Equatorial Coordinates"
		unitcoord = 'fk5'
	endif else begin
		print, "Map Coordinates not Recognize - There could be an issue with region files in wcs system'
		unitcoord = 'wcs'
	endelse	
endelse	

if (keyword_set(smoothing) eq 1) then begin


   	print, "Smoothing Keyword active: Using a smoothing length = ", smoothing
  	a=smooth(a,smoothing,/NAN,/edge_truncate)

endif

;_________________________________________________________
; Computing Image derivatives
;_________________________________________________________
if keyword_set(no_overwrite) eq 1 then  begin
	
	file = outputDir+'/'+'allder2_'+a1
	print, "Checking if derivative images are already computed" 
	dd = file_search(file,count=count)
	
	if KEYWORD_SET(local_thresh) then begin 
		
		print, "Checking if local threshold images are already computed"
		dd1 = file_search(outputDir+'/'+'*_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',count=count1) 
		if (count1 eq 4) then begin
			print, 'Found four thresholding images - Not recomputing them'
			local_thresh_comp = 0
		endif else begin
			print, 'Not found all the thresholding images - Computing'
			local_thresh_comp = local_tresh
		endelse		
		
	endif
		
	if (count le 0) then begin 
		print, "No Derivate image found"	

		if keyword_set(smoothing) then begin
		 	if keyword_set(derivate) then begin
				dd = im_deriv(a, outputDir, a1, ha1, smoothing=smoothing, derivate=derivate, local_thresh=local_thresh_comp) 
			endif else dd = im_deriv(a, outputDir, a1, ha1, smoothing=smoothing, local_thresh=local_thresh_comp)
		endif else begin
			if keyword_set(derivate) then dd = im_deriv(a, outputDir, a1, ha1, derivate=derivate, local_thresh=local_thresh_comp) else dd = im_deriv(a, outputDir, a1, ha1, local_thresh=local_thresh_comp)
		endelse
	endif

endif else begin

	if KEYWORD_SET(local_thresh) then begin 
		
		print, "Checking if local threshold images are already computed"
		dd1 = file_search(outputDir+'/'+'*_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',count=count1) 
		if (count1 eq 4) then begin
			print, 'Found four thresholding images - Not recomputing them'
			local_thresh_comp = 0
		endif else begin
			print, 'Not found all the thresholding images - Computing'
			local_thresh_comp = local_thresh
		endelse		
		
	endif else local_thresh_comp = 0.

		
	if keyword_set(smoothing) then begin
		if keyword_set(derivate) then begin
			dd = im_deriv(a, outputDir, a1, ha1, smoothing=smoothing, derivate=derivate, local_thresh=local_thresh_comp)
		endif else dd = im_deriv(a, outputDir, a1, ha1, smoothing=smoothing, local_thresh=local_thresh_comp) 
	endif else begin
		if keyword_set(derivate) then dd = im_deriv(a, outputDir, a1, ha1, derivate=derivate, local_thresh=local_thresh_comp) else dd = im_deriv(a, outputDir, a1, ha1, local_thresh=local_thresh_comp)
	endelse
	
endelse

xd2 = readfits(outputDir+'/'+'der2x_'+a1,ha1,/silent)
yd2 = readfits(outputDir+'/'+'der2y_'+a1,ha1,/silent)
x45d2 = readfits(outputDir+'/'+'der2x45_'+a1,ha1,/silent)
y45d2 = readfits(outputDir+'/'+'der2y45_'+a1,ha1,/silent)
der2=(xd2+yd2+x45d2+y45d2)/4.

if keyword_SET(TIME) then begin
	print, "Estimating Time for computing derivates"
	timer, /STOP, /PRINT, dt
	timer, /START
endif

sz = size(der2)

n1 = sz(2)
n2 = sz(1)

;n1=n_elements(der2(0,*))
;n2=n_elements(der2(*,0))

;__________________________________________________
; Statistic on image derivates and create Mask file
;__________________________________________________

if NOT keyword_set(local_thresh) then local_thresh = 0

if NOT keyword_SET(ABSCURV) then begin
	if not keyword_set(LOCAL_THRESH) then begin
				
		print,'N sigma Threshold = ', stsig
 		mask = im_threshold(der2, nsigma) 

		sdev = mask.thresh/nsigma
	
		print,'Standard Deviation of Second Derivative image = ',sdev
		print,'You have chosen a threshold of ',mask.thresh, ' corresponding to ',nsigma,' sigma' 

	endif else begin

		print, 'You have chosen to compute local thresholding'
		print,'N sigma Threshold = ', stsig
		 
		xd2m = readfits(outputDir+'/'+'der2x_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',ha1m,/silent)
		yd2m = readfits(outputDir+'/'+'der2y_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',ha1m,/silent)
		x45d2m = readfits(outputDir+'/'+'der2x45_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',ha1m,/silent)
		y45d2m = readfits(outputDir+'/'+'der2y45_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',ha1m,/silent)
		
		if local_thresh lt 10 then begin
			
			window = 30
			print, 'Adopting default size to evaluate the local threshold'
		endif else begin
			window = local_thresh
		endelse
					
		print, 'Adopting the local thresholding computed in windows of size ', window, ' pixels' 
		
		mask = im_local_thresh(der2, nsigma)
	endelse

endif else begin

	if keyword_set(LOCAL_THRESH) then print, "WARNING. ABSOLUTE CURVATURE and LOCAL THRESHOLD are not COMPATIBLE together. OVERRIDING THE LOCAL THRESHOLD keyword"
	print, "Threshold defined in absolute curvature value"
	print, "Threshold = ", nsigma
	mask = im_threshold(der2, nsigma, /ABSCURV) 

endelse

hamask = ha1
sxaddpar, hamask, 'THRESH', mask.thresh
sxaddpar, hamask, 'NPIXMASK', npixmask
if keyword_SET(SMOOTHING) then sxaddpar, hamask,'SMOOTH', smoothing
if keyword_SET(ABSCURV) then sxaddpar, hamask,'ABSCURV', abscurv

writefits,outputDir+'/'+'mask_'+stsig+'_'+a1,mask.mask,hamask

if N_ELEMENTS(nanmasksurround) gt 1 then begin

	print, 'Found regions nearby NaN pixels - Saving mask'
	writefits,outputDir+'/'+'mask_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'_nan.fits',mask.masknan,hamask
endif

print, 'Search for pixels above threshold'

mas1=where(mask.mask eq 10000,n)

if (n le 1) then return,-1  ;at least two pixels are needed otherwise the grouping will fail
print,'Found ',n,' pixels above the threshold'
print,'Removing Pixel Groups smaller than ', NPIXMASK,' pixels'
cut_ = npixmask - 1			; Size of the Clumps to be removed 

;;********** Code to remove single pixels
;
;if not keyword_set(ALLOW_SINGLE) then begin
;
;	print, 'Pre-Filtering: Removing lonely pixels from the mask'
;	mask_in=mask.mask
;	if not keyword_set(ALL_NEIGHBOURS) then neigh_single,mask_in,mask_out else neigh_single,mask_in,mask_out, /ALL_NEIGHBOURS
;	print,'Remaining pixels above threshold after pre-filtering', n_elements(where(mask_out))
;	mask.mask=mask_out
;
;endif

;if KEYWORD_SET(all_neighbours) then  mas = remove_regions(mask.mask,cut= cut_, /ALL_NEIGHBOURS) else mas = remove_regions(mask.mask,cut= cut_)	; Remove all the Clumps with size smaller than NPIXMASK

if not keyword_set(ALLOW_SINGLE) then begin
	
	if KEYWORD_SET(all_neighbours) then  mas = remove_regions_new(mask.mask,cut= cut_, /ALL_NEIGHBOURS) else mas = remove_regions_new(mask.mask,cut= cut_)	; Remove all the Clumps with size smaller than NPIXMASK

endif else mas = mask.mask

writefits,outputDir+'/'+'mask_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'_removed.fits', mas, hamask

mas1 = where(mas eq 1., n)
print,'Found ', n, ' pixels in regions wide at least ', npixmask, ' pixels' 

if n eq 0 then begin
	print, "There isn't any region satisfying the selected criteria" 
	print, "Threshold level too high or too large NPIXMASK"
	return, -1
	
endif

if N_ELEMENTS(nanmasksurround) gt 1 then begin
	
	print, 'Found regions nearby NaN pixels - Clearing mask'

	if not keyword_set(ALLOW_SINGLE) then begin
	
		if KEYWORD_SET(all_neighbours) then  masnan = remove_regions_new(mask.masknan,cut= cut_, /ALL_NEIGHBOURS) else masnan = remove_regions_new(mask.masknan,cut= cut_)	; Remove all the Clumps with size smaller than NPIXMASK

	endif else masnan = mask.mask
	
	writefits,outputDir+'/'+'mask_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'_nan_removed.fits', masnan, hamask
	
	mas2 = where(masnan eq 1., nnan)

;___________________________________________________________ NEW CHECK	(why did i do this?)

;	if nnan gt n then begin 

		print,'Found ', nnan - n, ' further pixels in regions wide at least ', npixmask, ' pixels' 
	
		;if keyword_set(ALL_NEIGHBOURS) then maslabel = label_region(masnan, /ALL_NEIGHBORS, /ULONG) else maslabel = label_region(masnan, /ULONG)		
		if keyword_set(ALL_NEIGHBOURS) then maslabel = test_label_region(masnan) else maslabel = test_label_region(masnan)       

		maslabelnan = FLOAT(maslabel)*(masnan - mas)
	
		maslabelsort = maslabelnan(sort(maslabelnan))
		maslabeluniq = maslabelsort(uniq(maslabelsort))
		
		checkmaslabeluniq = where(maslabeluniq gt 0, nnanregions)

	
		if nnanregions gt 0 then begin
			nanregions = maslabeluniq(where(maslabeluniq gt 0))
	
			for lab = 0, nnanregions-1 do begin
			
				maslabel(where(maslabel eq nanregions(lab))) = 0.
			
			endfor 

		endif
		
		nmaslabel = where(maslabel gt 0, nnmaslabel)
		
		if nnmaslabel gt 0 then maslabel(nmaslabel) = 1  else print, 'Found that the regions in the initial Mask files are also found in the Mask taking into account nearby NaN pixels'
	
		print, 'Updating the Mask file taking into account the regions with nearby NaN pixels'
	
		mas = maslabel
	
;	endif
	
;___________________________________________________________ 
	
	masfurther = masnan - mas
;	masnan_ = masnan
;	masnan(*) = 0.
;	masnan(where(maslabel gt 0)) = 1.
	
endif 

if keyword_SET(TIME) then begin
	print, "Estimating Time for Thresholding and Removing Clusters smaller than "+string(npixmask,FORMAT='(I3)')+" pixels"
	timer, /STOP, /PRINT, dt
	timer, /START
endif

;____________________________________________________
; Group Pixels to detect Sources
;____________________________________________________

source=0

strkeyw = ''
if keyword_set(ALL_NEIGHBOURS) then strkeyw = strkeyw + ', /ALL_NEIGHBOURS'
if keyword_set(THRESH) then strkeyw = strkeyw+ ', thresh=thresh'
if keyword_set(LOCAL_THRESH) then strkeyw = strkeyw + ',/LOCAL_THRESH'
if keyword_set(QUIET) then strkeyw = strkeyw+', /QUIET'


z  = execute('source= find_sources(mas,a, der2, nsigma'+strkeyw+superres_key+')')

;if keyword_set(ALL_NEIGHBOURS) then begin
;	if keyword_set(thresh) then z=execute('source= find_sources(mas,a,xd2,yd2,x45d2,y45d2,der2, nsigma, thresh=thresh, /ALL_NEIGHBOURS'+superres_key+')') else$
;				    z=execute('source = find_sources(mas,a,xd2,yd2,x45d2,y45d2,der2, nsigma,/ALL_NEIGHBOUR'+superres_key+')')
;endif else begin
;	if keyword_set(thresh) then z=execute('source = find_sources(mas,a,xd2,yd2,x45d2,y45d2,der2, nsigma, thresh=thresh'+superres_key+')') else $
;				    z=execute('source = find_sources(mas, a,xd2,yd2,x45d2,y45d2,der2, nsigma'+superres_key+')')
;endelse
	
; Find Centers of detected sources. Check in the single pixel group if there are

checknsources = size(source)
if checknsources(0) ne 0 then ks1 = n_elements(source(0,*)) else ks1 = 0

if n_elements(nanmasksurround) gt 1 then begin 

	strkeywnan = strkeyw+',/SOURCESNAN'
	zz  = execute('sourcenan= find_sources(masfurther,a, der2, nsigma'+strkeywnan+superres_key+')')
	
	checksourcenan = size(sourcenan, /TYPE)
	if checksourcenan eq 4 then begin

		sourcenan(11,*) = sourcenan(11,*) + ks1
		if ks1 gt 0 then source = [[source], [sourcenan]] else source = sourcenan
 	endif
endif

if (source(0,0) eq -1) then begin

  	print,'No valid clusters of curvature pixels found'
  	return,0

endif


print, "------------------------------------------------------------------------------------------"
print, "Total sources found "+strtrim(n_elements(source(0,*)),2)+ " : "+strcompress(string(ks1),/REMOVE_ALL)+ " from analysis of all directions and "+strcompress(string(n_elements(source(0,*))- ks1),/REMOVE_ALL)+ " nearby Nan pixels" 
print, "------------------------------------------------------------------------------------------"
ks = n_elements(source(0,*)) 



if keyword_SET(TIME) then begin

	print, "Estimating Time for estimate Sources Positions"
	timer, /STOP, /PRINT, dt
	timer, /START

endif

;______________________________________________________
; Estimate source sizes from the second derivative maps
;______________________________________________________


print, 'Estimates initial guess for Gauss fitting'

openw,unit1,outputDir+'/sources_der2ellipses_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'.reg',/get_lun
printf,unit1,'# File produced by detection.pro'
printf,unit1,'# field :  '+a1
printf,unit1,'# Coordinates of points for initial FWHM guess'
printf,unit1,'global color=green width=1 font="helvetica 10 normal" select=1 highlite=1 fixed=0 edit=1 move=1 delete=1 include=1 source=1'
printf,unit1,'physical'
st='point('
st1=') # point=box'

source_size=fltarr(4,ks)
borderflag = lonarr(ks)
clumpflag = lonarr(ks)
guessflag = lonarr(ks)
nanflag = lonarr(ks)

; Vector with fitted ellipse major and minor axis, the position angle and a flag to tell the gauss fitting program 
; if the ellipse properties are to be kept fixed =1 or free =0)

source_ellipse=fltarr(4,ks) ; 

openw,unit20, outputDir+'/'+'sources_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'_ellipse.dat', width=3000, /get_lun

; ELLIPSE DETAILS HEADER
;_____________________________________________________________________________________________________________

tags = ['ID', 'DER_ELL1', 'DER_ELL2', 'DER_ELL3', 'DER_ELL4', 'DER_ELL5', 'DER_ELL6', 'DER_ELL7', 'DER_ELL8', $
'FLAG_Y', 'FLAG_XY', 'FLAG_X', 'FLAG_YX', $
'FLAG_ELL1', 'FLAG_ELL2', 'FLAG_ELL3', 'FLAG_ELL4', 'FLAG_ELL5', 'FLAG_ELL6', 'FLAG_ELL7', 'FLAG_ELL8', $
'X_ELL1', 'X_ELL2', 'X_ELL3', 'X_ELL4', 'X_ELL5', 'X_ELL6', 'X_ELL7', 'X_ELL8', $
'Y_ELL1', 'Y_ELL2', 'Y_ELL3', 'Y_ELL4', 'Y_ELL5', 'Y_ELL6', 'Y_ELL7', 'Y_ELL8']

tagsfmt = ['INT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', $
'INT', 'INT', 'INT', 'INT', $
'INT', 'INT', 'INT', 'INT', 'INT', 'INT', 'INT', 'INT', $
'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', $
'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT']

tagsunit = [' ', 'MJy/sr/pix^2', 'MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2','MJy/sr/pix^2',$
' ', ' ', ' ', ' ', $
' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', $
'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', $
 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel', 'pixel']

field_len = max([strlen( [tags, tagsfmt,tagsunit]),15])

printstr= '|'
for r = 0, n_elements(tags)-1 do printstr = printstr+pad_parameter(tags(r),' ', field_len)+'|'
printf, unit20, printstr

printstr= '|'
for r = 0, n_elements(tagsfmt)-1 do printstr = printstr+pad_parameter(tagsfmt(r),' ', field_len)+'|'
printf, unit20, printstr

printstr= '|'
for r = 0, n_elements(tagsunit)-1 do printstr = printstr+pad_parameter(tagsunit(r),' ', field_len)+'|'
printf, unit20, printstr

;_____________________________________________________________________________________________________________

;der2_ = [yd2, x45d2, xd2, y45d2]


der2_ = dblarr(2*(range+2)+1,2*(range+2)+1, 4)

;	INCREASED THE SIZE OF THE SUBREGION CHECKED FOR THE MINIMA - Those are old lines
;der2_ = dblarr(2*range+1,2*range+1, 4)

;der2_(*,*,0) = yd2
;der2_(*,*,1) = x45d2
;der2_(*,*,2) = xd2
;der2_(*,*,3) = y45d2

aDirection_ = ['X','XY','Y','YX']
indx_ = [4,2,0,6]

;defin OBJ ROI for ellipse region of interests
;roi_total=obj_new('IDLanROIGroup')

source_mask=a & source_mask(*,*)=0.
szSouMask = size(source_mask)

nquiet = ks/20.
counter = 0.

for sou=0L,ks-1 do begin
;	timer, /START

;	print, "SOURCE", sou, source(0:4, sou)
;	if quiet eq 0 then if (sou ne 0) and ((sou mod nquiet)  eq 0) then print, "Analyzed already "+strcompress(string(LONG(sou/nquiet*5)),/REMOVE_ALL)+" % of "+strcompress(string(LONG(ks)),/REMOVE_ALL)+" sources"
	
	if quiet eq 0 then if (sou ne 0) and (sou ge counter*nquiet)  then begin
		print, "Analyzed already "+strcompress(string(LONG(counter*5)),/REMOVE_ALL)+" % of "+strcompress(string(LONG(ks)),/REMOVE_ALL)+" regions"
		counter = counter+1
	endif  
	if quiet eq 0 then  if (sou eq ks -1) then print, "Analyzed already "+strcompress(string(100),/REMOVE_ALL)+" % of "+strcompress(string(LONG(ks)),/REMOVE_ALL)+" regions"
	
	
  	ell_xpoi=fltarr(8)
  	ell_ypoi=fltarr(8)
  	der_values=fltarr(8)
	
  	radii=fltarr(8)  ; Hold the distance of opposite points in the four directions
  	use_points=intarr(8) & use_points(*)=0
	
	flag_direction = intarr(4) ; Flag keeping the information if the derivate is still decreasing when reached the position equal to range
	
	; Do not evaluate the size for sources near the borders

  	if (source(0,sou) le range+3 or source(1,sou) le range+3 or source(0,sou) ge n_elements(der2(*,0))-(range+3) or source(1,sou) ge n_elements(der2(0,*))-(range+3)) then begin

;	INCREASED THE SIZE OF THE SUBREGION CHECKED FOR THE MINIMA - Those are old lines
; 	if (source(0,sou) lt range+1 or source(1,sou) lt range+1 or source(0,sou) gt n_elements(der2(*,0))-(range+1) or source(1,sou) gt n_elements(der2(0,*))-(range+1)) then begin

    	source_size(*,sou)=0
		bordersource=1
  	
  	endif else begin
		bordersource=0
	endelse
	if (bordersource eq 0) then begin

		der2_(*,*,0) = yd2(nearint(source(0,sou))-(range+2):nearint(source(0,sou))+(range+2),nearint(source(1,sou))-(range+2):nearint(source(1,sou))+(range+2))
		der2_(*,*,1) = x45d2(nearint(source(0,sou))-(range+2):nearint(source(0,sou))+(range+2),nearint(source(1,sou))-(range+2):nearint(source(1,sou))+(range+2))
		der2_(*,*,2) = xd2(nearint(source(0,sou))-(range+2):nearint(source(0,sou))+(range+2),nearint(source(1,sou))-(range+2):nearint(source(1,sou))+(range+2))
		der2_(*,*,3) = y45d2(nearint(source(0,sou))-(range+2):nearint(source(0,sou))+(range+2),nearint(source(1,sou))-(range+2):nearint(source(1,sou))+(range+2))

; 		INCREASED THE SIZE OF THE SUBREGION CHECKED FOR THE MINIMA - Those are old lines
;		der2_(*,*,0) = yd2(nearint(source(0,sou))-range:nearint(source(0,sou))+range,nearint(source(1,sou))-range:nearint(source(1,sou))+range)
;		der2_(*,*,1) = x45d2(nearint(source(0,sou))-range:nearint(source(0,sou))+range,nearint(source(1,sou))-range:nearint(source(1,sou))+range)
;		der2_(*,*,2) = xd2(nearint(source(0,sou))-range:nearint(source(0,sou))+range,nearint(source(1,sou))-range:nearint(source(1,sou))+range)
;		der2_(*,*,3) = y45d2(nearint(source(0,sou))-range:nearint(source(0,sou))+range,nearint(source(1,sou))-range:nearint(source(1,sou))+range)
	
		for jj = 0,3 do begin
			
			dummy_x = source(0,sou) - (nearint(source(0,sou))-(range+2))
			dummy_y = source(1,sou) - (nearint(source(1,sou))-(range+2))

;	 		INCREASED THE SIZE OF THE SUBREGION CHECKED FOR THE MINIMA - Those are old lines
;			dummy_x = source(0,sou) - (nearint(source(0,sou))-range)
;			dummy_y = source(1,sou) - (nearint(source(1,sou))-range)

			;sou_ = sou_size(der2_(*,*,jj), source(0,sou), source(1,sou), direction=aDirection_(jj),range=range)
			sou_ = sou_size(der2_(*,*,jj), dummy_x, dummy_y, direction=aDirection_(jj),range=range)
			source_size(jj,sou) = sou_.dist

			radii(indx_(jj)) = sou_.radii(0)
			radii(indx_(jj)+1) = sou_.radii(1)
			;ell_xpoi(indx_(jj)) = sou_.ellpos(0,0)
			;ell_xpoi(indx_(jj)+1) = sou_.ellpos(1,0)
			;ell_ypoi(indx_(jj)) = sou_.ellpos(0,1)
			;ell_ypoi(indx_(jj)+1) = sou_.ellpos(1,1)
			
;	 		INCREASED THE SIZE OF THE SUBREGION CHECKED FOR THE MINIMA - Those are old lines
			;ell_xpoi(indx_(jj)) = sou_.ellpos(0,0) + nearint(source(0,sou))-range
			;ell_xpoi(indx_(jj)+1) = sou_.ellpos(1,0) + nearint(source(0,sou))-range
			;ell_ypoi(indx_(jj)) = sou_.ellpos(0,1) + nearint(source(1,sou))-range
			;ell_ypoi(indx_(jj)+1) = sou_.ellpos(1,1) +nearint(source(1,sou))-range

			ell_xpoi(indx_(jj)) = sou_.ellpos(0,0) + nearint(source(0,sou))-(range+2)
			ell_xpoi(indx_(jj)+1) = sou_.ellpos(1,0) + nearint(source(0,sou))-(range+2)
			ell_ypoi(indx_(jj)) = sou_.ellpos(0,1) + nearint(source(1,sou))-(range+2)
			ell_ypoi(indx_(jj)+1) = sou_.ellpos(1,1) +nearint(source(1,sou))-(range+2)
			flag_direction(jj) = sou_.flag
			der_values(indx_(jj)) = sou_.posvalues(0)
			der_values(indx_(jj)+1) = sou_.posvalues(1)
			
		endfor
		
;		print, ell_xpoi, ell_ypoi
; _____________________________________________________________________________		
; _____________________________________________________________________________		

;
; CREATE FILE WITH RADII IN DIFFERENT DIRECTIONS
;

		
	; ____________________________________________________
	; Check points to be not considered in ellipse fitting 
    	; ____________________________________________________

;		mom=moment(radii,sdev=sig_size)
;    		qmom=where(abs(radii-mom(0)) le 1.*sig_size)
;
;    		colpoi=strarr(8)
;    		colpoi(*)='red'
;    		colpoi(qmom)='green'
;
;    		for iii=0,n_elements(qmom)-1 do use_points(qmom(iii))=1		; Select only points whose distances do not discard excessively from source center
		
		flag_colpo = intarr(8)
		
	        dist_paragone=fltarr(8)
	        for tt=0,7,2 do begin

;          dist_paragone(tt)=((radii(tt)+radii(tt+1))/2.)
;          dist_paragone(tt+1)=dist_paragone(tt)
		
			radialdistance = [radii(tt),radii(tt+1)]
        		dist_paragone(tt)=min(radialdistance)
	 	 	check_dist = dist_paragone(tt)
	  
	 		if dist_paragone(tt) le psfpix/2. then begin
	  			indx_temp = where(radialdistance ne check_dist, nindx_temp)
				if nindx_temp ne 0 then dist_paragone(tt) = radialdistance(indx_temp(0))
	  		endif 
          		
			dist_paragone(tt+1)=dist_paragone(tt)
        	endfor
          
        	colpoi=strarr(8)
 		colpoi(*)='red'
        	for tt=0,7 do begin
          		
			if (abs(radii(tt)-dist_paragone(tt))/dist_paragone(tt) le 0.2) then begin
            			use_points(tt)=1
    				colpoi(tt)='green'
          		
			endif
        	endfor
				
		;________________________________________________________________________________________________________________
		; NEW!!! NEW!!! Check if more contiguous points are marked as not used if so replace their distance with the mean 
		;
	
		arrorder = [0,2,4,6,1,3,5,7]
		check_use_points = use_points(arrorder)		; Resort the array to have the points ordered in PA 
	
		check_use_points_ext = [use_points(7),check_use_points,use_points(0)]	; Add the other point before PA = 0 and after PA 335
		dummy_check_use_points = check_use_points_ext
	
		indx_check = where(check_use_points_ext eq 0, nremove)	; Check if there are points that should not be used
	
		if nremove gt 0 then begin
	
			for nnrem = 0, nremove-1 do begin
		
				if indx_check(nnrem) ne 0 and indx_check(nnrem) ne 9 then begin	; We do not want to check the multiple points added for continuity at PA = -45 and PA = 405
			
					if check_use_points_ext(indx_check(nnrem) - 1) eq 0 and check_use_points_ext(indx_check(nnrem) + 1) eq 0 then begin	; Check if neighbour points are active or not
				
						dummy_check_use_points(indx_check(nnrem)) = 1
				
						if indx_check(nnrem) lt 5 then begin
							
							tt = 2*(indx_check(nnrem) -1)
					;		dist_paragone(tt) = (radii(tt)+radii(tt+1)/2.)
							colpoi(tt) = 'yellow'
						endif else begin
					
							tt = 2*(indx_check(nnrem) -1 - 4) +1
							colpoi(tt) = 'yellow'
					
						endelse	
					endif
				endif	
		
			endfor
		
		endif
	
		dummy = dummy_check_use_points[1:8]
		arrorder = [0,4,1,5,2,6,3,7]
		use_points = dummy(arrorder)
		
		indxred = where(colpoi eq 'red', nindxred)
		if nindxred ne 0 then flag_colpo(indxred) = 1
		indxgreen = where(colpoi eq 'green', nindxgreen)
		if nindxgreen ne 0 then flag_colpo(indxgreen) = 0
		indxyellow = where(colpoi eq 'yellow', nindxyellow)
		if nindxyellow ne 0 then flag_colpo(indxyellow) = 2
		
		
; TEMP METHOD
;	if nremove gt 0 then begin
;		
;		indx_p = indx + 1 
;		check_indx_p = where(indx_p eq 8, ncheck)
;		if ncheck gt 0 then indx_p(where(indx_p eq -1)) = 0
;
;		indx_m = indx - 1
;		
;		check_indx_m = where(indx_m eq .1, ncheck)
;		if ncheck gt 0 then indx_m(where(indx_m eq -1)) = 7
;		
;	
;	endif

		; ____________________________________________________
		; SAVE THE POSITIONS AND THE VALUES OF THE MINIMA OF DERIVATE
		; ____________________________________________________


		strtoprint = ' '+ pad_parameter(sou+1, ' ', field_len)+ ' '+ $
		pad_parameter(der_values(0), ' ', field_len)+ ' '+ pad_parameter(der_values(2), ' ', field_len)+ ' '+pad_parameter(der_values(4), ' ', field_len)+ ' '+pad_parameter(der_values(6), ' ', field_len)+ ' '+$
		pad_parameter(der_values(1), ' ', field_len)+ ' '+ pad_parameter(der_values(3), ' ', field_len)+ ' '+pad_parameter(der_values(5), ' ', field_len)+ ' '+pad_parameter(der_values(7), ' ', field_len)+ ' '+$
		pad_parameter(flag_direction(0), ' ', field_len)+ ' '+ pad_parameter(flag_direction(1), ' ', field_len)+ ' '+pad_parameter(flag_direction(2), ' ', field_len)+ ' '+pad_parameter(flag_direction(3), ' ', field_len)+ ' '+$
		pad_parameter(flag_colpo(0), ' ', field_len)+ ' '+ pad_parameter(flag_colpo(2), ' ', field_len)+ ' '+pad_parameter(flag_colpo(4), ' ', field_len)+ ' '+pad_parameter(flag_colpo(6), ' ', field_len)+ ' '+$
		pad_parameter(flag_colpo(1), ' ', field_len)+ ' '+ pad_parameter(flag_colpo(3), ' ', field_len)+ ' '+pad_parameter(flag_colpo(5), ' ', field_len)+ ' '+pad_parameter(flag_colpo(7), ' ', field_len)+ ' '+$
		pad_parameter(ell_xpoi(0), ' ', field_len)+ ' '+ pad_parameter(ell_xpoi(2), ' ', field_len)+ ' '+pad_parameter(ell_xpoi(4), ' ', field_len)+ ' '+pad_parameter(ell_xpoi(6), ' ', field_len)+ ' '+$
		pad_parameter(ell_xpoi(1), ' ', field_len)+ ' '+ pad_parameter(ell_xpoi(3), ' ', field_len)+ ' '+pad_parameter(ell_xpoi(5), ' ', field_len)+ ' '+pad_parameter(ell_xpoi(7), ' ', field_len)+ ' '+$
		pad_parameter(ell_ypoi(0), ' ', field_len)+ ' '+ pad_parameter(ell_ypoi(2), ' ', field_len)+ ' '+pad_parameter(ell_ypoi(4), ' ', field_len)+ ' '+pad_parameter(ell_ypoi(6), ' ', field_len)+ ' '+$
		pad_parameter(ell_ypoi(1), ' ', field_len)+ ' '+ pad_parameter(ell_ypoi(3), ' ', field_len)+ ' '+pad_parameter(ell_ypoi(5), ' ', field_len)+ ' '+pad_parameter(ell_ypoi(7), ' ', field_len)+ ' '
		
    		printf,unit20, strtoprint
	
;;		timer, /STOP, /PRINT, dt
;;		print, 'Time for processing the ellipse position of a single source ', dt
;;		timer, /start
		; ____________________________________________________
		; Fit the Ellipse  
	    	; ____________________________________________________

    		ell_fac=2./1.47  
	
		;  Ellipse fitting routine returns the semi-axis
		;  Factor of 1.47 is due to the the difference between the flexus (where the second derivative has its maximum -or minimum in the way we do it here)
		;  and the width of half maximum

    		distarr = [source_size(0,sou), source_size(1,sou), source_size(2,sou), source_size(3,sou)]
    		paarr=[0.,45.,90.,135.]/180.*!pi
	
    		qdinimax=where(distarr eq max(distarr))
    		qdinimin=where(distarr eq min(distarr))
    	
		start_par=[distarr(qdinimin)/2.,distarr(qdinimax)/2.,source(0,sou),source(1,sou),paarr(qdinimax)] ; Starting parameters: Semiaxis Minor, Semiaxis Major, Center x0, center 	y0, Position Angle minor axis

		; Save the points for the ellipse fitting
	          
    		for kp1=0,7 do printf,unit1,st,strcompress(string(ell_xpoi(kp1)+1),/remove_all),',',strcompress(string(ell_ypoi(kp1)+1),/remove_all),st1,' color=',colpoi(kp1)
    		printf,unit1,'# text('+strcompress(string(source(0,sou)+1),/remove_all)+','+strcompress(string(source(1,sou)+1),/remove_all)+' text={'+strcompress(string(fix(sou)+1),/remove_all)+'}'

	;________________________________________________________________________________
	; Identify the Region of Interest to fit the ellipse from Second Derivative Image
	;________________________________________________________________________________
;		OLD METHOD	
;
;    		subim=der2(source(0,sou)-range:source(0,sou)+range,source(1,sou)-range:source(1,sou)+range)
;    		subim_mask=subim & subim_mask(*,*)=0.

    		qusepoints=where(use_points eq 1)

		; Build Mask of pixels inside the polygon identified by the 8 points of the ellipse	

;		OLD METHOD	
;
;		for i1=-range,range do begin
;      			for j1=-range,range do begin
;        			if (inside(i1+range,j1+range,ell_xpoi(qusepoints)-source(0,sou)+range,ell_ypoi(qusepoints)-source(1,sou)+range) eq 1) then subim_mask(i1+range,j1+range)=1.
;      			endfor
; 	  	endfor
;    	
;    		roi_index=where(subim_mask eq 1,nindex)
	
		XEllipse =  ell_xpoi(qusepoints) - (nearint(source(0,sou))-range)
		YEllipse =  ell_ypoi(qusepoints) - (nearint(source(1,sou))-range)
		ell_mask = maskRoIfromVertex(XEllipse, YEllipse, dims=[2*range+1, 2*range+1])
		ell_mask = ell_mask ge 1

;
; COME HERE
;stop

;		ell_roi = Obj_New('IDLanROI', ell_xpoi(qusepoints) - (nearint(source(0,sou))-range), ell_ypoi(qusepoints) - (nearint(source(1,sou))-range))
;		ell_mask = ell_roi->ComputeMask(dimensions=[2*range+1,2*range+1])
;		ell_mask = FLOAT(ell_mask)/max(ell_mask)
		
    		roi_index=where(ell_mask eq 1,nindex)
		
		; Fitting the Ellipse region with FIT_ELLIPSE procedure
;;		timer, /STOP, /PRINT, dt
;;		print, 'Time for building the ellipse mask for fitting of a single source ', dt
;;		timer, /START

    		if (nindex gt 2) then begin
		
      			fitell=test_fit_ellipse(roi_index,xsize=2.*range+1,ysize=2.*range+1,center=ellc,orientation=ello,semiaxes=ells)
;; 		timer, /STOP, /PRINT, dt
;;		print, 'Time for fitting the ellipse mask for a single source ', dt
;;		timer, /START
   	
			; this constant value has to be subtracted off the fitted ellipse SEMIaxes because with the RoI ellipse fitting the fitted ellipse is always bigger 
			; than the area delimitated by the 8 points. it seems that subtracting two pixels brings the ellipse axes (and so subtracting 1 form each semi-axis) 
			; to more reasonable values...VERY EMPIRICAL

; RIGA MODIFICATA DA SERGIO IL 19 AGOSTO 2013   		
;			ax_corr=0.5  
			ax_corr=0.

      			source_ellipse(0,sou)=(ells(0)-ax_corr)*ell_fac  
      			source_ellipse(1,sou)=(ells(1)-ax_corr)*ell_fac
      			source_ellipse(2,sou)=ello/180.*!pi   ;angle is in radians 
   	        	source_ellipse(3,sou)=1
		
		; Region Files with the ellipses fitted on the negative minima of the second derivative
		; Not the rescaled FWHM
		
			printf,unit1,'ellipse('+strcompress(string(source(0,sou)+1),/remove_all)+','+strcompress(string(source(1,sou)+1),/remove_all)+','+strcompress(string(ells(0)-ax_corr),/remove_all)+','+strcompress(string(ells(1)-ax_corr),/  remove_all)+','+strcompress(string(ello mod 360.),/remove_all)+')'
  
		; Safety Check - If the detected source is too big, more than 3 times the PSF, or too small, less than the PSF, 
		; it is assumed as guess a sligthly elongated ellipse.
	
      			if (source_ellipse(0,sou) gt 3.*psfpix or source_ellipse(1,sou) gt 3.*psfpix) then begin
        			source_ellipse(0,sou)=1.4*psfpix
; CAMBIATO DA SERGIO il 19 AGOSTO 2013
        			source_ellipse(1,sou)=1.4*psfpix
        			source_ellipse(2,sou)=0.
        			source_ellipse(3,sou)=0 ;if the estimated ellipse is greater than 3 times the PSF then the estimate is likely to be wrong so that the flag is set to 0 to leave it free in gaussian fitting
      			endif

; RIGHE MODIFICATA DA SERGIO IL 19 AGOSTO 2013   		
;SM			if (source_ellipse(0,sou) lt psfpix) then begin
			if (source_ellipse(0,sou) lt 0.95*psfpix) then begin
      			source_ellipse(0,sou)=1.4*psfpix
				source_ellipse(3,sou)=-1.	; FLAG equal at -1 in the case that the X axis is smaller than the PSF		
 	     		endif
      	
; RIGHE MODIFICATA DA SERGIO IL 19 AGOSTO 2013   		
;SM			if (source_ellipse(1,sou) lt psfpix) then begin
			if (source_ellipse(1,sou) lt 0.95*psfpix) then begin
        			source_ellipse(1,sou)=1.4*psfpix
        			source_ellipse(3,sou)=-2. 	; FLAG equal at -2 in the case that the Y axis is smaller than the PSF
      			endif
      		
		endif else begin
      		
			source_ellipse(0,sou)=1.4*psfpix
      			source_ellipse(1,sou)=1.4*psfpix
      			source_ellipse(2,sou)=0.
      			source_ellipse(3,sou)=-9.		; FLAG equal at -9 in the case that there were not enough pixels to evaluate the source size
			
      		endelse
      	   	
;		obj_destroy, ell_roi
		
	        ; create final ellipse ROIs for source mask creation to be given to PHOTOMETRY; use ellipse masks twice the FWHM to exclude also tails of sources
   		x_vert=fltarr(8) & y_vert=fltarr(8)

		if not keyword_set(fact_mask) then fact_mask = 2


   		for vert=0,7 do begin
			x_vert(vert)=source(0,sou)+(cos(source_ellipse(2,sou))*source_ellipse(0,sou)/2.*fact_mask*cos(vert*!pi/4.))-(sin(source_ellipse(2,sou))*source_ellipse(1,sou)/2.*fact_mask*sin(vert*!pi/4.))
  			y_vert(vert)=source(1,sou)+(sin(source_ellipse(2,sou))*source_ellipse(0,sou)/2.*fact_mask*cos(vert*!pi/4.))+(cos(source_ellipse(2,sou))*source_ellipse(1,sou)/2.*fact_mask*sin(vert*!pi/4.))
		endfor
		
		;dummyout = inside(xSouMask_, ySouMask_, y_vert, ypos)
		;tmp_mask = reform(dummyout, xSouMask[1], sz[2])
		
		; Building the mask for the source - If the initial image is quite large the maskRoIfromVertex will be extremely slow.
		;
		; I am going to produce the temporary mask with size (2*(range+2) + 1)
		; This is proceeding way faster!
		
		min_x_vert = max( [NEARINT(min(x_vert) - (range + 2)), 0]) 
		max_x_vert = min( [NEARINT(max(x_vert) + (range + 2)), szSouMask[1]-1])

		min_y_vert = max( [NEARINT(min(y_vert) - (range + 2)), 0])	
		max_y_vert = min( [NEARINT(max(y_vert) + (range + 2)),  szSouMask[2]-1])	
		
		dummyszSouMask1 =  (max_x_vert - min_x_vert) + 1
		dummyszSouMask2 =  (max_y_vert - min_y_vert) + 1
		
		dummy_x_vert = x_vert - min_x_vert 
		dummy_y_vert = y_vert - min_y_vert
		
		tmp_mask = maskRoIfromVertex(dummy_x_vert, dummy_y_vert, dims=[dummyszSouMask1, dummyszSouMask2])
		;tmp_mask = maskRoIfromVertex(x_vert, y_vert, dims=[szSouMask[1], szSouMask[2]])
		tmp_mask = (tmp_mask ge 1)*(sou+1)
		
		;source_mask=source_mask+tmp_mask
		source_mask[min_x_vert:max_x_vert, min_y_vert:max_y_vert] = source_mask[min_x_vert:max_x_vert, min_y_vert:max_y_vert]+tmp_mask
		qsum=where(source_mask gt sou+1,nqsum)	
		if (nqsum gt 0) then source_mask(qsum)=sou+1		; APPROXIMATION: PIXELS IN COMMON BETWEEN TWO SOURCES WILL BE CONSIDERED TO BELONG TO ONLY ONE OF THE TWO SOURCES (the one with highest ID) 
				
		
;;		timer, /STOP, /PRINT, dt
;;		print, 'Time for building the source mask of a single source ', dt
;;		timer, /START


		;;;

		;obj_destroy,roi
;;;;;;;;;;;;;;;		
      			
	endif else begin
      		
		print,'Source ',sou,' is too close to image border...'
      		source_size(*,sou)=0
    	
	endelse
	
	borderflag(sou) = bordersource

	belong = where(source(11,*) eq source(11,sou),nbelong)
	if nbelong gt 1 then clumpflag(sou) = source(11,sou)
	guessflag(sou) = n_elements(qusepoints)
	
		
;;	timer, /STOP, /PRINT, dt
;;	print, 'Time for building other housekeeping of a single source ', dt
;;	timer, /START

	
endfor

outellipse ='sources_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'_ellipse.dat'

if keyword_set(TIME) then begin

	print, "Estimating Time for estimate Sources Sizes"
	timer, /STOP, /PRINT, dt
	timer, /START

endif

close,unit1
close,unit20
free_lun,unit1
free_lun,unit20

; writes the sources mask image form the ellipse RoI
writefits,outputDir+strmid(a1,0,strpos(a1,'.fits'))+'_sourcesmask.fits',source_mask,ha1

; Build up of strings of RA and DEC in sexagesimal units (hh:mm:ss) (dd:mm:ss).

ra=dblarr(n_elements(source(0,*)))
dec=dblarr(n_elements(source(0,*)))

ras=strarr(n_elements(ra))
decs=strarr(n_elements(ra))


for jj=0L,n_elements(source(0,*))-1 do begin
  	
	xyad,ha1,reform(source(0,jj)),reform(source(1,jj)),ra0,dec0,/celestial
  	ra(jj)=ra0
  	dec(jj)=dec0
	dummy_ra = sixty(ra0/15.)
	dummy_dec = sixty(dec0)
	
	dummy_hour = string(dummy_ra(0),format='(I2)')
	if dummy_dec(0) lt 0 then signstring= '-' else signstring = ' '
	dummy_degree = signstring+string(ABS(dummy_dec(0)),format='(I2)')
	if dummy_ra(1) lt 1. then dummy_min = '00' else if dummy_ra(1) lt 10. then dummy_min = '0'+strcompress(string(dummy_ra(1),format='(I1)'),/REMOVE_ALL) else dummy_min = strcompress(string(dummy_ra(1),format='(I2)'),/REMOVE_ALL)
	dummy_ra(2) = nearint(dummy_ra(2)*100.)/100.
	if dummy_ra(2) lt 1. then dummy_sec = '0'+strcompress(string(dummy_ra(2),format='(F4.2)'),/REMOVE_ALL) else if dummy_ra(2) lt 10. then dummy_sec = '0'+strcompress(string(dummy_ra(2),format='(F4.2)'),/REMOVE_ALL) else dummy_sec = strcompress(string(dummy_ra(2),format='(F5.2)'),/REMOVE_ALL)
	
	if dummy_dec(1) lt 1. then dummy_amin = '00' else if dummy_dec(1) lt 10. then dummy_amin = '0'+strcompress(string(dummy_dec(1),format='(I1)'),/REMOVE_ALL) else dummy_amin = strcompress(string(dummy_dec(1),format='(I2)'),/REMOVE_ALL)
	dummy_dec(2) = nearint(dummy_dec(2)*100.)/100.
	if dummy_dec(2) lt 1. then dummy_asec = '0'+strcompress(string(dummy_dec(2),format='(F4.2)'),/REMOVE_ALL) else if dummy_dec(2) lt 10. then dummy_asec = '0'+strcompress(string(dummy_dec(2),format='(F4.2)'),/REMOVE_ALL) else dummy_asec = strcompress(string(dummy_dec(2),format='(F5.2)'),/REMOVE_ALL)

	
	ras(jj) = strcompress(dummy_hour+':'+dummy_min+':'+dummy_sec,/REMOVE_ALL)
	decs(jj) = strcompress(dummy_degree+':'+dummy_amin+':'+dummy_asec,/REMOVE_ALL)
	
endfor


; __________________________________________
; Save Detection Files
; __________________________________________

print,' '
print,'Writes output file (detection catalogue)'

outfile='sources_'+stsig+'_'+strmid(a1,0,strpos(a1,'.fits'))+'.dat'


;_________________________________________________________

launchstring = "redo = detection('"+string(inputDir)+"',"+"'"+string(a1)+"',"+string(nsigma)+",'"+string(outputDir)+"',"+"'"+string(outfile)+"'"

if keyword_set(smoothing) then launchstring = launchstring+', smoothing='+string(smoothing)
if keyword_set(npixmask) then launchstring = launchstring+', npixmask='+string(npixmask)
if keyword_set(psfpix) then launchstring = launchstring+', psfpix='+string(psfpix)
if keyword_set(derivate) then launchstring = launchstring+', derivate='+string(derivate)
if keyword_set(no_overwrite) then launchstring = launchstring+', no_overwrite='+string(no_overwrite)
if keyword_set(allow_single) then launchstring = launchstring+', allow_single='+string(allow_single)
if keyword_set(all_neighbours) then launchstring = launchstring+', all_neighbours='+string(all_neighbours)
if keyword_set(thresh) then launchstring = launchstring+', thresh='+string(thresh)
if keyword_set(range) then launchstring = launchstring+', range='+string(range)
if keyword_set(super_resolution) then launchstring = launchstring+', super_resolution='+string(super_resolution)
if keyword_set(abscurv) then launchstring = launchstring+', abscurv='+string(abscurv)
if keyword_set(local_thresh) then launchstring = launchstring+', local_thresh='+string(local_thresh)
if keyword_set(fact_mask) then launchstring = launchstring+', fact_mask='+string(fact_mask)
if keyword_set(checkhits) then launchstring = launchstring+', checkhits='+string(checkhits)
if keyword_set(quiet) then launchstring = launchstring+', quiet='+string(quiet)
if keyword_set(TIME) then launchstring = launchstring+', TIME='+string(TIME)

launchstring = strcompress(launchstring+')',/REMOVE_ALL)

;_________________________________________________________


openw,unit,outputDir+'/'+outfile,/get_lun, width=1000

printf,unit,'\ # File produced by detection.pro - New Version 0.99! IPAC TABLE ALSO FOR THE DETECTION OUTPUT'
printf,unit,'\ # field :  '+a1
printf,unit,'\ # String used to launch detection '

if strlen(launchstring) lt 500 then printf,unit,'\ '+launchstring else begin
	
	splitstr = strsplit(launchstring,',', /EXTRACT)
	npieces = n_elements(splitstr)

	startstr = '\'
	for p = 0, FLOOR(npieces/3) do startstr = startstr+' '+ splitstr(p)+','
	printf, unit, startstr+'$'
	
	startstr = '\'
	for p = FLOOR(npieces/3) + 1, FLOOR(2*npieces/3)  do startstr = startstr+' '+ splitstr(p)+','
	printf, unit, startstr+'$'
	
	startstr = '\'
	for p = FLOOR(2*npieces/3) + 1, npieces-1  do startstr = startstr+' '+ splitstr(p)+','
	printf, unit, strmid(startstr, 0, strpos(startstr, ',', /REVERSE_SEARCH))

endelse		

printf,unit,'\ # Coordinates of detected sources'


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

for kp = 0L, n_elements(source(0,*))-1 do begin
	
	printstr = ' '+ pad_parameter(kp+1, ' ', field_len)+ ' '+ pad_parameter(source(0,kp)+1, ' ', field_len)+ ' '+ pad_parameter(source(1,kp)+1, ' ', field_len)+$
		' '+ pad_parameter(source_ellipse(0,kp), ' ', field_len)+ ' '+ pad_parameter(source_ellipse(1,kp), ' ', field_len)+ $
		' '+ pad_parameter(source_ellipse(2,kp), ' ', field_len)+ ' '+ pad_parameter(Strtrim(string(source_ellipse(3,kp),format='(I3.1)'),2), ' ', field_len)+ $
		' '+ pad_parameter(ra(kp), ' ', field_len)+ ' '+ pad_parameter(dec(kp), ' ', field_len)+ $
		' '+ pad_parameter(ras(kp), ' ', field_len)+ ' '+ pad_parameter(decs(kp), ' ', field_len)+ $
		' '+ pad_parameter(borderflag(kp), ' ', field_len)+ ' '+ pad_parameter(clumpflag(kp), ' ', field_len)+ ' '+ pad_parameter(strtrim(string(source(10,kp),format='(F9.2)'),2), ' ', field_len)+ ' '+ pad_parameter(guessflag(kp), ' ', field_len)+' '+ pad_parameter(strtrim(string(source(12,kp),format='(I3.1)'),2), ' ', field_len)+$
		' '+ pad_parameter(source(2,kp), ' ', field_len)+ ' '+ pad_parameter(source(3,kp), ' ', field_len)+$
		' '+ pad_parameter(source(4,kp), ' ', field_len)+ ' '+ pad_parameter(source(5,kp), ' ', field_len)+$
		' '+ pad_parameter(source(6,kp), ' ', field_len)+ ' '+ pad_parameter(source(7,kp), ' ', field_len)+$
		' '+ pad_parameter(source(8,kp), ' ', field_len)+ ' '+ pad_parameter(source(9,kp), ' ', field_len)
		
	printf, unit, printstr

endfor

;printf,unit,'# sourceID      X      Y     Xaxis    Yaxis    PA   FitFlag   RA        DEC           RA            DEC   ';       Back       Rms  '
;printf,unit,'#              pxl    pxl   pxl     pxl     Rad            Deg        Deg         h:m:s          d:m:s  ';     MJy/sr    MJy/sr'
;printf,unit,'# ============================================================================';======================='
;for kp=0L,n_elements(source(0,*))-1 do printf,unit,format='(2x,i5,2(2x,f10.3),1x,4(2x,f6.1),3x,f10.6,2x,f10.6,2x,a12,2x,a12,2x,f7.2,2x,f7.2)',
;kp+1,source(0,kp)+1,source(1,kp)+1,source_ellipse(0,kp),source_ellipse(1,kp),source_ellipse(2,kp),source_ellipse(3,kp),ra(kp),dec(kp),ras(kp),decs(kp)

close,unit
free_lun,unit


; __________________________________________
; Save Region files for detected sources
; __________________________________________

if unitcoord eq 'fk5' then begin

	coord = [transpose(ras),transpose(decs)]

endif else begin
	
	if unitcoord eq 'galactic' then begin
		  
		  lll=dblarr(n_elements(source(0,*)))
		  bbb=dblarr(n_elements(source(1,*)))
	
		  for jj = 0L,n_elements(source(0,*))-1 do begin
		  	
			xyad,ha1,reform(source(0,jj)),reform(source(1,jj)),l0,b0,/galactic
			lll(jj)=l0
			bbb(jj)=b0
	
		  endfor
		  
  		  if n_elements(lll) gt 1 then begin 
		   
			  coord = [transpose(STRCOMPRESS(string(lll, FORMAT='(F15.6)'),/REMOVE_ALL)), transpose(STRCOMPRESS(string(bbb, FORMAT='(F15.6)'),/REMOVE_ALL))]
		   
		  endif else  coord = [STRCOMPRESS(string(lll, FORMAT='(F15.6)'),/REMOVE_ALL),STRCOMPRESS(string(bbb, FORMAT='(F15.6)'),/REMOVE_ALL)]

	endif

endelse

save_ = save_reg(source, source_ellipse, 'blue', a1, outputDir, stsig, nanflag=source(12,*))
save_ = save_reg(source, source_ellipse, 'red', a1, outputDir, stsig, coord_system=coord,unitcoord=unitcoord, nanflag=source(12,*))

print,n_elements(source(0,*)),' sources found'
print,' '

if keyword_Set(TIME) then begin
	
	print, "Estimating Time for Saving files"
	timer, /STOP, /PRINT, dt

endif
;stop
return,n_elements(source(0,*))

end

