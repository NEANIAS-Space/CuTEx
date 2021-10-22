function im_deriv, a, outdir, a1, ha1, derivate=derivate, smoothing=smoothing, local_threshold=local_threshold

COMMON DERIV, xd2, yd2, x45d2, y45d2
COMMON DERIVTHRESH, xd2m, yd2m, x45d2m, y45d2m
COMMON NANMAP, nanmask, nanmasksurround, xd2nan, yd2nan, x45d2nan, y45d2nan, nanpixels

;print, "LOCAL THRESHOLD", local_threshold
print, "Computing the image derivates"
border = 2L

sz = size(a)
n1 = sz(2)
n2 = sz(1)

n1=n_elements(a(0,*))
n2=n_elements(a(*,0))

xd1=fltarr(n2,n1)
xd2=fltarr(n2,n1)
yd1=fltarr(n2,n1)
yd2=fltarr(n2,n1)
xd2nan=fltarr(n2,n1)
yd2nan=fltarr(n2,n1)
x45d2nan=fltarr(n2,n1)
y45d2nan=fltarr(n2,n1)

; Compute the first and second derivates

print, 'Calculating 1st and 2nd derivatives along x and y'

if NOT KEYWORD_set(derivate) then begin
	
	print, 'Using 5-points Derivate '
	
	for i=0L,n2-1 do begin
  		xd1(i,*)=deriv_5(a(i,*))
  		xd2(i,*)=deriv_5(xd1(i,*))
	endfor

	for i=0L,n1-1 do begin
  		yd1(*,i)=deriv_5(a(*,i))
  		yd2(*,i)=deriv_5(yd1(*,i))
	endfor
	
endif else begin
	
	print, 'Using 3-points Derivate '
	
	for i=0L,n2-1 do begin
  		xd1(i,*)=deriv(a(i,*))
  		xd2(i,*)=deriv(xd1(i,*))
	endfor

	for i=0L,n1-1 do begin
  		yd1(*,i)=deriv(a(*,i))
  		yd2(*,i)=deriv(yd1(*,i))
	endfor
endelse

der2=fltarr(n2,n1)
print, 'Calculating 1st and 2nd derivatives along diagonals'

if NOT KEYWORD_set(derivate) then begin
	print, 'Using 5-points Derivate '
	deriv_45,a,x45d2,y45d2
endif else begin
	print, 'Using 3-points Derivate '
	deriv_45,a,x45d2,y45d2, derivate=3
endelse
	
; Invert derivatives to have them positive

xd2=-1.*xd2
yd2=-1.*yd2
x45d2=-1.*x45d2
y45d2=-1.*y45d2

der2=(xd2+yd2+x45d2+y45d2)/4.
sdev = stdev(der2)

print, 'Writing derivatives fits files '

hamask = ha1
if keyword_set(smoothing) then sxaddpar, ha1, 'SMOOTH', smoothing
sxaddpar,hamask,'RMS', sdev

writefits,outdir+'/'+'allder2_'+a1,der2,hamask
writefits,outdir+'/'+'der1x_'+a1,xd1,ha1
writefits,outdir+'/'+'der1y_'+a1,yd1,ha1
writefits,outdir+'/'+'der2x_'+a1,xd2,ha1
writefits,outdir+'/'+'der2y_'+a1,yd2,ha1
writefits,outdir+'/'+'der2x45_'+a1,x45d2,ha1
writefits,outdir+'/'+'der2y45_'+a1,y45d2,ha1

if keyword_set(local_threshold) then begin
	
	print, 'You have chosen to compute local thresholds - Starting computation'
	
	if LOCAL_THRESHOLD lt 10 then begin
		window = 30
		print, 'Adopting default size to evaluate the local threshold'
	endif else begin
		window = local_threshold
	endelse
	
	zero = 1
	sxaddpar, ha1, 'WINDOW', window
	sxaddpar, ha1, 'ZERO', zero
	print, 'Computing the local threshold in direction Y'
	sigma_median = make_sigma(xd2, window=window)	
	writefits,outdir+'/'+'der2x_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',sigma_median.medianframe,ha1
	
	print, 'Computing the local threshold in direction X'
	sigma_median = make_sigma(yd2, window=window)	
	writefits,outdir+'/'+'der2y_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',sigma_median.medianframe,ha1

	print, 'Computing the local threshold in direction - 45 '
	sigma_median = make_sigma(x45d2, window=window)	
	writefits,outdir+'/'+'der2x45_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',sigma_median.medianframe,ha1
	
	print, 'Computing the local threshold in direction + 45'
	sigma_median = make_sigma(y45d2, window=window)	
	writefits,outdir+'/'+'der2y45_'+strmid(a1,0,strpos(a1,'.fits'))+'_median.fits',sigma_median.medianframe,ha1
	
	print, 'Computation of local threshold complete'
	
endif

; SEARCHING PIXELS WHERE THERE IS A SIGNAL BUT DUE TO A NEARBY NAN 
; THE DERIVATIVE INFORMATION IN ONE DIRECTION IS LOST

if N_ELEMENTS(NANMASK) gt 1 then begin	

		nanmasxx = where(FINITE(xd2) eq 0 and NANMASK ge 0, nnanmasxx)
		nanmasyy = where(FINITE(yd2) eq 0 and NANMASK ge 0, nnanmasyy)
		nanmasxy = where(FINITE(x45d2) eq 0 and NANMASK ge 0, nnanmasxy)
		nanmasyx = where(FINITE(y45d2) eq 0 and NANMASK ge 0, nnanmasyx)


	if (nnanmasxx gt 0) then xd2nan(nanmasxx) = 1. 
	if (nnanmasyy gt 0) then yd2nan(nanmasyy) = 1. 
	if (nnanmasxy gt 0) then x45d2nan(nanmasxy) = 1.
	if (nnanmasyx gt 0) then y45d2nan(nanmasyx) = 1.

; REMOVING FROM THOSE PIXELS THE ONES THAT ARE ALREADY NAN
  
	if n_elements(nanpixels) gt 1 then begin

		xd2nan(nanpixels(*,0), nanpixels(*,1))  = 0.
		yd2nan(nanpixels(*,0), nanpixels(*,1))  = 0.
		x45d2nan(nanpixels(*,0), nanpixels(*,1))  = 0.
		y45d2nan(nanpixels(*,0), nanpixels(*,1))  = 0.

	endif	

	nanmasksurround = 4 - ( xd2nan + yd2nan + x45d2nan + y45d2nan)
	nanmasksurround(Where(nanmasksurround eq 4)) = 0.
endif else nanmasksurround = 0.

return, 0

END





