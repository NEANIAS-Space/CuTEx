function im_local_thresh, a, nsigma

COMMON DERIV, xd2, yd2, x45d2, y45d2
COMMON DERIVTHRESH, xd2m, yd2m, x45d2m, y45d2m
COMMON NANMAP, nanmask, nanmasksurround, xd2nan, yd2nan, x45dnan, y45dnan, nanpixels

border = 2L
n1=n_elements(a(0,*))
n2=n_elements(a(*,0))

thresh = nsigma

nsigma=float(nsigma)
stsig=pad_parameter(nsigma,'0',5)

mas=a
mas(*,*)=0
der2=(xd2+yd2+x45d2+y45d2)/4.

masxx=mas
masyy=mas
masxy=mas
masyx=mas


fract = xd2/(xd2m*nsigma)
pixdet = where(fract gt 1, npixdet)
if npixdet gt 0 then masxx(pixdet) = 10.

fract = yd2/(yd2m*nsigma)
pixdet = where(fract gt 1, npixdet)
if npixdet gt 0 then masyy(pixdet) = 10.

fract = x45d2/(x45d2m*nsigma)
pixdet = where(fract gt 1, npixdet)
if npixdet gt 0 then masxy(pixdet) = 10.

fract = y45d2/(y45d2m*nsigma)
pixdet = where(fract gt 1, npixdet)
if npixdet gt 0 then masyx(pixdet) = 10.

;mas=masxx+masyy+masxy+masyx
mas=masxx*masyy*masxy*masyx
dummy_mas=mas
mas(*)=0.
mas(0+border:n2-1-border,0+border:n1-1-border)=dummy_mas(0+border:n2-1-border,0+border:n1-1-border)


if n_elements(nanmasksurround) gt 1 then begin


	tempnanmasksurround = nanmasksurround/nanmasksurround
	tempnanmasksurround(where(FINITE(tempnanmasksurround) eq 0)) = 0.

	masnan =(masxx+masyy+masxy+masyx)/(nanmasksurround +1);* tempnanmasksurround

	masnan(where(masnan ge 10 or masnan lt 6.6)) = 0.
	
	bcheck = where(masnan ne 0, nbcheck)

	if nbcheck gt 0 then begin 
	
		masnan(where(masnan ne 0)) = 10000.

		dummy_mas=masnan
		masnan(*)=0.
 
		masnan(0+border:n2-1-border,0+border:n1-1-border)=dummy_mas(0+border:n2-1-border,0+border:n1-1-border)

		masnan = mas+masnan

	endif else begin
		
		masnan = 0.
		nanmasksurround = 0
	endelse	
		
endif else masnan = 0.

return, {mask:mas, thresh:thresh, masknan:masnan}

END
