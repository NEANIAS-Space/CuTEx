function im_threshold, a, nsigma, abscurv=abscurv

COMMON DERIV, xd2, yd2, x45d2, y45d2
COMMON NANMAP, nanmask, nanmasksurround, xd2nan, yd2nan, x45dnan, y45dnan, nanpixels

border = 2L

if NOT keyword_set(ABSCURV) then begin 

	print, 'Calculating statistics on 2nd derivative image(s)'

	n1=n_elements(a(0,*))
	n2=n_elements(a(*,0))

	; the RMS on the derivative image should exclude the border pixels
	
	sdev =  sdev_der(a,border=border, clip=3)
	print,"-------------------------------------"
	thresh=sdev*nsigma
	print,"Threshold Limit", thresh
	print,"-------------------------------------"

endif else begin

	n1=n_elements(a(0,*))
	n2=n_elements(a(*,0))

	thresh=nsigma
	print,"-------------------------------------"
	print,"Absolute Curvate Threshold set"
	print,"Threshold Limit", thresh
	print,"-------------------------------------"

endelse

nsigma=float(nsigma)
stsig=pad_parameter(nsigma,'0',5)

mas=a
masnan=a
mas(*,*)=0
der2=(xd2+yd2+x45d2+y45d2)/4.

masxx=mas
masyy=mas
masxy=mas
masyx=mas

qmasxx=where(xd2 gt thresh,nqmasxx)
qmasyy=where(yd2 gt thresh,nqmasyy)
qmasxy=where(x45d2 gt thresh,nqmasxy)
qmasyx=where(y45d2 gt thresh,nqmasyx)

if (nqmasxx gt 0) then masxx(qmasxx) = 10.
if (nqmasyy gt 0) then masyy(qmasyy) = 10.
if (nqmasxy gt 0) then masxy(qmasxy) = 10.
if (nqmasyx gt 0) then masyx(qmasyx) = 10.


;mas=masxx+masyy+masxy+masyx
mas=masxx*masyy*masxy*masyx

dummy_mas=mas
mas(*)=0.
mas(0+border:n2-1-border,0+border:n1-1-border)=dummy_mas(0+border:n2-1-border,0+border:n1-1-border)

if n_elements(nanmasksurround) gt 1 then begin

	tempnanmasksurround = nanmasksurround/nanmasksurround
	tempnanmasksurround(where(FINITE(tempnanmasksurround) eq 0)) = 0.

	masnan =(masxx+masyy+masxy+masyx)/(nanmasksurround +1) ;* tempnanmasksurround
	masnan(where(masnan ge 10 or masnan lt 6.6)) = 0.
	masnan(where(masnan ne 0)) = 10000.


	dummy_mas=masnan
	masnan(*)=0.
 
	masnan(0+border:n2-1-border,0+border:n1-1-border)=dummy_mas(0+border:n2-1-border,0+border:n1-1-border)

	masnan = mas+masnan

endif else masnan = 0.

return, {mask:mas, thresh:thresh, masknan:masnan}

END
