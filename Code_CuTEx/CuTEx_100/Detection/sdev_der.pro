function sdev_der, a, border=border, clip=clip, SILENT=SILENT

n1=n_elements(a(0,*))
n2=n_elements(a(*,0))

if keyword_SET(BORDER) then border = border else border = 2L
if keyword_set(clip) then clip=clip else clip = 3.

; the RMS on the derivative image should exclude the border pixels

pixels=a(0+border:n2-1-border,0+border:n1-1-border)
qgood=where(finite(pixels) and pixels ne 0,nqgood)

if (nqgood gt 0) then begin
	stat=moment(pixels(qgood),sdev=sdev)
  	qsel=where(abs(pixels) lt clip*sdev,nqsel)
  	if (nqsel gt 0) then begin
    		stat=moment(pixels(qsel),sdev=sdev) 

    		if NOT keyword_SET(SILENT) then begin
			print,"-------------------------------------"
	  		print,"Moments of the image",  stat
	  		print,"Standard deviation",sdev
	  		print,"-------------------------------------"
    		endif
  	endif else sdev=-1
endif else sdev=-1

return, sdev

end
