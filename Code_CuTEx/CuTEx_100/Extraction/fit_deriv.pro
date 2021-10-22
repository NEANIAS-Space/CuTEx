function fit_deriv, a, derivate=derivate

;print, "Computing the image derivates"
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

;print, 'Calculating 1st and 2nd derivatives along x and y'

if NOT KEYWORD_set(derivate) then begin
	
	;print, 'Using 5-points Derivate '
	
	for i=0L,n2-1 do begin
  		xd1(i,*)=deriv_5(a(i,*))
  		xd2(i,*)=deriv_5(xd1(i,*))
	endfor

	for i=0L,n1-1 do begin
  		yd1(*,i)=deriv_5(a(*,i))
  		yd2(*,i)=deriv_5(yd1(*,i))
	endfor
	
endif else begin
	
	;print, 'Using 3-points Derivate '
	
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
;print, 'Calculating 1st and 2nd derivatives along diagonals'

if NOT KEYWORD_set(derivate) then begin
	;print, 'Using 5-points Derivate '
	deriv_45,a,x45d2,y45d2
endif else begin
		;print, 'Using 3-points Derivate '
	deriv_45,a,x45d2,y45d2, derivate=3
endelse
	
; Invert derivatives to have them positive

xd2=-1.*xd2
yd2=-1.*yd2
x45d2=-1.*x45d2
y45d2=-1.*y45d2

der2=(xd2+yd2+x45d2+y45d2)/4.
sdev = stdev(der2)

return, {xd2:xd2, yd2:yd2, x45d2:x45d2, y45d2:y45d2}

END





