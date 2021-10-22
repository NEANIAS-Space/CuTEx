function mpfit_plane_points,im , rms1 = rms1, quiet=quiet, higher_order=higher_order

sz = size(im)

im1 = im
yfit = im1
yfit(*,*) = 0.

x=im1 & for k=0,n_elements(im1(*,0))-1 do x(k,*)=float(k)
y=im1 & for k=0,n_elements(im1(0,*))-1 do y(*,k)=float(k)

indxfin = where(FINITE(im1) eq 1, ncntfin)

xx = x*finite(im1)
yy = y*finite(im1)


dataplane = dblarr(3, ncntfin)

for l=0L,ncntfin-1 do begin 
	
	dataplane(0,l) = xx(indxfin(l))
	dataplane(1,l) = yy(indxfin(l))
	dataplane(2,l) = im(indxfin(l))
endfor


if NOT KEYWORD_SET(HIGHER_ORDER) then begin
  
	result = sfit(dataplane, 1, /IRREGULAR,KX=coeff, /MAX_DEGREE)	

	par=replicate({back:0.d0, backx:0.d0, backy:0.d0, chi2:0.d0, plane:fltarr(n_elements(im(*,0)),n_elements(im(0,*)))},1)
	
  	par(0).back=coeff(0)
  	par(0).backx=coeff(2)
  	par(0).backy=coeff(1)

	for i = 0, sz(1)-1 do begin
		for j = 0, sz(2)-1 do begin
		
			yfit(i,j) = coeff(0) + coeff(2)*x(i,j) + coeff(1)*y(i,j)
		endfor
	endfor
endif else begin

	result = sfit(dataplane, 2, /IRREGULAR,KX=coeff, /MAX_DEGREE)	
	
	par=replicate({back:0.d0, backx:0.d0, backy:0.d0, backxy:0.d0, backx2:0.d0, backy2:0.d0,  chi2:0.d0, plane:fltarr(n_elements(im(*,0)),n_elements(im(0,*)))},1)

  	par(0).back=coeff(0)
  	par(0).backx=coeff(3)
  	par(0).backy=coeff(1)
  	par(0).backxy=coeff(4)
  	par(0).backx2=coeff(5)
  	par(0).backy2=coeff(2)

	for i = 0, sz(1)-1 do begin
		for j = 0, sz(2)-1 do begin
		
			yfit(i,j) = coeff(0) + coeff(3)*x(i,j) + coeff(1)*y(i,j)+ coeff(5)*x(i,j)^2 + coeff(4)*x(i,j)*y(i,j) + coeff(2)*y(i,j)^2
		endfor
	endfor

endelse

chi = 0.
for k = 0L, ncntfin -1 do chi = chi + (im1(indxfin(k)) - yfit(indxfin(k)))^2

par(0).chi2 = chi
	
par(0).plane = yfit	


;  surface, im1,zrange=[min(im1),max(im1)]
;  surface, yfit, /NOERASE,zrange=[min(im1),max(im1)]
resi_fit = im1 - yfit
stats = moment(resi_fit(where(FINITE(resi_fit) eq 1)), sdev=sdev)


if NOT keyword_set(quiet)  then  print, "R.M.S. residuals = ", sdev

  
return,par

end
