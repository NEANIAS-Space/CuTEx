function make_gauss, im1, f0, x0, y0, x_sigma, y_sigma, pa

f=im1
f(*,*) = 0.

for i=0,n_elements(im1(*,0))-1 do begin
  	for j=0,n_elements(im1(0,*))-1 do begin
				

			  	f(i,j) = f(i,j) + f0*exp(-0.5*(((((i-x0)*cos(pa)+(j-y0)*sin(pa))/x_sigma)^2.)+((((j-y0)*cos(pa)-(i-x0)*sin(pa))/y_sigma)^2.)))


	endfor
endfor

return, f

end
	
