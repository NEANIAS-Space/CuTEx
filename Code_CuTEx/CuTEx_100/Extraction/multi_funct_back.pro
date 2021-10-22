function multi_funct_back,x,y,params, ngauss=ngauss

;if keyword_set(ngauss) then print, ngauss
;help, params

; Fit of multiple gaussian with following functional form
;
; f(x,y) = a0 + b0*x + c0*y + d0*x*y + e0*x^2 + f0*y^2 +  Sum (i = 0, ngaussiane) A_i * exp ( -1/2*((( x-x_i)*cos(th_i) - (y-y_i)*sen(th_i))^2/sigm_x_i^2 + (( y-y_i)*cos(th_i) + (x-x_i)*sen(th_i))^2/sigm_y_i^2))
;
; params is the vector inside which there are all the fitting paramaters
;
; Legend of the vector params
; ----------------------------------------------------------------------------
; Background fitted by a plane with orientation in the space a0 + b0*x + c0*y 	
; params(0) = a0
; params(1) = b0
; params(2) = c0
; params(3) = d0
; params(4) = e0
; params(5) = f0
; ----------------------------------------------------------------------------
; Single Gaussian parameters
; params(6) = A0    - Single gaussian Intensity
; params(7) = x0    - X-position of the guessed single gaussian center 	
; params(8) = y0    - Y-position of the guessed single gaussian center 	
; params(9) = sigma_x0 - Width along the X-axis of the guessed single gaussian
; params(10) = sigma_y0 - Width along the Y-axis of the guessed single gaussian
; params(11) = theta0 - Position angle of projected ellipse 
; ----------------------------------------------------------------------------
; Multiple Gaussians parameters   - i-th gaussian
; params(6)  -->    params(0 + 6*i)   f.e. 2nd gaussian params(12), 3rd gaussian params(18)
; params(7)  -->    params(1 + 6*i)   f.e. 2nd gaussian params(13), 3rd gaussian params(19)
; params(8)  -->    params(2 + 6*i)   f.e. 2nd gaussian params(14), 3rd gaussian params(20)
; params(9)  -->    params(3 + 6*i)   f.e. 2nd gaussian params(15), 3rd gaussian params(21)
; params(10)  -->    params(4 + 6*i)   f.e. 2nd gaussian params(16), 3rd gaussian params(22)
; params(11)  -->    params(5 + 6*i)   f.e. 2nd gaussian params(17), 3rd gaussian params(23)
; ----------------------------------------------------------------------------
; 

f=x

for i=0,n_elements(x(*,0))-1 do begin
  	for j=0,n_elements(y(0,*))-1 do begin
		
		f(i,j) = params(0)+ i*params(1) + j*params(2) + params(3)*i*j + params(4)*i^2 + params(5)*j^2
		
		for ng= 1,ngauss do begin
			
				f(i,j) = f(i,j) + params(0+ 6*ng)*exp(-0.5*(((((i-params(1+6*ng))*cos(params(5 + 6*ng))+(j-params(2 + 6*ng))*sin(params(5 + 6*ng)))/params(3 + 6*ng))^2.)+((((j-params(2 + 6*ng))*cos(params(5 + 6*ng))-(i-params(1 + 6*ng))*sin(params(5 + 6*ng)))/params(4 + 6*ng))^2.)))

		endfor
	endfor
endfor

return,f
end
			
			
