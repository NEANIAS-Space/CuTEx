function multi_funct,x,y,params, ngauss=ngauss

;if keyword_set(ngauss) then print, ngauss
;help, params

; Fit of multiple gaussian with following functional form
;
; f(x,y) = a0 + b0*x + c0*y +  Sum (i = 0, ngaussiane) A_i * exp ( -1/2*((( x-x_i)*cos(th_i) - (y-y_i)*sen(th_i))^2/sigm_x_i^2 + (( y-y_i)*cos(th_i) + (x-x_i)*sen(th_i))^2/sigm_y_i^2))
;
; params is the vector inside which there are all the fitting paramaters
;
; Legend of the vector params
; ----------------------------------------------------------------------------
; Background fitted by a plane with orientation in the space a0 + b0*x + c0*y 	
; params(0) = a0
; params(1) = b0
; params(2) = c0
; ----------------------------------------------------------------------------
; Single Gaussian parameters
; params(3) = A0    - Single gaussian Intensity
; params(4) = x0    - X-position of the guessed single gaussian center 	
; params(5) = y0    - Y-position of the guessed single gaussian center 	
; params(6) = sigma_x0 - Width along the X-axis of the guessed single gaussian
; params(7) = sigma_y0 - Width along the Y-axis of the guessed single gaussian
; params(8) = theta0 - Position angle of projected ellipse (should be oriented in anticlockwise from x-axis of image
; ----------------------------------------------------------------------------
; Multiple Gaussians parameters   - i-th gaussian
; params(3)  -->    params(0 + (3 + 6*i))   f.e. 2nd gaussian params(9), 3rd gaussian params(15)
; params(4)  -->    params(1 + (3 + 6*i))   f.e. 2nd gaussian params(10), 3rd gaussian params(16)
; params(5)  -->    params(2 + (3 + 6*i))   f.e. 2nd gaussian params(11), 3rd gaussian params(17)
; params(6)  -->    params(3 + (3 + 6*i))   f.e. 2nd gaussian params(12), 3rd gaussian params(18)
; params(7)  -->    params(4 + (3 + 6*i))   f.e. 2nd gaussian params(13), 3rd gaussian params(19)
; params(8)  -->    params(5 + (3 + 6*i))   f.e. 2nd gaussian params(14), 3rd gaussian params(20)
; ----------------------------------------------------------------------------
; 

f=x

for i=0,n_elements(x(*,0))-1 do begin
  	for j=0,n_elements(y(0,*))-1 do begin
		
		f(i,j) = params(0)+ i*params(1) + j*params(2)
		
		for ng= 0,ngauss-1 do begin
			
			if (ng eq 0) then begin
			  	f(i,j) = f(i,j) + params(3)*exp(-0.5*(((((i-params(4))*cos(params(8))+(j-params(5))*sin(params(8)))/params(6))^2.)+((((j-params(5))*cos(params(8))-(i-params(4))*sin(params(8)))/params(7))^2.)))
			endif else begin	
				f(i,j) = f(i,j) + params(0+(3+ 6*ng))*exp(-0.5*(((((i-params(1+(3 + 6*ng)))*cos(params(5 + (3 + 6*ng)))+(j-params(2 + (3 + 6*ng)))*sin(params(5 + (3 + 6*ng))))/params(3 + (3 + 6*ng)))^2.)+((((j-params(2 + (3 + 6*ng)))*cos(params(5 + (3 + 6*ng)))-(i-params(1 + (3 + 6*ng)))*sin(params(5 + (3 + 6*ng))))/params(4 + (3 + 6*ng)))^2.)))

			endelse				

		endfor
	endfor
endfor

return,f
end
			
			
