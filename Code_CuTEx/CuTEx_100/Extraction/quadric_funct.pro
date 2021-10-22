function quadric_funct,x,y,params

; Fit of a second order polinomial to the background.
; Keep in mind that this represent a subset of possible quadric
; 
; General Form of a quadric
;
; a11*x^2 + a22*y^2 + a33*z^2 + a44 + 2*a12*x*y + 2*a13*x*z + 2*a23*y*z + 2*a14*x + 2*a24*y + 2*a34*z + a44 = 0
;
; 
;		|a11  a12  a13  a14|
;		|a21  a22  a23  a24|
;	A = 	|a31  a32  a33  a34|
;		|a41  a42  a43  a44|
;		
;			  |-	> 0  (Hyperbolic Hyperboloid, Hyperbolic, Paraboloid)
;			  |
;	if det A 	 -|-	= 0  (Degenerate quadric, cylinders and cones) 
; 			  |	
;			  !-	< 0  (Elliptic Hyperboloid, Ellipsoid, Elliptic Paraboloid)
;		
;		 |a11  a12  a13|        
;	A' =	 |a21  a22  a23| 
;		 |a31  a32  a33| 
;	
;
;	if det A =/= 0 then	|-	det A'   = 0  (Paraboloid)
;				|-	det A' =/= 0  (Hyperboloid, Ellipsoid)
;
;	if det A   = 0 then	|-	det A'   = 0  (Cylinder or Degenerate quadric)
;				|-	det A' =/= 0  (Cone)
;
;	Exist 16 normal forms in 3D space, of which only 5 are not degenerate (planes, lines, points or nothing!)
;
;	Ellipsoid
;	One sheet Hyperboloid
;	Two sheets Hyperboloid
;	Elliptical Paraboloid
;	Hyperbolic Paraboloid
;
;	Given a quadric G(x,y,z) = 0 then it can be always putted, through a coordinate system transformation, in one of the two forms:
;	
;	i)  a*x^2 + b*y^2 + c*z^2 = d
;	ii) a'*x^2 + b'*y^2 = 2*d'*z	
; 
;	Here we fit only quadric of the second type (thus (Hyperbolic/Elliptical Paraboloid)
;
; 	Thus functional forms of the type: z = a*x^2 + b*y^2 + c*x*y + d*x + e*y + f = 0
; 	Non quadratic terms are due to axis traslation.
; 	

f=x

for i=0,n_elements(x(*,0))-1 do begin
  for j=0,n_elements(y(0,*))-1 do begin
	f(i,j)= params(0)+i*params(1)+j*params(2)+i*j*params(3)+(i^2)*params(4)+(j^2)*params(5)

  endfor
endfor

return,f
end
