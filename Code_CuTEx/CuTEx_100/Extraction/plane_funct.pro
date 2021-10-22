function plane_funct,x,y,params

f=x

for i=0,n_elements(x(*,0))-1 do begin
  for j=0,n_elements(y(0,*))-1 do begin
	f(i,j)= params(0)+i*params(1)+j*params(2)

  endfor
endfor

return,f
end
