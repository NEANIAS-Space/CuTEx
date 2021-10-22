pro autovalori,array_in,autoval,autovect

info= size(array_in)
if info[1] ne 2 then MESSAGE, "Input must be an 2 x 2 array."

;calcolo autovalori array 2x2
;a00 a10
;a01 a11

;polinomio caratteristico
;lambda^2-(a00+a11)*lambda+(a00*a11-a10*a01)=0
a=1
b=-(array_in(0,0)+array_in(1,1))
c=((array_in(0,0)*array_in(1,1))-(array_in(1,0)*array_in(0,1)))
;print,'a,b,c',a,b,c
lambda1=(((-1.*b)+sqrt((b^2.-(4.*a*c))))/(2.*a))
lambda2=(((-1.*b)-sqrt((b^2.-(4.*a*c))))/(2.*a))

if (lambda1 gt lambda2) then autoval=[lambda1,lambda2] else autoval=[lambda2,lambda1]
;print,'eigenvalues',autoval
autovec=fltarr(info[1],info[1])
if (array_in(1,0) eq array_in(0,1)) then begin
  ;print,'diagonal array'
  autovec(0,0)=0
  autovec(1,0)=1
  autovec(0,1)=1
  autovec(1,1)=0
endif else begin
  for i=0,1 do begin
    lam=autoval(i)
    if (array_in(1,0) eq 0. and array_in(0,1) ne 0.) then begin
      ;print,'(1,0) element is 0'
      if (array_in(0,0)-lam) eq 0 then begin
        coeff2=((array_in(0,1)/(array_in(1,1)-lam)))
        coeff1=coeff2
        autovec(0,i)=1
        autovec(1,i)=coeff1
      endif else begin
        coeff1=0
        coeff2=0
        autovec(0,i)=coeff1
        autovec(1,i)=1
      endelse
      ;print,coeff1,coeff2
    endif else if (array_in(0,1) eq 0. and array_in(1,0) ne 0.) then begin
      ;print,'(0,1) element is 0'
      if (array_in(1,1)-lam) eq 0 then begin
        coeff1=((array_in(0,0)-lam)/array_in(1,0))
        coeff2=coeff1
      endif else begin
        coeff1=0
        coeff2=0
      endelse
      ;print,coeff1,coeff2
      autovec(0,i)=1
      autovec(1,i)=coeff1
    endif else begin
      coeff1=((array_in(0,0)-lam)/array_in(1,0))
      coeff2=((array_in(0,1)/(array_in(1,1)-lam)))
      ;print,coeff1,coeff2
      if (abs(coeff1) le abs(coeff2)+abs(coeff2)/10. and abs(coeff1) ge abs(coeff2)-abs(coeff2)/10.) then begin
        autovec(0,i)=1
        autovec(1,i)=coeff1
      endif else MESSAGE, "Eigenvector at the same eigenvalue are different."
    endelse
  endfor
endelse
autovect=autovec
end
