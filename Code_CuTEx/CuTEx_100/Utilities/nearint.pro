function nearint,input

sign=input
qpos=where(input ge 0,nqpos,complement=qneg,ncomplement=nqneg) 
if (nqpos gt 0) then sign(qpos)=1
if (nqneg gt 0) then sign(qneg)=-1

out=fix(input)
for i=0,n_elements(input)-1 do begin
if ((abs(input(i))-fix(abs(input(i)))) gt .5) then begin
  out(i)=fix(ceil(abs(input(i))))
endif else begin
  out(i)=fix(floor(abs(input(i))))
endelse
endfor
return,out*sign
end
