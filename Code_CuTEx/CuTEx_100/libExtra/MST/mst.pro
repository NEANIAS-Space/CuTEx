;  NAME:
;      MST
; Minimum spanning tree bestimmen  
 FUNCTION MST,dm,ist
 if n_params() lt 2 then ist=0
 sdm=size(dm) 
 if sdm(0) ne 2 then message,'Distancematrix not 2-dim.' 
 if sdm(1) ne sdm(2) then message,'Distancematrix not a sqare' 
 n=sdm(1) 
 ss=bytarr(n) & ss(ist)=1b  
 aa=intarr(2,n-1)  
 cs=intarr(n) & cs(*)=ist  
 while total(ss) ne n do begin  
  t=where(ss eq 0)  
  mini=min(dm(t,cs(t)),kl)  
  s=t(kl mod nearint(n-total(ss)))  
  ss(s)=1  
  nss=total(ss)  
  aa(*,nss-2)=[s,cs(s)]  
  if nss lt n then begin 
    t=where(ss eq 0) 
    xx=where(dm(t,replicate(s,n-nss)) lt dm(t,cs(t))) 
    sxx=size(xx) 
    if sxx(0) eq 1 then cs(t(xx))=s 
  endif
 endwhile 
 return,aa
 end
