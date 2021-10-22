pro deriv_45,inar,x45,y45, derivate=derivate

nn1=n_elements(inar(0,*))
nn2=n_elements(inar(*,0))

x45=fltarr(nn2,nn1)
y45=fltarr(nn2,nn1)

x45(*,*)=0.
y45(*,*)=0.

;incr=sqrt(2.)
incr=1.

;costruzione di y45

for i=4L,nn2-1L do begin
  if (i le nn1-1L) then jstop=i else jstop=nn1-1L
  aprov=fltarr(fix(jstop+1))
  dprov=fltarr(fix(jstop+1))
  indi=0
  for j=0L,jstop do begin
    aprov(j)=inar(i-indi,j)
    ;print,i-indi,j
    indi=indi+1L
  endfor
  if NOT KEYWORD_SET(derivate) then begin 
;  	print,'5 - points'
  	dprov(*)=deriv_5(deriv_5(aprov(*),hspace=incr),hspace=incr)
  endif else begin
;  	print,'3 - points'
	dprov(*)=deriv(deriv(aprov(*)))/(incr)^2
  endelse
  
  indi=0L
  for j=0L,jstop do begin
    y45(i-indi,j)=dprov(j)
    ;print,i-indi,j
    indi=indi+1L
  endfor
endfor

i2=nn2-1L

for j=1L,nn1-5L do begin
  indj=0
  diff=(j+nn2-1L)-(nn1-1L)
  if (diff lt 0) then indistop=nn2-1L else indistop=nn2-1L-diff
  aprov=fltarr(fix(indistop+1L))
  dprov=fltarr(fix(indistop+1L))
  for indi=0L,indistop do begin
    aprov(indi)=inar(i2-indi,j+indj)
    ;print,i2-indi,j+indj
    if (j+indj lt nn1-1L) then indj=indj+1L else indj=indj
  endfor
  if NOT KEYWORD_SET(derivate) then begin
;    	print,'5-points derivate'
	dprov(*)=deriv_5(deriv_5(aprov(*),hspace=incr),hspace=incr)
  endif else begin
;    	print,'3-points derivate'
	dprov(*)=deriv(deriv(aprov(*)))/(incr)^2
  endelse
  indj=0
  for indi=0L,indistop do begin
    y45(i2-indi,j+indj)=dprov(indi)
    ;print,i2-indi,j+indj
    if (j+indj lt nn1-1L) then indj=indj+1L else indj=indj
  endfor
endfor

;fine derivata y45


;costruzione di x45

for i=0L,nn2-5L do begin
  diff=(i+nn1-1L)-(nn2-1L)
  if (diff lt 0) then jstop=nn1-1L else jstop=nn1-1L-diff
  aprov=fltarr(fix(jstop+1L))
  dprov=fltarr(fix(jstop+1L))
  indi=0L
  for j=0L,jstop do begin
    aprov(j)=inar(i+indi,j)
    ;print,i+indi,j
    indi=indi+1L
  endfor
  if NOT KEYWORD_SET(derivate) then begin
;	  print,'5-points derivate'
	  dprov(*)=deriv_5(deriv_5(aprov(*),hspace=incr),hspace=incr)
  endif else begin
;	  print,'3-points derivate'
	  dprov(*)=deriv(deriv(aprov(*)))/(incr)^2
  endelse
  indi=0L
  for j=0,jstop do begin
    x45(i+indi,j)=dprov(j)
    ;print,i+indi,j
    indi=indi+1L
  endfor
endfor

for j=1L,nn1-5L do begin
  diff=(j+nn2-1L)-(nn1-1L)
  if (diff lt 0) then istop=nn2-1L else istop=nn2-1L-diff
  aprov=fltarr(fix(istop+1L))
  dprov=fltarr(fix(istop+1L))
  indj=0L
  for i=0L,istop do begin
    aprov(i)=inar(i,j+indj)
    ;print,i,indj+j
    indj=indj+1L
  endfor
  if NOT KEYWORD_SET(deriv) then begin
;  	print,'5-points derivate'
	dprov(*)=deriv_5(deriv_5(aprov(*),hspace=incr),hspace=incr)
  endif else begin
;  	print,'3-points derivate'
	dprov(*)=deriv(deriv(aprov(*)))/(incr)^2
  endelse
  indj=0
  for i=0L,istop do begin
    x45(i,j+indj)=dprov(i)
    ;print,i,indj+j
    indj=indj+1L
  endfor
endfor

;fine derivata x45

end
