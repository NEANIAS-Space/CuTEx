function sou_size, ima, x, y, direction=direction, range=range
	
if (keyword_set(range) eq 0) then range = 8

if (keyword_set(direction) eq 0) then begin
	print, "Error direction must be chosen"
	return, -1
endif

ellpos = dblarr(2,2)

; INCREASED THE SIZE OF THE SUBREGION CHECKED FOR THE MINIMA

v_minus = dblarr(range+2)
v_plus = dblarr(range+2)
posvalues = dblarr(2)

if (direction eq 'X') or (direction eq 'Y') then begin

	coeff = 1
	r = 0
	if (direction eq 'X') then begin

		rangex = range+1
		rangey = 0
		q = 1
		p = 0
		dir_ = 1
	endif else begin

		rangex = 0
		rangey = range+1
		q = 0
		p = 0
		dir_ = 2
	endelse
	
;	v_minus = ima(x-rangex:x,y-rangey:y) 
;	v_plus = ima(x:x+rangex,y:y+rangey)  
	v_minus = reverse(ima(x-rangex:x,y-rangey:y), dir_) 
	v_plus = ima(x:x+rangex,y:y+rangey)  
	
endif else begin 

	if (direction eq 'XY') or (direction eq 'YX') then begin
		coeff = sqrt(2.)
		r = 1
		q = 1
		
		if (direction eq 'XY') then begin
		  for k=0, range+1 do v_minus(k) = ima(x - k,y - k)
		  for k=0, range+1 do v_plus(k) = ima(x + k,y + k)
;		  for k=range+1,0 do v_minus(k) = ima(x - k,y - k)
;		  for k=0, range+1 do v_plus(k) = ima(x + k,y + k)
        endif else if (direction eq 'YX') then begin
		  for k=0, range+1 do v_minus(k) = ima(x - k,y + k)
		  for k=0, range+1 do v_plus(k) = ima(x + k,y - k)        
;		  for k=range+1,0 do v_minus(k) = ima(x - k,y + k)
;		  for k=0, range+1 do v_plus(k) = ima(x + k,y - k)        
        endif
;		endfor

	endif else begin 
		print, "Wrong Direction chosen"
		return, -2
	endelse		
endelse	

;;;;; Check for NaN and interpolate linearly the derivate --- Too slow and many cases - To be implemented in future
;;;;;
;;;;; minus_finite = FINITE(v_minus)
;;;;; v_minus_finite = where(minus_finite eq 0, nv_minus_finite)
;;;;;
;;;;; if nv_minus_finite gt 0 and nv_minus_finite lt n_elements(v_minus) then begin
;;;;;
;;;;;	for q = 0,nv_minus_finite do begin
;;;;;		
;;;;;		if  q ne range+1 then or q ne 0 then 
;;;;;	 
;;;;;			qf = 1
;;;;;			check = 0
;;;;;			dummy_minus = 0
;;;;;			dummy_plus  = 0
;;;;;			x1dummy_qf = 0
;;;;;			x2dummy_qf = 0
;;;;;			
;;;;;			repeat begin
;;;;;				if (dummy_minus ne 0) and  minus_finite(v_minus_finite(q) - qf) eq 1 then begin 
;;;;;					
;;;;;					dummy_minus = v_minus(v_minus_finite(q) - qf)
;;;;;					x1dummy_qf = v_minus_finite(q) - qf
;;;;;					check = check + 1
;;;;;					
;;;;;				endif
;;;;;				
;;;;;				if (dummy_plus ne 0) and  minus_finite(v_minus_finite(q) + qf) eq 1 then begin 
;;;;;					
;;;;;					dummy_plus = v_minus(v_minus_finite(q) + qf)
;;;;;					x2dummy_qf = v_minus_finite(q) + qf
;;;;;					check = check + 1
;;;;;					
;;;;;				endif
;;;;;
;;;;;			until  check eq 2
;;;;;			
;;;;;			
;;;;;			
;;;;;		endif 
;;;;;		
;;;;;		if q eq 0 
;;;;;		
;;;;;		if q eq range+1
;;;;;		
;;;;;  endif
;;;;;		

	
;;;; KEEP THIS WAY --- NaN are never greater than any other number	
	
;	v_minus_min = min(v_minus,indm)
;	v_plus_min = min(v_plus,indp)
; modified by Sergio to take the FIRST minimum and not the absolute one

indm=1
while ((v_minus(indm) gt v_minus(indm+1)) and (indm lt range)) do indm++	

;indm=range
;while (v_minus(indm) gt v_minus(indm-1) and indm gt 0) do indm--
;;indm--
;;v_minus_min = v_minus(indm)

indp=1
while ((v_plus(indp) gt v_plus(indp+1)) and (indp lt range)) do indp++	

;indp--
;v_plus_min = v_plus(indp)

checkflag = 0
if indm eq range then checkflag = checkflag + 1
if indp eq range then checkflag = checkflag + 1

;	dist = coeff*(range + indp - indm)		

	dist = coeff*(indp + indm)		

;FOR DEBUG PURPOSE

;print, "V_MINUS", v_minus
;print, "INDM", indm
;print, "V_PLUS", v_plus
;print, "INDP", indp
	
;	ellpos(0,0) = x - q* (range - indm) + r*(range - 2*indm)
;	ellpos(1,0) = x + q* indp 
;	ellpos(0,1) = y + (q - 1)*(range - indm) - r*((p -1)*( indm)  - p*( - indm))
;	ellpos(1,1) = y - (q - 1)* indp + r*(p *(indp) +  (p-1)* (indp))

if (direction eq 'Y') then begin

	ellpos(0,0) = x
	ellpos(1,0) = x
	ellpos(0,1) = y - indm ;-1
	ellpos(1,1) = y + indp 
;	ellpos(0,1) = y - (range - indm)
;	ellpos(1,1) = y + indp 

endif else if (direction eq 'X') then begin

	 ellpos(0,0) = x -indm 
	 ellpos(1,0) = x + indp 
;	 ellpos(0,0) = x - (range-indm+1)
;	 ellpos(1,0) = x + indp -1
	 ellpos(0,1) = y 
	 ellpos(1,1) = y

endif else if (direction eq 'XY') then begin
	 
	 ellpos(0,0) = x - indm
	 ellpos(1,0) = x + indp
	 ellpos(0,1) = y - indm
	 ellpos(1,1) = y + indp 
;	 ellpos(0,0) = x - (range - indm +1 )
;	 ellpos(1,0) = x + indp -1
;	 ellpos(0,1) = y - (range -indm)
;	 ellpos(1,1) = y + indp 
	 
endif else if (direction eq 'YX') then begin
	 
	 ellpos(0,0) = x - indm 
	 ellpos(1,0) = x + indp
	 ellpos(0,1) = y + indm 
	 ellpos(1,1) = y - indp
;	 ellpos(0,0) = x - (range-indm+1)
;	 ellpos(1,0) = x + indp -1
;	 ellpos(0,1) = y + indm
;	 ellpos(1,1) = y - (range - indp)
	 
endif

radii = fltarr(2)
radii(0) = pix_dist(ellpos(0,0),x,ellpos(0,1),y)
radii(1) = pix_dist(ellpos(1,0),x,ellpos(1,1),y)

;print, v_minus
;print, v_plus
;print, indm
;print, indp

posvalues(0) = ima(ellpos(0,0), ellpos(0,1))
posvalues(1) = ima(ellpos(1,0), ellpos(1,1))

return, {dist:dist, radii:radii, ellpos:ellpos, posvalues:posvalues, flag:checkflag}

END

