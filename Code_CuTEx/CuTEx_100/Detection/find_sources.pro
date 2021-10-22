function find_sources, mas, a,der2, nsigma, all_neighbours=all_neighbours, thresh = thresh, super_resolution = super_resolution, local_thresh=local_thresh, sourcesnan=sourcesnan, quiet=quiet

; 14/02/2012	-	Changes Implemented. 	Saving also information on the derivative values and the threshold levels
;
;		-				Warning. It will crash for sources close to the border. Depends on the distlen

COMMON DERIV, xd2, yd2, x45d2, y45d2
COMMON DERIVTHRESH, xd2m, yd2m, x45d2m, y45d2m
COMMON NANMAP, nanmask, nanmasksurround, xd2nan, yd2nan, x45d2nan, y45d2nan, nanpixels

distlen = 1.	; Distance in pixels within which it is searched the maximum of the intensity map to be compared with the maximum in second derivate

if keyword_set(quiet) then quiet = 1. else quiet = 0
if not keyword_set(thresh) then thresh=0.5

verbose = 0
sdev = sdev_der(der2,/SILENT,border=2, clip=3)
sdevx = sdev_der(xd2,/SILENT,border=2, clip=3)
sdevy = sdev_der(yd2,/SILENT,border=2, clip=3)
sdevx45 = sdev_der(x45d2,/SILENT,border=2, clip=3)
sdevy45 = sdev_der(y45d2,/SILENT,border=2, clip=3)

if (sdev eq -1 or sdevx eq -1 or sdevy eq -1 or sdevx45 eq -1 or sdevy45 eq -1) then return,-1

mas1 = where(mas eq 1., n)
print, 'Grouping nearest neighbors pixels'

;if KEYWORD_SET(all_neighbours) then mas_label = label_region(mas, /ALL_NEIGHBORS, /ULONG) else mas_label = label_region(mas, /ULONG)	; Define Groups 
if KEYWORD_SET(all_neighbours) then mas_label = test_label_region(mas) else mas_label = test_label_region(mas)   ; Define Groups  

mas_indx = where(mas_label gt 0, nqg)

if (nqg le 0) then begin
	print,'No grouped mask pixels were found'
	return,-1
endif

print, "Number of confirmed pixels in Groups ", nqg
sourced=fltarr(13,n)		; Array containing the coordinate of Pixels identified as center of sources 
				; Added also the save of the Absolute Curvature on the center of source along the four direction
				; Save also the minimum value of the curvature thresholded
				; Save also the distance of the maximum of the flux from the peak of the curvature
				; Save the number of the CLUMPOFPIXEL that the source belong
				; Save the number of derivate directions for which it is not a NaN
ks = 0L

mas_sort = mas_label(mas_indx(sort(mas_label(mas_indx))))								; Order groups
mas_uniq = uniq(mas_sort)												; Group lists without repeated
ngrps =  n_elements(mas_uniq)
if not keyword_set(sourcesnan) then print, 'Found ', ngrps, ' different regions satisfing selected criteria' else print, 'Found ', ngrps,' further regions satisfying the criteria '
mas_reg = mas_sort(mas_uniq)

nquiet = ngrps/20.
counter = 0.
for i=0L, ngrps-1 do begin
	
	if quiet eq 0 then if (i ne 0) and (i ge counter*nquiet)  then begin
		print, "Analyzed already "+strcompress(string(LONG(counter*5)),/REMOVE_ALL)+" % of "+strcompress(string(LONG(ngrps)),/REMOVE_ALL)+" regions"
		counter = counter+1
	endif  
	if quiet eq 0 then if (i eq ngrps -1) then print, "Analyzed already "+strcompress(string(100),/REMOVE_ALL)+" % of "+strcompress(string(LONG(ngrps)),/REMOVE_ALL)+" regions"
		
	g = where(mas_label eq mas_reg(i),ng)										; Get all pixels belonging to the same group
	
	int = fltarr(ng) & intx = fltarr(ng) & inty = fltarr(ng) & int45x = fltarr(ng) & int45y = fltarr(ng) 		; Build up the static arrays with all the pixel informations

	wheretomulti, mas_label, g, c_reg, r_reg									; c_reg and r_reg are the positions in the array
	
	int = der2(g)		; Value of mean curvature for the group analized
    	intx = xd2(g)
    	inty = yd2(g)
    	int45x = x45d2(g)
    	int45y = y45d2(g)
	
	if keyword_set(sourcesnan) then nderiv =  (4 - ( xd2nan(g) + yd2nan(g) + x45d2nan(g) + y45d2nan(g))) else nderiv = intarr(ng) + 4
	
	foundatleastone = 0
	
	for k=0,ng-1 do begin						; Identify Group Center -- Case there is one pixel "statistically significative" 
		    dist = fltarr(ng)
			for w = 0, ng-1 do dist[w] = norm([c_reg(k),r_reg(k)]-[c_reg(w),r_reg(w)])
			
			qneig = where(dist gt 0 and dist lt 1.5,nqneig)                ; To get the adiacent and the diagonal touching pixels
			
; by Sergio
			
		    	if (keyword_set(super_resolution)) then begin 
		    
;			      if ( (intx(k) gt max(intx(qneig))+thresh*sdevx) or (inty(k) gt max(inty(qneig))+thresh*sdevy) or (int45x(k) gt max(int45x(qneig))+thresh*sdevx45) or (int45y(k) gt max(int45y(qneig))+thresh*sdevy45) )   then begin

		      ; here we check that a point has higher derivative than its neighbour in AT LEAST two derivative direction 			
	
			      derpeak=0
			      if (intx(k) gt max(intx(qneig))+thresh*sdevx) then derpeak=derpeak+1
			      if (inty(k) gt max(inty(qneig))+thresh*sdevy) then derpeak=derpeak+1
			      if (int45x(k) gt max(int45x(qneig))+thresh*sdevx45) then derpeak=derpeak+1
			      if (int45y(k) gt max(int45y(qneig))+thresh*sdevy45) then derpeak=derpeak+1
			      
			      if (derpeak gt 1) then begin
		
					if verbose eq 0 then begin
						print, "Determining Source Center using classical criteria on first neighbours (1 global image r.m.s. above other pixels): SUPER RESOLUTION ON"
						verbose = 1
					endif	
					; check that no other source is 1.5pxl adiacent ot an already found one
            				
					if (ks gt 0) then begin ; for the first ever source this check should not be done
            	  				dd=fltarr(ks)
            	 				
						for ww = 0L, ks-1L do begin
            	  				  	cc = norm([c_reg(k),r_reg(k)]-[sourced(0,ww),sourced(1,ww)])
            	    					dd(ww)=cc
            	  				endfor
            	  				
						qtooclose=where(dd lt 1.5,nqtooclose)
				            	
						if (nqtooclose eq 0) then begin
							sourced(0,ks) = c_reg(k)
							sourced(1,ks) = r_reg(k)


							sourced(2,ks) = xd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(3,ks) = yd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(4,ks) = x45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(5,ks) = y45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))

		
							if keyword_set(local_thresh) then begin
					
								sourced(6,ks) =  xd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
								sourced(7,ks) =  yd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
								sourced(8,ks) =  x45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
								sourced(9,ks) =  y45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
				
							endif else begin
					
								sourced(6,ks) = nsigma*sdevx
								sourced(7,ks) = nsigma*sdevy
								sourced(8,ks) = nsigma*sdevx45
								sourced(9,ks) = nsigma*sdevy45
			
							endelse
	
							checksurr = a(nearint(sourced(0,ks))-distlen:nearint(sourced(0,ks))+distlen,nearint(sourced(1,ks))-distlen:nearint(sourced(1,ks))+distlen)
							maxflux = max(checksurr,maxpos)
							wheretomulti, checksurr, maxpos, cmax, rmax
							maxfluxpos = norm([cmax, rmax] - [distlen,distlen])
							
							sourced(10,ks) = maxfluxpos
							sourced(11,ks) = mas_reg(i)	; Number of the pixel group where the source belong
							
							H = histogram(nderiv, min=0, max=4, location=xH)
							maxH = max(H,xmaxH)
							sourced(12,ks) = xH(xmaxH)
							foundatleastone = 1
							ks = ks + 1L
				  		endif
					endif else begin   ; first source is always good as there are no other neighbour to check vicinity
				  		
						sourced(0,ks) = (sourced(0,ks) + c_reg(k))/2.
				  		sourced(1,ks) = (sourced(1,ks) + r_reg(k))/2.

;						sourced(0,ks) = c_reg(k)
;				  		sourced(1,ks) = r_reg(k)

						sourced(2,ks) = xd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
						sourced(3,ks) = yd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
						sourced(4,ks) = x45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
						sourced(5,ks) = y45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))

		
						if keyword_set(local_thresh) then begin
				
							sourced(6,ks) =  xd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(7,ks) =  yd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(8,ks) =  x45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(9,ks) =  y45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
			
						endif else begin
				
							sourced(6,ks) = nsigma*sdevx
							sourced(7,ks) = nsigma*sdevy
							sourced(8,ks) = nsigma*sdevx45
							sourced(9,ks) = nsigma*sdevy45
			
						endelse

						checksurr = a(nearint(sourced(0,ks))-distlen:nearint(sourced(0,ks))+distlen,nearint(sourced(1,ks))-distlen:nearint(sourced(1,ks))+distlen)
						maxflux = max(checksurr,maxpos)
						wheretomulti, checksurr, maxpos, cmax, rmax
						maxfluxpos = norm([cmax, rmax] - [distlen,distlen])
						
						sourced(10,ks) = maxfluxpos
						sourced(11,ks) = mas_reg(i)	; Number of the pixel group where the source belong
						
						H = histogram(nderiv, min=0, max=4, location=xH)
						maxH = max(H,xmaxH)
						sourced(12,ks) = xH(xmaxH)
							
						foundatleastone = 1
				  		ks = ks + 1L
					endelse
		      		endif
			endif else begin
			  	
				;if ( (int(k) gt max(int(qneig))+thresh*sdev) )   then begin	; here we check that  a point has higher derivative than its neighbour using the AVERAGE derivative
				qmaxabsolute=where(int(qneig) lt int(k),nqmaxabs) ; it has to be a local maximum w.r.t. number of neighbour pixels
				qmax=where(int(qneig)+thresh*sdev lt int(k),nqmax)  ; it has to be a maximum also in a statistically significant way, i.e. including the detection threshold, but we are allowing to be so for all neighbour pixels minus 1				
				if (nqneig eq 1) then nqneig_lim=nqneig else nqneig_lim=nqneig-1
				if (nqmax ge nqneig_lim and nqmaxabs eq nqneig) then begin  ;it is true that we are allowing one neighbour pixels to fail the statistically significant local maximum criteria, but yet the maximum has to be a maximum
					if verbose eq 0 then begin
						print, "Determining Source Center using classical criteria on first neighbours (1 global image r.m.s. above other pixels)"
						verbose = 1
					endif	
					; check that no other source is 1.5pxl adiacent ot an already found one
            				
					if (ks gt 0) then begin ; for the first ever source this check should not be done
            	  				dd=fltarr(ks)
            	 				
						for ww = 0L, ks-1L do begin
            	  				  	cc = norm([c_reg(k),r_reg(k)]-[sourced(0,ww),sourced(1,ww)])
            	    					dd(ww)=cc
            	  				endfor
            	  				
						qtooclose=where(dd lt 1.5,nqtooclose)
				            	
						if (nqtooclose eq 0) then begin

							sourced(0,ks) = c_reg(k)
							sourced(1,ks) = r_reg(k)
							sourced(2,ks) = xd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(3,ks) = yd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(4,ks) = x45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(5,ks) = y45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))

		
							if keyword_set(local_thresh) then begin
				
								sourced(6,ks) =  xd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
								sourced(7,ks) =  yd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
								sourced(8,ks) =  x45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
								sourced(9,ks) =  y45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
			
							endif else begin
				
								sourced(6,ks) = nsigma*sdevx
								sourced(7,ks) = nsigma*sdevy
								sourced(8,ks) = nsigma*sdevx45
								sourced(9,ks) = nsigma*sdevy45
			
							endelse

							checksurr = a(nearint(sourced(0,ks))-distlen:nearint(sourced(0,ks))+distlen,nearint(sourced(1,ks))-distlen:nearint(sourced(1,ks))+distlen)
							maxflux = max(checksurr,maxpos)
							wheretomulti, checksurr, maxpos, cmax, rmax
							maxfluxpos = norm([cmax, rmax] - [distlen,distlen])
						
							sourced(10,ks) = maxfluxpos
							sourced(11,ks) = mas_reg(i)	; Number of the pixel group where the source belong
				  	
							H = histogram(nderiv, min=0, max=4, location=xH)
							maxH = max(H,xmaxH)
							sourced(12,ks) = xH(xmaxH)
						
							foundatleastone = 1
							ks = ks + 1L
						endif
					endif else begin
						sourced(0,ks) = c_reg(k)
						sourced(1,ks) = r_reg(k)
						sourced(2,ks) = xd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
						sourced(3,ks) = yd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
						sourced(4,ks) = x45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
						sourced(5,ks) = y45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))

		
						if keyword_set(local_thresh) then begin
				
							sourced(6,ks) =  xd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(7,ks) =  yd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(8,ks) =  x45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
							sourced(9,ks) =  y45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
			
						endif else begin
				
							sourced(6,ks) = nsigma*sdevx
							sourced(7,ks) = nsigma*sdevy
							sourced(8,ks) = nsigma*sdevx45
							sourced(9,ks) = nsigma*sdevy45
			
						endelse
												
									
						checksurr = a(MAX([nearint(sourced(0,ks))-distlen,0]):MIN([nearint(sourced(0,ks))+distlen,N_ELEMENTS(A(*,0))-1]),MAX([nearint(sourced(1,ks))-distlen,0]):MIN([nearint(sourced(1,ks))+distlen,N_ELEMENTS(A(0,*))-1]))
						maxflux = max(checksurr,maxpos)
						wheretomulti, checksurr, maxpos, cmax, rmax
						maxfluxpos = norm([cmax, rmax] - [distlen,distlen])
						
						sourced(10,ks) = maxfluxpos
						sourced(11,ks) = mas_reg(i)	; Number of the pixel group where the source belong
				  	
						H = histogram(nderiv, min=0, max=4, location=xH)
						maxH = max(H,xmaxH)
						sourced(12,ks) = xH(xmaxH)
						
						foundatleastone = 1
						ks = ks + 1L
					
					endelse
			  	endif
			endelse
	endfor	
	
	if foundatleastone eq 0 then begin				; Identify Group Center -- No pixels with high curvature found, mean position of group pixels
      		sourced(0,ks)=total(c_reg)/ng
      		sourced(1,ks)=total(r_reg)/ng
		sourced(2,ks) = xd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
		sourced(3,ks) = yd2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
		sourced(4,ks) = x45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
		sourced(5,ks) = y45d2(nearint(sourced(0,ks)),nearint(sourced(1,ks)))

		
		if keyword_set(local_thresh) then begin
		
			sourced(6,ks) =  xd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
			sourced(7,ks) =  yd2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
			sourced(8,ks) =  x45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
			sourced(9,ks) =  y45d2m(nearint(sourced(0,ks)),nearint(sourced(1,ks)))
		
		endif else begin
		
			sourced(6,ks) = nsigma*sdevx
			sourced(7,ks) = nsigma*sdevy
			sourced(8,ks) = nsigma*sdevx45
			sourced(9,ks) = nsigma*sdevy45
		
		endelse
		
		checksurr = a(nearint(sourced(0,ks))-distlen:nearint(sourced(0,ks))+distlen,nearint(sourced(1,ks))-distlen:nearint(sourced(1,ks))+distlen)
		maxflux = max(checksurr,maxpos)
		wheretomulti, checksurr, maxpos, cmax, rmax
		maxfluxpos = norm([cmax, rmax] - [distlen,distlen])
			
		sourced(10,ks) = maxfluxpos
		sourced(11,ks) = mas_reg(i)	; Number of the pixel group where the source belong

		H = histogram(nderiv, min=0, max=4, location=xH)
		maxH = max(H,xmaxH)
		sourced(12,ks) = xH(xmaxH)
					
      		ks=ks+1      		
	endif
  		
endfor	

if (ks eq 0) then begin
  	print,'No sources found at detection stage'
  	return,-1
endif else if not keyword_set(sourcesnan) then print, 'Found '+strcompress(string(LONG(ks)),/REMOVE_ALL)+ ' sources at detection stage' else print, 'Found '+strcompress(string(LONG(ks)),/REMOVE_ALL)+' further candidate sources nearby NaN pixels'

sourced=sourced(*,0:ks-1)
source=sourced			; List of detected sources			

return, source

end
