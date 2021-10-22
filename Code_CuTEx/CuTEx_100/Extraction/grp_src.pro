;;;;;;;;;;;;;;;
function pix_dist,x1,x2,y1,y2

dis=sqrt( ((x1-x2)^2.) + ((y1-y2)^2.))

return,dis
end

function grp_src,x,y,dthreshold,dir,radec=radec, verbose=verbose, distances=distances

; this function takes in input the list of sources detected by FIND provided as X,Y in pixel units 
; (or in RA-DEC in decimal degrees if the RADEC keyword is set), and groups them (assigning an index group) 
; according to the reciprocal distance being less than the assigned threshold (threshold is intespreted 
; as arcsec if the RADEC keyword is set) DIR is the directory to write the region file
;
;_________________________________________________________________________________
; Like group_sources.pro with the difference that keyword distances is implemented
; Implementation of different distances for group with for different sources
;
;
; KEYWORD:			Distances -

if keyword_set(distances) then if n_elements(distances) ne n_elements(x) then begin
		print, "Error: Set Keyword Distance only if you want a defined distance to group each source"
		return, -1
endif
	

nstars=n_elements(x)
group=lonarr(n_elements(x))
dis=fltarr(n_elements(x)-1)
group(*)=0L
ngroup=1L
index=lindgen(nstars)
old_ngroup=0L

; select work arrays for distance computation from a star index i; basically it is the arrays without star i.
; I want to do this instead of just removing star i form the arrays because I want to preserve the original star indexing in X,Y
; i.e. get the other stars

for i=0L,nstars-1 do begin

	if keyword_set(verbose) then print,'Analysing pixel ',i,' out of ',nstars,' pixels found above the given threshold'

	dis(*)=0.
	qi=where(index eq i,complement=qineg, ncomplement=nqineg) 
	; get the nearest stars
	if (keyword_set(radec)) then begin
		for j=0L,nqineg-1 do begin
      			gcirc,1,x(qineg(j))/15.,y(qineg(j)),x(i)/15.,y(i),dis1
      			dis(j)=dis1
    		endfor
	endif else begin 
    		for j=0L,nqineg-1 do dis(j)=pix_dist(x(qineg(j)),x(i),y(qineg(j)),y(i))
	endelse

	; Identifying the index of the the elements that are withing the setted threshold distance
	if keyword_set(DISTANCES) then begin 
  	
		q_ = 0
	
		for k = 0L, nqineg-1 do begin
	
  			dthreshold_= dthreshold*(distances(i) + distances(qineg(k)) )
			if keyword_set(verbose) then begin
			
				print, "Width ", FIX(i), " source = ", distances(i) 
				print, "Width ", FIX(qineg(k)), " source = ", distances(qineg(k)) 
				print, "Minimum groupping distance = " ,dthreshold_
				print, "Distance source ", i, " from source ", qineg(k), " = ", dis(k)
			endif
			if dis(k) le dthreshold_ then q_ = [q_, k]
		
		endfor
	
		nq = n_elements(q_)-1
	
		if n_elements(q_) gt 1 then q = q_(1:nq) else q = -1
;		print, "Number of sources in the group ", nq , " Indices of sources groupped ", q	
  
	endif else begin
  
  		dthreshold_ = dthreshold
   		q=where(dis le dthreshold_, nq)
  
  	endelse
 
	; if star i already belongs to a group or some of the neighbour stars already belong 
	; to another group then rename the entire group with the current NGROUP
    	
  	if (nq gt 0) then begin
		old_ngroup=0  ;reset old ngroup to 0
    		if (group(i) ne 0) then old_ngroup=group(i)
    		group(i)=ngroup

	; understands to which old groups the distance selected sources already belong
    		oldgroups=0.
   	 	qoldgroups=where(group(qineg(q)) ne 0)
    
		if (qoldgroups(0) ne -1) then begin
			oldgroups=group(qineg(q(qoldgroups)))
			for ll=0L,n_elements(oldgroups)-1 do begin
	        		qoldi=where(group eq oldgroups(ll),nqoldi)
            			if (nqoldi gt 0) then group(qoldi)=ngroup
			endfor
 	   	endif
    		group(qineg(q))=ngroup

    		ngroup++
 	endif
endfor

if (n_params() gt 3) then begin
  	openw,unit1,dir+'_group.reg',/get_lun
  	color='blu'
  	printf,unit1,'# Region file format: DS9 version 4.0'
  	printf,unit1,'# Filename: fake
  	printf,unit1,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  	printf,unit1,'image'
  	for i=0,nstars-1 do printf,unit1,'# text('+strcompress(string(x(i)+1.),/remove_all)+','+strcompress(string(y(i)+1.),/remove_all)+' text={'+strcompress(string(fix(group(i))),/remove_all)+'}'
  	free_lun,unit1
endif

return,group
end

