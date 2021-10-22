;;;;;;;;;;;;;;;
function pix_dist,x1,x2,y1,y2

dis=sqrt( ((x1-x2)^2.) + ((y1-y2)^2.))

return,dis
end
;;;;;;;;;;;;;;;;;

function group_sources,x,y,dthreshold,dir,radec=radec, verbose=verbose, closest_neigh=closest_neigh

; this function takes in input the list of sources detected by FIND provided as X,Y in pixel units 
; (or in RA-DEC in decimal degrees if the RADEC keyword is set), and groups them (assigning an index group) 
; according to the reciprocal distance being less than the assigned threshold (threshold is intespreted 
; as arcsec if the RADEC keyword is set) DIR is the directory to write the region file

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

	if keyword_Set(verbose) then print,'Analysing pixel ',i,' out of ',nstars,' pixels found above the given threshold'

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

  	q=where(dis le dthreshold, nq)
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

;; if star i already belongs to a group or some of the neighbour stars already belong to another group 
;  then rename the entire group with the current NGROUP
;    old_ngroup=0  ;reset old ngroup to 0
;    if (group(i) ne 0) then old_ngroup=group(i)
;    group(i)=ngroup
;    group(qineg(q))=ngroup
;    if (old_ngroup ne 0) then begin
;      qoldi=where(group eq old_ngroup,nqoldi)
;      if (nqoldi gt 0) then group(qoldi)=ngroup
;    endif
;    ngroup++
;  endif
;endfor

if (n_params() gt 3) then begin
  	openw,unit1,dir+'_group.reg',/get_lun
  	color='blue'
  	printf,unit1,'# Region file format: DS9 version 4.0'
  	printf,unit1,'# Filename: fake'
  	printf,unit1,'global color='+color+' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
  	printf,unit1,'image'
  	for i=0L,nstars-1 do printf,unit1,'# text('+strcompress(string(x(i)+1.),/remove_all)+','+strcompress(string(y(i)+1.),/remove_all)+' text={'+strcompress(string(fix(group(i))),/remove_all)+'}'
  	free_lun,unit1
endif

return,group
end

