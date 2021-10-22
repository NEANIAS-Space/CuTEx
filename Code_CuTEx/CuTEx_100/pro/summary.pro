pro summary,file,id,i1,i2,j1,j2,status,subset=subset, outfile=outfile, $
     silent = silent, mask = mask, result = result
;+
;  NAME:
;     SUMMARY
;  PURPOSE:
;     Summarize the  shutter states on sections of a single image
; CALLING SEQUENCE:
;     summary2, file, [i1,i2,j1,j2 , status] 
; OPTIONAL INPUT KEYWORDS:
;      SUBSET  - Name of subset file giving regions to analyze
;      OUTFILE - Name of output file to contain results
;      /SILENT - Suppress terminal messages
;      MASK - Name of .mask file
; OPTIONAL OUTPUT KEYWORD
;      RESULT - 1 if successful, 0 otherwise
;     
;if N_params() lT 2 then begin
;    print,'Syntax -summary, file, [i1,i2,j1,j2 , status] 
;    return
;endif    
result = 0
if N_elements(file) NE 1  then file = ''

if (file EQ '')  then begin
       file = dialog_pickfile(filter='*.array', $
                 title='Select array file to analyze')
       if file[0] EQ '' then return
       if N_elements(file) EQ 0 then return
endif
nsection = N_elements(j2)
if (nsection EQ 0) then begin
if N_elements(subset) EQ 0 then subset = 'subset.txt'
a = file_search(subset,count=nsub)

if Nsub EQ 0 then begin 
    if not keyword_set(silent) then $
      message,'No subset file found',/INF 
  endif   else begin
      if not keyword_set(silent) then message,/inf, $
          'Using section file ' + subset
      readcol,a,id,i1,i2,j1,j2,f='a,i,i,i,i',/sil
      i1 = i1 -1
      j1 = j1 -1
      i2 = i2 -1
      j2 = j2 -1
 endelse   
endif    
      nsub = N_elements(id)
   domask =0b   
   masktype = size(mask,/tname)
   
   if masktype eq 'INT' then begin
            
       mask = dialog_pickfile(filter='*.mask', $
                 title='Select .mask file')
      if mask[0] NE '' then domask = 1b
   endif  else if masktype EQ 'STRING' then domask = 1b
 if domask EQ 0 then begin 
    if not keyword_set(silent) then $
       message,'No mask file selected ',/INF 
       
 endif else readmask,mask,bmask
 
if N_elements(outfile) EQ 0  and not keyword_set(silent) then $
   message,'Processing image:',/INF

readarray,file,icol1,irow1,status1

   
dim = [max(icol1), max(irow1)]
;if Nsub Eq 0 then begin
;     i1 = 0  & j1 = 0
;     i2 = dim[0]-1 & j2 = dim[1]-1
;     nsub = 1
;endif     
if Nsub GT 0 then begin
if (max(i2) ge dim[0]) or (max(j2) ge dim[1]) then begin
   message,'out of range subscripts for image ' + file + $
       strjoin(strcompress(dim),/single),/con
   return
   endif
endif
im1 = bytarr(dim)
im1[icol1-1,irow1-1] = status1 + (status1 LT 0)*4
if domask EQ 1 then begin
       im1 = im1*(1b-bmask)
       npix = long(total( im1 GT 0))
endif else npix = N_elements(im1)

if n_elements(outfile) EQ 1 then begin
    if size(outfile,/tname) NE 'STRING' then begin
          t1 = strpos(file,'.png')
	  t2 = strpos(file,'.array')
	  if t1 GT 0 then pos = t1 < t2 else pos = t2
	  outfile = strmid(file,0,pos) + '.summary'
    endif	  
	  openw,lun,outfile,/get_lun
endif
if N_elements(outfile) Eq 1 then begin 
  message,/inf,'Output written to ' + outfile
  printf,lun,'Summary'
  printf,lun,'File: ' + file
  if domask then printf,lun,'Mask file: ' + mask
  printf,lun,'Image size: ',strjoin(strtrim(dim,2),/single,' by ')
  printf, lun,$
' Subset    X1   X2   Y1   Y2   Total   Closed   Open  Unknown'

endif else if not keyword_set(silent) then begin

   print,'Summary'
   print,'File: ' + file
   print,'Image size: ',strjoin(strtrim(dim,2),/single,' by ')
   print, $
     ' Subset    X1   X2   Y1   Y2   Total   Closed   Open  Unknown'
endif
g1 = histogram(im1,min=0,max=3)

ff = '(a6,2x,4i5,3i8,i6)'
if Nsub GT 0 then status = intarr(3, nsub)

for i=-1,nsub-1 do begin
if i GE 0 then begin
;     if i2[i] GE dim[0] then message, $
;     'ERROR - Out of range Cols: '+ $
;           strtrim(i1[i]+1,2) + ' to ' + strtrim(i2[i]+1,2) +  '#Cols ' +$
;	   strtrim(dim[0],2)
;     if j2[i] GE dim[1] then message, $
;     'ERROR - Out of range Rows: '+  $
;         strtrim(j1[i]+1,2) + ' to ' + strtrim(j2[i]+1,2) + '#Rows ' + $
;	 strtrim( dim[1], 2)
      g1 = histogram(im1[i1[i]:i2[i],j1[i]:j2[i]],min=0,max=3)
      name = id[i]
      npts = total(g1[1:3])
      if N_elements(outfile) EQ 1 then $
      printf, lun, $
        name,i1[i]+1,i2[i]+1,j1[i]+1,j2[i]+1,npts,g1[1],g1[2],g1[3],f=ff  else $ 
      if not keyword_set(silent) then begin
      print, name,i1[i]+1,i2[i]+1,j1[i]+1,j2[i]+1,npts,g1[1],g1[2],g1[3],f=ff    
      endif
      status[0,i] = g1[1:3]
endif else begin 
      g1 = histogram(im1,min=0,max=3)
      name = "Total"
      g = where(im1,npts)
      if N_elements(outfile) EQ 1 then $
      printf, lun, "Total",1,1,dim[0],dim[1],npts,g1[1],g1[2],g1[3],f=ff else $
      if not keyword_set(silent) then $
            print,"Total",1,1,dim[0],dim[1],npts,g1[1],g1[2],g1[3],f=ff
        
endelse
      
     
endfor

      if N_elements(outfile) EQ 1 then free_lun,lun
result= 1
return
end
