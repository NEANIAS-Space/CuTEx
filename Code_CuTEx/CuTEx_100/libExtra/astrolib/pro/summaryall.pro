pro summaryall,subset=subset,select=select,section=section,mask=mask

if keyword_set(select) then begin
     dir = dialog_pickfile( /directory, $
     title='Select directory containing the *.array files')
     if N_elements(dir) EQ 0 then return
endif else dir = ''

a = file_search(dir + '*.array', Count= n)
if     n EQ 0 then begin
     message,'No *.array files found',/con
     return
endif     

if keyword_set(section) then begin
   if N_elements(subset) EQ 0 then subset = 'subset.txt'
   a1 = file_search(subset,count=nsub)
   if Nsub EQ 0 then message,'ERROR - No subset file found' else begin
      readcol,a1[0],id,i1,i2,j1,j2,f='a,i,i,i,i',/sil
      i1 = i1 -1
      j1 = j1 -1
      i2 = i2 -1
      j2 = j2 -1
      nsub = N_elements(id)
 endelse
      goodfile = bytarr(n)
      bigstatus = bytarr(3,nsub,n)
      for i=0,n-1 do begin      
      message,'Processing ' + a[i],/inf
      summary, a[i], id, i1,i2,j1,j2,status, subset= subset,/silent, $
          mask= mask, result= result
      bigstatus[0,0,i] = status
      goodfile[i] = result
endfor
      get_lun,lun
  
     for j=0,nsub-1 do begin
      if dir EQ '' then cd,current=dir
     message,'Creating file: ' + id[j] + '.summary',/INF
     openw,lun, dir + path_sep() + id[j] + '.summary'
     printf,lun,'Subset file:' + subset
     printf,lun,'Col1: Col2  Row1 Row 2'
     printf,lun,i1[j]+1,i2[j]+1,j1[j]+1,j2[j]+1,f='(i4,2x,i4,2x,i4,1x,i4)'
     printf,lun,' '
     printf,lun,'   Image                               ' + $ 
        ' Closed  Open   Broken'
    for i=0,n-1 do begin
       if goodfile[i] then begin
       fname = a[i]
       fdecomp,fname,disk,dir,file
         t1 = strpos(file,'.png')
      t2 = strpos(file,'.resu')
      if t1 GT 0 then pos = t1 < t2 else pos = t2
      if pos GT 0 then ff = strtrim(strmid(file,0,pos),2)  else ff = file

     printf,lun,ff, bigstatus[0,j,i],bigstatus[1,j,i],bigstatus[2,j,i], $
                  f= '(a,T38,3i7)'
       endif 
     endfor
      close,lun
    endfor
     free_lun,lun 

endif else begin
  for i=0,n-1 do begin
      message,'Processing ' + a[i],/inf
      summary, a[i],id, i1,i2,j1,j2,status,  $
          subset= subset,/outfile,/silent,mask=mask
  endfor
endelse

return
end      
