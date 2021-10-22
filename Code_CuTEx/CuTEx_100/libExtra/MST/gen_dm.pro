 ; NAME: 
 ;     GEN_DM 
 ; Distanzmatrix berechnen 
 FUNCTION GEN_DM,x,y,MAINMAX=mm 
 sx=size(x) 
 sy=size(y)
 w=where(sx ne sy,cw)  
 if cw ne 0 then message,'Pointvectors differ' 
 if sx(0) ne 1 then message,'Pointvectors not 1-dim.'        
 dm=fltarr(sx(1),sx(1))    
 for i=0,sx(1)-1 do for j=i,sx(1)-1 do begin    
  dm(i,j)=norm([x(i),y(i)]-[x(j),y(j)])    
  dm(j,i)=dm(i,j)    
 endfor    
 if keyword_set(mm) then begin 
  maxi=max(dm)+1.    
  for i=0,sx(1)-1 do dm(i,i)=maxi 
 endif 
 return,dm 
 end 
