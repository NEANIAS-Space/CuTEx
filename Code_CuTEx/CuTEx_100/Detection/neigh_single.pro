pro neigh_single,mask_in,mask_out, all_neighbours=all_neighbours

; Procedure written by Davide Elia to remove the lonely single pixels from the mask

sz=size(mask_in)
s1=sz[1]
s2=sz[2]

mask_1=fltarr(s1+2,s2+2)
mask_1[1:s1,0:s2-1]=mask_in
mask_2=fltarr(s1+2,s2+2)
mask_2[0:s1-1,1:s2]=mask_in
mask_3=fltarr(s1+2,s2+2)
mask_3[2:s1+1,1:s2]=mask_in
mask_4=fltarr(s1+2,s2+2)
mask_4[1:s1,2:s2+1]=mask_in

if keyword_set(ALL_NEIGHBOURS) then begin

	mask_5=fltarr(s1+2,s2+2)
	mask_5[0:s1-1,0:s2-1]=mask_in
	mask_6=fltarr(s1+2,s2+2)
	mask_6[2:s1+1,0:s2-1]=mask_in
	mask_7=fltarr(s1+2,s2+2)
	mask_7[0:s1-1,2:s2+1]=mask_in
	mask_8=fltarr(s1+2,s2+2)
	mask_8[2:s1+1,2:s2+1]=mask_in

endif


if keyword_set(ALL_NEIGHBOURS) then mask18=mask_1+mask_2+mask_3+mask_4+mask_5+mask_6+mask_7+mask_8 else mask14=mask_1+mask_2+mask_3+mask_4 

if keyword_Set(ALL_NEIGHBOURS) then mask_check = mask18 else mask_check = mask14

mask_check=mask_check[1:s1,1:s2]



mref=reform(mask_check,s1*s2,1)
m_in=reform(mask_in,s1*s2,1)
w0=where(m_in eq 10000 and mref eq 0,c0)
if c0 gt 0 then   m_in[w0]=0
mask_out=fltarr(s1,s2)
mask_out=reform(m_in,s1,s2)

end
