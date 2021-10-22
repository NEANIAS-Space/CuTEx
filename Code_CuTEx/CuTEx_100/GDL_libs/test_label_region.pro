function test_label_region,in_arr;,ALL_NEIGHBORS=ALL_NEIGHBORS

inf=size(in_arr)
n_col=inf(1)
n_row=inf(2)
n_elem=inf(4)
source_mask=lonarr(n_col,n_row) & source_mask(*,*)=0
do_arr=intarr(n_col,n_row) & do_arr(*,*)=0

sel=where(in_arr ne 0,nsel)

if (nsel gt 0) then begin
  wheretomulti, in_arr, sel , c_int, r_int
  source_ind=1
  for jsel=0,nsel-1 do begin
    min_arr=in_arr(c_int(jsel)-1:c_int(jsel)+1,r_int(jsel)-1:r_int(jsel)+1)
    selmin=where(min_arr ne 0,nselmin)
    if nselmin gt 1 then begin
      wheretomulti,min_arr,selmin,cm,rm
      cm=cm-1
      rm=rm-1
      c_pointsel=c_int(jsel)+cm
      r_pointsel=r_int(jsel)+rm

      check_arr=do_arr(c_pointsel,r_pointsel)
      sel_check=where(check_arr ne 0,nsel_check)
      if (nsel_check eq 0) then begin
        source_mask(c_pointsel,r_pointsel)=source_ind
        source_ind=source_ind+1
        do_arr(c_pointsel,r_pointsel)=1
      endif else begin
        arr_ind=source_mask(c_pointsel,r_pointsel)
        new_ind=arr_ind(sel_check(0))
        source_mask(c_pointsel,r_pointsel)=new_ind
        do_arr(c_pointsel,r_pointsel)=1
      endelse
    endif else source_mask(c_int(jsel),r_int(jsel))=0
  endfor
endif else source_mask=in_arr

return,source_mask

end
