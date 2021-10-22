
;-- Return matching LASCO C2/C3 and EIT filenames in a structure array
;   Written, 20-Feb-2007, Zarro (ADNET)


function eit_lasco_files,tstart,tend,_extra=extra,verbose=verbose,count=count

dstart=get_def_times(tstart,tend,dend=dend,_extra=extra,/utc,$
                     no_next=~valid_time(tend),round=~valid_time(tstart))

fstruct=-1
verbose=keyword_set(verbose)

;-- search LASCO images first

if verbose then message,'searching for LASCO images..',/cont
lasco_path='$SOHO_PUBLIC/data/summary/lasco'
lasco_files=list_fids(dstart,dend,path=lasco_path,suffix='*fts',$
             count=count,verbose=verbose)

if count eq 0 then begin
 message,'no LASCO files found',/cont
 return,-1
endif

c2=where(stregex(lasco_files,'c2',/bool),count)
if count eq 0 then begin
 message,'no LASCO C2 files found',/cont
 return,-1
endif else c2_files=lasco_files[c2]

c3=where(stregex(lasco_files,'c3',/bool),count)
if count eq 0 then begin
 message,'no LASCO C3 files found',/cont
 return,-1
endif else c3_files=lasco_files[c3]

;-- next search for nearest EIT images

if verbose then message,'searching for EIT images..',/cont
eit_path='$SOHO_PRIVATE/data/planning/eit'

eit_files=list_fids(dstart,dend,path=eit_path,suffix='*fts',$
             count=count,verbose=verbose)

if count eq 0 then begin
 message,'no EIT files found',/cont
 return,-1
endif

;-- now cycle thru each EIT wavelength to find nearest C2/C3 pair

wave=['171','195','284','304']
c2_times=fid2time(c2_files)
c3_times=fid2time(c3_files)
eit_times=fid2time(eit_files)

for i=0,n_elements(c3_files)-1 do begin
 c3_file=c3_files[i]
 c3_time=c3_times[i]
 chk=near_time(c2_times,c3_time)
 c2_file=c2_files[chk[0]]
 for j=0,n_elements(wave)-1 do begin
  eit=where(stregex(eit_files,wave[j],/bool),count)
  if count gt 0 then begin
   eit_wave_files=eit_files[eit]
   eit_wave_times=eit_times[eit]
   chk=near_time(eit_wave_times,c3_time)
   eit_file=eit_wave_files[chk[0]]
   struct={c3:c3_file,c2:c2_file,eit:eit_file}
   tstruct=merge_struct(tstruct,struct)
  endif
 endfor
endfor

count=n_elements(tstruct)
if count ne 0 then fstruct=temporary(tstruct)

return,fstruct & end
 
