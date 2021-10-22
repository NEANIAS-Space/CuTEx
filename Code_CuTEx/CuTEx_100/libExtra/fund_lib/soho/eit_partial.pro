;+
; Project     : HESSI
;
; Name        : EIT_PARTIAL
;
; Purpose     : Update EIT INDEX for partial
;               fov images with instrument pointing
;
; Category    : Ancillary GBO Synoptic
;
; Syntax      : IDL> index=eit_partial(index,header=header)
;
; Inputs      : INDEX = index structure
;
; Outputs     : INDEX with updated CRPIX/CRVAL/XCEN/YCEN values
;
; Keywords    : HEADER with updated CRPIX/CRVAL/XCEN/YCEN values
;
; History     : Written 20 March 2002, D. Zarro, L-3Com/GSFC
;               Modified 13 June 2005, Zarro (L-3Com/GSFC) 
;                - added check for header
;
; Contact     : dzarro@solar.stanford.edu
;-

function eit_partial,index,header=header,verbose=verbose,partial=partial

if not exist(index) then return,''
nindex=n_elements(index)
partial=bytarr(nindex)
if not have_proc('eit_point') then return,index
if not is_struct(index) then return,index

;-- check if partial frame image needs pointing update

count=0
if have_tag(index,'object') and have_tag(index,'history') then begin
 partial=strpos(strlowcase(index.object),'partial') gt -1
 need_update=strpos(strlowcase(strjoin(index.history)),'pointing update') eq -1
 do_partial=where( (partial gt 0) and (need_update gt 0) ,count)
 if nindex eq 1 then partial=partial[0]
endif 

if count eq 0 then return,index


rindex=index
nindex=index[do_partial]

;-- update CDELT1

cdelt=call_function('eit_pixsize')
nindex=rep_tag_value(nindex,cdelt,'cdelt1')
nindex=rep_tag_value(nindex,cdelt,'cdelt2')

;-- Sun center in arcsec (XCEN/YCEN)

nindex=rep_tag_value(nindex,0.,'xcen')
nindex=rep_tag_value(nindex,0.,'ycen')

np=n_elements(nindex)

have_com=0b
have_p1=have_tag(nindex,'p1_x')
if not have_p1 then begin
 if have_tag(nindex,'comment') then begin
  stc=stc_key(nindex[0].comment)
  have_com=have_tag(stc,'p1_x')
  if not have_com then return,nindex
 endif
endif

for i=0,np-1 do begin
 point=call_function('eit_point',nindex[i].date_obs,nindex[i].wavelnth)
 point=comdim2(point)
 sx=point[0]
 sy=point[1]

 if have_p1 then begin
  fx=(nindex[i].p1_x+nindex[i].p2_x)/2.
  fy=(nindex[i].p1_y+nindex[i].p2_y)/2.
 endif

 if have_com then begin
  stc=stc_key(nindex[i].comment)
  fx=(float(stc.p1_x)+float(stc.p2_x))/2.
  fy=(float(stc.p1_y)+float(stc.p2_y))/2.
 endif

 nindex[i].xcen=(fx-sx)*cdelt
 nindex[i].ycen=(fy-sy)*cdelt
endfor

;-- update CRPIX/CRVAL

nindex=rep_tag_value(nindex,0.,'crval1')
nindex=rep_tag_value(nindex,0.,'crval2')
nindex=rep_tag_value(nindex,0.,'crpix1')
nindex=rep_tag_value(nindex,0.,'crpix2')

nindex.crpix1=comp_fits_crpix(nindex.xcen,nindex.cdelt1,nindex.naxis1,nindex.crval1)
nindex.crpix2=comp_fits_crpix(nindex.ycen,nindex.cdelt2,nindex.naxis2,nindex.crval2)

temp=rindex[do_partial]
struct_assign,nindex,temp
rindex[do_partial]=temporary(temp)

;-- update history

np=n_elements(rindex)
history=rindex[0].history
nhist=n_elements(history)
rhistory=strarr(nhist+1,np)
rhistory[0:nhist-1,*]=rindex.history
rhistory[nhist,do_partial]='Pointing update'
rindex=rep_tag_value(rindex,strarr(nhist+1),'history')
rindex.history=rhistory

;-- return HEADER in keyword output

if arg_present(header) then header=struct2fitshead(rindex(np-1)) 

return,rindex

end

