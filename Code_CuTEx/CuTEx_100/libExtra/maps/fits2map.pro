;+
; Project     : SOHO-CDS
;
; Name        : FITS2MAP
;
; Purpose     : Make an image map from a FITS file
;
; Category    : imaging
;
; Syntax      : fits2map,file,map
;
; Inputs      : FILE = FITS file name (or FITS data + HEADER)
;
; Outputs     : MAP = map structure
;
; Keywords    : SUB = [x1,x2,y1,y2] = indicies of subarray to extract
;               SOHO = designate as SOHO-viewed
;               OUTSIZE = image dimensions of map
;               HEADER = FITS header (output of last file read)
;               OLD_WAY = use old read method
;               OBJECT = return list of map objects
;               APPEND = append map objects to existing list
;
; History     : Written 22 January 1998, D. Zarro, SAC/GSFC
;               Modified, 22 April 2000, Zarro (SM&A/GSFC)
;               Modified, 1 April 2005, Zarro (L-3Com/GSFC) 
;                - accounted for 180 degree roll
;
; Contact     : dzarro@solar.stanford.edu
;-

pro fits2map,file,map,err=err,$
                  _extra=extra,header=fhead,sub=sub,old_way=old_way,$
                  object=object,append=append,extension=extension

err=''

;-- check inputs

if datatype(file) ne 'STR' then begin
 err='Invalid filename input'
 pr_syntax,'fits2map,file,map'
 return
endif


if not exist(extension) then extension=0 
delvarx,map
nfiles=n_elements(file)
for i=0,nfiles-1 do begin
 chk=loc_file(file[i],count=count,err=err)
 if count gt 0 then begin
  data=mrdfits(file[i],extension,fhead,_extra=extra,/fscale)

  crota=0.
  if have_tag(index,'crot',pos,/exact) then crota=abs(index.(pos)) else $
   if have_tag(index,'crota1',pos,/exact) then crota=abs(index.(pos)) else $
    if have_tag(index,'crota',pos,/exact) then crota=abs(index.(pos)) else $
     if have_tag(index,'sc_roll',pos,/exact) then crota=abs(index.(pos))
  roll_correct=abs(crota) eq 180

  if roll_correct then begin
   message,'Applying 180 degree roll correction',/cont
   data=rotate(temporary(map.data),2)
   index=rot_fits_head(index)
  endif

  index=fitshead2struct(fhead)
  index2map,index,data,imap,sub=sub,err=err,/no_copy,_extra=extra
  if roll_correct then begin
   map.roll_angle=0.
   map.roll_center=[map.xc,map.yc]
  endif
  map=merge_struct(map,imap)
 endif
endfor


return & end
