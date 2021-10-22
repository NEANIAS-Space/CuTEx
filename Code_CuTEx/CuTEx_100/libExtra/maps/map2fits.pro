;+
; Project     : SOHO-CDS
;
; Name        : MAP2FITS
;
; Purpose     : write image map to FITS file
;
; Category    : imaging
;
; Syntax      : map2fits,map,file
;
; Inputs      : MAP = image map structure
;               FILE = FITS file name
;
; Keywords    : ERR = error string
;               BYTE_SCALE = byte scale data
;
; History     : Written 22 August 1997, D. Zarro, SAC/GSFC
;               Version 2, 11-Jul-2003, William Thompson, GSFC
;                       Write both DATE_OBS and DATE-OBS
;
; Contact     : dzarro@solar.stanford.edu
;-

   pro map2fits,map,file,err=err,byte_scale=byte_scale

   err=''
   on_error,1

   if (not valid_map(map)) or (datatype(file) ne 'STR') then begin
    pr_syntax,'map2fits,map,file'
    return
   endif

   np=n_elements(map)
   if np ne n_elements(file) then begin
    err='Output file must be string array since map contains multiple images'
    message,err,/cont
    return
   endif

;-- form output file name

   cd,curr=cdir

   for i=0,np-1 do begin
    break_file,file(i),dsk,dir,dfile,dext
    if trim(dsk+dir) eq '' then fdir=cdir else fdir=dsk+dir
    if test_open(fdir,/write,err=err) then begin
;     if trim(dext) eq '' then fext='.fts' else fext=dext
     fext=dext
     filename=concat_dir(fdir,dfile)+fext

;-- unpack data

     unpack_map,map(i),data,xp,yp,dx=cdelt1,dy=cdelt2,xc=xcen,yc=ycen,err=err,$
      nx=naxis1,ny=naxis2

;-- scale data?

     if keyword_set(byte_scale) then bscale,data,top=255

;-- add header for the output array.

     fxhmake,header,data,/date

;-- add FITS parameters CRPIX, CRVAL, etc.

     crpix1=comp_fits_crpix(xcen,cdelt1,naxis1,crval1)
     crpix2=comp_fits_crpix(ycen,cdelt2,naxis2,crval2)

     fxaddpar, header, 'ctype1', 'solar_x','Solar X (cartesian west) axis'
     fxaddpar, header, 'ctype2', 'solar_y','Solar Y (cartesian north) axis'

     fxaddpar, header, 'cunit1', 'arcsecs','Arcseconds from center of sun'
     fxaddpar, header, 'cunit2', 'arcsecs','Arcseconds from center of sun'

     fxaddpar, header, 'crpix1', crpix1, 'Reference pixel along X dimension'
     fxaddpar, header, 'crpix2', crpix2, 'Reference pixel along Y dimension'

     fxaddpar, header, 'crval1',0, 'Reference position along X dimension'
     fxaddpar, header, 'crval2',0, 'Reference position along Y dimension'

     fxaddpar, header, 'cdelt1',cdelt1,'Increments along X dimension'
     fxaddpar, header, 'cdelt2',cdelt2,'Increments along Y dimension'

     fxaddpar,header,'date_obs',map(i).time,'Observation date'
     fxaddpar,header,'date-obs',map(i).time,'Observation date'
     if tag_exist(map,'dur') then fxaddpar,header,'exptime',map(i).dur,'Exposure duration'
     fxaddpar,header,'origin',map(i).id,'Data description'

     if tag_exist(map,'soho') then begin
      if map(i).soho then fxaddpar,header,'telescope','SOHO','Telescope'
     endif

;-- write out the file
   
     fxaddpar, header, 'filename', filename
     fxwrite, filename, header,data
    endif
   endfor

   return & end
