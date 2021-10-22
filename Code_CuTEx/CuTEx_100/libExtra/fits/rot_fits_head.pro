;+
; Project     : HESSI
;
; Name        : ROT_FITS_HEAD
;
; Purpose     : Rotate FITS header information
;
; Category    : FITS, Utility
;
; Syntax      : IDL> header=rot_fits_head(header)
;
; Inputs      : HEADER = FITS header (string or index structure format)
;
; Outputs     : HEADER with roll adjusted CRPIX, CROTA, CEN values
;
; History     : Written, 3-Feb-2004, Zarro (L-3Com/GSFC)
;               Modified, 13-Apr-2005, Zarro (L-3Com/GSFC) 
;                - fixed long to float conversion
;
; Contact     : dzarro@solar.stanford.edu
;-

function rot_fits_head,header

if not exist(header) then return,''

rheader=header

;-- handle structure header input

if is_struct(rheader) then begin
 if have_tag(rheader,'sc_roll') then rheader.sc_roll=0.
 if have_tag(rheader,'crota',/exact) then rheader.crota=0.
 if have_tag(rheader,'crota1',/exact) then rheader.crota1=0.
 if have_tag(rheader,'crot',/exact) then rheader.crot=0.
 crpix1 = header.naxis1-1.-header.crpix1
 crpix2 = header.naxis2-1.-header.crpix2
 rheader.crpix1=crpix1
 rheader.crpix2=crpix2
 xc=comp_fits_cen(crpix1,header.cdelt1,header.naxis1,header.crval1)
 yc=comp_fits_cen(crpix2,header.cdelt2,header.naxis2,header.crval2)
 if have_tag(rheader,'xcen') then rheader.xcen=xc
 if have_tag(rheader,'ycen') then rheader.ycen=yc
 if have_tag(rheader,'crotacn1') then rheader.crotacn1=xc
 if have_tag(rheader,'crotacn2') then rheader.crotacn2=yc
endif

;-- handle string header input

if is_string(rheader) then begin
 naxis1=fxpar(rheader, 'naxis1')
 naxis2=fxpar(rheader, 'naxis2')
 crpix1=fxpar(rheader,'crpix1')
 crpix2=fxpar(rheader,'crpix2')
 cdelt1=fxpar(rheader,'cdelt1')
 cdelt2=fxpar(rheader,'cdelt2')
 crval1=fxpar(rheader,'crval1')
 crval2=fxpar(rheader,'crval2')
 crpix1 = float(naxis1)-1.-float(crpix1)
 crpix2 = float(naxis2)-1.-float(crpix2)
 xcen=comp_fits_cen(crpix1,cdelt1,naxis1,crval1)
 ycen=comp_fits_cen(crpix2,cdelt2,naxis2,crval2)
 rep_fits_head,rheader,'SC_ROLL','0.0'
 rep_fits_head,rheader,'CROT','0.0'
 rep_fits_head,rheader,'CROTA','0.0'
 rep_fits_head,rheader,'CROTA1','0.0'
 rep_fits_head,rheader,'XCEN',trim(xcen,'(f10.2)')
 rep_fits_head,rheader,'YCEN',trim(ycen,'(f10.2)')
 rep_fits_head,rheader,'CRPIX1',trim(crpix1,'(f10.2)')
 rep_fits_head,rheader,'CRPIX2',trim(crpix2,'(f10.2)')
endif

return, rheader
end


