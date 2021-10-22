;+
; Project     : SOHO, YOHKOH
;
; Name        : COMP_FITS_CEN
;
; Purpose     : compute XCEN (or YCEN) from FITS standard keywords
;
; Category    : imaging, FITS
;
; Explanation : 
;
; Syntax      : xcen=comp_fits_cen(crpix1,cdelt1,naxis1,crval1)
;                                   OR
;               ycen=comp_fits_cen(crpix2,cdelt2,naxis2,crval2)
; Examples    :
;
; Inputs      : CRPIX = reference pixel coordinate
;               CDELT = pixel scaling
;               NAXIS = pixel dimension of image
;
; Opt. Inputs : CRVAL = reference data coordinate [def=0]

; Outputs     : CEN = center of FOV in data units
;
; History     : Written, 15 November 1998, D.M. Zarro (SM&A)
;
; Contact     : dzarro@solar.stanford.edu
;-

function comp_fits_cen,crpix,cdelt,naxis,crval

present=exist(naxis) and exist(crpix) and exist(cdelt)

if not present then begin
 pr_syntax,'cen=comp_fits_cen(crpix,cdelt,naxis [,crval])'
 return,0.
endif

if not exist(crval) then crval=0.

cen=float(crval)+float(cdelt)*( (float(naxis)+1.)/2. - float(crpix))

return,cen
end

