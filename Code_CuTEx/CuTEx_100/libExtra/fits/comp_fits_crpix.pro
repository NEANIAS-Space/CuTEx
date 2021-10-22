;+
; Project     : SOHO, YOHKOH
;
; Name        : COMP_FITS_CRPIX
;
; Purpose     : Compute CRPIX from FOV center 
;
; Category    : imaging, FITS
;
; Explanation : 
;
; Syntax      : crpix=comp_fits_crpix(cen,cdelt,naxis,crval)
;
; Examples    :
;
; Inputs      : CEN = data coordinate of FOV center
;               CDELT = pixel scaling
;               NAXIS = pixel dimension of image
;
; Opt. Inputs : CRVAL = reference data coordinate [def=0]
;
; Opt. Inputs : None
;
; Outputs     : CRPIX = reference pixel coordinate
;
; History     : Written, 15 November 1998, D.M. Zarro (SM&A)
;
; Contact     : dzarro@solar.stanford.edu
;-

function comp_fits_crpix,cen,cdelt,naxis,crval

present=exist(naxis) and exist(cdelt)

if not present then begin
 pr_syntax,'crpix=comp_fits_crpix(cen,cdelt,naxis [,crval])'
 return,0.
endif

if not exist(cen) then cen=0.
if not exist(crval) then crval=0.

crpix=(float(naxis)+1.)/2. - (float(cen)-float(crval))/float(cdelt)

return,crpix

end

