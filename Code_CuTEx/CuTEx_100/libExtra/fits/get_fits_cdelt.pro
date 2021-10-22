;+
; Project     : SOHO, YOHKOH
;
; Name        : GET_FITS_CDELT
;
; Purpose     : Get FITS CDELT values from header
;
; Category    : imaging, FITS
;
; Explanation : Try to determine FITS scaling info
;
; Syntax      : get_fits_time,stc,time
;
; Inputs      : STC - FITS header in structure format (such as from HEAD2STC)
;
; Outputs     : CDELT1, CDELT2 - image pixel scaling
;
; Keywords    : TIME - optional image time (if already determined)
;               ERR  - error message
;
; History     : Written, 15 November 1998, D.M. Zarro (SM&A)
;
; Contact     : dzarro@solar.stanford.edu
;-


pro get_fits_cdelt,stc,cdelt1,cdelt2,time=time,err=err

err=''

if not is_struct(stc) then begin
 err='Input argument error'
 pr_syntax,'get_fits_cdelt,stc,cdelt1,cdelt2'
 return
endif

cdelt1=1. & cdelt2=1.

;-- look in the obvious places first

stc=rep_tag_name(stc,'cdel1','cdelt1',/quiet)
stc=rep_tag_name(stc,'cdel2','cdelt2',/quiet)

stc=rep_tag_name(stc,'dxb_img','cdelt1',/quiet)
stc=rep_tag_name(stc,'dyb_img','cdelt2',/quiet)

cdelt1=float(gt_tagval(stc,/cdelt1,found=found1,missing=1.))
cdelt2=float(gt_tagval(stc,/cdelt2,found=found2,missing=1.))

;-- try to figure it from any radius information stored in FITS file

if (not found1) or (not found2) then begin
 soho=strpos(string(gt_tagval(stc,/telescop)),'SOHO') ne -1 
 rad=gt_tagval(stc,'radius',found=found3)
 if not found3 then rad=gt_tagval(stc,'solar_r',found=found3)
 if found3 then begin
  terr=''
  dtime=anytim2utc(time,err=terr)
  if terr ne '' then get_fits_time,stc,dtime,/current
  h=float(pb0r(dtime,/arc,soho=soho(0)))
  cdelt1=h(2)/float(rad)
  cdelt2=cdelt1
 endif
endif

if (cdelt1(0) eq 0.) or (cdelt2(0) eq 0.) then $
 err='Could not determine FITS scaling'

return & end

