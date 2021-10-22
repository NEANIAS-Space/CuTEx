;+
; Project     : SOHO-CDS
;
; Name        : MK_24BIT
;
; Purpose     : scale image to 24 bit color table
;
; Category    : imaging
;
; Syntax      : mk_24bit,image,r,g,b
;
; Inputs      : IMAGE = input image
;               R,G,B = color table vectors
;
; Outputs     : IMAGE24 = scaled image
;
; Keywords    : NO_SCALE = set to not byte scale
;
; History     : Written 11 Jan 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function mk_24bit,image,r,g,b,no_scale=no_scale

dim=data_chk(image,/ndim)
if dim ne 2 then begin
 message,'input image must be 2-d',/cont
 pr_syntax,'image24=mk_24bit(image,r,g,b)
 return,''
endif

;-- usr current internal color table if one not provided

if n_params(0) ne 4 then tvlct,r,g,b,/get

if keyword_set(no_scale) then scaled=image else $
 scaled = bytscl(image, top=!d.table_size-1)
s = size(scaled, /dimensions)
image24 = bytarr(3, s[0], s[1])
image24[0, *, *] = r[scaled]
image24[1, *, *] = g[scaled]
image24[2, *, *] = b[scaled]

return,image24

end


