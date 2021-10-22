;+
; Project     : SOHO-CDS
;
; Name        : MK_8BIT
;
; Purpose     : scale image to 8 bit color table
;
; Category    : imaging
;
; Syntax      : mk_8bit,image,r,g,b
;
; Inputs      : IMAGE = input image
;               R,G,B = color table vectors
;
; Outputs     : IMAGE8 = scaled image
;
; Keywords    : None
;
; History     : Written 21 Oct 2000, D. Zarro, EIT/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function mk_8bit,image,r,g,b,_extra=extra

dim=data_chk(image,/ndim)
if dim ne 3 then begin
 message,'input image must be 3-d',/cont
 pr_syntax,'image8=mk_8bit(image,r,g,b)
 return,''
endif

;-- use current internal color table if one not provided

if n_params(0) ne 4 then tvlct,r,g,b,/get

image8 = color_quan(image, 1, r, g, b, colors=!d.table_size, _extra=extra)

;- Sort the color table from darkest to brightest
      
table_sum = total([[long(r)], [long(g)], [long(b)]], 2)
table_index = sort(table_sum)
image_index = sort(table_index)
r = r[table_index]
g = g[table_index]
b = b[table_index]
oldimage = image8
image8[*] = image_index[temporary(oldimage)]

return, image8
end

