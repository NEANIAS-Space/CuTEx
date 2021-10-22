;+
; NAME:
;   Nearest_Index
;
; PURPOSE:
;   Search for the index of the value nearest to x in the vector mx.
;
; CATEGORY:
;   Array
;
; CALLING SEQUENCE:
;   result = Nearest_Index(x,xarray)
;
; INPUTS:
;   x:  a value in the range [Min(xarray),Max(xarray)]
;   xarray:  an idl array (typically a vector)
;
; OUTPUTS:
;   result:  The index of the value in xarray that is closest to x.
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-11-29.
;-
function Nearest_Index, x, mx
;  old method
;  ix   = -1 
;; This will cause an error if it gets returned and 
;; used as an array reference
;  diff = 1E10 
;  for i = 0L, N_Elements(mx)-1 do begin
;    if (Abs(x-mx[i]) lt diff) then begin
;      diff = Abs(x-mx[i])
;      ix1 = i
;    endif
;  endfor
; new method (10 times faster for xarray of length 100 -- this
;  improvement will become more important for longer vectors)
  xx = Abs(mx-x)
  ix = (Where(xx eq Min(xx),cnt))[0]
  Return, ix
end


