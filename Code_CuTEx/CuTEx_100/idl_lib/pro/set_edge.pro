;+
; NAME:
;    SET_EDGE
;
; PURPOSE:
;    Fill the boundary of a two dimensional array with a value.
;
; CATEGORY:
;    Miscellaneous
;
; CALLING SEQUENCE:
;    result = Set_Edge(array,value)
;
; INPUTS:
;    array: the two-dimensional array to be changed.
;    value: the value to put into the border of the array.
;
; OUTPUTS:
;    result: the modified array.
;
; EXAMPLE:
;    a = Set_Edge(FltArr(10,10),1.5)
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-04-11.
;-

function Set_Edge, arr, value

  s = Size(arr)
  tmp = arr
  if (s[0] eq 2) then begin
    x=s[1]
    y=s[2]

    tmp[[0,x-1],0:y-1] = value
    tmp[0:x-1,[0,y-1]] = value
  endif

  Return,tmp
end
