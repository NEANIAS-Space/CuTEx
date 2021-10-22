;+
; NAME:
;       LENGTH
;
; PURPOSE:
;       Find the length of a vector (or the longest dimension of an array).
;
; CATEGORY:
;       Array
;
; CALLING SEQUENCE:
;       result = Length(array)
;
; INPUTS:
;       array:  a vector or array.
;
; OUTPUTS:
;       result: the length of the input vector (if the input is an
;               array with more than 1 dimension then result is the 
;               length of the longest dimension of 'array').
;
; EXAMPLE:
;   IDL>        a = StrArr(10)                                   
;   IDL>        Print,Length(a)
;            10
;   IDL>        b = FltArr(10,10,100)
;   IDL>        Print,Length(b)
;            100       
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-06-09.
;-

function Length,vec
; Returns the length of a vector (or array) based on the assumption 
; that the largest index represents the length

  s = Size(vec)
  len = Max(s[1:s[0]])
  
  Return,len
end
