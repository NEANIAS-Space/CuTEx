;+
; NAME:
;      STRMERGE
;
; PURPOSE:
;      Merge two string arrays. 
;
; CATEGORY:
;      Strings
;
; CALLING SEQUENCE:
;      result = StrMerge(s1,s2)
;
; INPUTS:
;      s1:  a string array 
;      s2:  a string array
;
; KEYWORDS:
;      IN_BOTH: If this keyword is set only strings that are present in
;               both s1 and s2 will remain in the result.
; 
; OUTPUTS:
;      result:  The combination of the input string arrays, sorted.
;
; SIDE EFFECTS:
;      The output array (result) is sorted (by IDL's SORT and UNIQ) functions.
;
; EXAMPLE:
;      IDL> a=String([1,2,3,4,5,6,7,8,9,0])
;      IDL> b=String([5,6,7,10,11,12,13,14,15])
;      IDL> print,strmerge(a,b)
;         0        1        2        3        4        5        6        7
;          8        9       10       11       12       13       14       15
;      IDL> print,strmerge(' hello',b)
;       5        6        7       10       11       12       13       14
;       15  hello
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-05-16.
;       Modified by:    Edward C. Wiebe, 2002-07-23 (Added IN_BOTH keyword.)
;-
function StrMerge, s1, s2, IN_BOTH = in_both

; merge two string arrays retaining only single copies of matching
; array elements.

  tmp  =[s1,s2]
  result = tmp[Sort(tmp)]

  if (Keyword_Set(in_both)) then Return,result[NonUniq(result)] $
  else Return,result[Uniq(result)]

end
