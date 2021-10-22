;+
; NAME:
;       SWAP
;
; PURPOSE:
;       This procedure will swap the values of two variables.
;     
; CATEGORY:
;       Miscellaneous
;
; CALLING SEQUENCE:
;       Swap, var1, var2 
;
; INPUTS:
;       var1:   an IDL variable
;       var2:   an IDL variable
;
; OUTPUTS:
;       var1:   an IDL variable (now containing the value of var2)
;       var2:   an IDL variable (now containing the value of var1)
;
; PROCEDURE:
;       var1 and var2 are swapped using a temporary variable.
;
; EXAMPLE:
;       a = FIndGen(10)
;       b = FIndGen(100)
;       Swap,a,b
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-02-12.
;-

pro Swap, v1, v2

  tmp = Temporary(v1)
;  tmp = v1
  v1 = Temporary(v2)
  v2 = Temporary(tmp)

  Return
end
