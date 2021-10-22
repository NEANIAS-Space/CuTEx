function Month_Num,month
;+
; NAME:
;         MONTH_NUM
;
; PURPOSE:
;        This function returns the month number given the name.
;
; CATEGORY:
;        Calendar  
;
; CALLING SEQUENCE:
;        Result = MONTH_NUM( name )
;
; INPUTS:
;       name:  the name of the month.  A text string at least three
;       characters long and at least the first three characters of the
;       months name.  name can be an array of strings or a scalar
;       string. 
;
; OUTPUTS:
;       Result:  The month's number (1-12). Returns -1 if the name of
;       the month does not match sufficiently to guarantee an answer.
;
; EXAMPLE:
;       num = Month_Num('Aug')
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 1999-10-14
;-


  names = 'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'

  n = N_Elements(month)
  if (n gt 0) then result = IntArr(n)-1 else result = -1

  if (Var_Type(month) eq 7) then begin
    for n=0L,n-1 do begin
      result[n] = Floor(StrPos(names,StrUpCase(StrMid(month[n],0,3)))/3.)
    endfor
    indx = Where(result ge 0,cnt)
    if (cnt gt 0) then result[indx] = result[indx] + 1
  endif else begin
    Print,'MONTH_NUM: argument must be a string.'
  endelse
 
  Return,result
end
