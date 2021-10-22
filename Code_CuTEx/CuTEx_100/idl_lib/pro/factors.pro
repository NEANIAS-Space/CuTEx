;+
; NAME:
;	FACTORS
;
; PURPOSE:
;	This function calculates the prime factors of natural number.
;
; CATEGORY:
;	Mathematics
;
; CALLING SEQUENCE:
;	Result = FACTORS( X )
;
; INPUTS:
;	X:  A positive number, of type integer, to be factored.
;
; OUTPUT:
;	Result:  A vector containing the factors of X.
;
; USES:
;	PRODUCT.pro
;
; PROCEDURE:
;	This function recursively tries dividing the input number by
;	factors until it finally has them all.
;
; EXAMPLE:
;	Calculate the factors of 28.
;	  result = factors( 28 )
;	Returns [2,2,7]
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-08-24.
;-

;***********************************************************************

FUNCTION FACTORS, X

;***********************************************************************
;Variables

;The input number to factor
num = x

;Output
fact = [1]

;***********************************************************************
;Factor the Number

;Even numbers
if num gt 1 then begin
  val = 2
  check = 0
  while check eq 0 do begin
    check = round( val*(num/val-1.*num/val) )
    if check eq 0 then begin
      fact = [fact,val]
      num = num / val
    endif
  endwhile
endif

if num gt 1 then begin
  val = 3
  while num gt 1 do begin
    ;Make sure Val is not a product of values in Fact
    check = 0
    nfact = n_elements(fact)
    for i=1,nfact-2 do begin
      check1 = round( fact[i]*(val/fact[i]-1.*val/fact[i]) )
      if (check1 eq 0) and (fact[i] ne fact[nfact-1]) then check = 1
    endfor
    ;Check if Val is a factor of Num
    if (check eq 0) and (num ge val) then begin
      check2 = round( val*(num/val-1.*num/val) )
      if check2 eq 0 then begin
        fact = [fact,val]
        num = num / val
      endif else begin
        val = val + 2
      endelse
    endif else begin
      val = val + 2
    endelse
  endwhile
endif

;Remove leading "1" in Fact
if n_elements(fact) ne 1 then fact = fact[1:n_elements(fact)-1]

;***********************************************************************
;The End

return, fact
END
