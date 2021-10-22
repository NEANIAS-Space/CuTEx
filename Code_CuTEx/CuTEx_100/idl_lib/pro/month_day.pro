;+
; NAME:
;	MONTH_DAY
;
; PURPOSE:
;	This function returns the first and last days of the month.
;
; CATEGORY:
;	Calendar
;
; CALLING SEQUENCE:
;	Result = MONTH_DAY( Index )
;
; INPUTS:
;	Index:  The index number of the calendar month (0-11), of
;	        type integer.  Can be a scalar or array.
;
; OUTPUTS:
;	Result:  Returns the index number, with respect to the calendar year,
;	         of the first and last days of the specified month.
;
; KEYWORD PARAMETERS:
;	ALL:      If set, the index values of all of the days in the month(s)
;                 will be returned.  Otherwise, only the first and last are
;                 returned.
;       LEAPYEAR: If set, the first and last days will be for months in leap
;                 years.
;
; USES:
;
; PROCEDURE:
;	This function returns a vector with the index number, with respect to
;	the calendar year, of the first and last days of the month "Index."
;
; EXAMPLE:
;	Get the end days of calendar month #1 (i.e. February).
;	  result = MONTH_DAY( 1 )
;	Result = [ 31, 58 ]
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2002-01-13.
;       Modified:       Benjamin S. Grandey, 2010-02-05 (added LEAPYEAR
;                       keyword).
;-

;***********************************************************************

FUNCTION MONTH_DAY, Index, $
                    ALL=allopt, LEAPYEAR=leapyearopt

;***********************************************************************
;Variables and Constants

;Number of months in a year
constants, mina=mina

;Number of months entered
n = n_elements(index)

;Convert Index to the 0 to 11 interval
months = index - mina * floor( 1. * index / mina )

;Return all days option
allopt = keyword_set(allopt)

;Return leap year option
leapyearopt = keyword_set(leapyearopt)

;Vector of index values of the first days of each month
smday = [0,31,59,90,120,151,181,212,243,273,304,334]

;Vector of index values of the last days of each month
emday = [30,58,89,119,150,180,211,242,272,303,333,364]

;Correct vectors of index values for leap years
if leapyearopt then begin
  smday[2:11] = smday[2:11]+1
  emday[1:11] = emday[1:11]+1
endif

;***********************************************************************
;Calculate Month Days

;First and last days
if not(allopt) then begin
  days = intarr(2,n)
  for i=0,n-1 do days[*,i] = [ smday[months[i]], emday[months[i]] ]
endif

;All days
if allopt then begin
  days = [0]
  for i=0,n-1 do begin
    days = [ days, $
             smday[months[i]] + indgen(emday[months[i]]-smday[months[i]]+1) ]
  endfor
  days = days[1:n_elements(days)-1]
endif

;***********************************************************************
;The End

return, days
END
