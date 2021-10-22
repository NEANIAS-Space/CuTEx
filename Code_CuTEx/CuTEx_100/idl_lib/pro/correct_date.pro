;+
; NAME:
;    CORRECT_DATE
;
; PURPOSE:
;    This function returns a corrected version of the input date, when the days 
;    are out of the range for the month.
;
; CATEGORY:
;    Calendar
;
; CALLING SEQUENCE:
;    Result = CORRECT_DATE( Year, Month, Day )
;    Result = CORRECT_DATE( Date )
;
; INPUTS:
;    Day:  The input day in the string format 'DD' or integer format DD.  Can 
;        be a vector of length N_DATE.
;    Date/Year:  If Month and Day are not input, then this is the input date in 
;        the string format 'YYYY-MM-DD' or the long integer format YYYYMMDD.  
;        If Month and Day are input, then this is the input year in the string 
;        format 'YYYY' or integer format YYYY.  Can be a vector of length 
;        N_DATE.
;    Month:  The input month in the string format 'MM' or integer format MM.  
;        Can be a vector of length N_DATE.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    Result:  The corrected output date(s).  If Month and Day are not given 
;        then it returns the date in the same format as Date;  otherwise if 
;        Year is a string then it returns the date as a string in the form 
;        'YYYY-MM-DD', or if Year is an integer then it returns the date as an 
;        integer in the form YYYYMMDD.
;
; USES:
;    month_day.pro
;    str.pro
;    var_type.pro
;
; PROCEDURE:
;    This function iterates through alterations to the date until it fits the 
;    standard format.  This currently assumes non-leap years.
;
; EXAMPLE:
;    The following should give Result = '2009-11-01'.
;      Result = correct_date( '2009-10-23' )
;
; MODIFICATION HISTORY:
;    Written by:  Daithi A. Stone, 2009-11-23.
;    Modified:    Benjamin S. Grandey, 2010-02-05 (can now handle leap years).
;    Modified:    DAS, 2010-02-23 (fixed bug with leap years implementation 
;        that did not allow vector input)
;-

;***********************************************************************

FUNCTION CORRECT_DATE, $
    Year, Month, Day

;***********************************************************************
; Constants and Options

; The number of months in a year
mina = 12

; Determine input size
n_date = n_elements( year )

; Determine date format and extract year, month, and day.
; If Year, Month, and Day are input
if keyword_set( month ) then begin
  year_out = fix( year )
  month_out = fix( month )
  day_out = fix( day )
  ; If the input is of type string
  if var_type( year ) eq 7 then begin
    string_opt = 1
    dash_opt = 1
  ; If the input is of type integer
  endif else begin
    string_opt = 0
  endelse
; If the date is given in Year
endif else begin
  ; If the input is of type string
  if var_type( year ) eq 7 then begin
    string_opt = 1
    temp = strsplit( year[0], '-', extract=1, count=n_temp )
    ; If the date format is 'YYYYMMDD'
    if n_temp eq 1 then begin
      year_out = fix( strmid( year, 0, 4 ) )
      month_out = fix( strmid( year, 4, 2 ) )
      day_out = fix( strmid( year, 6, 2 ) )
      dash_opt = 0
    ; If the date format is 'YYYY-MM-DD'
    endif else begin
      year_out = intarr( n_date )
      month_out = intarr( n_date )
      day_out = intarr( n_date )
      for i_date = 0, n_date - 1 do begin
        temp = strsplit( year[i_date], '-', extract=1 )
        year_out[i_date] = fix( temp[0] )
        month_out[i_date] = fix( temp[1] )
        day_out[i_date] = fix( temp[2] )
      endfor
      dash_opt = 1
    endelse
  ; If the input is of type integer
  endif else begin
    year_out = fix( year / 10000 )
    month_out = fix( year / 100 - year_out * 100l )
    day_out = fix( year - year_out * 10000l - month_out * 100l )
    string_opt = 0
  endelse
endelse

;***********************************************************************
; Convert unusual dates to standard format

; Iterate through dates
for i_date = 0, n_date - 1 do begin
  ; Initialise check flag
  check = 0
  ; Proceed while check flag is not set
  while check eq 0 do begin
    ; Check to see if year is a leapyear
    if ( year_out[i_date] mod 400 eq 0 ) or $
        ( ( year_out[i_date] mod 4 eq 0 ) $
        and ( year_out[i_date] mod 100 ne 0 ) ) then begin
      leapyear_opt = 1
    endif else begin
      leapyear_opt = 0
    endelse
    ; Determine number of days in the input month
    dinm = month_day( fix( month_out[i_date] ) - 1, LEAPYEAR=leapyear_opt )
    dinm = dinm[1] - dinm[0] + 1
    ; If Day is larger than the number of days in the month
    if day_out[i_date] gt dinm then begin
      day_out[i_date] = day_out[i_date] - dinm
      month_out[i_date] = month_out[i_date] + 1
      if month_out[i_date] gt mina then begin
        year_out[i_date] = year_out[i_date] + 1
        month_out[i_date] = 1
      endif
    ; If Day is less than one
    endif else if day_out[i_date] lt 1 then begin
      dinm = month_day( fix( month_out[i_date] ) - 2, LEAPYEAR=leapyear_opt )
      dinm = dinm[1] - dinm[0] + 1
      day_out[i_date] = day_out[i_date] + dinm
      month_out[i_date] = month_out[i_date] - 1
      if month_out[i_date] le 0 then begin
        year_out[i_date] = year_out[i_date] - 1
        month_out[i_date] = mina
      endif
    ; If this is an acceptable date then flag us out of loop
    endif else begin
      check = 1
    endelse
  endwhile
endfor

;***********************************************************************
; Convert result to output format

; If we want integer output
if string_opt eq 0 then begin
  result = year_out * 10000l + month_out * 100l + day_out * 1l
; If we want string output
endif else begin
  year_out = str( year_out )
  month_out = str( month_out, length=2, filler='0' )
  day_out = str( day_out, length=2, filler='0' )
  ; If we want dashes
  if dash_opt eq 1 then begin
    result = year_out + '-' + month_out + '-' + day_out
  ; If we do not want dashes
  endif else begin
    result = year_out + month_out + day_out
  endelse
endelse

;***********************************************************************
; The End

return, result
END
