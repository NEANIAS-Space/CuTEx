;+
; NAME:
;    SEASON_NAME
;
; PURPOSE:
;    This function returns the initials of the months contained in a given 
;    season.
;
; CATEGORY:
;    Calendar
;
; CALLING SEQUENCE:
;    Result = season_name( Season [, Len_season] )
;
; INPUTS:
;    Season:  The index (January=0,...) value of the middle month of
;        the season. 0=January, ..., 11=December
;
; OPTIONAL INPUTS:
;    Len_season:  The number of months in a season.  The default value  is 
;        "3".  If Len_Season is even the extra month is added at the end.
;
; USES:
;    -
;
; PROCEDURE:
;    This function builds the season's lettered abbreviation.
;
; EXAMPLE:
;    Get the lettered abbreviation for the summer months.
;      result = season_name( 6, 3 )
;    Result = 'JJA'
;
; MODIFICATION HISTORY:
;    Written by:  Daithi A. Stone (stoned@csag.uct.ac.za), 2000-08-30
;    Modified:    DAS, 2010-01-05 (Allowed input outside-24<season<23;  editted 
;        documentation and formatting; removed dependency on constants.pro)
;-

;***********************************************************************

FUNCTION SEASON_NAME, $
    Season, $
    Len_season

;***********************************************************************
; Constants

; Number of months in a year
mina = 12

; Default season length
if not( keyword_set( len_season ) ) then len_season = 3

;***********************************************************************
; Build Season Name

; Initialise output
result = ''

; Iterate through months in the season
for i = 0, len_season - 1 do begin
  ; Indentify this month's index
  month = season - ceil( len_season / 2. - 1 ) + i
  ; Convert month index to 0 to 11 range
  while month lt 0 do month = month + mina
  while month ge mina do month = month - mina
  ; Determine and add first letter of month's name
  if month eq 0 then result = result + 'J'
  if month eq 1 then result = result + 'F'
  if month eq 2 then result = result + 'M'
  if month eq 3 then result = result + 'A'
  if month eq 4 then result = result + 'M'
  if month eq 5 then result = result + 'J'
  if month eq 6 then result = result + 'J'
  if month eq 7 then result = result + 'A'
  if month eq 8 then result = result + 'S'
  if month eq 9 then result = result + 'O'
  if month eq 10 then result = result + 'N'
  if month eq 11 then result = result + 'D'
endfor

;***********************************************************************
; The End

return, result
END
