;+
; Project     : HESSI
;
; Name        : GOES_SAT
;
; Purpose     : convenient list of GOES satellite names
;
; Category    : synoptic gbo
;
; Syntax      : IDL> print, goes_sat()
;
; Inputs      : None
;
; Outputs     : GOES10 GOES8 GOES9 GOES7 GOES6
;
; Keywords    : NUMBER = return GOES satellite numbers
;
; History     : Written 18 June 2002, D. Zarro, LAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
; Modifications:
;   19-Apr-2007, Kim.  Added goes 11.
;-


function goes_sat,index,number=number

sats=['12','11','10','9','8','7','6']
if keyword_set(number) then gsat=fix(sats) else gsat='GOES'+sats
nsat=n_elements(gsat)
if is_number(index) then return, gsat(0 > index < (nsat-1)) else return,gsat

end

