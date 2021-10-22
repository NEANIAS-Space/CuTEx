;+
; Project     : HESSI     
;                   
; Name        : UT_DIFF
;               
; Purpose     : compute difference between local and UT time
;               
; Category    : time utility
;               
; Syntax      : IDL> print,ut_diff()
;
; Inputs      : None
;               
; Outputs     : hours difference between local and UT time
;               
; Keywords    : None
;               
; History     : 11-Nov-2002, Zarro (EER/GSFC)  Written
;     
; Contact     : dzarro@solar.stanford.edu
;-

function ut_diff

;-- compute hours difference between local and UT. If negative, we must be
;   east of Greenwich

diff=(systime(/julian,/sec)-systime(/julian,/utc))*24.

return, diff
end
