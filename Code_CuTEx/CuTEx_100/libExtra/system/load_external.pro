;+
; Project     : Hinode/EIS
;
; Name        : load_external
;
; Purpose     : Load platform/OS appropriate shareable object
;
; Category    : utility system
;
; Syntax      : IDL> load_external
;
; Inputs      : None
;
; Outputs     : None (though SSW_EXTERNAL_F is redefined)
;
; Keywords    : None
;
; History     : Written, 14-Feb-2007, Zarro (ADNET)
;-

pro load_external

;-- bail out if Windows or SSW_EXTERNAL_F exists

if os_family(/lower) eq 'windows' then return
chk=getenv('SSW_EXTERNAL_F')
if file_test(chk) then return
share_obj=ssw_bin('external.so')
if file_test(share_obj[0]) then mklog,'SSW_EXTERNAL_F',share_obj

return & end
