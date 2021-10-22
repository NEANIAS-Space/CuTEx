;+
; Project     : SOHO-CDS
;
; Name        : VALID_MAP
;
; Purpose     : check if input image map is of valid type
;
; Category    : imaging
;
; Syntax      : valid=valid_map(map)
;
; Inputs      : MAP = image map structure
;
; Outputs     : VALID = 1/0 if valid/invalid
;
; Keywords    : OLD_FORMAT = 1/0 if using old .xp, .yp format or not
;
; History     : Written 22 October 1997, D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function valid_map,map,err=err,old_format=old_format

valid=0b
err='missing or invalid input map'

;-- check if true MAP object (IDL > 5)

sz=size(map)
dtype=sz[n_elements(sz)-2]
if dtype eq 11 then begin
 if not call_function('obj_valid',map(0)) then return,valid
 if obj_class(map[0]) ne 'MAP' then return,valid
 valid=valid_map(map[0]->get(),old_format=old_format)
 return,valid
endif

;-- otherwise check for required tags

if dtype ne 8 then return,valid
if not tag_exist(map,'DATA') then return,valid
if not tag_exist(map,'TIME') then return,valid

old_format=tag_exist(map,'xp') and tag_exist(map,'yp')
if not old_format then begin
 if not tag_exist(map,'XC') then return,valid
 if not tag_exist(map,'YC') then return,valid
 if not tag_exist(map,'DX') then return,valid
 if not tag_exist(map,'DY') then return,valid
endif

;-- if we made it here then we're ok

err=''
valid=1b
return,valid & end

