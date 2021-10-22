;+
; Project     : HESSI
;
; Name        : MAP2EARTH
;
; Purpose     : convert SOHO-view map to Earth-view
;
; Category    : imaging
;
; Syntax      : emap=map2earth(map)
;
; Inputs      : MAP = image map structure
;
; Outputs     : EMAP = remapped structure 
;
; Opt. Outputs: None
;
; Keywords    : None
;
; History     : Written 18 Oct 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-


function map2earth,map

return,map2l1(map,/inverse)

end


