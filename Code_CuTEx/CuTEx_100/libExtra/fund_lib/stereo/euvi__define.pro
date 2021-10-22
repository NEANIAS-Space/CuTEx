;+
; Project     : STEREO
;
; Name        : EUVI__DEFINE
;
; Purpose     : Define a EUVI data/map object
;
; Category    : Objects
;
; Syntax      : IDL> a=obj_new('euvi')
;
; History     : Written 11 November 2007, D. Zarro (ADNET)
;
; Contact     : dzarro@solar.stanford.edu
;-
;----------------------------------------------------------------------

function euvi::search,tstart,tend,_ref_extra=extra

return,self->secchi::search(tstart,tend,_extra=extra,det='euvi')

end

;------------------------------------------------------------------------
;-- EUVI data structure

pro euvi__define,void                 

void={euvi, inherits secchi}

return & end
