;---------------------------------------------------------------------------
; Document name: axis__define.pro
; Created by:    Andre_Csillaghy, August 21, 2003
;
; Time-stamp: <Mon Sep 15 2003 11:03:20 csillag soleil.ifi.fh-aargau.ch>
;---------------------------------------------------------------------------
;
;+
; PROJECT:
;       HESSI
;
; NAME:
;       AXIS__DEFINE
;
; PURPOSE:
;
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;       axis__define,
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; OPTIONAL OUTPUTS:
;       None.
;
; KEYWORDS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; PROCEDURE:
;
; RESTRICTIONS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; EXAMPLES:
;
;
; SEE ALSO:
;
; HISTORY:
;       Version 1, August 21, 2003,
;           A Csillaghy, csillag@ssl.berkeley.edu
;
;--------------------------------------------------------------------------
;

function axis, axis, _extra = _extra

return, obj_new( 'axis', axis, _extra = _extra )

end