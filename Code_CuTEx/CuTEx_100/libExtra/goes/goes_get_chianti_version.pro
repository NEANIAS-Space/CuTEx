;+
; Project:
;     SDAC
; Name:
;     GOES_GET_CHIANTI_VERSION
;
; Usage:
;     print, goes_get_chianti_version()
;
;Purpose:
;     Return chianti version used in goes_get_chianti_temp and goes_get_chianti_em
;
;Category:
;     GOES, SPECTRA
;
;Method:
;     Anyone who updates goes_get_chianti_temp and goes_get_chianti_em to use a newer
;     version of chianti should modify this appropriately.

; MODIFICATION HISTORY:
;     Kim Tolbert, 13-Dec-2005
;     Kim, 30-Nov-2006.  Changed to 5.2 after onlining new tables from S. White
;
;-
;-------------------------------------------------------------------------

function goes_get_chianti_version

return, '5.2'

end