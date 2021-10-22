;+
; Project     : HINODE/EIS
;
; Name        : GOES_SERVER
;
; Purpose     : return available GOES data server
;
; Category    : synoptic sockets
;
; Inputs      : None
;
; Outputs     : SERVER = GOES server name
;
; Keywords    : SDAC = return server for SDAC GOES FITS files
;
; History     : Written 16-Nov-2006, Zarro (ADNET/GSFC)
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

function goes_server,_ref_extra=extra,sdac=sdac

return,keyword_set(sdac)?goes_sdac_server(_extra=extra):goes_yohkoh_server(_extra=extra)

end
