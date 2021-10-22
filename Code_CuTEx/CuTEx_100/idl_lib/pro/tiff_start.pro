;+
; NAME:
;    TIFF_START.PRO
;
; PURPOSE:
;    Prepare IDL to create a .tiff file using the z-buffer device.
;
; CATEGORY:
;    Graphics
;
; KEYWORDS:
;    ENABLE_Z: setting this keyword enables the z-buffering capabilities
;              of the 'z' device.  This is not needed for two-D plots 
;              and is off by default.
;
; CALLING SEQUENCE:
;    TIFF_START
;
; INPUTS:
;    None.
;
; SIDE EFFECTS:
;    Changes the graphics device to "z".
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-04-29.
;-
pro TIFF_Start, ENABLE_Z=enable_z
  Set_Plot,'z'
  if (not Keyword_Set(enable_z)) then Device,Z_BUFFERING=0
  Return
end





