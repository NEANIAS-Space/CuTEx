;+
; NAME:
;    TIFF_WRITE.PRO
;
; PURPOSE:
;    Write out the contents of the currently active window as a .tiff
;    file.  If TIFF_START is called before the graphics are generated
;    the current device will be the z-buffer.
;
; CATEGORY:
;    Graphics
;
; CALLING SEQUENCE:
;    TIFF_WRITE
;
; KEYWORD PARAMETERS:
;    FILE:   Set this to your preferred filename.  "idl.tiff" is the
;            default.
;    NO_X:   Do not set the display device to x when finished.  If you
;            do not have a valid xdisplay setting the display to "x"
;            causes an error. 
;
; OUTPUTS:
;    Writes a .tiff file.
;
; SIDE EFFECTS:
;    Sets the graphics device to 'x' upon completion unless NO_X is set.
;
; EXAMPLE:
;    TIFF_Write, FILE='/home/ewiebe/mypath/myfile.tiff'
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-04-29.
;-
pro TIFF_Write, FILE=file, NO_X = no_x
  if (Keyword_Set(file)) then f = file else f = 'idl.tiff'
  TVLCT,r,g,b,/GET
  img = TVRD(/ORDER)  
  Write_TIFF,f,img,RED=r,GREEN=g,BLUE=b
  if (not Keyword_Set(no_x)) then Set_Plot,'x' 
  Return
end
