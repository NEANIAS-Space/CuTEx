;+
; NAME:
;    PNG_WRITE.PRO
;
; PURPOSE:
;    Write out the contents of the currently active window as a .png
;    file.  If PNG_START is called before the graphics are generated
;    the current device will be the z-buffer.
;
; CATEGORY:
;    Graphics
;
; CALLING SEQUENCE:
;    PNG_WRITE
;
; KEYWORD PARAMETERS:
;    FILE:   set this to your preferred filename.  "idl.png" is the default.
;    NO_X:   Do not set the display device to x when finished.  If you
;            do not have a valid xdisplay setting the display to "x"
;            causes an error. 
;
; OUTPUTS:
;    Writes a file.
;
; SIDE EFFECTS:
;    Sets the graphics device to 'x' upon completion.
;
; EXAMPLE:
;    PNG_Write, FILE='/home/ewiebe/mypath/myfile.png'
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-01-29.
;       Modified: Edward C. Wiebe, 2002-04-29 (added NO_X)
;-
pro PNG_Write, FILE=file, NO_X = no_x
  if (Keyword_Set(file)) then f = file else f = 'idl.png'
  TVLCT,r,g,b,/GET
  img = TVRD()  
  Write_PNG,f,img,r,g,b
  if (not Keyword_Set(no_x)) then Set_Plot,'x'
  Return
end
