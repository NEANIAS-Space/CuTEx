;+
; NAME:
;	PORTRAIT
;
; PURPOSE:
;	This procedure sets the plotting output to portrait orientation.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	PORTRAIT
;
; PROCEDURE:
;	This procedure sets the IDL device to a portrait orientation.
;
; EXAMPLE:
;	Open a postscript file for output.
;	  ps_open
;	Set the orientation to portrait.
;	  portrait
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-13.
;-

;***********************************************************************

PRO PORTRAIT

;***********************************************************************
;Set Device to Portrait Orientation

device, /portrait

;***********************************************************************
;The End

return
END
