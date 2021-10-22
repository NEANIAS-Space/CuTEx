;+
; NAME:
;	LANDSCAPE
;
; PURPOSE:
;	This procedure sets the plotting output to landscape orientation.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	LANDSCAPE
;
; PROCEDURE:
;	This procedure sets the IDL device to a landscape orientation.
;
; EXAMPLE:
;	Open a postscript file for output.
;	  ps_open
;	Set the orientation to landscape.
;	  landscape
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-13.
;-

;***********************************************************************

PRO LANDSCAPE

;***********************************************************************
;Set Device to Landscape Orientation

device, /landscape

;***********************************************************************
;The End

return
END
