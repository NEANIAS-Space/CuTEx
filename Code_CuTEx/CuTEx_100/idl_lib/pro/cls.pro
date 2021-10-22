;+
; NAME:
;	CLS
;
; PURPOSE:
;	This procedure clears the IDL terminal window.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	CLS
;
; PROCEDURE:
;	This procedure uses the UNIX "clear" command to clear the
;	terminal window.
;
; EXAMPLE:
;	Clear the terminal window.
;	  cls
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-07-05.
;	Modified:	Daithi A. Stone, 2000-07-14 (Added
;			documentation).
;-

;***********************************************************************

PRO CLS

;***********************************************************************
;Clear the Text in the Terminal Window   

spawn, 'clear'

;***********************************************************************
;The End

return
END
