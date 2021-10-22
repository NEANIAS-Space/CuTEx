;+
; NAME:
;	PS_CLOSE
;
; PURPOSE:
;	This procedure closes a postscript file for plotting output.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	PS_CLOSE
;
; KEYWORDS:
;	LANDSCAPE:  Set this keyword to set the plotting orientation to
;	            landscape.  Default is portrait orientation.
;	PORTRAIT:  Set this keyword to set the plotting orientation to
;	           portrait.  This is in fact the default.
;
; USES:
;	LANDSCAPE.pro
;	PORTRAIT.pro
;
; PROCEDURE:
;	This procedure closes the postscript output file and sets the
;	IDL device back to a normal plotting window.
;
; EXAMPLE:
;	Open the postscript file IDLOUT.PS with landscape orientation.
;	  ps_open, /landscape, filename='idlout.ps'
;	Close the postscript file and return to portrait orientation.
;	  ps_close, /portrait
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-13.
;-

;***********************************************************************

PRO PS_CLOSE, LANDSCAPE=landscapeopt, PORTRAIT=portraitopt

;***********************************************************************
;Un-set Postscript Device

if keyword_set(landscapeopt) then landscape
if keyword_set(portraitopt) then portrait

;Set to screen output (close postscript output)
device, /close
set_plot, 'x'

;***********************************************************************
;The End

return
END
