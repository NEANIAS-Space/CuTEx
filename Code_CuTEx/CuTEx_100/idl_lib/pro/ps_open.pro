;+
; NAME:
;	PS_OPEN
;
; PURPOSE:
;	This procedure opens a postscript file for plotting output.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	PS_OPEN
;
; KEYWORDS:
;	LANDSCAPE:  Set this keyword to set the plotting orientation to
;	            landscape.  Default is portrait orientation.
;	PORTRAIT:  Set this keyword to set the plotting orientation to
;	           portrait.  This is in fact the default.
;	COLOR:  Set this keyword to allow colour in the output.
;	        Default is no colour.
;	FILENAME:  Set this keyword to the name of the output file.
;	           Default is IDL.PS in the present directory.
;
; USES:
;	LANDSCAPE.pro
;	PORTRAIT.pro
;
; PROCEDURE:
;	This procedure sets the IDL device to a postscipt output with
;	the desired characteristics.
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

PRO PS_OPEN, LANDSCAPE=landscapeopt, PORTRAIT=portraitopt, $
             COLOR=coloropt, $
             FILENAME=filename

;***********************************************************************
;Variables

;Filename
if not(keyword_set(filename)) then filename = 'idl.ps'

;***********************************************************************
;Set Postscript Device

;Set to postscript output
set_plot, 'ps'

;Set device options
if keyword_set(landscapeopt) then landscape
if keyword_set(portraitopt) then portrait
if keyword_set(filename) then device, file=filename
if keyword_set(coloropt) then device, /color

;***********************************************************************
;The End

return
END
