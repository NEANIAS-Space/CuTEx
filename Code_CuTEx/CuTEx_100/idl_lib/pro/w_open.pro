;+
; NAME:
;	W_OPEN
;
; PURPOSE:
;	This procedure opens a new plotting window.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	W_OPEN, Index
;
; OPTIONAL INPUTS:
;	Index:  A scalar index label for the window.
;
; PROCEDURE:
;	This procedure runs the IDL procedure WINDOW.  I just prefer
;	this call name.
;
; EXAMPLE:
;	Open window #2.
;	  w_open, 2
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-13.
;-

;***********************************************************************

PRO W_OPEN, Index

;***********************************************************************
;Open Window

window, index

;***********************************************************************
;The End

return
END
