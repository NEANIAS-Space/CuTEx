;+
; NAME:
;	W_CLOSE
;
; PURPOSE:
;	This procedure closes an existing plotting window.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	W_CLOSE, Index
;
; OPTIONAL INPUTS:
;	Index:  The window's scalar index label.
;
; PROCEDURE:
;	This procedure runs the IDL procedure WDELETE.  I just prefer
;	this call name.
;
; EXAMPLE:
;	Close window #2.
;	  w_close, 2
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-13.
;-

;***********************************************************************

PRO W_CLOSE, Index

;***********************************************************************
;Close Window

wdelete, index

;***********************************************************************
;The End

return
END
