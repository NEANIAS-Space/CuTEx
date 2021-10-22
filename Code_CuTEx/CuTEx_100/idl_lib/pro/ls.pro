;+
; NAME:
;	LS
;
; PURPOSE:
;	This procedure runs the UNIX ls (list files) command.
;
; CATEGORY:
;	File System
;
; CALLING SEQUENCE:
;	LS [, Directory]
;
; INPUTS:
;	Directory:  A string containing the directory to be listed.
;	            The default is the present working directory.
;
; PROCEDURE:
;	This procedure spawns the UNIX ls command.
;
; EXAMPLE:
;	List the files in the present working directory.
;	  ls
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 1999-03-12.
;	Modified:	Daithi A. Stone, 2000-06-29 (added Directory
;			input).
;-

;***********************************************************************

PRO LS, Directory

;***********************************************************************
;List Files

if keyword_set(directory) then spawn, 'ls '+directory $
                          else spawn, 'ls'

;***********************************************************************
;The End

return
END
