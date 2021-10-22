; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;+
; NAME:
;     RINGBELL
;
; PURPOSE:
;     Ring the system bell.
;
; TYPE:
;     PROCEDURE
;
; CATEGORY:
;     UTILITIES
;
; CALLING SEQUENCE:
;     RINGBELL [, numRings]
;
; INPUTS:
;     numRings (OPTIONAL): Integer number of times to ring bell.
;
; KEYWORD PARAMETERS:
;     NONE
;    
; OUTPUTS:
;     NONE
;
; COMMON BLOCKS:
;     NONE
;
; SIDE EFFECTS:
;     None known.
;
; RESTRICTIONS:
;     None known.
;
; DEPENDENCIES:
;     NONE
;
; MODIFICATION HISTORY:
;     Written, Robert.Mallozzi@msfc.nasa.gov, 1998 July
;
;-
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
PRO RINGBELL, numRings

    IF (N_PARAMS() EQ 0) THEN $
       numRings = 1 
    
    FOR i = 0, numRings - 1 DO BEGIN
        
	; Ring the bell, then erase the newline
	;
	PRINT, STRING (7B), STRING (8B)
	
	; To ring bell multiple times, a WAIT is required.  This could
	; fail if the user has the bell set to a long duration.
	;
	IF (numRings GT 1) THEN $
	   WAIT, 0.5 

   ENDFOR

   
END
