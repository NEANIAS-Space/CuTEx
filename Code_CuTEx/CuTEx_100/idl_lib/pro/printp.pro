;+
; NAME:
;       PRINTP
;
; PURPOSE:
;       Print text to the terminal and suppress the carriage return at
;       the end of the line.  This allows more characters to be added
;       to the same line.
;
; CATEGORY:
;       Miscellaneous
;
; CALLING SEQUENCE:
;       PrintP,text
;
; INPUTS:
;       text:  a string
;
; OUTPUTS:
;       Prints the text in the terminal.
;
; EXAMPLE:
;       PrintP,'Here is some text.' 
;       Print,"I've added some more text."
;
;  IDL prints
;     
;    Here is some text.I've added some more text.
;
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 1999-09-29.
;-
pro PrintP, text
  Print,text,FORMAT='(A,$)'
  Return
end
