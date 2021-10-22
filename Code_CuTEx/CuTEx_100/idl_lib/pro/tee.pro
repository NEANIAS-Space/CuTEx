;+
; NAME:
;        TEE
;
; PURPOSE:
;        Mimics the Unix tee command allowing output to go to the
;        terminal and to a file.
;
; CATEGORY:
;        Input/Output
;
; CALLING SEQUENCE:
;        Tee,lun,text
;
; INPUTS:
;        lun     An open(w) file unit. If the filename keyword is set 
;                lun is a filename to be opened with append and closed
;                before returning.
;        string  The string to write
;	
; KEYWORD PARAMETERS:
;        FILENAME If this keyword is set the variable in lun is
;                 assumed to be the name of an unopened file.  The file is
;                 opened with the APPEND keyword and closed before
;                 returning to the calling program.
;
; OUTPUTS:
;       The contents of string are written to the terminal and to the
;       file specified by lun.
;
; SIDE EFFECTS:
;       None known
;
; RESTRICTIONS:
;       lun must be an open(w) file unit or a string.
;
; EXAMPLE:
;       tee,lun,'some text'
;       tee,'output.txt','some text to print out',/FILENAME
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-08-29.
;-

pro Tee,lun,string,FILENAME=filename

; if there is an error go back to the calling routine.
  On_Error,2

; Print to the terminal
  Print,string

  if (N_Elements(lun) gt 0) then begin
;   is lun a filename?
    if (Keyword_Set(filename)) then begin
      if (StrLowCase(Var_Type(lun,/TEXT)) eq 'string') then begin
        OpenW,fid,lun,/GET_LUN,/APPEND     
      endif else begin
        Print,'TEE ERROR: filename, "'+StrTrim(lun,2)+'", not a string.'
        Return
      endelse
    endif else begin
      result = FStat(lun)
      if (result.WRITE ne 0) then begin
        fid = lun
      endif else begin
        Print,'TEE ERROR: File on unit '+StrTrim(lun,2)+' not open for writing.'
        Return
      endelse
    endelse

;   Print to the file
    PrintF,fid,string

    if (Keyword_Set(filename)) then begin
      Free_Lun,fid
    endif 
 
  endif
  
  Return
end
