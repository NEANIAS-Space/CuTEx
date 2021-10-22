;+
; NAME:
;      PathTo
;
; PURPOSE:
;      Find a file in the $IDL_PATH search tree.
;
; CATEGORY:
;      File System
;
; CALLING SEQUENCE:
;      result = PathTo(file)
;
; INPUTS:
;      file:  the name of a file to find.
;
; KEYWORDS:
;      verbose: set this to get some progress messages.
;
; OUTPUTS:
;      result:  The complete path to file (including file) if found in
;               the $IDL_PATH search tree or a null ('') string if not.
;
; SIDE EFFECTS:
;      This procedure uses FindFile to search the $IDL_PATH variable for
;      file.  The speed of this operation depends on the depth of the
;      search and the operating system.  The search stops when the
;      file is found or all of $IDL_PATH has been searched.  This could be a
;      slow operation.
;
; EXAMPLE:
;     result = PathTo('mydata.dat')
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-02-27.
;       Modified:       Edward C. Wiebe, 2002-03-05 (!path doesn't include
;                       all directories.  Switched to the expand_path
;                       function)
;       Modified:       Edward C. Wiebe, 2002-03-06 (added searching
;                       message (dots) and verbose keyword)
;-
function PathTo,file, VERBOSE=verbose

; Use the shell $IDL_PATH variable to find the "first" location of a file in
; the $IDL_PATH tree.  This is a bit expensive since each directory
; must be searched with the FindFile routine.  It is faster if the
; file you want is near the top of !PATH.

  paths  = Expand_Path(GetEnv('IDL_PATH'),/ALL_DIRS,/ARRAY,COUNT=cnt)
  np     = cnt
  n      = 0
  done   = 0
  result = ''

  if (Keyword_Set(verbose)) then vb = 1 else vb = 0
  if (vb) then PrintP,'Searching for ' + file + ' '   
  while (n le np-1) and (not done) do begin
    r = FindFile(paths[n]+'/'+file,COUNT=cnt)
;    Print,n,cnt,paths[n],file
    if (vb) then PrintP,'.'
    if (cnt gt 0) then begin
      done = 1
      result = paths[n]+'/'+file
    endif else begin 
      n = n + 1
    endelse  
  endwhile
  Print
  if (vb) and (done) then  Print,'Found '+ result
  if (vb) and (not done) then Print,'File not found.'

  Return,result
end
