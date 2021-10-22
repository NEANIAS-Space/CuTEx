;+
; NAME:
;        SREAD
;
; PURPOSE:
;       This function extracts column delimited data (whitespace
;       between columns) of data from a file and loads it into an
;       string array.  
;
; CATEGORY:
;       Input/Output
;
; CALLING SEQUENCE:
;       Result = sread( Filename )
;
; INPUTS:
;	Filename:  The name of the file to open and read.
;
; KEYWORD PARAMETERS:
;	NOCOMPRESS:  If set then whitespaces are not compressed to length 1.  
;		The default is compression.
;	SEPARATOR:  The string that separates columns of data.
;	SKIP_LINES: An integer number of lines to skip from the beginning of 
;		the file.
;	TRUNCATE:  If this keyword is set any (non comment) lines longer than 
;		the first line will be truncated.
;	VERBOSE:  Write some information about the reading process to the 
;		terminal.
;
; OUTPUTS:
;	Result:  A string array matching the content of the file is returned.  
;		A null string ('') is returned if there is an error.
;
; USES:
;	str.pro
;
; RESTRICTIONS:
;	I'm sure there are some but they are unknown.
;
; EXAMPLE:
;	text = sread( file, SKIP_LINES=5 )
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-06-09.
;       Modified:       Edward C. Wiebe, 2002-02-07 (changed code to always 
;		return '' if there is an error)
;       Modified:       Edward C. Wiebe, 2002-03-05 (fixed a bug where the 
;		routine stopped with an error if the input filename was '' -- 
;		it now returns '') 
;	Modified:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2005-03-10
;		(switched use of obsolete findfile to file_search)
;	Modified:	DAS, 2007-01-17 (added NOCOMPRESS keyword)
;	Modified:	DAS, 2009-09-29 (changed use of stringc to str.pro)
;-

FUNCTION sread, $
	Filename, $
	SKIP_LINES=skip_lines, $  
	VERBOSE=verbose, $
	SEPARATOR=separator, $
	TRUNCATE=truncate, $
	DEBUG=debug, $
	NOCOMPRESS=nocompressopt

  if (filename eq '') then begin
    Message,/INFO,'Filename is a null string.'
    Return,''
  endif

; evaluate the keywords 
  if (not Keyword_Set(separator)) then sep = ' ' else sep = separator
  all = 0
  if (not Keyword_Set(verbose)) then verbose = 0 else verbose = 1
  if (not Keyword_Set(skip_lines)) then skip_lines = 0

; does the file exist?
  a = file_search( filename, COUNT = count )
  if (count eq 0) then begin
    Print,' SREAD ERROR: at read.pro, FILE NOT FOUND. '+filename
    Return, ''
  endif else begin
;   Open the file and read the first (valid) line
    OpenR,lun,filename,/GET_LUN
    a = FStat(lun)
    if (a.size gt 0) then begin
      s = ''
      if (Keyword_Set(skip_lines)) then begin
        skip_lines = Round(skip_lines)        
        for i=1,skip_lines do ReadF,lun,s
      endif
  
;     Get the first line (not a comment) and parse it to figure out how many 
;     columns there are
      stop = 0
      while ((not EOF(lun)) and (not stop)) do begin
        ReadF,lun,s
        if not( keyword_set( nocompressopt ) ) then begin
          s = StrCompress(StrTrim(s,2),REMOVE_ALL=all)
        endif
        if (StrPos(s,';') lt 0) then begin
          stop = 1
        endif
      endwhile
;     if the first valid line has zero length then return a null string
      if (StrLen(s) eq 0) then begin
        if (verbose) then Print,'SREAD: no valid data in '+filename+'.'
        Return, ''
      endif

;     Begin the process of reading the data.

      s         = StrSplit(s,sep,/EXTRACT)  
;     get the number of elements in the first valid line  
      n         = (Size(s))[1]

      line      = ''
      start_len = Long(1000)
      inc_len   = Long(1000)
      tot_len   = start_len
      tmp       = StrArr(n,start_len)
      cnt       = Long(0)
    
      if (verbose) then Print,'Opening '+filename
   
      Point_Lun,lun,0  
      if (Keyword_Set(skip_lines)) then begin
;       Check that skip_lines is an integer 
        s = ''
        if (Var_Type(skip_lines) eq 2) then begin
          for i=1,skip_lines do ReadF,lun,s
        endif
      endif
   
      inc_cnt = 0
      wrote_message = 0
;     read to the end of the file
      while (not EOF(lun)) do begin
        s = ''
        ReadF,lun,line          
        if not( keyword_set( nocompressopt ) ) then begin
          line = StrCompress(StrTrim(line,2),REMOVE_ALL=all) 
        endif
        if (StrPos(line,';') lt 0) then begin
          sepstring  = StrSplit(line,sep,/EXTRACT)
          if (keyword_Set(debug)) then Print,n,size(sepstring),cnt
          if (Keyword_Set(truncate)) then begin
            tmp[*,cnt] = sepstring[0:n-1]
          endif else begin
            nn = (Size(sepstring))[1]
            if (nn gt n) then begin
;             Since we encountered a longer line (a line with more
;             elements than we expected) increase that dimension of
;             temp and don't forget n.      
              if (verbose) then Print,'READS:  Found a longer line;' $
                                     +' adjusting array.'
              tmp2 = StrArr(nn,tot_len)
              tmp2[0:n-1,0:cnt-1] = tmp[*,0:cnt-1]
              n    = nn
              tmp  = tmp2
            endif
            tmp[0:nn-1,cnt] = sepstring
          endelse
          cnt = cnt + 1
        endif
;       increase the size of the arrays to account for more lines 
        if (cnt gt tot_len-1) then begin 
          tot_len = tot_len + inc_len
          tmp2 = StrArr(n,tot_len)
          tmp2[0:n-1,0:cnt-1] = tmp
          tmp = tmp2
          inc_cnt = inc_cnt +1
          if (inc_cnt ge 10) then begin
            inc_len = inc_len*2
            inc_cnt = 0
            if (not wrote_message) then begin
              if (verbose) then Print,"This file appears to be quite long."
              wrote_message = 1
            endif
            ;if (verbose) then Print,"I'm increasing the read increment to " $
            ;                       +StringC(inc_len)+" lines."
            if verbose then begin
              Print, "I'm increasing the read increment to " + str( inc_len ) $
                  + " lines."
            endif
          endif
        endif
      endwhile
 
      Free_lun,lun

      if (verbose) then begin 
        ;Print,'Read '+StringC(cnt)+' lines from '+filename+'.' 
        Print, 'Read ' + str( cnt ) + ' lines from ' + filename + '.' 
      endif
      
;     Now reduce the array if we made it too big
      if (n eq 1) then begin
        tmp2 = StrArr(cnt)
        tmp2[*] = tmp[0,0:cnt-1]
      endif else begin
        tmp2 = StrArr(n,cnt)
        tmp2 = tmp[0:n-1,0:cnt-1]   
      endelse

      Return,tmp2
 
    endif else begin
      Print,'File '+filename+' has zero length.'
      Return, ''
    endelse

  endelse
end
