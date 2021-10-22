;+
; NAME:
;       READ
;
; PURPOSE:
;	This function extracts column delimited data (whitespace
;	between columns) from a file and loads it into an array
;	It is intended to work with integer or floating point 
;	data but will always read the data as if it were floating
;	point.
;
; CATEGORY:
;       Input/Output
;
; CALLING SEQUENCE:
;       data = READ(Filename)
;
; INPUTS:
;  filename -- the name of the file to open and read
;
; KEYWORD PARAMETERS:
;	CHANGE_CHAR:  An array of the same size as CONVERT_CHAR containing a 
;		list of entries with which to replace the elements in 
;		CONVERT_CHAR.  The default is to replace them with whitespace.
;	CONVERT_CHAR:  A string array or string of elements to convert to
;	        whitespace in the data file.  Be careful about putting
;		numerical digits here as you may lose data.
;       IGNORE_COLUMNS: An integer number of columns of text to ignore
;                       in the fileon.  
;               ignore_columns > 0 : ignore columns on the left
;               ignore_columns < 0 : ignore columns on the right
;	N_LINES:  The number of lines to read in.  The default is until the
;		end of the file.
;	NO_TABS:  If set, the function assumes that there are no tab characters
;	          in the data file, and skips the routine (expand_tabs) that
;	          removes the tabs.  This speeds up the function a fair bit.
;       PAD: If set any blanks encountered in the data will be filled
;            with the value in PAD.  NOTE:  The blanks are appended at the end
;            of the line for now.  -- This is to be fixed in the future.
;	SKIP_LINES:  An integer number of lines to skip from the beginning
;	             of the file.
;	VERBOSE:  Write some information about the reading process to
;	          the terminal.
;
;
; USES:
;	expand_tabs.pro (function Expand_Tabs)
;	str.pro
;
; PROCEDURE:
;
; EXAMPLE:
;	Read numbers from an ASCII file containing only numbers.
;	  array = read( inputfile.txt )
;
; MODIFICATION HISTORY:
;	Written by:	Edward C. Wiebe, 1997-09-04.
;	Modified:	ECW, 2000-07-06 (Added convert_char capability).
;	Modified:	Daithi A. Stone, 2001-02-14 (Modified documentation,
;			altered VERBOSE keyword).
;       Modified:       ECW, 2001-03-21 (added IGNORE_COLUMNS keyword)
;       Modified:       ECW, 2001-08-31 (added code to return for too few
;                       columns)
;       Modified:       ECW, 2001-10-12 (replaced Print statements
;                       with Message statements)
;	Modified:	DAS, 2001-11-20 (added NO_TABS keyword)
;       Modified:       ECW, 2002-04-17 (added the PAD Keyword)  
;       Modified:       ECW, 2002-08-01 (added negative ignore_columns feature)
;	Modified:	DAS, 2004-02-24 (added CHANGE_CHAR, N_LINES keywords)
;	Modified:	DAS, 2009-09-29 (changed use of stringc to str.pro)
;-

;***********************************************************************

FUNCTION READ, Filename                       $
               ,CONVERT_CHAR=convert_char     $
               ,CHANGE_CHAR=change_char       $
               ,SKIP_LINES=skip_lines         $
               ,VERBOSE=verbose               $
               ,IGNORE_COLUMNS=ignore_columns $
               ,N_LINES=n_lines $
               ,NO_TABS=notabsopt              $
               ,PAD = pad

;***********************************************************************
  t1 = 0

  notabsopt=Keyword_Set(notabsopt)
  if (not Keyword_Set(verbose)) then verbose = 0 else verbose = 1

  if (not Keyword_Set(skip_lines)) then skip_lines = 0

  read_all_columns = 1
  ignore_right     = 0
  if (not Keyword_Set(ignore_columns)) then begin
    igcol = 0 
  endif else begin
    if (Var_Type(ignore_columns) eq 2) then begin
      read_all_columns = 0
      igcol = ignore_columns 
      if (igcol lt 0) then begin
        ignore_right = 1 
        igcol = Abs(igcol)
      endif
    endif else begin
      igcol = 0
      Message,/INFO,'Ignore_columns not an integer. Using 0.'
    endelse
  endelse

  a = FindFile(filename, COUNT = count)

  if (count eq 0) then begin    
    Message,'File not found. '+filename, /CONTINUE
    Return, 0
  endif else begin
    OpenR,lun,filename,/GET_LUN
    a = FStat(lun)
    if (a.size gt 0) then begin
      s = ''
      if (Keyword_Set(skip_lines)) then begin
        skip_lines = Round(skip_lines)        
        for i=1,skip_lines do ReadF,lun,s
      endif
      
;     Get the first line and parse it to figure out how many 
;     columns there are
      ReadF,lun,s
      if (not notabsopt) then s = Expand_Tabs(s)
      if (not read_all_columns) then begin
        if (ignore_right) then s=StrMid(s,0,igcol) else s = StrMid(s,igcol)
      endif
      if (Keyword_Set(convert_char)) then begin
        check = 0
        if keyword_set( change_char ) then begin
          nchange = n_elements( convert_char )
          if n_elements( change_char ) eq nchange then check = 1
          if n_elements( change_char ) eq 1 then check = 1
        endif
        if check eq 1 then begin
          for i = 0, nchange - 1 do begin
            convert_len = strlen( convert_char[i] )
            pos = 0
            while pos ge 0 do begin
              pos = strpos( s, convert_char[i] )
              if pos ge 0 then begin
                s = strmid( s, 0, pos ) + ' ' + change_char[i] + ' ' $
                    + strmid( s, pos+convert_len, strlen( s ) )
              endif
            endwhile
          endfor
        endif else begin
          s = StrJoin(StrSplit(s,'['+StrJoin(convert_char)+']'   $
                               ,/EXTRACT,/REGEX),' ')
        endelse
      endif

      s         = StrSplit(StrTrim(StrCompress(s),2),' ',/EXTRACT)

      n         = Size(s)
      n         = n[1]    
      start_len = 1000L
      inc_len   = 1000L
      tot_len   = start_len
      tmp       = FltArr(n,start_len)
      cnt       = 0L
      
      if (verbose) then Message,/INFO,'Opening '+filename
      
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
      checkend = 0
      while not( checkend ) do begin
        s = ''
        ReadF,lun,s
        if not(notabsopt) then s = Expand_Tabs(s)
        if (not read_all_columns) then begin
          if (ignore_right) then s=StrMid(s,0,igcol) else s = StrMid(s,igcol)
        endif
;        s = StrMid(s,igcol)
        if (Keyword_Set(convert_char)) then begin        
          check = 0
          if keyword_set( change_char ) then begin
            nchange = n_elements( convert_char )
            if n_elements( change_char ) eq nchange then check = 1
            if n_elements( change_char ) eq 1 then check = 1
          endif
          if check eq 1 then begin
            for i = 0, nchange - 1 do begin
              convert_len = strlen( convert_char[i] )
              pos = 0
              while pos ge 0 do begin
                pos = strpos( s, convert_char[i] )
                if pos ge 0 then begin
                  s = strmid( s, 0, pos ) + ' ' + change_char[i] + ' ' $
                      + strmid( s, pos+convert_len, strlen( s ) )
                endif
              endwhile
            endfor
            s = strsplit( s, ' ', /extract )
          endif else begin
            s = StrSplit(StrJoin(StrSplit(s,'['+StrJoin(convert_char)+']'   $
                                          ,/EXTRACT,/REGEX),' '),' ',/EXTRACT)
          endelse
        endif else begin
          s = StrSplit(StrCompress(s),' ',/EXTRACT)
        endelse

        if (N_Elements(s) ne n) then begin
          Message, 'Encountered line with wrong number of columns.  ' $
                   + 'Occured at line ' + StrTrim(String(cnt+1+skip_lines),2) $
                   + ' in ' + filename + '.', /INFO
          if (Keyword_Set(pad)) then begin
            nelem = N_Elements(s)
            s = [s,StrArr(n-nelem)+String(pad)]
          endif else begin            
            Return,0
          endelse
        endif


        tmp[*,cnt] = Float(s)        
        cnt = cnt + 1
        if (cnt gt tot_len-1) then begin 
          tot_len = tot_len + inc_len
          tmp2 = FltArr(n,tot_len)
          tmp2[0:n-1,0:cnt-1] = tmp
          tmp = tmp2
          inc_cnt = inc_cnt +1
          if (inc_cnt ge 10) then begin
            inc_len = inc_len*2
            inc_cnt = 0
            if (verbose) then begin
              if (not wrote_message) then begin
                Message,/INFO,'This file appears to be quite long.'
                wrote_message = 1
              endif
              ;Message, /INFO, "I'm increasing the read increment to " $
              ;         + StringC(inc_len) + " lines."
              Message, /INFO, "I'm increasing the read increment to " $
                  + str( inc_len ) + " lines."
            endif
          endif
        endif
        if EOF(lun) then checkend = 1
        if keyword_set( n_lines ) then begin
          if cnt eq n_lines then checkend = 1
        endif
      endwhile

      Free_lun,lun

      ;if verbose then Message, /INFO, 'Read '+StringC(cnt)+' lines from ' $
      ;  + filename+'.'
      if verbose then begin
        Message, /INFO, 'Read ' + str( cnt ) + ' lines from ' + filename + '.'
      endif

;     Now reduce the array if we made it too big
      if (n eq 1) then begin
        tmp2 = FltArr(cnt)
        tmp2[*] = tmp[0,0:cnt-1]
      endif else begin
        tmp2 = FltArr(n,cnt)
        tmp2 = tmp[0:n-1,0:cnt-1]   
      endelse
      Return,tmp2

    endif else begin
      Message, 'File ' + filename + ' has zero length.' $
               , /CONTINUE
      Return, 0
    endelse
  endelse
end

