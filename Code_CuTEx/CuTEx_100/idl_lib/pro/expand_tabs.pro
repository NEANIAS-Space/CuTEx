;+
; NAME: 
;       EXPAND_TABS
; 
; PURPOSE: 
;       Replace tab characters in a text string with spaces.
;
; CATEGORY:
;       Strings
;
; CALLING SEQUENCE:
;       result = Expand_Tabs( astring )
;
; INPUTS:
;       str:  A text string containing tab characters. 
;
; KEYWORD PARAMETERS:
;       SPACECHAR: The 'space' character to be used to expand the
;                  tabs.  Either text or Byte values may be
;                  specified. The default value is the space character
;                  (32B) 
;       TABCHAR:  A character to expand rather than the tab
;                 character.  Either text or Byte values may be
;                 specified.  The default values are the tab characters
;                 (9B,11B).  The input value may be a one-dimensional
;                 array of values to use as tab characters.  Each
;                 character will be matched indiviually.  No matching
;                 of substrings longer than one character is possible.
;       TABSPACE:  The maximum number of 'space' characters to use to
;                  expand tabs.  The default value is eight (8).
;
; OUTPUTS:
;       result:  A string with the (specified) tab characters expanded
;                up to multiples of the TABSPACE (modulo TABSPACE).
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2001-04-02.
;-

function Expand_Tabs,str                 $
                     , TABSPACE=tabspace  $
                     , TABCHAR=tabchar    $
                     , SPACECHAR=spacechar

; define some constants/check keywords
  if (Keyword_Set(tabchar)) then begin
    vt = Var_Type(tabchar)
    case vt of
      1: atab = tabchar
      7: atab = Byte(tabchar)
      else: begin
        Print,'EXPAND_TABS ERROR: TABCHAR not byte or string.'
      end
    endcase
  endif else atab = [9B,11B]

  if (Keyword_Set(spacechar)) then begin
    vt = Var_Type(spacechar)
    case vt of
      1: spc = spacechar
      7: spc = Byte(spacechar)
      else: begin
        Print,'EXPAND_TABS ERROR: SPACECHAR not byte or string.'
      end
    endcase
  endif else spc = 32B   
; for convenience in debugging, 
; an '@' is a String(64B)
; a  ' ' is a String(32B) 

  if (Keyword_Set(tabspace)) then begin
    vt = Var_Type(tabspace)
    if (vt ge 1) or (vt le 4) then begin
      tspace = tabspace 
    endif else begin
      Print,'EXPAND_TABS ERROR: TABSPACE not valid. '
    endelse
  endif else tspace = 8

; convert the string to bytes.
  bstr = Byte(str)

; find the tabs (9B,11B)
  s = N_Elements(atab)
  if (s gt 1) then begin
    test = '(bstr eq atab[0]) '
    for n=1,s-1 do test = test + 'or (bstr eq atab['+String(n)+']) '
    check = Execute('indx=Where('+test+',cnt)')
  endif else begin
    indx = Where(bstr eq atab, cnt)    
  endelse

  if (cnt gt 0) then begin
;   There are some tabs, expand them one by one.  
    nstr = StrArr(cnt+1)

;   handle the first tab manually
    if (indx[0] eq 0) then begin
;     the first char in the string is a tab.
      nstr[0] = String(Replicate(spc,tspace))
    endif 
    if (indx[0] gt 0) then begin
      length = tspace - (indx[0] mod tspace)
      nstr[0] = String(bstr[0:indx[0]-1])+String(Replicate(spc,length))
    endif

;   deal with any more tabs in the string
    for n=1,cnt-1 do begin   
      length = tspace - ((indx[n]-indx[n-1]) mod tspace) + 1
;     is the previous tab adjacent?
      if ((indx[n]-indx[n-1]) eq 1) then begin   
        nstr[n] = String(Replicate(spc,tspace)) 
      endif else begin
        nstr[n] = String(bstr[indx[n-1]+1:indx[n]-1]) $
          + String(Replicate(spc,length))
      endelse               
    endfor      

;   check for a leftover piece to the right of the last tab
    if (indx[cnt-1] ne StrLen(str)-1) then begin 
      nstr[cnt] = String(bstr[indx[cnt-1]+1:*])
    endif
;   recombine the string
    result = StrJoin(nstr)  

;   check for tabchars that stayed in the result
    indx = Where(Byte(result) eq atab,cnt)
    if (cnt gt 0) then Print,'EXPAND_TABS ERROR: tab in result.'
  endif else result = str

;  Print,'1234567812345678123456781234567812345678123456781234567812345678'
;  Print,'       |       |       |       |       |       |       |       |'


  Return,result
end


