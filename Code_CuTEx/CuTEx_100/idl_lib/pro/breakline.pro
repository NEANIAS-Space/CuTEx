;+
; NAME:
;     BREAKLINE
;
; PURPOSE:
;     This function breaks a line of text into pieces less than
;     or equal to a maximum length.  Text is broken on 
;     whitespace.
;
; CATEGORY:
;     Miscellaneous
;
; CALLING SEQUENCE:
;     result = BreakLine(line,maxlength)
;
; INPUTS:
;    line:    A string of text.
;    maxlen:  the maximum length of the substrings.
;
; OUTPUTS:
;    result:  a string array containing the substrings.
;
; EXAMPLE:
;    result = BreakLine('The quick brown fox jumped over the
;                        lazy dog', 15)
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2002-05-08.
;-

function BreakLine, line, maxlen
 
; line shold be a string 

  result=-1

  length = StrLen(line)
  num = length/maxlen
  rem = length mod maxlen 

  sline = StrSplit(line,LENGTH=len)
  
  if (maxlen lt Max(len)) then begin
    Message,/INFO,"The line can't be broken with maxlen=" $
                 +StrTrim(maxlen,2)                       $
                 +'.  It requires at least maxlen='       $
                 +StrTrim(Max(len),2)+'.'
    Return,StrArr(num)+'-1'
  endif

  places = num+(rem ne 0)
  result = StrArr(places)

  sz = Size(sline)
  cnt = sz[1]
  done = 0
  n = -1
  m = 0
  check = 0
  r = 0
  while (not done) do begin
    nn = 0
    l  = ''
    check = check +1
    while (nn lt maxlen) and (n lt cnt-1) do begin
      n = n + 1
      nn = nn + len[n]+(n ne m)
    endwhile
    
    if (nn gt maxlen) then begin
      n = n - 1      
      nn = nn - len[n]
    endif     

    for b=m,n do begin
      l = l + StrMid(line,sline[b],len[b])
      if (b ne n) then  l = l + ' '
    endfor

    if (r eq places) then begin 
      places = places + 1
      tmp = result
      result = StrArr(places)
      result[0:r-1] = tmp      
    endif
    result[r] = l
    r = r + 1
    m  = n + 1
    nn = 0
    if (n eq cnt-1) or (check gt length) then done = 1     

  endwhile

  
  Return,result
end

