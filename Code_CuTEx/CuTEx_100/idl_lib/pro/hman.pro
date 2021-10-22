;+
; NAME:
;       HMAN.PRO
;
; PURPOSE:
;      Play the "Hangman" word guessing game.  Each time you issue the
;      hman command a single game is played (guess one word).
;
; CATEGORY:
;      Miscellaneous
;
; CALLING SEQUENCE:
;      HMAN
;
; INPUTS:
;      No parameters.  Requires a file called "words.dat".
;
; KEYWORD PARAMETERS:
;      None.
;
; OUTPUTS:
;      None.
;
; SIDE EFFECTS:
;      HMAN takes over character input at the command line.  Enter 0
;      to quit.
;
; EXAMPLE:
;      HMAN
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2001-04-30.
;-

pro hman
    
  r = FindFile('words.dat', COUNT=cnt)
  if (cnt eq 0) then begin
    f = PathTo('words.dat')
  endif else f = r[0]
  l = SRead(f)
  
  s = (Size(l))[1]

  done = 0

  maxnguess = 8

; choose a word
  a = Fix((RandomU(junk,100))[0]*(s))
  word = StrUpCase(l[a])
  len  = StrLen(word)
  test = StrArr(len)+'_'      

  prompt  = 'Guess a letter: '
  guess   = ''  
  count   = 0
  nguess  = 0
  guesses = StrArr(26)+'*'
  offs    = Byte('A')
  while not done do begin
;   print out the word so far (blanks and/or chars)  
    CLS
    Print
    Print,test
    Print
    Print,guesses
    Print
    if (nguess eq maxnguess-1) then begin
      Print,String(maxnguess-nguess,FORMAT='(I1)')+' guess left.' 
    endif else begin
      Print,String(maxnguess-nguess,FORMAT='(I1)')+' guesses left.'
    endelse
    Read, PROMPT=prompt, guess
    nguess = nguess + 1

;   reduce guess to a single upper case character
    guess = StrUpCase(StrMid(guess,0,1))

    if (guess eq '0') then begin
      done = 1
    endif else begin
;     store this guess if it hasn't been used already.
;     store in alphabetical order.
      p = StrPos(StrJoin(guesses,/SINGLE),guess)
      if (p eq -1) then begin 
        guesses[Byte(guess)-offs] = guess
                
        r = -2
        pos = 0
        good = 0
        while (r ne -1) do begin
          r = StrPos(word,guess,pos)
          if (r ne -1) then begin 
;         found a matching letter
            count   = count + 1        
            test[r] = guess        
            pos     = r+1
            good    = 1
          endif
        endwhile
        if (good) then nguess = nguess-1
      endif else begin
        Print,'You already guessed '+guess+'.'
        nguess = nguess-1
      endelse
      if (count eq len) then begin
        done = 1
        Print,'The word is "'+word+'". You guessed it!'
        nguess = nguess-1
      endif
      
    endelse

    if (nguess eq maxnguess) then begin
      done = 1
      Print,"You didn't guess the word."+'  It was "'+word+'".'
    endif  
;    Print,word,test,guess,count
    
  endwhile

  Return
end
