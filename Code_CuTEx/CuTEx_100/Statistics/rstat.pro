PRO RSTAT, Data_in, Med, Hinge1, Hinge2, Ifence1, Ifence2, $
    Ofence1, Ofence2, Mind, Maxd, $
    TEXTOUT=textout, DESCRIP=descr, NOPRINT=noprint
;+
; NAME:
;   rstat
;
; PURPOSE:
;   Robust statistics on an array.  Results are output in terms of
;   "hinges" and "fences" as defined in Tukey's Exploratory Data
;   Analysis; these are the quantities that are typically used in
;   box-and-whisker plots.  These descriptive statistics are particularly
;   useful for telling whether a distribution has long tails or outliers.
;
; MAJOR TOPICS:
;   Statistics.
;
; CALLING SEQUENCE:
;   RSTAT, Data_in, Med, Hinge1, Hinge2, Ifence1, Ifence2, $
;       Ofence1, Ofence2, Mind, Maxd
;
; INPUT PARAMETERS:
;   data_in     Array of numbers to be characterized.
;
; INPUT KEYWORDS:
;   noprint     Flag.  Omit printing if set.
;   textout     The usual GSFC TEXTOUT parameter.
;   descrip     Descriptive line for hardcopy.
;                       
; OUTPUT PARAMETERS:
;   med         Median.
;   hinge1      Lower quartile.
;   hinge2      Upper quartile.
;   ifence1     Lower one of the inner fences =
;                   hinge1 - 1.5*(interquartile interval)
;   ifence2     Upper one of the inner fences =
;                   hinge2 + 1.5*(interquartile interval)
;   ofence1     Lower one of the outer fences =
;                   hinge1 - 3*(interquartile interval)
;   ofence2     Upper one of the outer fences =
;                   hinge2 + 3*(interquartile interval)
;   mind, maxd  Min and max.
;
; HISTORY:   2 Dec. 1996 - Written.  RSH, HSTX
;           15 Sep. 1999 - Spiffed up for usage similar to imlist.  RSH
;            1 June 2000 - Improved descriptive header for output.  RSH
;            3 July 2000 - IDL V5 and idlastro standards.  Check for
;                          existence of finite data.  RSH
;-

on_error, 2

IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  RSTAT, Data_in, Med, '
    print, 'Hinge1, Hinge2, Ifence1, Ifence2, Ofence1, Ofence2, Mind, Maxd'
    print, 'KEYWORD PARAMETERS:  TEXTOUT, DESCRIP, NOPRINT'
    RETURN
ENDIF

nf = '(G9.3)'
itf = '(I9)'
IF NOT keyword_set(textout) THEN textout=!TEXTOUT

IF datatype( TEXTOUT ) NE 'STR' THEN BEGIN
    textout = textout > 2                  ;Don't use /MORE
    IF (textout GE 3) and (textout NE 5) THEN hardcopy = 1 ELSE hardcopy = 0
ENDIF ELSE hardcopy = 1

textopen, 'RSTAT', TEXTOUT = textout, /STDOUT  

IF hardcopy THEN BEGIN   ;Direct output to a disk file
    printf,!TEXTUNIT,'RSTAT: ' + systime(0)
    IF NOT keyword_set( DESCR ) THEN BEGIN
       descr = ''
       read,'Enter a brief description to be written to disk: ',descr
    ENDIF
    printf,!TEXTUNIT,descr
    printf,!TEXTUNIT,' '
ENDIF  

nin = n_elements(data_in)
wfin = where(finite(data_in), nfin)

IF nfin LE 0 THEN BEGIN
    printf, !TEXTUNIT, 'No finite numbers in data array.'
    textclose,textout=textout
    RETURN
ENDIF

data = data_in[wfin]

s = sort(data)
n = n_elements(data)

dmed = 0.5*(n+1)
fdmed = floor(dmed)
IF (n MOD 2) EQ 0 THEN BEGIN
    end1 = fdmed-1
    end2 = fdmed
    med = 0.5*(data[s[end1]] + data[s[end2]])
    dlet = 'h'
ENDIF ELSE BEGIN
    end1 = fdmed-1
    end2 = fdmed-1
    med = data[s[end1]]
    dlet = ' '
ENDELSE
                 
n1 = end1 + 1
dhinge = 0.5*(n1+1)
fdhinge = floor(dhinge)
IF (fdmed MOD 2) EQ 0 THEN BEGIN
    hinge1 = 0.5*(data[s[fdhinge-1]] + data[s[fdhinge]])
    hinge2 = 0.5*(data[s[n-fdhinge]] + data[s[n-fdhinge-1]])
    hlet = 'h'
ENDIF ELSE BEGIN
    hinge1 = data[s[fdhinge-1]]
    hinge2 = data[s[n-fdhinge]]
    hlet = ' '
ENDELSE

mind = data[s[0]]
maxd = data[s[n-1]]

hspread = hinge2-hinge1
step = 1.5*hspread
ifence1 = hinge1 - step
ifence2 = hinge2 + step
ofence1 = ifence1 - step
ofence2 = ifence2 + step

wo1 = where(data LT ifence1 AND data GE ofence1, co1) 
wo2 = where(data GT ifence2 AND data LE ofence2, co2) 
wf1 = where(data LT ofence1, cf1) 
wf2 = where(data GT ofence2, cf2) 
wcen = where(data GE ifence1 AND data LE ifence2, ccen)

IF NOT keyword_set(noprint) THEN BEGIN

    printf, !TEXTUNIT, "Tukey's Descriptive Statistics: "
    printf, !TEXTUNIT, 'HINGES  = 1st and 3rd quartiles; SPREAD = interquartile; '
    printf, !TEXTUNIT, 'STEP = 1.5*spread; INNER FENCES are 1 step outside the'
    printf, !TEXTUNIT, 'hinges; OUTER FENCES are 2 steps outside the hinges'
    printf, !TEXTUNIT, ' '
    printf, !TEXTUNIT, 'N finite = ' + strn(n) $
        +'               (N total = '  +strn(nin) + ')'
    printf, !TEXTUNIT, ' '

    printf, !TEXTUNIT, '    Min         Hinge       Median      Hinge        Max'
    printf, !TEXTUNIT, string(mind,form=nf) + '   ' + string(hinge1,form=nf) $
        + '   ' + string(med,form=nf) + '   ' + string(hinge2,form=nf) $
        + '   ' + string(maxd,form=nf)
    printf, !TEXTUNIT, ' '
    printf, !TEXTUNIT, 'RANGE = '+strn(maxd-mind,form=nf) $
       + '; SPREAD = '+strn(hspread,form=nf) $
       + '; STEP = '+strn(step,form=nf) 
    printf, !TEXTUNIT, ' '

    bl = string(replicate(32b,12))
    printf, !TEXTUNIT, 'CENSUS OF OUTLIERS:'
    printf, !TEXTUNIT, bl+'count ' + string(cf2,form=itf)
    printf, !TEXTUNIT, bl+'                 -------- O.F. = ' + strn(ofence2)
    printf, !TEXTUNIT, bl+'count ' + string(co2,form=itf)
    printf, !TEXTUNIT, bl+'                 -------- I.F. = ' + strn(ifence2)
    printf, !TEXTUNIT, bl+'count ' + string(ccen,form=itf)
    printf, !TEXTUNIT, bl+'                 -------- I.F. = ' + strn(ifence1)
    printf, !TEXTUNIT, bl+'count ' + string(co1,form=itf)
    printf, !TEXTUNIT, bl+'                 -------- O.F. = ' + strn(ofence1)
    printf, !TEXTUNIT, bl+'count ' + string(cf1,form=itf)
    printf, !TEXTUNIT, bl+' '

ENDIF

textclose,textout=textout

RETURN
END         
