;+
; NAME: FILE_LINES
;
; PURPOSE: return the number of lines in an ASCII file
;
; CATEGORY:
;
; CALLING SEQUENCE:  nb_lines=FILE_LINES(filename)
;
; INPUTS:  -- the file name, can be prefixed by the path
;
; OPTIONAL INPUTS: none
;
; KEYWORD PARAMETERS:  /compress is not available now
;                      /noexpand_path is not available now
;
; OUTPUTS: -- the line number, in Long64 type
;
; OPTIONAL OUTPUTS: none
;
; COMMON BLOCKS: none
;
; SIDE EFFECTS:  as is on July 2012, we still have problems related to
;                WordExp() in "str.cpp", when a file is prefixed or
;                suffixed by white space, FILE_TEST() returns "1" wrongly.
;
; RESTRICTIONS:  only for Unix (Unix, Linux and Mac OS X) systems
;
; PROCEDURE:
;
; EXAMPLE:   print, FILE_LINES("/etc/passwd")
;
; MODIFICATION HISTORY:
;  - 26/07/2006: created by Alain Coulais (ARSC)
;  - 30/05/2008: Michael Mueller (U of Arizona) fixed inconsistent
;     handling of files that don't end in newline
;  - 14/01/2010: Lucio Baggio (LATMOS/CNRS) avoided shell interaction
;  - 04/07/2012: Alain 
;      * Correcting bug 3189065 : better message when no-existing file !
;      * Correcting bug 3175753 : bad value when filename begin with number
;      * managing input files list
;  - 25/03/2014: Alain
;      * pb when "~" in filename
;  - 11/05/2015: Alain : better managment of Directories !
;  - 26/03/2018: must be Long64 !
;
;-
; LICENCE:
; Copyright (C) 2006--2014 Alain Coulais
; This program is free software; you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation; either version 2 of the License, or     
; (at your option) any later version.
;-
;
function FILE_LINES, filenames, compress=compress, noexpand_path=noexpand_path, $
                     verbose=verbose, test=test
;
ON_ERROR, 2    ;Return to caller
;
if KEYWORD_SET(compress) then begin
    ;; we consider 2 solutions : zgrep and zcat
    zcommand=['']
    SPAWN, 'zgrep --help', res, error, exit_status=status
    if (status EQ 0) then begin
        zcommand=['zgrep -Ec "$" ']
    endif else begin
        SPAWN, 'zcat --help', res, error, exit_status=status
        if (status EQ 0) then zcommand=['zcat ','| wc -l']
    endelse
    if (STRLEN(zcommand[0]) EQ 0) then begin
        print, 'Sorry, no solution found for Keyword COMPRESS (please help !)'
        return, -1
    endif
endif
;
if KEYWORD_SET(noexpand_path) then begin
    print, 'Sorry, Keyword NOEXPAND_PATH is not available now.'
    return, -1
endif
;
nbp=LON64ARR(N_ELEMENTS(filenames))
;
for ii=0, N_ELEMENTS(filenames)-1 do begin
    ;;
    filename=filenames[ii]
    if (FILE_TEST(filename) EQ 0) then begin
       MESSAGE, 'Error opening file. File: '+filename
    endif
    if (FILE_TEST(filename,/directory) EQ 1) then begin
       MESSAGE, 'Unable to open directory. File: '+filename
    endif
    ;;
    ;; subsituting "~" when at first place
    ;; (seems to be no sense to be in another place)
    ;;
    if STRPOS(filename,'~') EQ 0 then begin
       filename=GETENV('HOME')+STRMID(filename,1)
    endif
    ;;
    if KEYWORD_SET(compress) then begin
        ;; we consider 2 solutions : zgrep and zcat
        if N_ELEMENTS(zcommand) EQ 1 then begin
            commande=zcommand[0]+filename
        endif else begin
            commande=zcommand[0]+filename+zcommand[1]
        endelse        
        SPAWN, commande, resultat
        nbp[ii]=(LONG64((STRSPLIT(resultat,' ',/extract))[0]))
    endif else begin
        commande=["wc", "-l",filename]
        SPAWN, commande, resultat, /NOSHELL
        nbp[ii]=(LONG64((STRSPLIT(resultat,' ',/extract))[0]))
        ;;nbp[ii]=(LONG(STRCOMPRESS(resultat,/remove_all)))(0)
        ;;
        ;; checking remaining missing bad endline
        commande=["tail","-c 1",filename]
        SPAWN, commande, resultat, /NOSHELL
        nbp[ii] += resultat NE ''
    endelse
    if KEYWORD_SET(verbose) then print, filename, '  :  ', nbp[ii]
endfor
;
if KEYWORD_SET(test) then STOP
;
if N_ELEMENTS(filenames) EQ 1 then return, nbp[0] else return, nbp
;
end
