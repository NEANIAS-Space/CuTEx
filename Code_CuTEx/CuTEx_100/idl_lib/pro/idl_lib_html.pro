;+
; NAME:
;       IDL_LIB_HTML
;
; PURPOSE:
;       This procedure creates the web page listing of a library's IDL 
;	utilities.
;
; CATEGORY:
;       Miscellaneous
;
; CALLING SEQUENCE:
;       IDL_LIB_HTML
;
; KEYWORD PARAMETERS:
;	ADMINWEB:  The administrator's web page.  The default is defined in
;		the Constants section.
;	ADMINNAME:  The administrator's name.  The default is defined in the
;		Constants section.
;	ADMINEMAIL:  The administrator's e-mail address.  The default is
;		defined in the Constants section.
;	FIRST_UPDATE:  A vector of strings containing information on updates 
;		to be added before the list of routines.
;	FUTITLE:  A string contain the title of the FIRST_UPDATE list of 
;		information.  The default is 'UPDATES'.
;	INDIR:  A scalar or vector string containing the directories in which 
;		to search for files.  The default is the current directory.
;	INFILE:  The flag for filenames to include in the library.  The 
;		default is '*.pro'.
;	MODIFIED_AUTHOR:  If set then authors under the "Modified" heading are 
;		included as Contributors in the output webpage.
;	OUTFILE:  The filename for the output webpage.  The default is 
;		'idl_lib.html'.
;	REVERSE_CATEGORY:  Order the categories in the output webpage in 
;		reverse alphabetical order.  The default is alphabetical order.
;	TAR:  If set, then a .tar archive file is created containing all of 
;		the routines in the library.  This archive will be included on 
;		the web page.
;	TITLE:  The title of the web page.
;	UPDATE:  A vector of strings containing information on updates to be 
;		added after the list of routines.
;	UTITLE:  A string contain the title of the UPDATE list of information. 
;		The default is 'UPDATES'.
;	ZIP:  If set, then a .zip archive file is created containing all of 
;		the routines in the library.  This archive will be included on 
;		the web page.
;
; USES:
;	month_name.pro
;	str.pro
;	string_from_vector.pro
;
; PROCEDURE:
;	This procedure reads the documentation of the IDL files in the group
;	utility directory, and creates a web page that lists them.
;
; MODIFICATION HISTORY:
;       Written by:     Daithi A. Stone (stoned@atm.ox.ac.uk), 2002-02-08.
;	Modified:	DAS, 2002-04-10 (STD.pro removal update)
;	Modified:	DAS, 2002-08-09 (CONSTANTS.pro changes update)
;	Modified:	DAS, 2002-11-29 (altered directory structure)
;	Modified:	DAS, 2002-12-06 (removed dependency on me)
;	Modified:	DAS, 2003-05-07 (renamed, and added ADMIN* keywords)
;	Modified:	DAS, 2004-06-24 (added old modification date printing)
;	Modified:	DAS, 2005-09-01 (added FIRST_UPDATE, FUTITLE, INDIR, 
;			INFILE, MODIFIED_AUTHOR, OUTFILE, REVERSE_CATEGORY, 
;			TITLE, UPDATE, UTITLE keywords;  permitted multiple 
;			input directories;  removed use of constants.pro)
;	Modified:	DAS, 2006-03-23 (corrected self-reference;  increased 
;			modification highlighting to 90 days)
;	Modified:	DAS, 2007-05-24 (added TAR and ZIP options)
;	Modified:	DAS, 2008-12-01 (updated administrator details)
;-

;***********************************************************************

PRO IDL_LIB_HTML, $
	ADMINWEB=adminweb, ADMINNAME=adminname, ADMINEMAIL=adminemail, $
	FIRST_UPDATE=first_update, FUTITLE=futitle, $
	INDIR=indir, INFILE=infile, $
	MODIFIED_AUTHOR=modified_authoropt, $
	OUTFILE=outfile, $
	REVERSE_CATEGORY=reverse_categoryopt, $
	TAR=taropt, ZIP=zipopt, $
	TITLE=title, $
	UPDATE=update, UTITLE=utitle

;***********************************************************************
; Constants and Variables

; Warnings string array
;update = [ '' ]

; Administrator's information
if not( keyword_set( adminweb ) ) then begin
  ;adminweb = 'http://www.atm.ox.ac.uk/user/stoned/'
  adminweb = 'http://www.csag.uct.ac.za/~daithi'
endif
if not( keyword_set( adminname ) ) then begin
  adminname = 'D&aacute;ith&iacute; Stone'
endif
if not( keyword_set( adminemail ) ) then begin
  ;adminemail = 'stoned@atm.ox.ac.uk'
  adminemail = 'stoned@csag.uct.ac.za'
endif

; Input directory
if not( keyword_set( indir ) ) then indir = ''
if not( keyword_set( infile ) ) then infile = '*.pro'
; Output file
if not( keyword_set( outfile ) ) then outfile = 'idl_lib.html'

; The time for mentioning modifications (in days)
histdelay = 90

; The default title
if not( keyword_set( title ) ) then title = 'A Library of IDL Programs'

; The default title for the update sections
if not( keyword_set( futitle ) ) then futitle = 'UPDATES'
if not( keyword_set( utitle ) ) then utitle = 'UPDATES'

; Default colours and text style
; Background colour
bgcol = 'black'
; Text colour
txcol = '#dd8800'
; Header colour
hdcol = '#ee0000'
; Link colour
lcol = '#00bbff'
; Viewed link colour
vlcol = '#0099ff'
; Text font
txfont = 'helvetica'
; Header font
hdfont = txfont
; Text bold
txbold = '<B>'
txbold1 = '</B>'
; Header bold
hdbold = txbold
hdbold1 = txbold1
; Text size
txsize = '4'
; Header size
hdsize = '<H2>'
hdsize1 = '</H2>'
; Title size
titsize = '<H1>'
titsize1 = '</H1>'

; Months in a year
mina=12

; Date and time
time = systime()
date = strmid( time, strlen(time)-4, 4 )
months = month_name( indgen(mina), /abbreviate )
for i=0,mina-1 do begin
  if strpos( time, months[i] ) ne -1 then begin
    if i + 1 lt 10 then date = date + '0' + str( i+1 ) $
                   else date = date + str( i+1 )
    if fix( strmid( time, 8, 2 ) ) lt 10 then begin
      date = date + '0' + strmid( time, 9, 1 )
    endif else begin
      date = date + strmid( time, 8, 2 )
    endelse
    hour = strmid( time, 11, 8 )
  endif
endfor
date = long( date )

;***********************************************************************
; Set-up for Reading IDL Files

; Find files
list = findfile( indir[0]+infile, count=npro )
nindir = n_elements( indir )
if nindir ne 1 then begin
  for i = 1, nindir - 1 do begin
    list = [ list, findfile( indir[i]+infile, count=temp ) ]
    npro = npro + temp
  endfor
endif

; Program information vectors
proname = strarr( npro )
propurp = strarr( npro )
procateg = strarr( npro )
prohist = strarr( npro )
prodate = lonarr( npro )
proauth = strarr( npro )
;Number of variables to read
nread = 4

;***********************************************************************
; Read IDL Files

; Set temporary variable, counters
temp = ''
checkname = 0
checkpurp = 0
checkcateg = 0
checkhist = 0

; Iterate through routines
for i = 0, npro-1 do begin
  ; Open file
  openr, 1, list[i]
  ; Set counters
  check = 0
  ; Run loop until all information retrieved or end of documentation
  while check le nread + 1 do begin

    ; Read line from file
    readf, 1, temp
    temp = strtrim( strcompress( temp ), 2 )
    if check eq 0 then begin
      if temp eq ';+' then check = 1
    endif

    ; If in documentation section
    if check ge 1 then begin
      ; Check for end of documentation
      if temp eq ';-' then check = nread + 2

      ; Read name
      if checkname ne 0 then begin
        proname[i] = strupcase( strtrim( strmid( temp, 1, $
                                                 strlen( temp ) - 1 ), 2 ) )
        checkname = 0
        check = check + 1
        ; Remove ".pro"
        pos = strpos( proname[i], '.PRO' )
        if pos ne -1 then proname[i] = strmid( proname[i], 0, pos )
      endif

      ; Read purpose
      if checkpurp ne 0 then begin
        if strlen( temp ) gt 3 then begin
          propurp[i] = propurp[i] + strmid( temp, 1, strlen( temp ) - 1 )
        endif else begin
          propurp[i] = strtrim( strcompress( propurp[i] ), 2 )
          checkpurp = 0
          check = check + 1
        endelse
      endif

      ; Read category
      if checkcateg ne 0 then begin
        procateg[i] = strupcase( strtrim( strmid( temp, 1, $
                                                  strlen( temp ) - 1 ), 2 ) )
        checkcateg = 0
        check = check + 1
      endif

      ; Read history
      if checkhist ne 0 then begin
        if strlen( temp ) gt 3 then begin
          ; Get author
          pos = strpos( temp, 'Written by:' )
          if pos ne -1 then begin
            pos1 = strpos( temp, ', ' )
            pos2 = strpos( temp, ' (' )
            if pos2 ne -1 then pos1 = min( [pos1,pos2] )
            proauth[i] = strmid( temp, pos+12, pos1-pos-12 )
          endif
          ; Get modifying authors if requested
          if keyword_set( modified_authoropt ) then begin
            pos = strpos( temp, 'Modified:' )
            if pos ne -1 then begin
              pos1 = strpos( temp, ', ' )
              pos2 = strpos( temp, ' (' )
              if pos2 ne -1 then pos1 = min( [pos1,pos2] )
              ; Only save this author name if it is not initials because 
              ; initials are often used as a shorthand for the original author 
              ; who will be in PROAUTH anyway
              temp1 = strmid( temp, pos+10, pos1-pos-10 )
              if strupcase( temp1 ) ne temp1 then begin
                if keyword_set( modauth ) then begin
                  modauth = [ modauth, temp1 ]
                endif else begin
                  modauth = temp1
                endelse
              endif
            endif
          endif
          ; Get more recent date
          pos = strpos( temp, '-' )
          if pos ne -1 then begin
            pos1 = strpos( strmid( temp, pos+1, strlen( temp )-pos ), '-' )
            if pos1 eq 2 then begin
              prodate[i] = long( strmid( temp, pos-4, 4 ) $
                                 + strmid( temp, pos+1, 2 ) $
                                 + strmid( temp, pos+4, 2 ) )
              checkhist = checkhist + 1
            endif
          endif
        endif else begin
          ; Create modification alert
          if checkhist eq 2 then begin
            prohist[i] = 'WRITTEN '
          endif else begin
            prohist[i] = 'MODIFIED '
          endelse
          prohist[i] = prohist[i] + strmid( str(prodate[i]), 0, 4 ) $
                       + '-' + strmid( str(prodate[i]), 4, 2 ) + '-' $
                       + strmid( str(prodate[i]), 6, 2 )
          checkhist = 0
          check = check + 1
        endelse
      endif

      ; Search for start of documentation fields
      if temp eq '; NAME:' then checkname = 1 
      if temp eq '; PURPOSE:' then checkpurp = 1 
      if temp eq '; CATEGORY:' then checkcateg = 1 
      if temp eq '; MODIFICATION HISTORY:' then checkhist = 1 

    endif
  endwhile

  ; Close file
  close, 1
endfor

;***********************************************************************
; Sort Programs

; Category list
categname = ['']
ncateg = 1
; Category classification of programs
idcateg = intarr(npro)

; Build categories
; Go through programs
for i = 0, npro-1 do begin
  ; Set counter
  check = 0
  ; Compare program category to category list
  for j = 0, ncateg-1 do begin
    if procateg[i] eq categname[j] then begin
      ;Link to existing category
      idcateg[i] = j
      check = 1
    endif
  endfor
  ; Create new category and link
  if check eq 0 then begin
    categname = [ categname, procateg[i] ]
    ncateg = ncateg + 1
    idcateg[i] = ncateg - 1
  endif
endfor

; Sort categories
categname = categname[1:ncateg-1]
ncateg = ncateg - 1
idcateg = idcateg - 1
sortcateg = sort( categname )
; Reverse order if requested
if keyword_set( reverse_categoryopt ) then sortcateg = reverse( sortcateg )

; Build author list
authname = ['']
nauth = 1
; Go through programs
for i = 0, npro-1 do begin
  ; Set counter
  check = 0
  ; Compare program author to author list
  for j = 0, nauth-1 do begin
    if proauth[i] eq authname[j] then check = 1
  endfor
  ; Add new author
  if check eq 0 then begin
    authname = [ authname, proauth[i] ]
    nauth = nauth + 1
  endif
endfor
; Go through modifying authors if requested
if keyword_set( modified_authoropt ) then begin
  for i = 0, n_elements( modauth ) - 1 do begin
    ; Set counter
    check = 0
    ; Compare program author to author list
    for j = 0, nauth-1 do begin
      if modauth[i] eq authname[j] then check = 1
    endfor
    ; Add new author
    if check eq 0 then begin
      authname = [ authname, modauth[i] ]
      nauth = nauth + 1
    endif
  endfor
endif
; Remove the initial entry (used for initialisation only)
authname = authname[1:nauth-1]
nauth = nauth - 1

;***********************************************************************
; Creat Archive Files

; Create .tar archive if requested
if keyword_set( taropt ) then begin
  ; Create archive file
  spawn, 'tar -cf ' + indir[0] + 'idl_lib.tar ' $
      + string_from_vector( list, spacer=' ' )
  ; Allow access to everyone
  spawn, 'chmod a+r ' + indir[0] + 'idl_lib.tar'
endif

; Create .zip archive if requested
if keyword_set( zipopt ) then begin
  ; Create archive file
  spawn, 'zip ' + indir[0] + 'idl_lib.zip ' $
      + string_from_vector( list, spacer=' ' )
  ; Allow access to everyone
  spawn, 'chmod a+r ' + indir[0] + 'idl_lib.zip'
endif

;***********************************************************************
; Create Web Page

; Open web page file
openw, 1, outfile

; Web page header
printf, 1, '<!-- *********************************************** /-->'
printf, 1, "<!-- ********** A Library of IDL Programs ********** /-->"
printf, 1, '<!-- **** Daithi A. Stone, stoned@atm.ox.ac.uk ***** /-->'
printf, 1, '<!-- * This file was produced by idl_lib_html.pro. * /-->'
printf, 1, '<!-- *********************************************** /-->'
printf, 1
printf, 1, '<HTML>'
printf, 1
printf, 1, '<HEAD>'
printf, 1, '<TITLE>' + title + '</TITLE>'
printf, 1, '<STYLE TYPE="text/css">'
printf, 1, '  A { text-decoration: none }'
printf, 1, '</STYLE>'
printf, 1, '</HEAD>'
printf, 1
printf, 1, '<BODY BGCOLOR="' + bgcol + '" LINK="' + lcol + '" VLINK="' $
           + vlcol + '" TEXT="' + txcol + '" SIZE="' + txsize $
           + '" FACE="' + txfont + '">'
printf, 1
printf, 1, '<CENTER>'
printf, 1, '  ' + titsize + '<FONT FACE="' + hdfont + '" COLOR="' + hdcol $
           + '">'
printf, 1, "    <BR>" + title + "<BR><BR>"
printf, 1, '  </FONT>' + titsize1
printf, 1, '</CENTER>'
printf, 1

; Write information to page
printf, 1, txbold
printf, 1, '<FONT COLOR="' + hdcol + '">Maintained by:</FONT>  '
printf, 1, adminname + ' (' + '<A HREF="mailto:' + adminemail + '">' $
           + adminemail + '</A>)<BR>'
printf, 1, '<FONT COLOR="' + hdcol + '">Last modified:</FONT>  '
printf, 1, strmid( str( date ), 0, 4 ) + '-' + strmid( str( date ), 4, 2 ) $
           + '-' + strmid( str( date ), 6, 2 ) + ', ' + hour + ' GMT by '
printf, 1, '<A HREF="' + adminweb + '/idl_lib/pro/idl_lib_html.pro">idl_lib_html.pro</A>.<BR>'
printf, 1, '<FONT COLOR="' + hdcol + '">Number of routines:</FONT>  '
printf, 1, str(npro) + '<BR>'
printf, 1, '<FONT COLOR="' + hdcol + '">Contributors:</FONT>  '
printf, 1, string_from_vector(authname) + '<BR><BR>'
printf, 1, '<FONT COLOR="' + hdcol + '">Licence:</FONT> The IDL routines available from this page of the University of Oxford Climate Dynamics Group are free for non-commercial use under the terms of this <A HREF="http://creativecommons.org/licenses/by-nc-sa/2.0/">Creative Commons License</A> unless otherwise noted in the routine.<BR><BR>' 
printf, 1, 'Please report any bugs to <A HREF="mailto:' + adminemail + '">' $
    + adminemail + '</A>.<BR>'
printf, 1, 'We welcome any modified or new routines you would like to add.<BR>'
printf, 1, 'We would like to know if you have found any of these routines useful.<BR>'
if keyword_set( taropt ) or keyword_set( zipopt ) then begin
  printf, 1, '<BR>'
  printf, 1, 'The entire routine library can be downloaded as an archive file.<BR>'
  if keyword_set( taropt ) then begin
    printf, 1, '  Click <A HREF="' + indir[0] + 'idl_lib.tar">here</A> for a .tar archive.<BR>'
  endif
  if keyword_set( zipopt ) then begin
    printf, 1, '  Click <A HREF="' + indir[0] + 'idl_lib.zip">here</A> for a .zip archive.<BR>'
  endif
endif
;if keyword_set( update ) then begin
;  printf, 1, 'See bottom for updates.<BR>'
;endif
printf, 1, '<BR>'
printf, 1, txbold1
printf, 1

; List update information now if requested
if keyword_set( first_update ) then begin
  ; Section title
  printf, 1, hdsize + '<FONT COLOR="' + hdcol + '">' + futitle + '</FONT>' $
      + hdsize1
  printf, 1, '<UL>' + txbold
  for i = 0, n_elements( first_update )-1 do begin
    printf, 1, '  <LI>' + first_update[i]
  endfor
  printf, 1, txbold1 + '</UL>'
  printf, 1, '<BR>'
endif

; Write categories to page
for i = 0, ncateg-1 do begin
  ; Category title
  printf, 1, hdsize + '<FONT COLOR="' + hdcol + '">'
  printf, 1, '  ' + categname[sortcateg[i]]
  printf, 1, '</FONT>' + hdsize1
  ; Program list
  printf, 1, txbold + '<UL>'
  id = where( idcateg eq sortcateg[i], nid )
  idsort = sort( strlowcase( proname[id] ) )
  id = id[idsort]
  for j = 0, nid-1 do begin
    printf, 1, '  <LI><A HREF="' + strlowcase( list[id[j]] ) + '">' $
               + proname[id[j]] + '</A> ' + propurp[id[j]]
    temp = julday( fix( strmid( str(date), 4, 2 ) ), $
                   fix( strmid( str(date), 6, 2 ) ), $
                   fix( strmid( str(date), 0, 4 ) ) ) $
           - julday( fix( strmid( str(prodate[id[j]]), 4, 2 ) ), $
                     fix( strmid( str(prodate[id[j]]), 6, 2 ) ), $
                     fix( strmid( str(prodate[id[j]]), 0, 4 ) ) )
    if temp lt histdelay then begin
      printf, 1, '<FONT COLOR="' + hdcol + '">' + prohist[id[j]] + '</FONT>'
    endif else begin
      printf, 1, prohist[id[j]]
    endelse
  endfor
  printf, 1, '</UL>'
  printf, 1, txbold1
  printf, 1
endfor

; List update information unless already done earlier in the webpage
if keyword_set( update ) then begin
  ; Section title
  printf, 1, '<BR>'
  printf, 1, hdsize + '<FONT COLOR="' + hdcol + '">' + utitle + '</FONT>' $
      + hdsize1
  printf, 1, '<UL>' + txbold
  for i = 0, n_elements( update )-1 do printf, 1, '  <LI>' + update[i]
  printf, 1, txbold1 + '</UL>'
endif

; Web page ending
printf, 1, '<BR>'
printf, 1, '<CENTER>' + txbold
printf, 1, '  <A HREF="' + adminweb + '">'
printf, 1, '    ' + adminname + "'s Homepage"
printf, 1, '  </A>'
printf, 1, txbold1 + '</CENTER>'
printf, 1
printf, 1, '</BODY>'
printf, 1, '</HTML>'

; Close web page file
close, 1

;***********************************************************************
; The End

return
END
