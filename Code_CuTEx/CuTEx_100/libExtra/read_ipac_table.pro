pro READ_IPAC_TABLE, filename, table, header, ERR=err, $
  VERBOSE=verbose, TYPES=types, COMPRESS=compress, COLUMNS=columns, $
  NMAX=nmax, NODATA=nodata, NAMES=names, ALIAS=alias, CUNITS=cunits, $
  NULL=null
;+
; NAME:
;   READ_IPAC_TABLE
;
; PURPOSE:
;   Read an IPAC ASCII table into an array of structures.
;
; CALLING SEQUENCE:
;   READ_IPAC_TABLE, filename, table, header, ALIAS=alias, ERR=err, /VERBOSE, 
;     COL_TYPES=col_types, COLUMNS=columns, /COMPRESS
;
; INPUTS:
;   filename = IPAC table filename
;
; OUTPUTS:
;   table = array of structures; each table row is one element, 
;     each table column is a tag in the structure
;
; OPTIONAL OUTPUTS:
;   header = (unprocessed) headers from the file [strarr()]
;
; KEYWORD INPUTS:
;   alias = list of aliases with which to replace column names; must match
;     the COLUMNS keyword one-to-one; e.g. if COLUMNS=['ra_swire'] and 
;     ALIAS=['ra'] then the table column 'ra_swire' will have a tag name
;     of 'RA' in the output structure [strarr()]
;   columns = list of column names to extract [strarr()]
;   /COMPRESS = forces the file to be read as a gzip-compressed
;     format; useful if the extension is not '.gz'
;   /NODATA = just read the header & column definitions
;   NMAX = maximum number of rows to read in one step.  If the actual 
;     number of rows in the table exceeds NMAX then the table is built up
;     by successively adding NMAX rows - this is inefficient for very large 
;     tables (it doubles the memory usage) but may be useful if you do not 
;     know how many rows there are.  Default is 10000.
;   TYPES = set data types for each column; this keyword overides the 
;     types given in the table or derived from the data; string array with
;     one element per column; valid types are ['D','DOUBLE','R','REAL','I',
;     'INT','C','CHAR']
;   /VERBOSE = print header and progress messages
;
; KEYWORD OUTPUTS:
;   err = error flag: (1) file access error; (3,5,7,9) format 
;     error for header or column definitions (all fatal); (11) type
;     conversion error
;   names = column names from the table, possibly modified by the ALIAS 
;     keyword and forced to be unique valid IDL names
;   cunits = array of units for each column, if present in the file (no
;     checks are performed to ensure these really are units - it just 
;     extracts the line following the column types).
;   null = structure containing null values for each tag in the output
;     table
;
; EXPLANATION:
;   See http://irsa.ipac.caltech.edu/applications/DDGEN/Doc/ipac_tbl.html
;   for definition of an IPAC table.
;
; PROCEDURES CALLED:
;   IDL Astronomy Library (http://idlastro.gsfc.nasa.gov):
;     STRN(), IDL_VALIDNAME(), VALID_NUM()
;
; HISTORY:
;   2003/11/11 written by Ian Waddington, University of Sussex
;   2004/06/03 minor modifications to structure assignment step (IW)
;   2004/06/16 added TYPES keyword; better handling of column 
;     definitions; catch 'null' values (IW)
;   2004/08/24 added COLUMNS, COMPRESS, NAMES, NODATA, & NMAX keywords;
;     improved memory usage (IW)
;   2004/12/06 added /CONVERT_ALL keyword to idl_validname() for V6.1 (IW)
;   2005/04/11 added ALIAS keyword (IW)
;   2005/05/31 improved progress reporting (IW)
;   2006/02/24 changed definitions of 'null'; added CUNITS keyword (IW)
;   2006/06/12 improved parsing of the type/units/null headers to
;     conform to the definition document (IW)
;   2006/07/17 fixed errors in indices of col_type[] (IW)
;   2006/07/21 fixed parsing error if all four column specifications
;     (name, type, units, null) are present (IW)
;   2006/08/01 added NULL keyword; read gzipped files without having to
;     specify the /COMPRESS keyword if the extension is '.gz' (IW)
;
;-


; keywords
err = 0
do_compress = keyword_set( compress )
verbose = keyword_set( verbose )
if ( n_elements(types) ne 0 ) then $
  types = strupcase(strtrim(temporary(types),2))
if ( n_elements(nmax) eq 0 ) then nmax = 10000L
nodata = keyword_set(nodata)


; if ALIAS keyword is given, it must match COLUMNS keyword
usealias = 0b
if ( n_elements(alias) gt 0 ) then begin
  if ( n_elements(columns) ne n_elements(alias) ) then begin
    err = 1
    message, 'elements in ALIAS keyword must match COLUMNS keyword', /INFO
    return
  endif
  alias = strupcase( strtrim( alias, 2 ) )
  usealias = 1
endif


; handle gzip-compressed files automatically
if ( do_compress eq 0 ) then begin
  suffix = strmid( filename, 2, /REVERSE_OFFSET )
  if ( suffix eq '.gz' ) then do_compress = 1
endif


; data types recognized
;table_type = [ 'INT', 'I', 'DOUBLE', 'D', 'FLOAT', 'F', 'REAL', 'R', $
;  'CHAR', 'C', 'DATE' ]
;idl_type = [ 3, 3, 5, 5, 4, 4, 4, 4, 7, 7, 7 ]
table_type = [ 'INT', 'I', 'LONG', 'L', 'DOUBLE', 'D', 'FLOAT', 'F', 'REAL', 'R', $
  'CHAR', 'C', 'DATE' ]
idl_type = [ 3, 3, 3, 3, 5, 5, 4, 4, 4, 4, 7, 7, 7 ]


; values to substitute for 'null' data
; - "reals" have IEEE NaN, "integers" have the maximum possible value
; - index on idl_null corresponds to the IDL type returned by size()
; - most of these data types are not valid for IPAC tables, but I include
;   them for completeness
idl_null = { undefined: 'null', $
             byte: 255B, $
             integer: 32767, $
             long: 2L^31-1, $
             float: !VALUES.F_NAN, $
             double: !VALUES.D_NAN, $
             complex: complex(!VALUES.F_NAN,!VALUES.F_NAN), $
             string: 'null', $
             structure: 'null', $
             dcomplex: complex(!VALUES.D_NAN,!VALUES.D_NAN), $
             pointer: ptr_new(), $
             object: obj_new(), $
             uint: 65535U, $
             ulong: 2UL^32-1, $
             long64: 2LL^63-1, $
             ulong64: 2ULL^64-1 $
           }


; open file
openr, in, filename, /GET_LUN, ERROR=ioerr, COMPRESS=do_compress
if ( ioerr ne 0 ) then begin
  message, !ERROR_STATE.MSG, /INFO
  err = 1
  return
endif
if verbose then message, 'reading <'+filename+'>', /INFO


; read first line; is it an IPAC table?
; (just checks first character is '\' or '|')
thisline = ''
readf, in, thisline
a = strmid( thisline, 0, 1 )
if ( a ne '\' and a ne '|' ) then begin
  message, '<'+filename+'> not an IPAC table', /INFO
  err = 3
  return
endif


; read the keyword and comment lines
if ( a eq '\' ) then begin
  header = thisline
  while ( a eq '\' ) do begin
    readf, in, thisline
    a = strmid( thisline, 0, 1 )
    if ( a eq '\' ) then header = [ header, thisline ]
  endwhile
  if verbose then for i = 0, n_elements(header)-1 do print, header[i]
endif


; read first header line - must be the column names
if ( a ne '|' ) then begin
  message, '<'+filename+'> error after header', /INFO
  err = 5
  return
endif
line_names = thisline
if verbose then print, line_names


; parse column names
spos = strsplit( line_names, '|', LENGTH=slen )
names = strmid( line_names, spos, slen )
names = strupcase( strtrim( names, 2 ) )
ncol = n_elements( names )


; which columns to extract
if ( n_elements(columns) eq 0 ) then begin
  usecol = indgen(ncol)
endif else begin
  columnsu = strupcase( strtrim( columns, 2 ) )
  for i = 0, ncol-1 do begin
    k = where( columnsu eq names[i], nk )
    if ( nk gt 0 ) then begin
      usecol = ( n_elements(usecol) eq 0 ) ? [i] : [ usecol, i ]
      if usealias then names[i] = alias[k[0]]
    endif
  endfor
  if ( n_elements(usecol) eq 0 ) then message, 'found no columns'
endelse
nusecol = n_elements(usecol)


; remove any non-alphanumeric characters from column names
for i = 0, ncol-1 do names[i] = idl_validname( names[i], /CONVERT_ALL )


; next line must be the data type
readf, in, thisline
a = strmid( thisline, 0, 1 )
; - flag when we find the first line of data
found_data = 0b
if ( a ne '|' ) then begin
  ; this is the first data line
  ; - if type definitions were given as an input keyword, then we're 
  ;   OK, otherwise we need to try and figure out the types from the data
  found_data = 1b
  if ( n_elements(types) eq 0 ) then begin
    ; no type definitions have been found
    message, 'INVALID IPAC TABLE <'+filename+'>', /INFO
    message, '... no column type definitions', /INFO
    message, '... will try to work it out, but there may be errors', /INFO
    cols = strsplit( thisline, /EXTRACT )
    ncols = n_elements( cols )
    types = strarr( ncols )
    for i = 0, ncols-1 do $
      if VALID_NUM( cols[i], /INTEGER ) then types[i] = 'I' $
        else if VALID_NUM( cols[i] ) then types[i] = 'R' $
        else types[i] = 'C'
  endif
endif else begin
  ; these are the data types
  ; - separate the columns
  if ( verbose eq 1 ) then print, thisline
  col = strupcase(strtrim(strmid( thisline, spos, slen ),2))
  ; - check for valid types
  for i = 0, ncol-1 do begin
    match = where( table_type eq col[i], nmatch )
    if ( nmatch eq 0 ) then begin
      message, 'unrecognized data type ('+col[i]+') - assuming string type', $
        /INFO
      col[i] = 'C'
    endif
  endfor
  ; - remember the table types
  types = col
endelse


; next line contains the unit definitions
col_units = replicate( 'unit', ncol )
if ( found_data eq 0b ) then begin
  readf, in, thisline
  a = strmid( thisline, 0, 1 )
  if ( a ne '|' ) then begin
    ; no units
    found_data = 1b
  endif else begin
    ; these are the units
    if ( verbose eq 1 ) then print, thisline
    col_units = strtrim( strmid( thisline, spos, slen ), 2 )
  endelse
endif
cunits = col_units[usecol]


; next line contains the definition of null values
col_null = replicate( 'null', ncol )
if ( found_data eq 0b ) then begin
  readf, in, thisline
  a = strmid( thisline, 0, 1 )
  if ( a ne '|' ) then begin
    ; no nulls
    found_data = 1b
  endif else begin
    ; these are the nulls
    if ( verbose eq 1 ) then print, thisline
    col_null = strtrim( strmid( thisline, spos, slen ), 2 )
    ; next line MUST be the data
    readf, in, thisline
  endelse
endif


; check the number of types matches the number of columns
; if we had to determine the types ourselves, we may have a mismatch
if ( n_elements( types ) ne ncol ) then begin
  message, 'column name/type mismatch', /INFO
  err = 7
  return
endif


; IDL type of each column
col_type = intarr( nusecol )


; use column definitions to create a structure
for i = 0, nusecol - 1 do begin

  ; identify the table type ('DOUBLE', 'INT', etc)
  j = where( table_type eq types[usecol[i]], nj )
  if ( nj ne 1 ) then begin
    error = 9
    message, 'cannot recognize data type: '+types[usecol[i]], /INFO
    return
  endif

  ; define the corresponding IDL data type
  col_type[i] = idl_type[j]

  ; create or add to structure
  if ( i eq 0 ) then begin
    table_s = create_struct( names[usecol[i]], fix('',TYPE=col_type[i]) )
  endif else begin
    thiscol = names[usecol[i]]
    ; check for multiple columns with the same name and distinguish 
    ; them with a subscript;  this will never happen for a valid IPAC
    ; table, but then again...
    k = where( names[0:usecol[i]-1] eq thiscol, nk )
    if ( nk ne 0 ) then begin
      thiscol = thiscol + '_' + STRN( nk )
      message, 'WARNING: duplicate column name has beed modified: '+thiscol, $
        /INFO
      message, 'WARNING: ... but NAMES parameter has not been updated', /INFO
    endif
    table_s = create_struct( table_s, thiscol, fix('',TYPE=col_type[i]) )
  endelse

endfor


; create a template with null values
null = table_s
for i = 0, nusecol - 1 do null.(i) = idl_null.(col_type[i])


; do we want to skip reading the data?
if nodata then goto, FINISHED


; report progress every 'count' rows
count = long(nmax/10.0d) > 2000L


; initialize the table with nmax rows
irow = 0L
ntotal = nmax
table = replicate( table_s, nmax )


; extract columns from first line, which has already been read
ioerr = 0b
on_ioerror, conversion_error
for i = 0, nusecol-1 do begin
  tmp = strtrim(strmid( thisline, spos[usecol[i]], slen[usecol[i]] ),2)
  if ( tmp eq col_null[usecol[i]] or tmp eq strupcase(col_null[usecol[i]]) ) $
    then tmp = idl_null.(col_type[i])
  table[0L].(i) = tmp
  continue
  conversion_error: ioerr = 1b
endfor
on_ioerror, null


; read data
on_ioerror, conversion_error1
while ( not eof( in ) ) do begin
  readf, in, thisline
  irow = irow + 1L
  if verbose then $
    if ( irow mod count eq 0L ) then $
      message, 'read '+STRN(irow)+' rows', /INFO
  if ( irow eq ntotal ) then begin
    ; extend the table - this uses a lot of memory and is inefficient for
    ; large tables
    table = [ table, replicate( table_s, nmax ) ]
    ntotal = n_elements( table )
  endif
  for i = 0, nusecol-1 do begin
    tmp = strtrim(strmid( thisline, spos[usecol[i]], slen[usecol[i]] ),2)
    if ( tmp eq col_null[usecol[i]] or tmp eq strupcase(col_null[usecol[i]]) $
      ) then tmp = idl_null.(col_type[i])
    table[irow].(i) = tmp
    continue
    conversion_error1: ioerr = 1b
  endfor
endwhile
on_ioerror, null
table = temporary(table[0:irow])
if verbose then message, 'read '+STRN(irow+1L)+' rows', /INFO


; report type conversion error
if ioerr then begin
  message, !ERROR_STATE.MSG, /INFO
  err = 11
endif


; close file
FINISHED:
close, in
free_lun, in
if verbose then message, 'done', /INFO


end
