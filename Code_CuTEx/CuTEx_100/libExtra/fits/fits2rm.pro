;==============================================================================
;+
; Name: fits2rm
;
; Category: HESSI, UTIL
;
; Purpose: Read a response matrix from a FITS file.  Optionally read EBOUNDS
; extension information.
;
; Calling sequence:
; fits2rm, 'file.fits', RM=rm, EBINS=ph_edges, DETBINS=ct_edges
;
; Inputs:
; file - name of FITS file to read from.
;
; Output keywords:
; RM - response matrix, dimensioned n_detector_channels x n_input_energy_bins
; EBINS - energy boundaries for input energy bins
; DETBINS - energy boundaries for detector channels
; PRIMARY_HEADER - first header from FITS file.
; EXT_HEADER - header from SPECRESP extension.
; EBOUNDS_HEADER - header from EBOUNDS extension.
; ERR_MSG = error message.  Null if no error occurred.
; ERR_CODE - 0/1 if [ no error / an error ] occurred during execution.
;
; Calls:
; exist, fits_info, fxpar, mrdfits, n_dimensions, required_tags, str2arr, trim
;
; Written: Paul Bilodeau, RITSS / NASA-GSFC, 18-May-2001
;
; Modification History:
; 9-aug-2002, Paul Bilodeau - added support for 3-d arrays.
; 19-Feb-2004, Kim Tolbert - added check that file is an rm file by looking for type*matrix
; 20-Jul-2004, Kim - call fits_info and mrdfits with /silent
; 11-Oct-2005, Kim - use fits_open instead of fits_info.  fits_info can't handle long headers -
;   doesn't read the whole header, so if a keyword is near bottom of header, can't find it.
;-
;------------------------------------------------------------------------------
PRO fits2rm, file, $
             extnumber, $
             PRIMARY_HEADER=prim_header, $
             EXT_HEADER=ext_header, $
             RM=rm, $
             EBINS=ebins, $
             EBOUNDS_HEADER=ebounds_header, $
             DETBINS=detbins, $
             ERR_MSG=err_msg, $
             ERR_CODE=err_code

err_msg = ''
err_code = 1

CATCH, err
IF err NE 0 THEN BEGIN
    err_msg = !err_string
    RETURN
ENDIF

; changed from fits_info because crashing with long headers
fits_open, file, fcb, /no_abort, message=err_msg
if err_msg ne '' then return
n_ext = fcb.nextend

count = 0
; Check that file has extensions, and that first extension has ...TYPE...MATRIX... in header
if n_ext gt 0 then begin
	dummy = mrdfits(file, 1, header1, /silent)
	q = where (stregex (header1, 'type.*matrix', /boolean, /fold_case), count)
endif
if count eq 0 then begin
	err_msg = 'Aborting.  File is not a response matrix file - ' + file
	return
endif

checkvar, extnumber, 1

; read the extnumber extension
prim = mrdfits( file, 0, prim_header, /silent )
specresp = mrdfits( file, extnumber, ext_header, /silent )
ebounds = mrdfits( file, extnumber+1, ebounds_header, /silent )

taglist = [ 'ENERG_LO', 'ENERG_HI', 'N_GRP', 'F_CHAN', 'N_CHAN', 'MATRIX' ]
struct_ok = required_tags( specresp[0], taglist, MISSING_TAGS=missing_tags )
IF 1 - struct_ok THEN BEGIN
    err_msg = 'Input from file ' + file + ' is missing fields: ' + $
      str2arr( missing_tags, ', ' )
    RETURN
ENDIF

n_channels = fxpar( ext_header, 'DETCHANS', count=count )
IF count LE 0 THEN BEGIN
    err_msg = file + ' RMF extension header is missing DETCHANS keyword. ' + $
      'Cannot extract matrix.'
    RETURN
ENDIF

n_rows = N_Elements( specresp )

; Find the TTYPE keyword corresponding to the F_CHAN field.
ttypes = Where( Strmid(ext_header,0,Strlen('TTYPE')) EQ 'TTYPE', n_ttypes )
IF n_ttypes EQ 0L THEN BEGIN
    err_msg = 'HEADER from ' + file + ' has no TTYPE Keywords.  ' + $
      'Cannot extract matrix.'
    RETURN
ENDIF

FOR i=1L, n_ttypes DO BEGIN
    val = trim( fxpar(ext_header, 'TTYPE'+trim(i), COUNT=count ) )
    IF val EQ 'F_CHAN' THEN $
      minchan = LONG( trim( fxpar( ext_header, 'TLMIN'+trim(i) ) ) )
ENDFOR

IF NOT( exist(minchan) ) THEN BEGIN
    err_msg = 'HEADER from ' + file + $
      ' does not contain minimum channel value.  Cannot extract matrix.'
    RETURN
ENDIF

lo_thres = fxpar( ext_header, 'LO_THRES', COUNT=count )
have_lo_thres = count GT 0L AND valid_num( lo_thres )
IF have_lo_thres THEN $
  lo_thres = Strlen( lo_thres ) GT 0L ? Float( lo_thres ) : 0.

ebins = Fltarr( 2, n_rows )
ebins[0,*] = specresp.energ_lo
ebins[1,*] = specresp.energ_hi

; Are we extracting a diagonal matrix?
; is_diagonal = n_channels EQ n_rows AND $
;   Total(specresp.n_chan) EQ n_channels AND $
;   Total(specresp.n_grp) EQ n_channels

IF have_lo_thres THEN BEGIN
    ;; Extracting a matrix with a specified threshold
    rm = Make_Array( n_channels, n_rows, /FLOAT, VALUE=lo_thres )
    use = Where( specresp.n_grp GT 0, n_use )
    FOR i=0L, n_use-1L DO BEGIN
        idx = use[i]
        n_sub = Total(  specresp[idx].n_chan )
        indices = Lonarr( n_sub )
        ;; generate the indices of the used elements
        FOR j=0L, specresp[idx].n_grp-1L DO BEGIN
            tmp = Lindgen(  specresp[idx].n_chan[j] )
            indices[ tmp ] = tmp + specresp[idx].f_chan[j] - minchan
        ENDFOR
        rm[indices,idx] = specresp[idx].matrix[0L:n_sub-1L]
    ENDFOR
ENDIF ELSE rm = Reform( specresp.matrix )

;; A 3-D matrix was stored - the second and third dimensions must
;; be transposed.
IF n_dimensions( rm ) EQ 3 THEN rm = Transpose( Temporary(rm), [0,2,1] )

struct_ok = required_tags( ebounds[0], ['CHANNEL','E_MIN','E_MAX'], $
      MISSING_TAGS=missing_tags )

IF NOT( struct_ok ) THEN BEGIN
    err_msg = 'EBOUNDS extension from file ' + file + $
      ' is missing fields: ' + str2arr( missing_tags, ', ' )
    RETURN
ENDIF

detbins = Fltarr( 2, n_channels )
detbins[0,*] = ebounds.e_min
detbins[1,*] = ebounds.e_max
err_code = 0

END
