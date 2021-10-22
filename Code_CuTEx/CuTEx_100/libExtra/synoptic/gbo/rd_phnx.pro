;---------------------------------------------------------------------------
;+
; NAME:
;       Radiospectrogram FITS File reader
;
; PURPOSE:
;       Reads (solar) radio spectrograms from RAG-FITS files,
;       including non-regular axes. 
;
; CALLING SEQUENCE:
;       rd_phnx, filename [, spectrogram, time_axis,
;       frequency_axis ]
;
; INPUT:
;       filename: string containing the name of the file with extension
; OUTPUTS:
;       image: 2D array containing the spectrogram.
;       frequency_axis: 1D array containing the frequency axis, in MHz
;       time_axis: 1D array containing the time in seconds from 1 Jan
;                  1979 (can be converted in any other time formats
;                  with Anytim). Can be set relative with the
;                  RELATIVE_TIME keyword. Uses DATE_OBS to set the
;                  correct time.
; KEYWORDS:
;       AXIS_HEADER: returns a structure containing the axis binary
;                    table header information
;       MAIN_HEADER: returns a structure containing the main header
;                    information 
;       RELATIVE_TIME: If set, the time axis starts with zero instead
;                      of starting with the value of DATE_OBS
; IMPLEMENTATION:
;       The procedure has been tested on FITS from ETHZ and Potsdam
;
; MODIFICATION HISTORY
;       Created: A. Csillaghy, ETHZ, October 1992
;       For IDL Sun in May 1993, A.Cs.
;       SILENT in june, 93, A.Cs
;       DATAMIN, DATAMAX; When only filename provided, the header
;               is read but not the array. Oct. 93, A.Cs
;       RELATIVETIME in august 1995, ACs
;       Read also compressed file in November 95, ACs
;       Adaptation for IDL5/SSW/Ragview in March 98 -- ACs
;       Adaptation for DATE-OBS keyword with time; Dec. 98, P.Hackenberg
;       Extended to read "transposed" fits data and extensions with
;               only time or frequency axis; Dec. 98, P.Hackenberg,
;               AIP Potsdam
;       Changed name from ragfitsread to radio_spectro_fits_read, 
;               adapted for ssw Nov 1999, csillag@ssl.berkeley.edu
;       Light version for Unix and PC in Jan/March 2000, csillag@ssl.berkeley.edu
;       Modified to handle compressed files and rename, 6-Dec-2000, Zarro (EIT/GSFC)
;-

PRO rd_phnx, filename, spectrogram, time_axis, frequency_axis, $
         MAIN_HEADER=main_header, $
         AXIS_HEADER=axis_header, $
         VERBOSE=verbose, $
         _EXTRA=_extra,$
         err=err

err=''
CheckVar, verbose, 0
CheckVar, noscale, 0
silent=verbose? 0:1
fscale=noscale? 1:0


IF N_Params() EQ 0 THEN BEGIN
    err= 'Usage: rd_phnx, filename, image, xAxis, yAxis ]'
    message,err,/INFORMATIONAL 
    RETURN
ENDIF

f = loc_File( filename, count=count )
IF count EQ 0 THEN BEGIN 
    err= filename_in + " not found"
    message,err, /CONTINUE
    RETURN
ENDIF

cfile=find_compressed(filename,err=err)
if err ne '' then return

MReadFITS, cfile, main_header, spectrogram, NOSCALE=noscale

IF verbose THEN BEGIN
    Print, 'Header of FITS file: '
    HELP, main_header, /str
ENDIF

; As discussed in
; ftp://fits.cv.nrao.edu/fits/data/samples/year-2000/year2000.txt
; there is a new DATExxx syntax (CCYY-MM-DDThh:mm:ss) which allows
; to include the time into the date. Thus the keywords TIME-OBS and
; TIME-END are no longer necessary, if the time is already specified
; in DATE-OBS and DATE-END.
; Peter Hackenberg 23.12.1998
;
; If time-obs is present, we store its contents into date-obs so that
; we have a standard date-obs and an obsolete time-obs. 

IF ChkTag( main_header, 'TIME_D$OBS') THEN BEGIN 
    main_header.date_d$obs =  main_header.date_d$obs + 'T' + main_header.time_d$obs
ENDIF

IF ChkTag( main_header, 'TIME_D$END') THEN BEGIN 
    main_header.date_d$end =  main_header.date_d$end + 'T' + main_header.time_d$end
ENDIF


main_header.cdelt2=abs( main_header.cdelt2)
main_header.cdelt1=abs( main_header.cdelt1)

; Usually radio spectrograms have a frequency and a time axis.
; In the fits definitions is no compulsory rule specifying
; which of these axes goes first and which is the second one.
; The RagView program uses time as x axis and frequency as y axis.
; We try to figure out from CTYPE1 and CTYPE2, whether the fits data 
; are already in right order or not. If not, we "transpose" the data.
; Peter Hackenberg 23.12.1998

err =  ''
FxBOpen, unit, cfile, 'Axes', axis_header, ERR=err

IF err EQ '' THEN BEGIN 
    axis_header = FITSHead2Struct( axis_header )
    
    FxBRead, unit, axis_1, 1 
    
    IF axis_header.tfields EQ 2 THEN BEGIN 
        FxBRead, unit, axis_2, 2 
    ENDIF ELSE BEGIN 
        axis_2 = Findgen( main_header.naxis2 ) * main_header.cdelt2 + main_header.crval2
    ENDELSE 
ENDIF ELSE BEGIN 
    axis_1 = Findgen( main_header.naxis1) * main_header.cdelt1 +  main_header.crval1
    axis_2 = Findgen( main_header.naxis2 ) * main_header.cdelt2 + main_header.crval2
ENDELSE


IF Grep( 'time', Strlowcase( main_header.ctype1 ) ) NE '' THEN BEGIN 
    time_axis =  axis_1
    frequency_axis =  axis_2
ENDIF ELSE BEGIN 
    frequency_axis =  axis_1
    time_axis = axis_2
    spectrogram =  Transpose( temporary( spectrogram ) )
ENDELSE 

time_axis =  time_axis + AnyTim( main_header.date_d$obs, /SEC )

free_lun,unit
END
