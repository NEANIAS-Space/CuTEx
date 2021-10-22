;+
; NAME:
;	HILBERT_SPEC
;
; PURPOSE:
;	This function estimates the Hilbert-Huang amplitude spectrum of an 
;	input matrix of time series (e.g. intrinsic mode functions).
;
; CATEGORY:
;	Time series analysis
;
; CALLING SEQUENCE:
;	Result = HILBERT_SPEC( Data, Dt )
;
; INPUTS:
;	Data:  Matrix of time series (e.g. intrinsic mode functions), of 
;		type floating point.  Dimensions are length of time series by 
;		number of time series.
;	Dt:  The time step of the data in Data.  Of type floating point.  If 
;		TIME is input, Dt is determined from TIME.
;
; KEYWORD PARAMETERS:
;	FILTER:  A string containing the name of the filter to use for two 
;		dimensional smoothing of the spectrum.  The default is set in 
;		FILTER_ND.pro.
;	FREQ:  Returns a vector of type floating point containing the 
;		frequency values corresponding to the spectral output.
;	INSTFREQ:  Returns a matrix containing the instantaneous frequencies 
;		of each component time series.  Dimensions are the same as 
;		Data, except for one less value in each of the time series.  
;		Of type floating point.
;	MARGINAL:  Returns a vector of type floating point containing the 
;		marginal power spectrum, determined by integrating the 
;		instantaneous power spectrum over time.
;	NFILTER:  An odd integer scalar containing the window width for two 
;		dimensional smoothing of the spectrum.  If not set then no 
;		smoothing is applied.
;	TIME:  A vector of type floating point containing the time values 
;		corresponding to the values in Data.
;
; OUTPUTS:
;	Result:  The time-frequency-amplitude matrix containing the Hilbert 
;		amplitude spectrum.  Dimensions are number of frequency 
;		components by length of time series minus one.  Of type 
;		floating point.
;	FREQ, INSTFREQ, MARGINAL
;
; USES:
;	CONSTANTS.pro
;	FILTER_ND.pro
;	FIRST_DIFF.pro
;	IMAG.pro
;	REAL.pro
;
; PROCEDURE:
;	This function uses the IDL HILBERT function to estimate the Hilbert 
;	transform of the inputted time series, and then uses this to estimate 
;	the instantaneous frequency spectrum according to Huang et alii.
;
; EXAMPLE:
;	Given a time series Y, with time step DT and time vector TIME.
;	Estimate the intrinsic mode functions.
;	  imf = EMD( y )
;	Estimate the Hilbert-Huang instantaneous spectrum.
;	  hhs = HILBERT_SPEC( imf, dt, freq=freq )
;	Contour plot the instantaneous power spectrum.
;	  contour, hhs^2, time[0:n_elements(time)-2], freq
;
; REFERENCES:
;	Huang et al, Royal Society Proceedings on Math, Physical, 
;	  and Engineering Sciences, vol. 454, no. 1971, pp. 903-995, 
;	  8 March 1998
; 
; MODIFICATION HISTORY:
; 	MatLab:	Ivan Magrin-Chagnolleau (ivan@ieee.org)
;	Matlab:	Anthony Wilson (anthony.wilson:zoo.ox.ac.uk), 2003
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2003-08-13
;			(adapted MatLab to IDL)
;	Modified:	DAS, 2004-07-27 (documentation for routine library)
;	Modified:	DAS, 2004-10-25 (outsourced work to unwrap.pro)
;	Modified:	DAS, 2005-08-05 (replaced sum_row.pro with total)
;-

FUNCTION HILBERT_SPEC, $
	Data, $
	Dt, $
	FILTER=filter, NFILTER=nfilter, $
	FREQ=freq, $
	INSTFREQ=instfreq, MARGINAL=marginal, $
	TIME=time, $
	EDGE_TRUNCATE=edgetruncateopt

;***********************************************************************
; Constants and Options

; Number of components in the Data matrix
nt = n_elements( data[*,0] )
ncomp = n_elements( data[0,*] )

; Absolute constants
constants, pi=pi, im=im

; A numerical fix term
epsilon = 0.00001

; Determine time coordinates of spectral output
if keyword_set( time ) then begin
  t = time
  dt = t[1] - t[0]
endif else begin
  if not( keyword_set( dt ) ) then begin
    dt = 1.
  endif else begin
    t = dt * findgen( nt ) + dt / 2.
  endelse
endelse

;***********************************************************************
; Calculate the Instanteous Spectrum

; Define the Hilbert transform of data
z = 0. * im * data
for i = 0, ncomp - 1 do z[*,i] = data[*,i] + im * real( hilbert( data[*,i] ) )

; Transform z to polar coordinates
; Modulus of z
magz = abs( z )
; Phase of z
angz = atan( imag( z ) / real( z ) )
id = where( real( z ) lt 0 , nid )
if nid ne 0 then angz[id] = angz[id] + pi

; Output matrices
; Amplitude spectrum
spec = fltarr( nt, nt )
; Instantaneous frequency
instfreq = fltarr( nt, ncomp )
; Instantaneous frequency index
instfreqid = intarr( nt, ncomp )

; Iterate through component time series
for i = 0, ncomp - 1 do begin

  ; Unwrap polar coordinate phase
  angz[*,i] = unwrap( angz[*,i] )

  ; Calculate instantaneous frequency
  ; This is the derivative of the phase (and convert to positive frequencies).
  instfreq[*,i] = [ angz[1,i]-angz[0,i], $
      ( first_diff( angz[*,i], /forward ) )[0:nt-2] ] / ( 2. * pi )
  ; Instantaneous frequency number
  instfreqid[*,i] = floor( abs( instfreq[*,i] ) * nt * 2 )

  ; Calculate Hilbert amplitude spectrum.
  ; This is the Hilbert transform amplitude corresponding to the given time 
  ; and frequency
  ctr = 0
  ; Iterate through time
  for j = 0, nt - 1 do begin
    ; Copy the frequency number
    id = instfreqid[j,i]
    ; Determine if it is a legal frequency number
    if ( id ge 0 ) and ( id le nt - 1 ) then begin
      ; Convert to the power spectrum
      spec[id,j] = spec[id,j] + magz[j,i]^2
    endif else begin
      ctr = ctr + 1
    endelse
  endfor
  if ctr ne 0 then begin
    print, 'Warning:  ' + str( ctr, 0 ) $
       + ' invalid frequency values on time series ' + str( i, 0 )
  endif

endfor

;***********************************************************************
; Calculate Optional Output

; Determine time coordinates of spectral output
freq = findgen( nt ) / nt * 0.5 / dt

; Smooth spectral power matrix using a Hanning filter
if keyword_set( nfilter ) then begin
  spec = filter_nd( spec, nfilter, 'hanning', edge_truncate=edgetruncateopt )
endif

; Calculate the marginal power spectrum by integrating over time
marginal = total( spec, 2 ) * dt

;***********************************************************************
; The End

return, spec
END
