;+
; NAME:
;	PCA
;
; PURPOSE:
;	This procedure calculates the principal components and empirical 
;	orthogonal functions of a given data set.
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	PCA, Data
;
; INPUT:
;	Data:  The input data array, of format N_POINT*N_SAMPLE.  For 
;		spatiotemporal data, N_POINT is the number of spatial 
;		elements while N_SAMPLE is the number of time elements.
;
; KEYWORD PARAMETERS:
;	CORRELATION:  If set, the eigen-decomposition is performed on the 
;		correlation matrix.  The default is the covariance matrix.
;	DOUBLE:  If set, the calculations are carried out in double precision 
;		arithmetic.  The default is single precision.
;	EVALUE:  An output vector containing the eigenvalues of the covariance 
;		matrix, of length N_PC (in descending order).
;	EVECTOR:  An output matrix containing the empirical orthogonal 
;		functions (EOFs, = eigenvectors of the covariance matrix), of 
;		format N_POINT*N_PC.
;	NAN:  If set, the procedure will ignore missing values (tagged as NaN) 
;		in the input data.  The default is to include NaNs.
;	NO_ANOMALY:  If set, the means are not subtracted from each point in 
;		the input data.  The default is to subtract the mean (and thus 
;		estimate the covariance matrix).
;	OUTDATA:  An output matrix containing the modified version of the 
;		input matrix Data used for the estimation (e.g. with means 
;		subtracted).
;	PC:  An output matrix containing the principle components, of format 
;		N_SAMPLE*NPC.
;	WEIGHT_POINT:  A vector of length N_POINT containing weightings for 
;		the DATA point fields.
;	WEIGHT_SAMPLE:  A vector of length N_SAMPLE containing weightings for 
;		the DATA sample fields.
;	UNWEIGHT:  If set, the EOFs and PCs are unweighted before being 
;		returned.  The default is the return of weighted EOFs and PCs. 
;		Note this does not matter if WEIGHT_POINT and WEIGHT_SAMPLE 
;		are not set.
;
; OUTPUT:
;	EVALUE, EVECTOR, OUTDATA, PC
;
; USES:
;	dimension.pro
;	real.pro
;	var_type.pro
;
; PROCEDURE:
;	This procedure uses the IDL5.4 EIGENQL routine to calcalute the 
;	eigenvalues and eigenvectors of the covariance matrix of the data.  
;	The PCOMP routine does this too, but does not incorporate weighting, 
;	and seems to be slower and more hungry for memory.
;	For more information on PCA see:
;	von Storch, H., and F. W. Zwiers.  1999.  Statistical Analysis in 
;	  Climate Research.  Cambridge University Press, Cambridge, 484p.
;	North, G. R., T. L. Bell, R. F. Cahalan, and F. J. Moeng.  1982.  
;	  Sampling errors in the estimation of empirical orthogonal 
;	  functions.  Monthly Weather Review, 110, 699-706.
;
; EXAMPLE:
;	Create a data set with both the points and space defined by sinusoidal 
;	functions.  Add in a bit of Gaussian noise.
;	  nt = 20 & m = 16
;	  data = fltarr(m,nt)
;	  for i=0,nt-1 do begin
;	    data[*,i] = cos(findgen(m)/(m-1.)*6.28) * cos(2.*i/(nt-1.)*6.28) $
;                       + 0.5 * sin(3.*findgen(m)/(m-1.)*6.28) $
;	                      * sin(4.*i/(nt-1.)*6.28) $
;                       + 0.2 * randomn(1,m)
;	  endfor
;	Calculate the principal components.
;	PCA, data, EVALUE=evalue, EVECTOR=evector, PC=pc
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2001-05-11.
;	Modified:	DAS, 2002-01-29 (added UNWEIGHT keyword).
;	Modified:	DAS, 2003-05-12 (edited format)
;	Modified:	DAS, 2004-08-18 (added CORRELATION, NO_ANOMALY and NAN 
;			keywords; removed bias in covariance matrix estimation)
;	Modified:	DAS, 2004-10-27 (added capability for complex input, 
;			very large input matrices; added OUTDATA keyword)
;	Modified:	DAS, 2005-08-05 (replaced SUM.PRO use with TOTAL)
;	Modified:	DAS, 2007-04-02 (fixed bug in implementation of TOTAL)
;	Modified:	DAS, 2007-10-31 (fixed bug in covariance calculation; 
;			changed WEIGHT keyword to WEIGHT_POINT;  added 
;			WEIGHT_SAMPLE keyword;  cleaned code and documentation)
;-

;***********************************************************************

PRO PCA, $
	Data, $
	DOUBLE=double_opt, NAN=nan_opt, $
	UNWEIGHT=unweight_opt, $
	CORRELATION=correlation_opt, NO_ANOMALY=no_anomaly_opt, $
	WEIGHT_POINT=weight_point, WEIGHT_SAMPLE=weight_sample, $
	EVALUE=e_value, EVECTOR=e_vector, PC=pc, $
	OUTDATA=data_out

;***********************************************************************
; Constants

; Data dimensions
n_point = n_elements( data[*,0] )
n_sample = n_elements( data[0,*] )

; The starting index value for loops (whether long integer is needed)
i_point_0 = 0
if var_type( n_point ) eq 3 then i_point_0 = long( i_point_0 )
i_sample_0 = 0
if var_type( n_sample ) eq 3 then i_sample_0 = long( i_sample_0 )

; Double precision option
double_opt = keyword_set( double_opt )
; Double precision seed
one = 1.
if double_opt eq 1 then one = double( one )

; Complex input flag
complex_flag = var_type( data )
if ( complex_flag eq 6 ) or ( complex_flag eq 9 ) then begin
  complex_flag = 1
endif else begin
  complex_flag = 0
endelse

; Option to ignore missing values
nan_opt = keyword_set( nan_opt )

; Unweighting of EOFs option
unweight_opt = keyword_set( unweight_opt )

; Data point weightings
temp = n_elements( weight_point )
if temp ne n_point then begin
  ; Print warning if weight_point is bad size
  if temp gt 0 then begin
    print, 'Warning: ignoring WEIGHT_POINT because it is the wrong size.'
  endif
  ; Set the default uniform weightings
  if temp eq 0 then weight_point = one + fltarr( n_point )
endif else begin
  ; Normalise weightings
  weight_point = weight_point / total( weight_point ) * n_point
endelse
if double_opt then weight_point = double( weight_point )

; Data sample weightings
temp = n_elements( weight_sample )
if temp ne n_sample then begin
  ; Print warning if weight_sample is bad size
  if temp gt 0 then begin
    print, 'Warning: ignoring WEIGHT_SAMPLE because it is the wrong size.'
  endif
  ; Set the default uniform weightings
  if temp eq 0 then weight_sample = one + fltarr( n_sample )
endif else begin
  ; Normalise weightings
  weight_sample = weight_sample / total( weight_sample ) * n_sample
endelse
if double_opt then weight_sample = double( weight_sample )

;***********************************************************************
; Prepare Data Array

; Copy data for covariance matrix calculations
data_w = one * data
if ( double_opt eq 1 ) and ( complex_flag eq 1 ) then begin
  data_w = dcomplex( data_w )
endif

; Subtract mean
if not( keyword_set( no_anomaly_opt ) ) then begin
  temp = reform( weight_sample, 1, n_sample )
  for i = i_point_0, n_point - 1 do begin
    data_w[i,*] = data_w[i,*] $
        - total( data_w[i,*] * weight_sample, nan=nan_opt ) $
        / total( finite( data_w[i,*] ) * weight_sample )
  endfor
endif

; Standardise the data (ie divide by the standard deviation) if desired
if keyword_set( correlation_opt ) then begin
  temp = reform( weight_sample, 1, n_sample )
  for i = i_point_0, n_point - 1 do begin
    if complex_flag eq 0 then begin
      temp_std = data_w[i,*] ^ 2
      temp_std = total( weight_sample * temp_std, nan=nan_opt ) $
          / total( weight_sample * finite( temp_std ) )
      data_w[i,*] = data_w[i,*] / sqrt( temp_std )
    endif else begin
      temp_std = data_w[i,*] * conj( data_w[i,*] )
      temp_std = total( real( weight_sample * temp_std ), nan=nan_opt ) $
          / total( weight_sample * finite( temp_std ) )
    endelse
  endfor
  temp_std = 0
endif

; Weight data for covariance matrix calculations
temp_weight = sqrt( weight_point )
for i = i_sample_0, n_sample - 1 do begin
  data_w[*,i] = temp_weight * data_w[*,i] * sqrt( weight_sample[i] )
endfor

; Calculate the conjugate transpose of the data
data_w_ct = transpose( data_w )
if complex_flag eq 1 then data_w_ct = conj( data_w_ct )

;***********************************************************************
; Estimate the Covariance Matrix

; If the sample size is smaller than the dimension of the space, then it is
; faster to first calculate the eigenvectors of the transpose.
if n_sample lt n_point then begin
  ; Initialise the covariance matrix
  cov = one * fltarr( n_sample, n_sample )
  ; Iterate through data points
  for i = i_sample_0, n_sample - 1 do begin
    for j = i_sample_0, i do begin
      ; Estimate the covariance
      temp = data_w[*,i] * reform( data_w_ct[j,*] )
      temp = total( temp, nan=nan_opt ) $
          / total( finite( temp ) * weight_point )
      cov[i,j] = temp
      cov[j,i] = temp
    endfor
  endfor
; Otherwise do things normally
endif else begin
  ; Initialise the covariance matrix
  cov = one * fltarr( n_point, n_point )
  ; Iterate through data points
  for i = i_point_0, n_point - 1 do begin
    for j = i_point_0, i do begin
      ; Estimate the covariance
      temp = reform( data_w[i,*] ) * data_w_ct[*,j]
      temp = total( temp, nan=nan_opt ) $
          / total( finite( temp ) * weight_sample )
      cov[i,j] = temp
      cov[j,i] = temp
    endfor
  endfor
endelse

;***********************************************************************
; Do Eigen-reduction

; Do the eigen-reduction
if complex_flag eq 0 then begin
  e_value = eigenql( cov, double=double_opt, eigenvectors=e_vector, $
      overwrite=1 )
endif else begin
  e_value = la_eigenql( cov, double=double_opt, eigenvectors=e_vector )
endelse

; Convert point eigenvectors back to sample eigenvectors if necessary (see 
; above)
if n_sample lt n_point then begin
  ; Normalise eigenvalues
  e_value = e_value * n_point / n_sample
  ; Convert back to sample eigenvectors
  temp = fltarr( n_point, n_sample )
  for i = i_sample_0, n_sample - 1 do begin
    for j = i_point_0, n_point - 1 do begin
      temp[j,i] = total( reform( e_vector[*,i] ) * data_w_ct[*,j], $
          nan=nan_opt )
    endfor
  endfor
  e_vector = temporary( temp )
  ; Normalise
  for i = i_sample_0, n_sample - 1 do begin
    if complex_flag eq 0 then begin
      e_vector[*,i] = e_vector[*,i] $
          / sqrt( total( e_vector[*,i] ^ 2, nan=nan_opt ) )
    endif else begin
      e_vector[*,i] = conj( e_vector[*,i] ) $
          / sqrt( total( e_vector[*,i] * conj( e_vector[*,i] ), nan=nan_opt ) )
    endelse
  endfor
endif

; Correct for negative eigenvalues (machine precision)
id = where( e_value lt 0, n_id )
if n_id gt 0 then e_value[id] = 0.

; Estimate the principal components.
; Initialise the matrix of principal components
n_pc = n_elements( e_vector[0,*] )
pc = one * fltarr( n_sample, n_pc )
; The starting index value for loops (whether long integer is needed)
i_pc_0 = 0
if var_type( n_pc ) eq 3 then i_pc_0 = long( i_pc_0 )
; Iterate through PCs and samples
for i = i_pc_0, n_pc - 1 do begin
  for j = i_sample_0, n_sample - 1 do begin
    ; Project the data onto the eigenvector (EOF)
    pc[j,i] = total( e_vector[*,i] * reform( data_w_ct[j,*] ), nan=nan_opt )
    ;pc[j,i] = total( e_vector[*,i] * reform( data_w_ct[j,*] ), nan=nan_opt ) $
    ;    / total( ( e_vector[*,i] ^ 2 ) * reform( finite( data_w_ct[j,*] ) ) )
  endfor
endfor

; Unweight EOFs
if unweight_opt then begin
  temp_weight = sqrt( weight_point )
  for i = 0, n_pc - 1 do e_vector[*,i] = e_vector[*,i] / temp_weight
endif

; Unweight PCs
if unweight_opt then begin
  temp_weight = sqrt( weight_sample )
  for i = 0, n_pc - 1 do pc[*,i] = pc[*,i] / temp_weight
endif

; Output modified input matrix
data_out = temporary( data_w )

;***********************************************************************
; The End

return
END
