;+
; NAME:
;	SAMPLE_PDF
;
; PURPOSE:
;	This function randomly samples a given probability density function.
;
; CATEGORY:
;	Statistics
;
; CALLING SEQUENCE:
;	Result = SAMPLE_PDF( Xpdf, Pdf, Nsample )
;
; INPUT:
;	Pdf:  A vector of type floating point containing the probability 
;		density function values.
;	Xpdf:  A vector of type floating point containing the location of the 
;		values in Pdf.
;	Nsample:  A scalar of type integer containing the sampling size.
;
; KEYWORD PARAMETERS:
;	-
;
; OUTPUT:
;	Result:  A vector of type floating point containing the randomly 
;		sampled locations.
;
; USES:
;	PDF_TO_CDF.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This function randomly samples the quantiles of a given probability
;	density function.
;
; EXAMPLE:
;	Define the PDF values and where they are.
;	  pdf = [ 0.1, 0.25, 0.3, 0.25, 0.1 ]
;	  xpdf = [ 0., 1., 2., 4., 5. ]
;	Sample from the PDF 1000 times.
;	  result = sample_pdf( xpdf, pdf, 1000 )
;	The result should have sampled 0 about 0.1*1000 times, etc.
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2003-09-25
;-

;***********************************************************************

FUNCTION SAMPLE_PDF, $
	Xpdf, Pdf, Nsample

;***********************************************************************
; Constants and Variables

; The size of the input PDF
npdf = n_elements( pdf )

; The location vector of the values in PDF
if n_elements( xpdf ) ne npdf then stop

;***********************************************************************
; Sample Randomly from the CDF

; Estimate the Cumulative Distribution Function
cdf = pdf_to_cdf( xpdf, pdf, xcdf=xcdf )

; Initialise an output vector of random sampling from the uniform distribution
xsample = randomu( seed, nsample )

; Iterate through samples.
; Check if the counter has to be a long integer.
i0 = 0
if var_type( nsample ) ne 2 then i0 = long( i0 )
for i = i0, nsample - 1 do begin
  ; Find randomly sampled point on the CDF
  id = max( where( cdf le xsample[i] ) )
  ; Take the location of that sampled point.
  ; Note that we take the location of that segment defined for the PDF, not 
  ; for the CDF, as this is the more representative location of the segment.
  xsample[i] = xpdf[id+1]
endfor

;***********************************************************************
; The End

return, xsample
END
