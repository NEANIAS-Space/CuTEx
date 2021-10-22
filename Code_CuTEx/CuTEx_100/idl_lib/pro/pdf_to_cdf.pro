;+
; NAME:
;	PDF_TO_CDF
;
; PURPOSE:
;	This function estimates a cumulative distribution function from a
;	given probability density function.
;
; CATEGORY:
;	Statistics
;
; CALLING SEQUENCE:
;	Result = PDF_TO_CDF( Xpdf, Pdf )
;
; INPUT:
;	Pdf:  A vector of type floating point containing the probability 
;		density function values.
;	Xpdf:  A vector of type floating point containing the location of the 
;		values in Pdf.
;
; KEYWORD PARAMETERS:
;	INTERPOLATE:  A vector of type floating point containing the location 
;		to calculate the values in Result.  The default is halfway 
;		between the values in Xpdf.
;	LSQUADRATIC:  If set the interpolation is done using a least squares 
;		quadratic fit.  The default is a linear interpolation.
;	QUADRATIC:  If set the interpolation is done by fitting a quadratic. 
;		The default is a linear interpolation.
;	SPLINE:  If set theinterpolation is done by fitting a cubic spline. 
;		The default is a linear interpolation.
;
; OUTPUT:
;	Result:  A vector of type floating point containing the cumulative 
;		distribution function values calculated from Pdf at the same 
;		positions as in Pdf.
;	XCDF:  A vector of type floating point containing the location of the 
;		values in Result.
;
; USES:
;	-
;
; PROCEDURE:
;	This function calculates the CDF values by summing over the preceding 
;	PDF values.
;
; EXAMPLE:
;       Define the PDF values and where they are.
;         pdf = [ 0.1, 0.25, 0.3, 0.25, 0.1 ]
;         xpdf = [ 0., 1., 2., 4., 5. ]
;	Calculate the CDF.
;	  result = pdf_to_cdf( xpdf, pdf, xcdf=xcdf )
;	result should be [ 0.1, 0.35, 0.65, 0.9, 1.0 ].
;	xcdf should be [ 0.5, 1.5, 3., 4.5, 5.5 ].
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone, 2003-07-21
;	Modified:	DAS, 2003-09-25 (added Xpdf input, INTERPOLATE, 
;		LSQUADRATIC, QUADRATIC, SPLINE keywords, XCDF output)
;-

;***********************************************************************

FUNCTION PDF_TO_CDF, $
	Xpdf, Pdf, $
	INTERPOLATE=interpolate, $
	XCDF=xcdf, $
	LSQUADRATIC=lsquadraticopt, QUADRATIC=quadraticopt, SPLINE=splineopt

;***********************************************************************
; Constants and Variables

; The size of the input PDF
npdf = n_elements( pdf )

; The location vector of the values in PDF
if n_elements( xpdf ) ne npdf then stop

;***********************************************************************
; Calculate XCDF, the Vector of Locations for the CDF Calculation

; Interpolate to points halfway between those in Xpdf.
; We are assuming here that values in PDF are the average values for boxes 
; centred on between the neighbouring XPDF values.  Thus, CDF[0] = PDF[0] not 
; at XPDF[0] but at ( XPDF[0] + XPDF[1] ) / 2.
xcdf = [ ( xpdf[0:npdf-2] + xpdf[1:npdf-1] ) / 2., $
    xpdf[npdf-1] + ( xpdf[npdf-1] - xpdf[npdf-2] ) / 2. ]

;***********************************************************************
; Estimate CDF

; Initialise output vector (CDF)
cdf = fltarr( npdf )

; Estimate the first value
cdf[0] = pdf[0]

; Iterate through the rest of the locations for estimation
for i = 1, npdf - 1 do begin
  cdf[i] = cdf[i-1] + pdf[i]
endfor

; Interpolate to locations specified in INTERPOLATE
if keyword_set( interpolate ) then begin
  ; Interpolate
  cdf = interpol( cdf, xcdf, interpolate, lsquadratic=lsquadraticopt, $
      quadratic=quadraticopt, spline=splineopt )
  ; Take the new location values
  xcdf = interpolate
endif

;***********************************************************************
; The End

return, cdf
END
