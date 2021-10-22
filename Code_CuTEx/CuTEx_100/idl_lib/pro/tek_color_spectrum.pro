;+
; NAME:
;	TEK_COLOR_SPECTRUM
;
; PURPOSE:
;	This function returns colour indices that make a spectrum out of the 
;	TEK_COLOR colours.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	Result = TEK_COLOR_SPECTRUM( [Ncolors] )
;
; INPUTS:
;	-
;
; OPTIONAL INPUTS:
;	Ncolors:  The number of indices to return.  Index values are 
;		interpolated between the default 12 values (so some values are 
;		repeated if Ncolors>12).
;
; KEYWORD PARAMETERS:
;	-
;
; OUTPUT:
;	Result:  The vector of colour index values.
;
; USES:
;	-
;
; PROCEDURE:
;	This function returns a set vector of index values.
;
; EXAMPLE:
;	tek_color
;	ncolors = 12
;	Result = TEK_COLOR_SPECTRUM( ncolors )
;	plot, findgen( ncolors + 2 ), /nodata
;	plots, findgen( ncolors ) + 1, findgen( ncolors ) + 1, psym=4, $
;	    color=result
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2002-09-11.
;	Modified:	DAS, 2004-07-21 (added library documentation, Ncolors 
;			input).
;-

FUNCTION TEK_COLOR_SPECTRUM, $
	Ncolors

;***********************************************************************
; Create Colour Spectrum Index Vector

color = [ 6, 13, 2, 8, 7, 9, 3, 10, 5, 11, 4, 12 ]

;***********************************************************************
; Option to Return Longer or Shorter Vector of Colour Indices

if keyword_set( ncolors ) then begin
  ncolors0 = n_elements( color )
  id = round( findgen( ncolors ) / ( ncolors - 1. ) * ( ncolors0 - 1 ) )
  color = color[id]
endif

;***********************************************************************
; The End

return, color
END
