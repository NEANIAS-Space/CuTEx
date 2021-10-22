; FUNCTION TO EMULATE THE BEHAVIOUR OF COMPUTING A MASK FROM THE OBJECT ROI IN IDL 
; ASSUMING A MASK_RULE = 2 = Boundary + Interior. All pixels falling on or within a region’s boundary are set. 
; 
; THE ROI OBJECT HAS TYPE IDLANROI
; RoI->ComputeMask(dimension=[x, y]) FROM IDL

; INPUTS:
; XVERT, YVERT = position of the vertex for the masked region
; DIMS = Size of the FINAL MASK

function maskRoIfromVertex, Xvert, Yvert, dims=dims
	;print, 'Masking around the following polygon '
	;print, 'X vert: ', Xvert
	;print, 'Y vert: ', Yvert
	
	xsize = dims[0]
	ysize = dims[1]
	
		
	xMask = indgen(xsize)
	yMask = indgen(ysize)
	
	; building up the 2D arrays for the central pixel position
	
	xxMask = xMask # (yMask*0. + 1.)
	yyMask = (xMask*0. + 1.) # yMask
	
	
	maskpoints = inside(xxMask, yyMask, Xvert, Yvert)
	maskpoints2 = inside(xxMask+0.5, yyMask+0.5, Xvert, Yvert)
	maskpoints3 = inside(xxMask+0.5, yyMask-0.5, Xvert, Yvert)
	maskpoints4 = inside(xxMask-0.5, yyMask-0.5, Xvert, Yvert)
	maskpoints5 = inside(xxMask-0.5, yyMask+0.5, Xvert, Yvert)
	
	ToTMask = maskpoints + maskpoints2 + maskpoints3 + maskpoints4 + maskpoints5
	Mask2D  = reform(ToTMask, xsize, ysize)
		
	return, Mask2D

end