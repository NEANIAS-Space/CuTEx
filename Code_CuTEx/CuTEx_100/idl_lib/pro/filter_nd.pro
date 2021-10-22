;+
; NAME:
;	FILTER_ND
;
; PURPOSE:
;	This function returns a smoothed version of an N-dimensional input 
;	array.
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	Result = FILTER_ND( X, Width, Window )
;
; INPUTS:
;	X:  An array of type floating point.
;	Width:  The width, of type integer, of the smoothing window.  Note 
;		that if Width is even, FILTER_ND uses Width+1.
;	Window:  A string containing the name of the smoothing window to 
;		return.  Options are 'boxcar', 'gaussian', 'hanning', 
;		'triangle'.  The default is a boxcar window.
;
; KEYWORD PARAMETERS:
;	EDGE_TRUNCATE:  Set this keyword to apply the smoothing to all points.
;		If the neighbourhood around a point includes a point outside 
;		the array, the nearest edge point is used to compute the 
;		smoothed result.  If EDGE_TRUNCATE is not set, the points near 
;		the edge are replaced with NaNs.
;	FILTER:  An array containing the filter window to use.  This overrides 
;		the window requested in the Window input.  This also returns 
;		the filter after use.
;	NAN:  Set this keyword to ignore NaN values in the input array, 
;		provided there is at least one defined value nearby.  The 
;		default is to return NaNs wherever they occur.
;	NO_NAN:  Obsolete version of NAN keyword retained for compatibility 
;		but no longer used.
;	WRAP_EDGES:  If set, the array is treated as being cyclic and the 
;		edges are joined together when smoothing.
;
; OUTPUTS:
;	Result:  Returns the smoothed version of the input array.
;
; USES:
;	CONSTANTS.pro
;	DIMENSION.pro
;	FILTER_WINDOW.pro
;
; PROCEDURE:
;	This function manually convolves the input array with the filter.
;
; EXAMPLE:
;       Create a vector of daily data and a sinusoid for a year.
;	  x = randomn( seed, 365 ) + sin( 6.28 * findgen( 365 ) / 365. )
;	Smooth x with a boxcar filter of 7 days, wrapping the edges together.
;	  result = filter( x, 7, 'boxcar', /wrap_edges )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2003-11-19.
;	Modified:	DAS, 2005-08-05 (replaced SUM.PRO use with TOTAL; 
;			added NAN keyword for consistency with other routines; 
;			removed functionality of NO_NAN keyword)
;-

;***********************************************************************

FUNCTION FILTER_ND, $
	X, $
	Width, $
	Window, $
	FILTER=filt, $
	WRAP_EDGES=wrapedgesopt, EDGE_TRUNCATE=edgetruncateopt, $
	NAN=nanopt, NO_NAN=nonanopt

;***********************************************************************
; Constants, Variables, and Options

; Load absolute constants
constants, nan=nan

; Determine dimension of input array
dim = dimension( x )

; Load smoothing window
if not( keyword_set( filt ) ) then begin
  filt = filter_window( width, window, dimension=dim )
endif else begin
  width = ( size( filt ) )[1]
endelse
; Total size of the filter
nfilt = n_elements( filt )
; Note the size of each dimension in the filter (there need to be 8 dimensions 
; since this is the maximum number of dimensions in an array in IDL)
nfiltdim = ( size( filt ) )[1:dim]
n = n_elements( nfiltdim )
if n lt 8 then nfiltdim = [ nfiltdim, 1 + intarr( 8 - n ) ]

; Edge handling options
wrapedgesopt = keyword_set( wrapedgesopt )
edgetruncateopt = keyword_set( edgetruncateopt )
; The default edge handling option
if wrapedgesopt or edgetruncateopt then begin
  noedgeopt = 0
endif else begin
  noedgeopt = 1
endelse

; NaN handling option
if keyword_set( nanopt ) then begin
  nanopt = 1
endif else begin
  nanopt = 0
endelse

; Half-length of filter
hwidth = ( width - 1 ) / 2

; Lengths of input array dimensions and of the total array
nxdim = ( size( x ) )[1:dim]
nx = n_elements( x )
; Need to increase nxdim to size 8 (since this is the maximum number 
; of dimensions in an input array in IDL)
n = n_elements( nxdim )
if n lt 8 then nxdim = [ nxdim, 1 + intarr( 8 - n ) ]

;***********************************************************************
; Initialise Working and Output Arrays

; Initialise the working array
; This means creating an array with padded borders of size hwidth for 
; dealing with smoothing at the edges.
; Increase sizes to make this border
nxworkdim = nxdim
iddimborder = where( nxworkdim gt 1 )
nxworkdim[iddimborder] = nxworkdim[iddimborder] + 2 * hwidth
; Create the vectored array
xwork = nan * fltarr( nxworkdim )
; Copy values into the working array.
; Initialise locations in the work array
j7 = 0
j6 = 0
j5 = 0
j4 = 0
j3 = 0
j2 = 0
j1 = 0
j0 = 0
; Initialise outside of NaN border check
check = 0
; Iterate through entries in a dimension in the work array
for i7 = 0, nxworkdim[7] - 1 do begin
  ; Determine the position of the values to take from the input array.
  ; If this dimension is of size 1
  if nxworkdim[7] gt 1 then begin
    ; If we are in the left border
    if i7 lt hwidth then begin
      ; Check if borders are supposed to be left as NaNs
      if noedgeopt then begin
        check = 1
      endif else begin
        check = 0
        ; If the WRAP_EDGES option is set
        if wrapedgesopt then j7 = nxdim[7] - i7 - 1
        ; If the EDGE_TRUNCATE option is set
        if edgetruncateopt then j7 = 0
      endelse
    ; If we are in the right border region
    endif else if i7 ge hwidth + nxdim[7] then begin
      ; Check if borders are supposed to be left as NaNs
      if noedgeopt then begin
        check = 1
      endif else begin
        check = 0
        ; If the WRAP_EDGES option is set
        if wrapedgesopt then j7 = i7 - hwidth - nxdim[7]
        ; If the EDGE_TRUNCATE option is set
        if edgetruncateopt then j7 = nxdim[7] - 1
      endelse
    ; If we are not in a border region
    endif else begin
      check = 0
      j7 = i7 - hwidth
    endelse
  endif
  ; Iterate through the next dimension in the work array if okay
  if check eq 0 then for i6 = 0, nxworkdim[6] - 1 do begin
    ; See the documentation for the i7 index, it is identical here
    if nxworkdim[6] gt 1 then begin
      if i6 lt hwidth then begin
        if noedgeopt then begin
          check = 1
        endif else begin
          check = 0
          if wrapedgesopt then j6 = nxdim[6] - i6 - 1
          if edgetruncateopt then j6 = 0
        endelse
      endif else if i6 ge hwidth + nxdim[6] then begin
        if noedgeopt then begin
          check = 1
        endif else begin
          check = 0
          if wrapedgesopt then j6 = i6 - hwidth - nxdim[6]
          if edgetruncateopt then j6 = nxdim[6] - 1
        endelse
      endif else begin
        check = 0
        j6 = i6 - hwidth
      endelse
    endif
    ; Iterate through the next dimension in the work array if okay
    if check eq 0 then for i5 = 0, nxworkdim[5] - 1 do begin
      ; See the documentation for the i7 index, it is identical here
      if nxworkdim[5] gt 1 then begin
        if i5 lt hwidth then begin
          if noedgeopt then begin
            check = 1
          endif else begin
            check = 0
            if wrapedgesopt then j5 = nxdim[5] - i5 - 1
            if edgetruncateopt then j5 = 0
          endelse
        endif else if i5 ge hwidth + nxdim[5] then begin
          if noedgeopt then begin
            check = 1
          endif else begin
            check = 0
            if wrapedgesopt then j5 = i5 - hwidth - nxdim[5]
            if edgetruncateopt then j5 = nxdim[5] - 1
          endelse
        endif else begin
          check = 0
          j5 = i5 - hwidth
        endelse
      endif
      ; Iterate through the next dimension in the work array if okay
      if check eq 0 then for i4 = 0, nxworkdim[4] - 1 do begin
        ; See the documentation for the i7 index, it is identical here
        if nxworkdim[4] gt 1 then begin
          if i4 lt hwidth then begin
            if noedgeopt then begin
              check = 1
            endif else begin
              check = 0
              if wrapedgesopt then j4 = nxdim[4] - i4 - 1
              if edgetruncateopt then j4 = 0
            endelse
          endif else if i4 ge hwidth + nxdim[4] then begin
            if noedgeopt then begin
              check = 1
            endif else begin
              check = 0
              if wrapedgesopt then j4 = i4 - hwidth - nxdim[4]
              if edgetruncateopt then j4 = nxdim[4] - 1
            endelse
          endif else begin
            check = 0
            j4 = i4 - hwidth
          endelse
        endif
        ; Iterate through the next dimension in the work array if okay
        if check eq 0 then for i3 = 0, nxworkdim[3] - 1 do begin
          ; See the documentation for the i7 index, it is identical here
          if nxworkdim[3] gt 1 then begin
            if i3 lt hwidth then begin
              if noedgeopt then begin
                check = 1
              endif else begin
                check = 0
                if wrapedgesopt then j3 = nxdim[3] - i3 - 1
                if edgetruncateopt then j3 = 0
              endelse
            endif else if i3 ge hwidth + nxdim[3] then begin
              if noedgeopt then begin
                check = 1
              endif else begin
                check = 0
                if wrapedgesopt then j3 = i3 - hwidth - nxdim[3]
                if edgetruncateopt then j3 = nxdim[3] - 1
              endelse
            endif else begin
              check = 0
              j3 = i3 - hwidth
            endelse
          endif
          ; Iterate through the next dimension in the work array if okay
          if check eq 0 then for i2 = 0, nxworkdim[2] - 1 do begin
            ; See the documentation for the i7 index, it is identical here
            if nxworkdim[2] gt 1 then begin
              if i2 lt hwidth then begin
                if noedgeopt then begin
                  check = 1
                endif else begin
                  check = 0
                  if wrapedgesopt then j2 = nxdim[2] - i2 - 1
                  if edgetruncateopt then j2 = 0
                endelse
              endif else if i2 ge hwidth + nxdim[2] then begin
                if noedgeopt then begin
                  check = 1
                endif else begin
                  check = 0
                  if wrapedgesopt then j2 = i2 - hwidth - nxdim[2]
                  if edgetruncateopt then j2 = nxdim[2] - 1
                endelse
              endif else begin
                check = 0
                j2 = i2 - hwidth
              endelse
            endif
            ; Iterate through the next dimension in the work array if okay
            if check eq 0 then for i1 = 0, nxworkdim[1] - 1 do begin
              ; See the documentation for the i7 index, it is identical here
              if nxworkdim[1] gt 1 then begin
                if i1 lt hwidth then begin
                  if noedgeopt then begin
                    check = 1
                  endif else begin
                    check = 0
                    if wrapedgesopt then j1 = nxdim[1] - i1 - 1
                    if edgetruncateopt then j1 = 0
                  endelse
                endif else if i1 ge hwidth + nxdim[1] then begin
                  if noedgeopt then begin
                    check = 1
                  endif else begin
                    check = 0
                    if wrapedgesopt then j1 = i1 - hwidth - nxdim[1]
                    if edgetruncateopt then j1 = nxdim[1] - 1
                  endelse
                endif else begin
                  check = 0
                  j1 = i1 - hwidth
                endelse
              endif
              ; Iterate through the next dimension in the work array if okay
              if check eq 0 then for i0 = 0, nxworkdim[0] - 1 do begin
                ; See the documentation for the i7 index, it is identical here
                if nxworkdim[0] gt 1 then begin
                  if i0 lt hwidth then begin
                    if noedgeopt then begin
                      check = 1
                    endif else begin
                      check = 0
                      if wrapedgesopt then j0 = nxdim[0] - i0 - 1
                      if edgetruncateopt then j0 = 0
                    endelse
                  endif else if i0 ge hwidth + nxdim[0] then begin
                    if noedgeopt then begin
                      check = 1
                    endif else begin
                      check = 0
                      if wrapedgesopt then j0 = i0 - hwidth - nxdim[0]
                      if edgetruncateopt then j0 = nxdim[0] - 1
                    endelse
                  endif else begin
                    check = 0
                    j0 = i0 - hwidth
                  endelse
                endif

                ; Copy values from the input array to the work array
                if check eq 0 then begin
                  xwork[i0,i1,i2,i3,i4,i5,i6,i7] = x[j0,j1,j2,j3,j4,j5,j6,j7]
                endif

              endfor
            endfor
          endfor
        endfor
      endfor
    endfor
  endfor
endfor

; Initialise the output array, of same size as the input array
xout = fltarr( nxdim )

;***********************************************************************
; Smooth Array

; Working vector of values nfiltdim-1
ntempdim = nfiltdim - 1
; Iterate through a dimension in the output array
for i7 = 0, nxdim[7] - 1 do begin
  ; Iterate through the next dimension
  for i6 = 0, nxdim[6] - 1 do begin
    ; Iterate through the next dimension
    for i5 = 0, nxdim[5] - 1 do begin
      ; Iterate through the next dimension
      for i4 = 0, nxdim[4] - 1 do begin
        ; Iterate through the next dimension
        for i3 = 0, nxdim[3] - 1 do begin
          ; Iterate through the next dimension
          for i2 = 0, nxdim[2] - 1 do begin
            ; Iterate through the next dimension
            for i1 = 0, nxdim[1] - 1 do begin
              ; Iterate through the next dimension
              for i0 = 0, nxdim[0] - 1 do begin

                ; Make sure it is normalised for the NO_NAN option
                if nanopt eq 0 then begin
                  if noedgeopt then begin
                    id = where( finite( xwork[ i0:i0+ntempdim[0], $
                       i1:i1+ntempdim[1], i2:i2+ntempdim[2], $
                       i3:i3+ntempdim[3], i4:i4+ntempdim[4], $
                       i5:i5+ntempdim[5], i6:i6+ntempdim[6], $
                       i7:i7+ntempdim[7] ] ) eq 1, nid )
                    factor = 1. * nfilt / nid
                  endif
                endif else begin
                  factor = 1.
                endelse
                ; Smooth the data
                xout[i0,i1,i2,i3,i4,i5,i6,i7] = factor * total( filt $
                    * xwork[ i0:i0+ntempdim[0], i1:i1+ntempdim[1], $
                    i2:i2+ntempdim[2], i3:i3+ntempdim[3], $
                    i4:i4+ntempdim[4], i5:i5+ntempdim[5], $
                    i6:i6+ntempdim[6], i7:i7+ntempdim[7] ], nan=nanopt )

              endfor
            endfor
          endfor
        endfor
      endfor
    endfor
  endfor
endfor

;***********************************************************************
; Finishing Touches

; Reform the output array
xout = reform( xout )

;***********************************************************************
; The End

return, xout
END
