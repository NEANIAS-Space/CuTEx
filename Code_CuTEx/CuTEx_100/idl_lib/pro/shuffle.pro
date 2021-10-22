;+
; NAME:
;	SHUFFLE
;
; PURPOSE:
;	This function shuffles the values in an array.
;
; CATEGORY:
;	Array
;
; CALLING SEQUENCE:
;	Result = SHUFFLE( X )
;
; INPUTS:
;	X:  An array of any type.
;
; KEYWORD PARAMETERS:
;	INDEX:  Returns the index values of the shuffled array.
;	REPLACE:  If set, the function shuffles with replacement.  The default
;		is without replacement.
;	SEED:  A seed for the random number generator to use.
;
; OUTPUTS:
;	Result:  Returns the shuffled version of array X.
;	INDEX:  See above.
;	SEED:  See above.  Also returns a seed for the next implementation of 
;		the random number generator.
;
; PROCEDURE:
;	This function used the RANDOMU IDL function to produce an array of 
;	random index values.
;
; EXAMPLE:
;	Define a vector.
;	  x = [0,1,2,3,4]
;	Shuffle the vector with replacement.
;	  result = shuffle( x, /replace )
;	The output COULD be result = [3,2,4,0,0].
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2001-07-18.
;	Modified:	DAS, 2002-11-21 (added seed initialisation).
;	Modified:	DAS, 2003-02-17 (allowed input of very long vectors)
;	Modified:	DAS, 2005-01-03 (added SEED keyword)
;-

;***********************************************************************

FUNCTION SHUFFLE, $
	X, $
	INDEX=index, $
	REPLACE=replaceopt, $
	SEED=seed

;***********************************************************************
; Constants and Variables

; Number of elements
n = n_elements( x )

; Index of shuffled values
index = intarr( n )

; Working vector of index values
idleft = indgen( n + 2 ) - 1
nleft = n_elements( idleft )

; Replacement option
replaceopt = keyword_set( replaceopt )

; Initialise the random number generator
if not( keyword_set( seed ) ) then seed = long( systime( /seconds ) )

;***********************************************************************
; Shuffle Values

for i = 0l, n - 1l do begin
  ; Determine how many values left to shuffle
  if not( replaceopt ) then nleft = n_elements( idleft )
  ; Get a random index value
  ind = floor( randomu( seed, /uniform ) * ( nleft - 2 ) ) + 1
  index[i] = idleft[ind]
  ; If not replacing values, remove the value
  if not( replaceopt ) then idleft = [ idleft[0:ind-1], idleft[ind+1:nleft-1] ]
endfor

; Create shuffled array
y = x[index]

; Reform output
xdim = ( size( x ) )[1:n_elements(size(x))-3]
y = reform( y , xdim )
index = reform( index , xdim )

;***********************************************************************
; The End

return, y
END
