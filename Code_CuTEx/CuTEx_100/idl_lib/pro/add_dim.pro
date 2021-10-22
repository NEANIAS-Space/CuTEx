;+
; NAME:
;	ADD_DIM
;
; PURPOSE:
;	This function returns an array with extra dimensions added, filled 
;	with copies of the original array.
;
; CATEGORY:
;	Array
;
; CALLING SEQUENCE:
;	Result = add_dim( Array, Id_dim_new, Len_dim_new )
;
; INPUTS:
;	Array:  An array of any type and number of dimensions.
;	Id_dim_new:  The index number indicating where to add the new 
;		dimensions.  For instance, if Array=fltarr(4,5) and 
;		Len_dim_new=3, then if Id_dim_new=0 then Result=fltarr(3,4,5), 
;		if Id_dim_new=1 then Result=fltarr(4,3,5), and if Id_dim_new=2 
;		then Result=fltarr(4,5,3).
;	Len_dim_new:  The size of the new dimensions to add.  This can be a 
;		scalar or vector.
;
; OUTPUTS:
;	Result:  The expanded array.
;
; PROCEDURE:
;	This function creates a new array and iteratively copies the old 
;	array into the new array.
;
; EXAMPLE:
;	Define a 4*5-element array.
;	  x = findgen( 4, 5 )
;	Add another dimension, of size 3, in the middle of the array.
;	  result = add_dim( x, 1, 3 )
;	The function should return a 4*3*5 element array with entries 
;	result[*,i,*] equal to reform( x, 4, 1, 5 ) for all i={0,1,2}.
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2007-08-01.
;	Modified:	DAS, 2007-12-18 (changed to allow larger arrays 
;			requiring long integer indices)
;	Modified:	DAS, 2008-03-18 (completed previous modification)
;	Modified:	DAS, 2009-03-31 (fixed bug in determining size and type 
;			of Array)
;-

;***********************************************************************

FUNCTION ADD_DIM, $
	Array, Id_dim_new, Len_dim_new

;***********************************************************************
; Constants

; Determine the size and type of Array
dim = size( array, dimension=1 )
n_dim = n_elements( dim )
if dim[0] eq 0 then begin
  n_dim = 1
  dim = 1
endif
type = size( array, type=1 )

; Determine the number of new dimensions to add
n_dim_new = n_elements( len_dim_new )

; Determine the dimension sizes of a three dimensional version of Array
dim_3 = lonarr( 3 )
if id_dim_new eq 0 then begin
  dim_3[0] = 1
endif else begin
  dim_3[0] = round( product( dim[0:id_dim_new-1] ), l64=1 )
endelse
dim_3[1] = 1
if id_dim_new eq n_dim then begin
  dim_3[2] = 1
endif else begin
  dim_3[2] = round( product( dim[id_dim_new:n_dim-1] ), l64=1 )
endelse

; Determine the dimensions of the new array
dim_out = lonarr( n_dim + n_dim_new )
if id_dim_new ne 0 then dim_out[0:id_dim_new-1] = dim[0:id_dim_new-1]
dim_out[id_dim_new:id_dim_new+n_dim_new-1] = len_dim_new
if id_dim_new ne n_dim then begin
  dim_out[id_dim_new+n_dim_new:n_dim+n_dim_new-1] = dim[id_dim_new:n_dim-1]
endif

; Determine the dimension sizes of a three dimensional version of the new array
dim_out_3 = [ dim_3[0], round( product( len_dim_new ) ), dim_3[2] ]

;***********************************************************************
; Create the New Array

; Reform Array to three dimensional format
array = reform( array, dim_3 )

; Initialise the new array in three dimensional format
result = make_array( dim_out_3, type=type )

; Iterate through elements of the new dimensions
i_0 = 0 * dim_out_3[1]
for i = i_0, dim_out_3[1] - 1 do begin
  ; Copy Array into the new array
  result[*,i,*] = array
endfor

; Reform Array back to its original dimensions
array = reform( array, dim )

; Reform the new array to intended dimensions
result = reform( result, dim_out )

;***********************************************************************
; The End

return, result
END
