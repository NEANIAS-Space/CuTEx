;+
; NAME:
;	NONUNIQ
;
; PURPOSE:
;	Return the subscripts of the non-unique elements in an array.
;
;	Note that repeated elements must be adjacent in order to be
;	found.  This routine is intended to be used with the SORT
;	function.  See the discussion of the IDX argument below.
;
;	This function is a simple modification of IDL's uniq function.
;
; CATEGORY:
;	Array 
;
; CALLING SEQUENCE:
;	UNIQ(Array [, Idx])
;
; INPUTS:
;	Array:	The array to be scanned.  The type and number of dimensions
;		of the array are not important.  The array must be sorted
;		into monotonic order unless the optional parameter Idx is 
;		supplied.
;
; OPTIONAL INPUT PARAMETERS:
;	IDX:	This optional parameter is an array of indices into Array
;		that order the elements into monotonic order.
;		That is, the expression:
;
;			Array(Idx)
;
;		yields an array in which the elements of Array are
;		rearranged into monotonic order.  If the array is not
;		already in monotonic order, use the command:
;
;			UNIQ(Array, SORT(Array))
;
;		The expression below finds the non-unique elements of an unsorted
;		array:
;
;			Array(UNIQ(Array, SORT(Array)))
;
; OUTPUTS:
;	An array of indicies into ARRAY is returned.  The expression:
;
;		ARRAY(UNIQ(ARRAY))
;
;	will be a copy of the sorted Array with non-duplicate adjacent
;	elements removed.
;
; COMMON BLOCKS:
;	None.
;
; MODIFICATION HISTORY:
;       Written by Edward C. Wiebe, 2002-07-23 (Modified IDL's
;       uniq.pro into nonuniq.pro - so this was really written by RSI)
;
;-
;

function NONUNIQ, ARRAY, IDX

; Check the arguments.
  s = size(ARRAY)
  if (s[0] eq 0) then return, 0		;A scalar
  if n_params() ge 2 then begin		;IDX supplied?
     q = array[idx]
     indices = where(q eq shift(q,-1), count)
     if (count GT 0) then return, idx[indices] $
     else return, n_elements(q)-1
  endif else begin
     indices = where(array eq shift(array, -1), count)
     if (count GT 0) then return, indices $
     else return, n_elements(ARRAY)-1
  endelse
end
