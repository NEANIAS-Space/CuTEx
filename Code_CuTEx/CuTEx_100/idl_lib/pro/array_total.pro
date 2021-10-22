;+
; NAME:
;	ARRAY_TOTAL
;
; PURPOSE:
; 	This function does more flexible array integration than IDL's total.
;
; CATEGORY:
;	Array
;
; CALLING SEQUENCE:
;	Result = array_total( Data [, Instruct] )
;
; INPUTS:
;	Data:  A numerical array of data to be processed, of any dimension.
;	Instruct:  An array of type string containing processing instructions, 
;		performed sequentially in the given order.  Each instruction is 
;		of the form 'INSTRUCT_TYPE=INSTRUCT_DIM'.  Possible values for 
;		INSTRUCT_TYPE are:
;		'total': calculate the total along the INSTRUCT_DIMdimension(s)
;		'mean': calculate the mean along the INSTRUCT_DIMdimension(s).
;		INSTRUCT_DIM is a sequence of dimension indices separated by 
;		commas.  These are the same as for IDL's total function, i.e. 
;		the left-most dimension is '1'.  If multiple dimensions are 
;		listed in a single instruction, then those dimensions are 
;		treated as one and the operation is performed simultaneously on 
;		all of them.  So 'mean=1,2' will average over dimensions 1 and 
;		2 simulataneously, while ['mean=1','mean=2'] will first average 
;		over dimension 1 and average then over dimension 2.
;	FRAC_COVERAGE_THRESH, NAN, REFORM, WEIGHT
;
; KEYWORD PARAMETERS:
;	COVERAGE:  Returns an array of the same dimensions of Result containing 
;		the fractional coverage of values in data, relative to the 
;		total possible (i.e. the (weighted) fraction of values that are 
;		not NaN), involved in the calculation of the final instruction 
;		in Instruct which produces Result.
;	FRAC_COVERAGE_THRESH:  The minimum fraction of (weighted) data used in 
;		each operation that must be defined (i.e. not NaN) in order to 
;		proceed with the calculation.  If the criterion is not met 
;		then a NaN is returned from that calculation.  The default is 0.
;	NAN:  If set then NaNs (not-a-number values) are ignored in the 
;		calculations.  The default is not set.
;	REFORM:  If set then the reform function is performed on Result, 
;		COVERAGE, DATA_MEAN, and MEAN COVERAGE to remove all 1-element 
;		dimensions.  The default is for these output to maintain the 
;		original number of dimensions, with some dimensions reduced to 
;		1-element size according to Instruct and REFERENCE.
;	WEIGHT:  An array of the same size as DATA containing relative 
;		weightings for the corresponding elements in DATA.  All 
;		calculations producing Result, COVERAGE, DATA_MEAN, and 
;		MEAN_COVERAGE take account of these weightings.  If not set 
;		then identical weightings are assumed for all elements.
;
; OUTPUTS:
;	Result:  Returns the processed version of Data.  This is of the same 
;		size as Data except that the dimensions with instructions 
;		specified in Instruct have only one element.  If REFORM is set 
;		then these 1-element dimensions are removed.
;	COVERAGE
;
; USES:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2008-03-11, 
;			adapted from mask_lonlatmonth.pro
;-

FUNCTION ARRAY_TOTAL, $
	Data, $
	Instruct, $
	FRAC_COVERAGE_THRESH=frac_coverage_thresh, $
	NAN=nan_opt, $
	REFORM=reform_opt, $
	WEIGHT=weight, $
	COVERAGE=coverage

;***********************************************************************
; Constants and Options

; Missing data flag
nan = !values.f_nan

; Note the dimensions of data
len = size( data )
n_len = len[0]
len = len[1:n_len]

; The default minimum coverage (0)
if n_elements( frac_coverage_thresh ) eq 0 then frac_coverage_thresh = 0

; Ensure normalised weightings
if keyword_set( weight ) then weight = weight / total( weight )

; The number of instructions
n_instruct = n_elements( instruct )

; Copy input array to output
result = data

;***********************************************************************
; Integrate Along Dimensions of Data

; Weight data array if requested
if keyword_set( weight ) then result = weight * result

; Iterate through commands
for i = 0, n_instruct - 1 do begin

  ; Initialise coverage array and weight if requested
  coverage = finite( result )
  if keyword_set( weight ) then coverage = weight * coverage

  ; Parse instruction
  instruct_type = strsplit( instruct[i], '=', extract=1 )
  instruct_dim = fix( strsplit( instruct_type[1], ',', extract=1, $
      count=n_instruct_dim ) )
  instruct_type = instruct_type[0]

  ; Perform integration in this dimension
  for j = 0, n_instruct_dim - 1 do begin
    result = total( result, instruct_dim[j], nan=nan_opt )
    coverage = total( coverage, instruct_dim[j] )
    if keyword_set( weight ) then weight = total( weight, instruct_dim[j] )
    ; Reform data to maintain dimensions
    len[instruct_dim[j]-1] = 1
    result = reform( result, len )
    coverage = reform( coverage, len )
    if keyword_set( weight ) then weight = reform( weight, len )
  endfor
  ; Perform averaging
  if instruct_type eq 'mean' then begin
    result = result / coverage
    if keyword_set( weight ) then result = result * weight
  endif

endfor

; Normalise weighting on data and coverage
if keyword_set( weight ) then begin
  result = result / weight
  coverage = coverage / weight
endif

; Flag insufficient coverage as missing
id = where( coverage lt frac_coverage_thresh, n_id )
if n_id gt 0 then result[id] = nan
id = -1

;***********************************************************************
; Post-Processing

; Reform output to requested format
if keyword_set( reform_opt ) then begin
  result = reform( result )
  coverage = reform( coverage )
endif

;***********************************************************************
; The End

return, result
END
