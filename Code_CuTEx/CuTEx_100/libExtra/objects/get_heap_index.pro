;+
; Project     : HESSI
;
; Name        : GET_HEAP_INDEX
;
; Purpose     : Function to return the heap index of a pointer or object as a string.  If variable is not a
;	pointer or object, returns an empty string.
;
; Category    : utility objects
;
; Explanation : Parses output of help,var,output=output to find heap index.  Will not work if RSI changes
;	format of help output.
;
; Syntax      : IDL> index = get_heap_index(var)
;
; Examples    : if  get_heap_index(a) eq get_heap_index(b) then print,'a and b are the same object'
;
; Inputs      :		var - object or pointer to get heap index of
;
; Opt. Inputs : None
;
; Outputs     : string containing heap index, or empty string if not a heap variable.
;
; Opt. Outputs: None
;
; Keywords    : None
;
; Restrictions: Uses output of help,var so if RSI changes the format of the help output,
;		this won't work.
;
; Side effects: None
;
; History     : Written 17 Aug 2000, Kim Tolbert
;
; Contact     : kim.tolbert@gsfc.nasa.gov
;-
function get_heap_index, var

help, var, output=output

pos = strpos (output, 'HeapVar')
if pos[0] eq -1 then return, ''

output = strmid(output, pos[0] + 7)
pos = strpos (output, '(')
if pos[0] eq -1 then pos = strpos (output, '>')
if pos[0] eq -1 then return ,''
output = strmid(output, 0, pos[0])

return, output[0]

end

