;+
; NAME:
;   Slice
;
; PURPOSE:
;   Slice a two dimensional array out of a three dimensional array.
;
; CATEGORY:
;   Array
;
; CALLING SEQUENCE:
;   result = Slice(indata, ortn, axes)
;
; INPUTS:
;   indata: a three-dimensional array to be sliced
;   ortn: one of (0,1,2) indicating the (XY, XZ, YZ) orientations for
;         the result.
;   axes: a three element vector giving the coordinates along each
;         axis where the data should be sliced.
;
; OUTPUTS:
;   result: a two dimensional array, a slice of indata.
;
; EXAMPLE:
; 
;   d    = FltArr(200,100,19)
;   axes = [0,50,0]
;   ortn = 1
;  
;   result = Slice(d,ortn,axes)
;   
;   Help, result;  
;     RESULT          FLOAT     = Array[200, 19]
;  
;   result = Slice(d,0,axes)
;   Help, result
;     RESULT          FLOAT     = Array[200, 100]
;  
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-06-09.
;-

function Slice,indata,ortn,axes
 
   XY = 0
   XZ = 1
   YZ = 2

   slc = -1
   
   if (Var_Type(indata) eq 10) then begin
     data = *indata
   endif else begin
     data = indata
   endelse

   s   = Size(data)
   imt = s[1]
   jmt = s[2]
   km  = s[3]
   
   if (ortn eq XY) and (ortn eq XZ) and (ortn eq YZ) then Return,-1
   if (N_Elements(axes) ne 3) then Return,-1
    
   case ortn of
   XY:begin
     slc = FltArr(imt,jmt)
     if (axes[2] lt km) then begin
         slc[*,*] = data[*,*,axes[2]] 
      endif else begin
        slc[*,*] = data[*,*,0]
        Print,'Could not slice at depth level greater than 0'
;       add an "official" error message here? ; @@
      endelse
   end
   XZ:begin
      slc = FltArr(imt,km)
      slc[*,*] = data[*,axes[1],*]
   end
   YZ:begin
      slc = FltArr(jmt,km)
      slc[*,*] = data[axes[0],*,*]
   end
   else:Return,-1
   endcase 

   Return, slc
end
