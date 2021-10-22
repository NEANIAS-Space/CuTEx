;+
; NAME:
;        TRUECOL_ROTATE
;
; PURPOSE:
;        This a wrapper for the rotate function.  Rotate doesn't like
;        arrays from true colour images.
;
; CATEGORY:
;        Graphics
;
; CALLING SEQUENCE:
;        result = TrueCol_Rotate(img,dir)
;
; INPUTS:
;       img:   The array holding a true colour image.   true colour 
;              images have a format like (3,x,y) where the first index
;              refers to the three different colour channels.
;       dir:   The direction argument passed to the IDL Rotate
;              function.  See the IDL Rotate function for more information.
;
; OUTPUTS:
;       result:  The rotated array.
;
; RESTRICTIONS:
;       The input array (img) must be of the form array(3,x,y) where x
;       and y are the horizontal and vertical dimensions of the image.
;       It would be easy to modify this routine to handle the other
;       forms of true colour images (where the colour index is the
;       second or third dimension instead of the first).
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-12-18.
;-

function TrueCol_Rotate, img, dir

; img    the (3,x,y) BytArr to be rotated 
; dir    the direction. See the IDL Rotate function for information
;        on this
  s = Size(img)
  if (s[0] eq 3) and (s[1] eq 3) then begin

    map = [[2,3],[3,2],[2,3],[3,2],[2,3],[3,2],[2,3],[3,2]]

    nimg = BytArr(3,s[map[0,dir]],s[map[1,dir]])
    
    tmp0 = BytArr(s[2],s[3])  

    tmp0[*,*] = img[0,*,*]
    tmp1 = Rotate(tmp0,dir)
    nimg[0,*,*] = tmp1

    tmp0[*,*] = img[1,*,*]
    tmp1 = Rotate(tmp0,dir)
    nimg[1,*,*] = tmp1

    tmp0[*,*] = img[2,*,*]
    tmp1 = Rotate(tmp0,dir)
    nimg[2,*,*] = tmp1
            
  endif else nimg = -1


  Return, nimg
end


