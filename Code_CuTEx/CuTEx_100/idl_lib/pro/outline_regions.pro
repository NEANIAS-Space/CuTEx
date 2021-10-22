;+
; NAME:
;   Outline_Regions
;
; PURPOSE:
;   Draw a line around regions on a contour plot.
;
; CATEGORY:
;   Graphics
;
; CALLING SEQUENCE:
;   Outline_Regions, map, x, y                         $
;                   , COLOUR    = colour               $
;                   , THICK     = thick                $
;                   , LINESTYLE = linestyle    
;
; INPUTS:
;   map:  an array defining the regions
;         lines are drawn between points with 1 and anything else that is
;         positive or zero.
;         map is a two-d array of 0 and 1 (also negative values now),
;         since lines are drawn between 0s and 1s it doesn't matter
;         which  of them represents land.  No lines are drawn between
;         ones and negative values.
;   x:  the column coordinate vector for the map used for mapping the 
;       lines onto a previously established  scaling
;   y:  the row coordinate vector for the map 
;
; KEYWORD PARAMETERS:
;    COLOUR:  a colour table index to use drawing the outlines.
;    THICK:  the IDL line thickness to use when drawing the outlines.
;    LINESTYLE:  the IDL line style to be used when drawing the outlines.
;
; SIDE EFFECTS:
;   The contents of the current plotting device are changed.
;
; MODIFICATION HISTORY:
; Written by:	Edward C. Wiebe, 1999-02-12.
; Modified by:  ECW,  17 July 2000 (some new rules were needed.  Added
;               a test for negative values in the map.)
;-
pro Outline_Regions, map, x, y                        $
                   , COLOUR    = colour               $
                   , THICK     = thick                $
                   , LINESTYLE = linestyle                  
 
  if (not Keyword_Set(colour)) then colour = !p.color   
  if (not Keyword_Set(linestyle)) then linestyle = 0 

  s    = Size(map)
  imt  = s[1]
  jmt  = s[2]
  dy   = 1
  dx   = 1 

  for j = 0, jmt-1 do begin
    ym = (y[Max([j-dy,0])]+y[j])/2.
    yp = (y[Min([j+dy,jmt-1])]+y[j])/2.
    for i = 0, imt-1 do begin
      if (map[i, j] eq 1) then begin
        xm = (x[Max([i-dx,0])]+x[i])/2.
        xp = (x[Min([i+dx,imt-1])]+x[i])/2.
        if (i gt 0) then begin
;        left side of the grid cell      
          t1 = (map[i-1, j] ne 1)
          t2 = (map[i-1, j] ge 0)
          if (t1 and t2) then                   $
            Plots, [xm,xm], [ym, yp] 	        $
                 , /DATA  	 	        $
                 , COLOR = colour		$
                 , THICK = thick		$
                 , LINESTYLE = linestyle
        endif
        if (i lt imt-1) then begin
;         right side of the grid cell
          t1 = (map[i+1, j] ne 1)
          t2 = (map[i+1, j] ge 0)
          if (t1 and t2) then                   $
            Plots, [xp,xp], [ym, yp] 	        $
                 , /DATA		 	$
                 , COLOR = colour		$
                 , THICK = thick		$
	          , LINESTYLE = linestyle
        endif
        if (j gt 0)  then begin  
;         bottom of the grid cell  
          t1 = (map[i, j-1] ne 1)
          t2 = (map[i, j-1] ge 0)
          if (t1 and t2) then                   $
            Plots,[xm, xp],[ym,ym] 	        $
                 , /DATA			$
                 , COLOR = colour		$
                 , THICK = thick		$
                 , LINESTYLE = linestyle
        endif
        if (j lt jmt-1) then begin
;         top of the grid cell
          t1 = (map[i, j+1] ne 1)
          t2 = (map[i, j+1] ge 0)
          if (t1 and t2) then                   $
            Plots,[xm, xp],[yp,yp] 	        $
                 , /DATA			$
                 , COLOR = colour		$
                 , THICK = thick		$
                 , LINESTYLE = linestyle
        endif
      endif
    endfor
  endfor

  Return
end
