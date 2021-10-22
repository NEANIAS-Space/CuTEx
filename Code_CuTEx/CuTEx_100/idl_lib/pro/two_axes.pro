;+
; NAME:
;     TWO_AXES
;
; PURPOSE:
;     Demonstrates how to make a plot with two different y-axes.
;
; CATEGORY:
;     GRAPHICS
;
; CALLING SEQUENCE:
;     Two_Axes
;
; INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     None
;
; OUTPUTS:
;     Makes a plot on current graphics device
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2001-10-10
;       Modified:       Edward C. Wiebe, 2002-01-10 (replaced b with
;                       bb.  Though there wasn't an error B and b are
;                       the same variable in IDL and this was confusing)
;-
pro Two_Axes

; make up two data sets
  d1 = [1, 2, 4,5,7,8,12,9,6,5,4,3]+1
  d2 = [12,10,9,8,5,4,2, 3,5,6,7,10]*250-1250

; make up a vector of x-coordinates.
  x    = IndGen(12)+1

; primary y-axis range
  A =  0.
  B = 15.

; secondary y-axis range
  G = -1000.
  H =  1750.

; establish data scaling (this is what IDL uses to plot points
; on the screen -- for further OPlots as well).  It is done by 
; calling plot with a dataset and optionally a yrange. Note that 
; the YSTYLE keyword is set to inhibit drawing an axis on the 
; right hand side.
  Plot,x,d1                           $
      ,PSYM=10                        $
      ,YRANGE=[A,B]                   $
      ,XRANGE=[1,12]                  $
      ,XSTYLE=1                       $
      ,YSTYLE=9                       $        
      ,TITLE='EXAMPLE TWO AXIS PLOT'  $
      ,YTITLE='AXIS 1 (scaling axis)' $
      ,XTITLE='X-Coordinates'         $
      ,POSITION=[0.1,0.1,0.9,0.95]

; draw a second axis on the right
  Axis,YAXIS=1,YRANGE=[G,H],YSTYLE=1,YTITLE='AXIS 2 (secondary axis)'
  
; calculate a new scaling to map dataset 2 onto axis 1.
  m = (B-A)/(H-G)       ; note that these were defined as floating 
  bb = (H*A-G*B)/(H-G)   ; point numbers -- this is important.

; plot the second dataset with the new scaling applied
  OPlot,x,d2*m+bb
  OPlot,x,d2*m+bb,PSYM=4,SYMSIZE=2
 
; print out the numbers to see what happened
  Print,' x       d1       d2       scaled d2'
  for n=0,11 do Print,x[n],d1[n],d2[n],d2[n]*m+bb,FORMAT='(I3,3F10.2)'
  Print,'m=',m,',     b=',b

  Return
end

;
; This is based on some basic algebra.
; Assume you have two functions of x, y1(x) and y2(x)
; Since the plots have linear axes the two functions will be related
; by:
;
; 1.   y1 = y2*m+b
;
; Now assume that the first axis has a range [A,B] and the second
; [G,H]. Then the following relationships are true.
;
; 2.   A = Gm + b
; 3.   B = Hm + b
; 
; subtracting 2. from 3. gives the slope m
;
; m = (B - A)/(H - G)
;
; substituting this into 2. or 3. gives the y intercept (b)
;
; b = (HA - GB)/(H - G)
;
; To plot your second dataset (y2) simply apply expression 1.
; above with these newly derived quantities.
;
; 4.   y = y2*m + b
;
; This gives a new dataset y scaled to the range [A,B]
; This could be repeated for other axis types (such as log) by
; subsituting for expression 1. above.
