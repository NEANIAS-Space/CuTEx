;+
; NAME:
;   Fill_Regions
;
; PURPOSE:
;   Fill regions with a colour on a plot of data from the UVic Climate
;   Model. 
;
; CATEGORY:
;   Graphics
;
; CALLING SEQUENCE:
;   Fill_Regions, map,x,y,ctab                           $
;                  , NO_EXTEND = no_extend               $
;                  , NO_RLE    = no_rle                  $
;                  , LINE_FILL = line_fill
; INPUTS:
;   map:  an integer two-dimensional array defining the regions with
;         different integers.
;
;     map[i,j] = 0 --> skip
;     Abs(map[i,j]) gt 0 --> colour a rectangle centred on x[i],y[j] using
;     the value in map as the index to an array of colour table values
;
;   x:  a vector of coordinates corresponding to the x (first) axis of
;       the array map.
;   y:  a vector of coordinates corresponding to the y (second) axis of
;       the array map.
;   ctab:  the colours to use when drawing the contents of map.  The
;          contents of ctab give the colour table entry to use for the
;          regions in map corresponding to the array indicies of ctab.
;
; KEYWORD PARAMETERS:
;   NO_EXTEND:  Setting this keyword prevents extending the regions
;               one half of a grid cell outward.
;   NO_RLE:  Do not use the run-length encoding scheme.  The RLE
;            scheme reduces the size of most postscript output that
;            includes a call to this procedure by reducing the number
;            of instructions in the resulting file.
;   LINE_FILL:  Use a line fill instead of a solid fill.  The
;               orientation of the lines depends on the regions
;               "value" in map. 
;    
; SIDE EFFECTS:
;   The contents of the current graphics device are changed.
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 1999-02-12.
;-

pro Fill_Regions, map,x,y,ctab                          $
                  , NO_EXTEND = no_extend               $
                  , NO_RLE    = no_rle                  $
                  , LINE_FILL = line_fill

;  Plots rectangular regions using an integer array.
;  
;  ewiebe Feb12 1999
;
;  map is a 2-d array of indices to the ctab array. it can contain
;      any value that is a valid index to ctab.  
;      map[i,j] = 0 (zero) has the special meaning that no box will 
;      be drawn for the i,j cell.
;  x,y are the horizontal and vertical coordinate arrays to be
;      used to map the filled regions into an established coordinate
;      system
;  ctab an array that maps the entries in map to a previously 
;      established colour table
;
;  NO_EXTEND: setting this keyword forces the region to be drawn at 
;             the intersection between points in the x and y arrays
;             The default action is to draw the region extended to
;	      one half of a grid spacing in all directions.

;   Message,/INFO,'BEGINS';@@

  s = Size(map)
  imt = s[1]
  jmt = s[2]
  mx = Max(map)
  mn = Min(map)

  if (Keyword_Set(no_rle)) then test = 0 else test = 1

  dx   = 1
  dy   = 1
  if (test eq 0) then begin
    if (Keyword_Set(no_extend)) then begin
      for j = 0, jmt-1 do begin
        ym = (y[Max([j-dy,0])]+y[j])/2.
        yp = (y[Min([j+dy,jmt-1])]+y[j])/2.
        for i = 0, imt-1 do begin
          if (map[i, j] ne 0) then begin
            xm = (x[Max([i-dx,0])]+x[i])/2.
            xp = (x[Min([i+dx,imt-1])]+x[i])/2.
            PolyFill, [xm, xm, xp, xp], [ym, yp, yp, ym], /DATA, $
                      COLOR = ctab[Abs(map[i,j])]
          endif
        endfor
      endfor
    endif else begin
      for j = 0, jmt-1 do begin
        ym = y[Max([j-dy,0])]
        yp = y[Min([j+dy,jmt-1])]
        for i = 0, imt-1 do begin
          if (map[i, j] ne 0) then begin
            xm = x[Max([i-dx,0])]
            xp = x[Min([i+dx,imt-1])]


            PolyFill, [xm, xm, xp, xp], [ym, yp, yp, ym], /DATA, $
                      COLOR = ctab[Abs(map[i,j])], LINE_FILL=line_fill,ORIENTATION=orient,SPACING=spacing

          endif
        endfor
      endfor
    endelse
  endif else begin
    init = -1
    blank_value = 0
    
    xx = FltArr(imt+1)
    yy = FltArr(jmt+1)
    for i=1,imt-1 do begin
      xx[i] = (x[i]-x[i-1])/2.+x[i-1]
    endfor
    
    for j=1,jmt-1 do begin
      yy[j] = (y[j]-y[j-1])/2.+y[j-1]
    endfor

    xx[0]   = 2*x[0]-xx[1]
    xx[imt] = 2*x[imt-1]-xx[imt-1]
    yy[0]   = 2*y[0]-yy[1]
    yy[jmt] = 2*y[jmt-1]-yy[jmt-1]
    
    for j= 0,jmt-1 do begin
      start = init
      stop  = init 
      draw  = 0
      ms    = 0
      msn   = 0
      m     = 0 
      if (Keyword_Set(no_extend)) then begin
        y1 = yy[j]
        y2 = yy[j+1]
      endif else begin
        if (j eq 0)     then y1=yy[0]   else y1 = y[j-1]
        if (j eq jmt-1) then y2=yy[j+1] else y2 = y[j+1]
      endelse
      
      for i=0,imt-1 do begin           
        if (start eq init) then begin
;         We are starting a new row/colour
          if (map[i,j] ne blank_value) then begin
            start = i  
            if (Keyword_Set(no_extend)) then begin
              x1 = xx[start]
            endif else begin   
              if (i eq 0) then x1 = xx[0] else x1 = x[start-1]
            endelse
;           get the colour for this box (ms = starting box colour)
            ms = Abs(map[i,j])
;           don't draw yet we need to check some conditions first.
            draw = 0
          endif 
        endif
        
        if (start ne init) then begin  
          if (i eq imt-1) then begin
;           is the next box beyond the edge? yes=draw,no=don't draw.
            draw = 1
            stop = i+1
          endif else begin  
            if (map[i+1,j] ne blank_value) then begin
;             get the colour of the next box.
              msn = Abs(map[i+1,j])
            endif else begin
;             The next point is masked, draw the current point.
              draw = 1
              stop = i+1            
            endelse  
            if ((not draw) and (msn ne ms)) then begin
;             is the next box the same colour? yes=don't draw,no=draw.
              draw = 1
              stop = i+1 
            endif else begin
;             The next point is the same colour.  Do Nothing.
            endelse  
          endelse     
        endif  
        if (draw) then begin
          if (Keyword_Set(no_extend)) then begin
            x2 = xx[stop]
          endif else begin
            if (i eq imt-1) then x2 = xx[imt] else x2 = x[stop]
          endelse

          if (Keyword_Set(line_fill)) then begin
            orient = 360.*(map[i,j]/Float(mx-mn))      
            spacing=0.1
;            Print,map[i,j],mx,mn,orient
          endif


          PolyFill, [x1, x1, x2, x2], [y1, y2, y2, y1], /DATA, $
                    COLOR = ctab[ms], LINE_FILL=line_fill,ORIENTATION=orient,SPACING=spacing
          if (Keyword_Set(outline)) then Box,x1,y1,x2-x1,y2-y1,/DATA  
          draw = 0
          start = init
        endif                 
      endfor
    endfor 

  endelse

;  Message,/INFO,'ENDS';@@
  
  Return
end
