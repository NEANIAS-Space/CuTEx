;+
; NAME: 
;        B_PLOT
;
; PURPOSE:
;        This procedure contours a data by drawing each element of an
;        array as a rectangular box.
;
; CATEGORY:
;        Graphics
;
; CALLING SEQUENCE:
;        B_PLOT,data,x,y
;
; INPUTS:
;        data:  A two dimensional array of values to be plotted
;        x:     A vector specifying the X coordinates for the plotted data
;        y:     A vector specifying the Y coordinates for the plotted data
;
; KEYWORD PARAMETERS:
;        BLANK_VALUE:  
;           A value which if encountered in the mask
;           prevents data being drawn in the corresponding
;           box. The default value is 1.
;        C_COLOURS:  
;           A vector of colour indexes to use when drawing
;           the boxes.  
;        CEIL:  
;           Indicates the use of the CEIL function when
;           interpolating the mask.  Requires the INTERP_FACTOR
;           keyword.
;        FLOOR:  
;           Indicates the use of the FLOOR function when
;           interpolating the mask.  Requires the INTERP_FACTOR
;           keyword.
;        INTERP_FACTOR:  
;           The number of gridpoints+1 which should be
;           added between the points specified by x and
;           y. Only integers are allowed.  Both x and y
;           are interpolated identically.
;        C_LEVELS:  
;           A vector containing the levels to use when binning
;           the data. If there are more levels than colours
;           specified the colours are used in cyclic fashion
;           as necessary.
;        LRG_VAL:    
;           A large value that sets the lower limit on masked data
;        MASK:  
;           A 2-dimensional integer array containing a mask to be
;           used when drawing the data.  When BLANK_VALUE is
;           encountered in mask the corresponding grid box is not
;           coloured. 
;        N_COLOURS:  
;           The number of colours to use when drawing the
;           data.  This is superceded by specifying the
;           levels.  The default is to use 100 colours from
;           colour index 3 to 103.
;        NOERASE:  
;           Don't draw axes or create a new data scaling. Just
;           draw the boxes.
;        OUTLINE:  
;           This a diagnostic option.  Each coloured box will
;           be outlined with a border.  Boxes are not all the
;           same size as a run length encoding scheme is used
;           to draw them.
;        ROUND:  
;           Indicates the use of the ROUND function when
;           interpolating the mask.  Requires the INTERP_FACTOR
;           keyword.  This is the default.
;        SML_VAL:
;           A small (or negative) value that sets the upper limit on
;           masked data.
;
;        _EXTRA:  This procedure takes extra keywords.  Examples are 
;                 values such as TITLE, [XY]TITLE, [XY]RANGE, etc.
;
; USES:
;        box.pro
;
; RESTRICTIONS:
;        None that I know of.       
;
; COMMENTS:
;        This routine makes use of a run-length encoding scheme to
;        reduce the size of postscript output. 
;
; EXAMPLE:
;        B_PLOT,RandomU(s,100,100),FIndGen(100),FIndGen(100)
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-07-14.
;       Modified:       Edward C. Wiebe, 2002-03-07 (added the
;                       c_levels keyword)
;       Modified:       Edward C. Wiebe, 2002-11-01 (added a new RLE
;                       scheme)
;       Modified:       Daithi Stone (stoned@csag.uct.ac.za), 2009-12-08 (added 
;                       box.pro to dependency list)
;
;-
;
pro B_Plot,indata,xt,yt                                        $
           , MISSING_VALUE = missing_value                     $
           , BLANK_VALUE   = blank_value                       $
           , C_COLOURS     = c_colours                         $
           , CEIL          = ceil                              $
           , FLOOR         = floor                             $
           , INTERP_FACTOR = interp_factor                     $
           , C_LEVELS      = c_levels                          $ 
           , LRG_VAL       = lrg_val                           $
           , MASK          = mask                              $
           , N_COLOURS     = n_colours                         $
           , NOERASE       = noerase                           $
           , NORLE         = norle                             $
           , OUTLINE       = outline                           $
           , ROUND         = round                             $
           , SML_VAL       = sml_val                           $           
           , SMOOTH_INLAND = smooth_inland                     $
           , POSITION      = position                          $
           , ORIENTATION   = orientation                       $
           , _EXTRA        = _extra                            

  s   = Size(indata)
  nx = s[1]
  ny = s[2]
  
  if (not Keyword_Set(mask)) then mask = IntArr(nx,ny)
  if (not Keyword_Set(blank_value)) then blank_value = 1
  if (not Keyword_Set(n_colours)) then n_colours = 100 
  if (Keyword_Set(c_colours)) then n_colours = Length(c_colours)

  if (not Keyword_Set(round)) then round = 0 else round = 1
  if (not Keyword_Set(floor)) then floor = 0 else floor = 1
  if (not Keyword_Set(ceil))  then ceil = 0  else ceil = 1

  if (not Keyword_Set(sml_val)) then sml = -1E10 else sml = sml_val
  if (not Keyword_Set(lrg_val)) then lrg = 1E10 else lrg = lrg_val
  
  if (not Keyword_Set(orientation)) then orientation = 0
  if (Keyword_Set(c_levels)) then clev = 1 else clev = 0

; Round is the default of [round,ceil,floor] if none or more than one are set.
  total = round+floor+ceil
  if (total gt 1) or (total eq 0) then begin
    round = 1
    floor = 0
    ceil  = 0
  endif

  if (not Keyword_Set(c_colours)) then begin
;   we use a colour table with 100 entries from 3 to 103
    maxnc = 100
    c_off = 3
    if (n_colours gt 1) then begin  
      c_colours = c_off+IndGen(n_colours)*maxnc/(n_colours-1)
    endif else begin
      c_colours = c_off+maxnc/2
    endelse
  endif 

  msk  = mask
  data = indata
  
  indx = Where((data gt sml) and (data lt lrg),cnt)
  if (cnt gt 0) then begin
    mn = Min(data[indx])
    mx = Max(data[indx]) 
  endif
  rng = mx-mn

  len = (Size(msk))[1]

  xx = xt
  yy = yt
  if (not Keyword_Set(interp_factor)) then factor = 1 else begin
;   convert interp_factor to next lower integer (1<=factor<=10)
    factor = Fix(interp_factor)
    if (factor lt 1) then factor = 1
    if (factor gt 10) then factor = 10

;    if (keyword_Set(smooth_inland)) then begin
;      indx = Where(msk eq 1,cnt)
;      if (cnt gt 0) then begin        
;        data[indx] = mn
;        data = Smart_Interp(data,1,LIMIT=4,MASK=msk)
;        Print,'Smart_Interp',Max(data),Min(data)
;      endif
;    endif

    nx  = nx*factor-(factor-1)
    ny  = ny*factor-(factor-1)  
    xint = FIndGen(nx)/Float(factor)
    yint = FIndGen(ny)/Float(factor)
    data = Interpolate(data,xint,yint,/GRID)

    if (round) then msk  = Round(Interpolate(Float(mask),xint,yint,/GRID))
    if (ceil)  then msk  = Ceil(Interpolate(Float(mask),xint,yint,/GRID))
    if (floor) then msk  = Floor(Interpolate(Float(mask),xint,yint,/GRID))

    xx   = Interpolate(xx,xint)
    yy   = Interpolate(yy,yint)
  endelse

; make an array of dx. (width of each grid cell).
  x = FltArr(nx+1)
  for i=1,nx-1 do begin
    x[i] = (xx[i]-xx[i-1])/2.+xx[i-1]
  endfor
  x[0]   = 2*xx[0]-x[1]
  x[nx] = 2*xx[nx-1]-x[nx-1]

; make an array of dy. ("height" of each grid cell).
  y = FltArr(ny+1)
  for j=1,ny-1 do begin
    y[j] = (yy[j]-yy[j-1])/2.+yy[j-1]
  endfor
  y[0]   = 2*yy[0]-y[1]
  y[ny] = 2*yy[ny-1]-y[ny-1]
  
  if (Var_Type(_extra) eq 8) then begin
    _extra_copy = _extra 
    tn = Tag_Names(_extra)
    if (StrPos(StrJoin(tn),'XSTYLE') ge 0) then _extra.xstyle=5
    if (StrPos(StrJoin(tn),'YSTYLE') ge 0) then _extra.ystyle=5    
  endif

; set up the axes 
  if (not Keyword_Set(noerase)) then begin
    Print,'B_PLOT: SETTING UP AXES AND SCALING.' ;@@
    Contour, IntArr(nx,ny),xx,yy                            $
             , /NODATA                                          $ 
             , XSTYLE = xstyle                                  $
             , YSTYLE = ystyle                                  $
             , XRANGE = [Min(xx),Max(xx)]                       $
             , YRANGE = [Min(yy),Max(yy)]                       $
             , POSITION = position                              $
             , _EXTRA = _extra
  endif
  
; Plot the coloured boxes ... 
  init = -1  
  dx = 1
  dy = 1

; There are two methods for plotting the boxes (both use run-length
; encoding).  The old method failed in some specific cases.  The new
; one is more general.   
  use_old = 0
  if (use_old) then begin
;   This is the old B_Plot RLE plotting method.
    for j= 0,ny-1 do begin
      start = init
      stop  = init 
      draw  = 0
      ms    = 0
      msn   = 0
      m     = 0
      y1    = y[j]
      y2    = y[j+1]
      for i=0,nx-1 do begin
        if (start eq init) then begin
;         We are starting a new row/colour
          if (msk[i,j] ne blank_value) then begin
            start = i
            x1 = x[i]           ;left side of box
;             get the colour for this box (ms = starting box colour)
            if (not clev) then begin
              ms = Floor((n_colours-1)*(data[i,j]-mn)/rng)
            endif else begin
;             search through levels until we find the box that the data
;             fits in?
              indx = Where(data[i,j] le c_levels,cnt)
              if (cnt gt 0) then begin
                ms = indx[0]
              endif else begin
                ms = 0          ;n_colours-1
              endelse
              if (ms eq n_colours) then ms = ms - 1
              
            endelse
;           don't draw yet we need to check some conditions first.
            draw = 0
            if (Keyword_Set(norle)) then begin
              draw = 1
              stop = i+1
            endif
          endif else begin
            ms = -1
          endelse
        endif
        
        if (start ne init) and (not draw) then begin  
          if (i eq nx-1) then begin
;           is the next box beyond the edge? yes=draw,no=don't draw.
            draw = 1
            stop = i+1
          endif else begin  
            if (msk[i+1,j] ne blank_value) then begin
;             get the colour of the next box.
              if (not clev) then begin
                msn = Floor((n_colours-1)*(data[i+1,j]-mn)/rng)
              endif else begin
;               search through levels until we find the box that the data
;               fits in?
                indx = Where(data[i+1,j] le c_levels,cnt)
                if (cnt gt 0) then begin      
                  msn = indx[0]
                endif else begin
                  msn = 0       ;n_colours-1
                endelse
                if (ms eq n_colours) then ms = ms - 1
              endelse
              
            endif else begin
;             The next point is masked, draw the current point.
              msn = -1
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
          x2 = x[stop]
          if (ms ge 0) then begin
            PolyFill, [x1, x1, x2, x2], [y1, y2, y2, y1], /DATA, $
                      COLOR = c_colours[ms] ;$
;               , CLIP = [Min(xt),Min(yt),Max(xt),Max(yt)],NOCLIP=0;@@
            if (Keyword_Set(outline)) then Box,x1,y1,x2-x1,y2-y1,/DATA  
          endif else begin
          endelse
          draw = 0
          start = init
          
        endif                 
      endfor
    endfor 
  endif else begin
;   This is the new B_PLOT RLE encoding method.  

    if (not Keyword_Set(c_levels)) then begin
      c_levels = FIndGen(n_colours)/(n_colours-1)*rng+mn
;              ms = Floor((n_colours-1)*(data[i,j]-mn)/rng)
    endif

    if (not Keyword_Set(missing_value)) then missing_value = 1E36
;   convert the data to an array of colour indicies. (this is plot method 2)
    plt = IntArr(nx,ny)-1
;   to avoid the problem of having a few points that are exactly equal
;   to the minimum level do the test from the second level on.     
    for n=1,n_colours-1 do begin
      indx = Where((data le c_levels[n]) and (plt lt 0) and $
                   (data ne missing_value) and (mask ne blank_value) ,cnt)
      if (cnt gt 0) then plt[indx] = n
    endfor

;   now plt is an array of colour table indicies. plt is equal to -1
;   where there was no valid data or the data was out of range.
;   
;    cmap = BytArr(nx,ny)-1
;    for n=1,n_colours-1 do begin
;      map = BytArr(nx,ny)
;      indx = Where(plt eq n,cnt)
;      if (cnt gt 0) then begin
;        for m=0,cnt-1 do begin
;          j = indx[m]/nx
;          i = indx[m] mod nx
;;         find other points in the map that are connected to the point
;;         at (i,j) and set them into map.
;          map = Connect(plt,i,j,map)
;        endfor
;      endif
;      indx = Where(map eq 1,cnt) 
;      if (cnt gt 0) then cmap[indx] = n
;    endfor      
 
    cmap = plt

    for j= 0,ny-1 do begin
      start = init
      stop  = init 
      draw  = 0
      y1    = y[j]
      y2    = y[j+1]
      for i=0,nx-1 do begin
        if (start eq init) then begin
;       We are starting a new row/colour
          if (cmap[i,j] ge 0) then begin
            start = i
            x1 = x[i]           ;left side of box
;           don't draw yet we need to check some conditions first.
            draw = 0
            if (Keyword_Set(norle)) then begin
              draw = 1
              stop = i+1
            endif
          endif else begin
;           do nothing since the current cell has a missing value
          endelse
        endif
        
        if (start ne init) and (not draw) then begin  
          if (i eq nx-1) then begin
;           is the next box beyond the edge? yes=draw,no=don't draw.
            draw = 1
            stop = i+1
          endif else begin  
            if (cmap[i+1,j] ge 0) then begin

            endif else begin
;             The next point is masked, draw the current point.
              draw = 1
              stop = i+1
            endelse  
            if ((not draw) and (cmap[i+1,j] ne cmap[i,j])) then begin
;             is the next box the same colour? yes=don't draw,no=draw.
              draw = 1
              stop = i+1 
            endif else begin
;             The next point is the same colour.  Do Nothing.
            endelse  
          endelse     
        endif  

        if (draw) then begin
          x2 = x[stop]
;          Print,[x1, x1, x2, x2], [y1, y2, y2, y1];@@
          PolyFill, [x1, x1, x2, x2], [y1, y2, y2, y1], /DATA, $
                    COLOR = c_colours[cmap[start,j]] ;$
            if (Keyword_Set(outline)) then Box,x1,y1,x2-x1,y2-y1,/DATA  
          draw = 0
          start = init
          
        endif  

      endfor
    endfor 

  endelse

; set up the axes 
  if (not Keyword_Set(noerase)) then begin
    Contour, IntArr(nx,ny),xx,yy                            $
             , /NODATA                                        $ 
             , XSTYLE = xstyle                                $
             , YSTYLE = ystyle                                $
             , XRANGE = [Min(xx),Max(xx)]                     $
             , YRANGE = [Min(yy),Max(yy)]                     $
             , /NOERASE                                       $
             , POSITION = position                            $
             , _EXTRA = _extra_copy
  endif

  Return
end
