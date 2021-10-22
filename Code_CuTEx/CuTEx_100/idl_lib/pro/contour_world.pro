;+
; NAME:
;    CONTOUR_WORLD
;
; PURPOSE:
;    This procedure draws a colour contour of a data field over a world map.
;
; CATEGORY:
;    Graphics
;
; CALLING SEQUENCE:
;    contour_world, Data, Lon, Lat
;
; INPUTS:
;    Data:  The data field array in [longitude,latitude] format, or a vector of 
;        [space] format.  Of type integer or floating point.
;    Lon:  A vector of the longitude coordinates of the data array, of type 
;        integer or floating point.  Of format [longitude] or [space], 
;        depending on the format of Data.
;    Lat:  A vector of the latitude coordinates of the data array, of type 
;        integer or floating point.  Of format [longitude] or [space], 
;        depending on the format of Data.
;
; KEYWORD PARAMETERS:
;    ARROWEND:  If set, then pointed end levels are plotted, indicating that 
;        the end levels extend beyond the range of the plot.  The default is 
;        rectangular end levels, like all the other levels.  A 2 element vector 
;        can also be input, where the first element gives the setting (0 or 1) 
;        for the left/bottom end and the second for the right/top end.
;    AXIS:  If set axis labels are printed.  This only works with the 
;        cylindrical projection at the moment.
;    BOLD:  If set then text is written in bold font, if set to zero then 
;        regular font is used.  The default is to be set.
;    BLOCKCELL:  If set then cells in a regular grid are filled with blocks.  
;        The default is to use IDL's interpolating contour procedure.  This is 
;        automatically set to 0 if irregularly gridded data has been input, 
;        i.e. if Data is a vector.
;    C_COLORS:  A vector of colour indices for the contoured levels.  The 
;        default may not look great with certain colour tables.
;    CENTRE:  The coordinates of the central point of the plot, of format 
;        [longitude,latitude].
;    CHARCOLOR:  The colour index of the map title and border, and the legend 
;        title and border.  The default is set in !P.COLOR.
;    CHARSIZE:  The size of the text characters.  The default is 1.
;    COASTS:  If set, coastlines, islands, and lakes are drawn.
;    COLOR:  The colour index of the continent outlines.  The default is set in 
;        !P.COLOR.
;    COUNTRIES:  If set, national borders are drawn.
;    HIRES:  If set, a high resolution map of boundaries is used.
;    LEGEND:  If set, the procedure plots a colour legend at the bottom of the 
;        window.  The default is no legend.  This can also be a 2-element 
;        vector containing the position ([x,y]) in normal coordinates of the 
;        lower left corner of the legend box.
;    LEVELS:  A vector of values for the contour levels.  This can be 
;        determined automatically from the data.
;    LIMIT:  A four-element vector specifying the limiting coordinates of the 
;        mapping area, in the format of [minlat,minlon,maxlat,maxlon].
;    NOERASE:  If set, the procedure does not erase the screen before 
;        plotting.  The default is to erase before plotting.
;    NOLINES:  If set, then no outlines are drawn.  The default is not set.
;    NTICKS:  The number of ticks and labels to be used in the legend.  This 
;        can be determined automatically.
;    OUTLINE_COLOR:  The colour index of the OUTLINE_DATA contour lines.  The 
;        default is COLOR.
;    OUTLINE_DATA:  An optional data array of the same size as Data.  Contour 
;        lines for OUTLINE_DATA are plotted on top of the colour-contours of 
;        Data.
;    OUTLINE_LEVELS:  Like LEVELS but corresponding to OUTLINE_DATA.  The 
;        default is LEVELS.
;    OUTLINE_POINT:  If contour lines are being drawn, then setting this 
;        keyword causes perpendicular ticks to be drawn on the contours lines.  
;        If set to -1 then the ticks point downhill on OUTLINE_DATA, if set to 
;        1 then they point uphill.  The 1 setting does not work of BLOCKCELL=0.
;    [X,Y]RANGE:  A 2-element vector containing the minimum and maximum 
;        [x,y]-coordinates to be plotted.
;    REGION:  Obsolete keyword retained for continuity.
;    RIVERS:  If set rivers are drawn.
;    THICK:  The line thickness used for drawing.
;    [X,Y]THICK:  The line thickness used for drawing the axes.  This only 
;        works if the AXIS keyword is set.
;    TICKNAME:  A vector of strings to place on the tick marks in the legend.  
;        This can be determined automatically.
;    TITLE:  A string containing the title of the plot.
;    [X,Y]TITLE:  A string containing the title of the [X,Y] axis.  This only 
;        works if the AXIS keyword is set.
;    UNITS:  A string containing the title or units subtitle for the legend.
;    WRAP:  If set, the plot connects the westernmost and eastermost points to 
;        produce a continuous plot.  The default is to do this only if the data 
;        appears global in longitude.
;    CYLINDRICAL:  If set a cylindrical projection is used.  This is the 
;        default.
;    MERCATOR:  If set a Mercator projection is used.
;    NORTH:  If set an orthographic projection is used, centred on the North 
;        Pole and covering the Northern Hemisphere.
;    ORTHOGRAPHIC:  If set an orthographic projection is used.
;    SOUTH:  If set an orthographic projection is used, centred on the South 
;        Pole and covering the Southern Hemisphere.
;
; USES:
;    choose_levels.pro
;    contour_legend.pro
;    dimension.pro
;    odd.pro
;    sign.pro
;
; PROCEDURE:
;    This procedure plots a world map using map_set.pro and map_continents.pro 
;    and then plots a colour contour on top using contour.pro.
;
; EXAMPLE:
;    Contour an increasing-value 20*10 array over a map of Earth.
;      arr = findgen(20,10)
;      loadct, 5
;      contour_world, arr
;
; MODIFICATION HISTORY:
;    Written by:  Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-09-21.
;    Modified:  DAS, 2000-09-28 (level-picking stuff).
;    Modified:  DAS, 2000-11-16 (fixed polar projection bug).
;    Modified:  DAS, 2000-11-29 (added NTICKS and TICKNAME keywords).
;    Modified:  DAS, 2001-04-11 (added ability to handle irregular data).
;    Modified:  DAS, 2001-05-16 (added REGION, WRAP, LIMIT keywords).
;    Modified:  DAS, 2002-04-10 (switched STD.pro to STDDEV.pro)
;    Modified:  DAS, 2004-10-26 (improved documentation;  added AXIS, COASTS, 
;        COUNTRIES, HIRES, RIVERS, THICK, [X,Y]THICK keywords)
;    Modified:  DAS, 2006-02-14 (added BLOCKCELL keyword;  removed some WRAP 
;        value selection stuff;  made REGION option automatic and therefore 
;        obsolete)
;    Modified:  DAS, 2008-03-14 (fixed bug with LIMIT keyword wrapping 
;        longitude when BLOCKCELL set)
;    Modified:  DAS, 2009-02-12 (added NOLINES keyword)
;    Modified:  DAS, 2009-09-02 (fixed bug using coordinate ordering with 
;        irregular data;  added use of BLOCKCELL with irregular data;  ensured 
;        LEGEND option does not overwrite map plotting coordinates)
;    Modified:  DAS, 2009-11-09 (added BOLD keyword)
;    Modified:  DAS, 2009-12-08 (added ARROWEND keyword)
;    Modified:  DAS, 2009-12-09 (added fix to ensure longitudes do not span 
;        more than 360 degrees)
;    Modified:  DAS, 2009-12-15 (fixed bug in dealing with values outside the 
;        range of LEVELS)
;    Modified:  DAS, 2009-12-23 (added OUTLINE contour feature)
;    Modified:  DAS, 2010-01-06 (added OUTLINE_POINT keyword;  editted format;  
;        fixed OUTLINE_COLOR bug)
;-

;***********************************************************************

PRO CONTOUR_WORLD, $
    Data0, Lon0, Lat0, $
    ARROWEND=arrowend_opt, $
    AXIS=axisopt, $
    BOLD=bold_opt, $
    BLOCKCELL=blockcellopt, $
    C_COLORS=c_colors, $
    CENTRE=centre, $
    CHARCOLOR=charcolor, CHARSIZE=charsize, $
    COASTS=coastsopt, COUNTRIES=countriesopt, HIRES=hiresopt, $
        RIVERS=riversopt, $
    COLOR=color, $
    CYLINDRICAL=cylindricalopt, MERCATOR=mercatoropt, $
        ORTHOGRAPHIC=orthographicopt, NORTH=northopt, SOUTH=southopt, $
    LEGEND=legend, $
    LEVELS=levels, NLEVELS=nlevels, $
    LIMIT=limit, $
    XMARGIN=xmargin, YMARGIN=ymargin, $
    NOERASE=noerase, $
    NOLINES=nolines_opt, $
    OUTLINE_COLOR=outline_color, OUTLINE_DATA=outline_data, $
        OUTLINE_LEVELS=outline_levels, OUTLINE_POINT=outline_point, $
    NTICKS=nticks, TICKNAME=tickname, $
    THICK=thick, xthick=xthick, ythick=ythick, $
    TITLE=title, XTITLE=xtitle, YTITLE=ytitle, $
    UNITS=units, $
    WRAP=wrapopt, $
    REGION=regionopt

;***********************************************************************
; Constants and Options

; Physical and mathematical constants
ndegree = 360
degrad = !pi / ndegree * 2.

;; Equivalent to zero threshold
;zthresh = 0.00001

; Legend option
legendopt = keyword_set( legend )

; Wrapping option
wrapopt = keyword_set( wrapopt )

; Option to print axes
axisopt = keyword_set( axisopt )

; Map projections
; Cylindrical projection
cylindricalopt = keyword_set( cylindricalopt )
; Mercator projection
mercatoropt = keyword_set( mercatoropt )
; Orthoscopic projection
orthographicopt = keyword_set( orthographic )
northopt = keyword_set( northopt )
southopt = keyword_set( southopt )
if northopt or southopt then begin
  orthographicopt = 1
endif else begin
  orthographicopt = 0
endelse

; Irregular gridding
if dimension( data0 ) eq 1 then begin
  irregularopt = 1
endif else begin
  irregularopt = 0
endelse

; Option to plot block cells rather than smooth contours
if keyword_set( blockcellopt ) then begin
  blockcellopt = 1
endif else begin
  blockcellopt = 0
endelse
;; This cannot work with an irregular grid
;if irregularopt eq 1 then blockcellopt = 0

; Arrowed end option
if keyword_set( arrowend_opt ) then begin
  if n_elements( arrowend_opt ) eq 1 then begin
    arrowend_opt = [ 0, 0 ] + arrowend_opt
  endif
endif else begin
  arrowend_opt = [ 0, 0 ]
endelse

;***********************************************************************
; Default Settings

; Printing settings
if !d.name eq 'PS' then begin
  if not( keyword_set( thick ) ) then thick = 3
  if not( keyword_set( xthick ) ) then xthick = thick
  if not( keyword_set( ythick ) ) then ythick = xthick
  !p.font = 0
  if n_elements( bold_opt ) eq 0 then bold_opt = 1
  device, helvetica=1, bold=bold_opt
endif

; Legend settings
if legendopt then begin
  if not( keyword_set( ymargin ) ) then ymargin = [6,3]
  if n_elements( legend ) ne 2 then legend = [0.3,0.09]
endif

; Default cylindrical map projection
if not( mercatoropt ) and not( orthographicopt ) then cylindricalopt = 1

; Copy input to temporary plotting arrays
data = data0
if keyword_set( lon0 ) then lon = lon0
if keyword_set( lat0 ) then lat = lat0
if keyword_set( limit0 ) then limit = limit0
; Ensure positive and increasing longitude
if ( irregularopt eq 0 ) and keyword_set( lon ) then begin
  id = where( lon lt 0, nid )
  if nid ne 0 then lon[id] = lon[id] + ndegree
  ; We redo the ordering of LON later in this section
  id = sort( lon )
  lon = lon[id]
  data = data[id,*]
endif
; Ensure increasing latitude
if ( irregularopt eq 0 ) and keyword_set( lat ) then begin
  id = sort( lat )
  lat = lat[id]
  data = data[*,id]
endif

; Longitude and latitude dimensions
if keyword_set( lon ) then begin
  nlon = n_elements( lon )
endif else begin
  nlon = n_elements( data[*,0] )
  lon = findgen( nlon ) / nlon * ndegree
endelse
if keyword_set( lat ) then begin
  nlat = n_elements( lat )
endif else begin
  nlat = n_elements( data[0,*] )
  lat = ( findgen( nlat ) - ( nlat - 1. ) / 2. ) / nlat * 2. * ndegree / 4.
endelse

; Find convenient longitude order
if irregularopt eq 0 then begin
  ; Find a gap in longitude
  fd = abs( lon[1:nlon-1] - lon[0:nlon-2] )
  fd = round( fd / min( fd ) )
  id = ( where( fd gt 1, nid ) )[0]
  ; Re-organise longitude if we have found a gap
  if nid ne 0 then begin
    id1 = indgen( id + 1 )
    id2 = indgen( nlon - 1 - id ) + id + 2
    lon[id2] = lon[id2] - ndegree
    lon = lon[[id2,id1]]
    data = data[[id2,id1],*]
  endif
endif
; Set the default mapping limits
if not( keyword_set( limit ) ) then begin
  if irregularopt eq 1 then begin
    limit = [ min( lat ), min( lon ), max( lat ), max( lon ) ]
  endif else begin
    limit = [ lat[0] - ( lat[1] - lat[0] ) / 2., $
        lon[0] - ( lon[1] - lon[0] ) / 2., $
        lat[nlat-1] + ( lat[nlat-1] - lat[nlat-2] ) / 2., $
        lon[nlon-1] + ( lon[nlon-1] - lon[nlon-2] ) / 2. ]
  endelse
; Otherwise ensure increasing longitude limits
endif else begin
  if limit[3] lt limit[1] then limit[1] = limit[1] - ndegree
endelse
; Ensure legal ranges
if cylindricalopt + mercatoropt eq 1 then begin
  if limit[0] lt -ndegree / 4. then limit[0] = -ndegree / 4.
  if limit[2] gt ndegree / 4. then limit[2] = ndegree / 4.
  if limit[1] gt limit[3] then limit[1] = limit[3]
  if limit[3] - ndegree - limit[1] gt 0 then limit[3] = limit[1] + ndegree
endif

; Ensure that the longitude values are within the mapping limits
if dimension( data ) eq 2 then begin
  id = where( lon - ndegree ge limit[1], n_id_1 )
  if n_id_1 gt 0 then lon[id] = lon[id] - ndegree
  id = where( lon + ndegree le limit[3], n_id_2 )
  if n_id_2 gt 0 then lon[id] = lon[id] + ndegree
  id = sort( lon )
  if max( abs( id[1:nlon-1] - id[0:nlon-2] ) ) gt 1 then begin
    lon = lon[id]
    data = data[id,*]
  endif
endif

;; Choice to wrap the data around the map sides
;if not( wrapopt ) then begin
;  fd = lon[1:nlon-1] - lon[0:nlon-2]
;  if stddev( fd[0:nlon-2], nan=1 ) lt zthresh then begin
;    if round( fd[0] - fd[nlon-1] ) eq ndegree then wrapopt = 1
;  endif
;endif

; Central point of the plot
if keyword_set( centre ) then begin
  centlon = centre[0]
  centlat = centre[1]
endif else if keyword_set( limit ) then begin
  centlon = ( limit[1] + limit[3] ) / 2.
  centlat = 0.
endif else begin
  if odd( nlon ) then begin
    centlon = lon[(nlon-1)/2]
  endif else begin
    centlon = mean( lon[nlon/2-1:nlon/2] )
  endelse
  centlat = 0
  if southopt then centlat = -ndegree / 4
  if northopt then centlat = ndegree / 4
endelse

; Contour levels
if not( keyword_set( levels ) ) then begin
  if not( keyword_set( nlevels ) ) then nlevels = 29
  levels = choose_levels( data )
  levels = levels[0] + findgen( nlevels+1 ) / nlevels $
      * ( levels[n_elements(levels)-1] - levels[0] )
endif
nlevels = n_elements( levels )
; Ensure proper behaviour at end levels
levels_contour = levels
if keyword_set( arrowend_opt ) then begin
  if arrowend_opt[0] eq 1 then begin
    temp = min( data, nan=1 )
    if temp lt levels_contour[0] then begin
      if temp lt 0 then begin
        temp = temp * 1.1
      endif else begin
        temp = temp / 1.1
      endelse
      levels_contour[0] = temp
    endif
  endif
  if arrowend_opt[1] eq 1 then begin
    temp = max( data, nan=1 )
    if temp gt levels_contour[nlevels-1] then begin
      if temp lt 0 then begin
        temp = temp / 1.1
      endif else begin
        temp = temp * 1.1
      endelse
      levels_contour[nlevels-1] = temp
    endif
  endif
endif

; Colour scale
if not( keyword_set( c_colors ) ) then c_colors = indgen( nlevels + 1 ) + 2

; The defaults for the option of plotting contour lines
if keyword_set( outline_data ) then begin
  if not( keyword_set( outline_color ) ) then begin
    if keyword_set( color ) then begin
      outline_color = color
    endif else begin
      outline_color = !p.color
    endelse
  endif
  if not( keyword_set( outline_levels ) ) then outline_levels = levels
  if keyword_set( outline_point ) then downhill_opt = -1 * outline_point
endif

;***********************************************************************
; Plot Map

; Record !p.multi settings (MAP_SET messes them up)
pmulti = !p.multi

; Set up map
map_set, centlat, centlon, 0, cylindrical=cylindricalopt, $
    mercator=mercatoropt, orthographic=orthographicopt, color=charcolor, $
    isotropic=1, noerase=noerase, title=title, charsize=charsize, $
    xmargin=xmargin, ymargin=ymargin, limit=limit

; Contour over map.
; If we have irregularly gridded data for a smooth contour style of plot
if ( irregularopt eq 1 ) and ( blockcellopt eq 0 ) then begin
  contour, data, lon, lat, levels=levels_contour, c_colors=c_colors, $
      cell_fill=1, overplot=1, irregular=1
; If we have irregularly gridded data for a block cell style of plot
endif else if ( irregularopt eq 1 ) and ( blockcellopt eq 1 ) then begin
  ; Set the polygon size
  n_vertex = 4
  ; Iterate through elements in the data
  for i = 0, nlon - 1 do begin
    ; Find n_vertex neighbouring points to this one
    temp = ( lon - lon[i] ) ^ 2 + ( lat - lat[i] ) ^ 2
    id_data = sort( temp )
    id_data = id_data[1:n_vertex]
    lon_near = lon[id_data]
    lat_near = lat[id_data]
    ; Sort vertices in counterclockwise order from the smallest angle
    ang = atan( lat_near - lat[i], lon_near - lon[i] )
    id = sort( ang )
    lon_near = lon_near[id]
    lat_near = lat_near[id]
    ang = ang[id]
    ; If the largest angle is greater than pi
    temp = [ ang[1:n_vertex-1], ang[0] + 2. * !pi ] - ang
    if max( temp ) gt 0.9 * !pi then begin
      ; Retain the closest points
      temp_1 = ( lon_near - lon[i] ) ^ 2 + ( lat_near - lat[i] ) ^ 2
      id = sort( temp_1 )
      ; If more than 1.5*pi is missing then retain only half the points
      if max( temp ) gt 1.4 * !pi then begin
        lon_near = lon_near[id[0:(n_vertex+1)/2-1]]
        lat_near = lat_near[id[0:(n_vertex+1)/2-1]]
      ; Otherwise retain 3/4 of the points
      endif else begin
        lon_near = lon_near[id[0:3*n_vertex/4-1]]
        lat_near = lat_near[id[0:3*n_vertex/4-1]]
      endelse
      ; Create fake points reflected across our point
      lon_near = [ lon_near, lon[i] + ( lon[i] - lon_near ) ]
      lat_near = [ lat_near, lat[i] + ( lat[i] - lat_near ) ]
      ; Restrict to n_vertex points
      n_temp = n_elements( lon_near )
      if n_temp gt n_vertex then begin
        ang = atan( lat_near - lat[i], lon_near - lon[i] )
        id = sort( ang )
        lon_near = lon_near[id]
        lat_near = lat_near[id]
        ang = ang[id]
        temp = [ ang[1:n_temp-1], ang[0] + 2. * !pi ] - ang
        ; Remove redundant points
        id = where( temp gt !pi / 10., n_id )
        lon_near = lon_near[id]
        lat_near = lat_near[id]
        ang = ang[id]
        temp = temp[id]
        if n_id gt n_vertex then begin
          id = sort( temp )
          id = id[0:n_vertex-1]
          lon_near = lon_near[id]
          lat_near = lat_near[id]
        endif
      endif
    endif
    ; Determine vertices of polygon
    temp_lon = fltarr( n_vertex )
    temp_lat = fltarr( n_vertex )
    temp_lon[0] = ( lon_near[n_vertex-1] + lon_near[0] ) / 2.
    temp_lat[0] = ( lat_near[n_vertex-1] + lat_near[0] ) / 2.
    for j = 1, n_vertex - 1 do begin
      temp_lon[j] = ( lon_near[j-1] + lon_near[j] ) / 2.
      temp_lat[j] = ( lat_near[j-1] + lat_near[j] ) / 2.
    endfor
    ; Determine colour
    id = max( where( levels_contour - data[i] lt 0 ) )
    if id eq -1 then id = 0
    ; Plot cell
    polyfill, temp_lon, temp_lat, color=c_colors[id]
  endfor
; If we have regularly gridded data for a block cell style of plot
endif else if blockcellopt eq 1 then begin
  ; Take one pass for the data and one for the outline contours
  for i_pass = 0, keyword_set( outline_data ) do begin
    ; Determine the cells within LIMIT (the mapping range)
    id_lon = where( ( lon ge limit[1] ) and ( lon le limit[3] ), n_id_lon )
    id_lat = where( ( lat ge limit[0] ) and ( lat le limit[2] ), n_id_lat )
    ; Initialise cell border vectors
    temp_lon = fltarr( 4 )
    temp_lat = fltarr( 4 )
    ; Iterate through latitudes
    for i_lat = 0, n_id_lat - 1 do begin
      ; Determine the latitude borders of this cell
      if i_lat eq 0 then begin
        temp = max( [ limit[0], $
            lat[id_lat[0]] - ( lat[id_lat[1]] - lat[id_lat[0]] ) / 2. ] )
        temp = max( [ temp, -ndegree / 4. + 0.1 ] )
        temp_lat[[0,1]] = temp
      endif else begin
        temp_lat[[0,1]] = ( lat[id_lat[i_lat]] + lat[id_lat[i_lat-1]] ) / 2.
      endelse
      if i_lat eq n_id_lat - 1 then begin
        temp = min( [ limit[2], lat[id_lat[n_id_lat-1]] $
            + ( lat[id_lat[n_id_lat-1]] - lat[id_lat[n_id_lat-2]] ) / 2. ] )
        temp = min( [ temp, ndegree / 4. - 0.1 ] )
        temp_lat[[2,3]] = temp
      endif else begin
        temp_lat[[2,3]] = ( lat[id_lat[i_lat+1]] + lat[id_lat[i_lat]] ) / 2.
      endelse
      ; Iterate through longitudes
      for i_lon = 0, n_id_lon - 1 do begin
        ; Determine if this cell has data
        if finite( data[id_lon[i_lon],id_lat[i_lat]] ) eq 1 then begin
          ; Determine the longitude borders of this cell
          if i_lon eq 0 then begin
            temp_lon[[0,3]] = max( [ limit[1], $
                lon[id_lon[0]] - ( lon[id_lon[1]] - lon[id_lon[0]] ) / 2. ] )
          endif else begin
            temp_lon[[0,3]] = ( lon[id_lon[i_lon]] + lon[id_lon[i_lon-1]] ) / 2.
          endelse
          if i_lon eq n_id_lon - 1 then begin
            temp_lon[[1,2]] = min( [ limit[3], lon[id_lon[n_id_lon-1]] $
                + ( lon[id_lon[n_id_lon-1]] - lon[id_lon[n_id_lon-2]] ) / 2. ] )
          endif else begin
            temp_lon[[1,2]] = ( lon[id_lon[i_lon+1]] + lon[id_lon[i_lon]] ) / 2.
          endelse
          ; If we are drawing filled contours
          if i_pass eq 0 then begin
            ; Determine colour
            id = max( where( levels_contour $
                - data[id_lon[i_lon],id_lat[i_lat]] lt 0 ) )
            if id eq -1 then id = 0
            ; Plot cell
            polyfill, temp_lon, temp_lat, color=c_colors[id]
          ; If we are drawing outline contours
          endif else begin
            ; Determine the contour level of this cell
            id_level = max( where( outline_levels - $
                outline_data[id_lon[i_lon],id_lat[i_lat]] lt 0 ) )
            ; Determine the contour level of the cell to the east
            if i_lon eq n_id_lon - 1 then begin
              if wrapopt eq 1 then begin
                id_level_1 = max( where( outline_levels $
                    - outline_data[id_lon[0],id_lat[i_lat]] lt 0 ) )
              endif else begin
                id_level_1 = id_level
              endelse
            endif else begin
              id_level_1 = max( where( outline_levels $
                  - outline_data[id_lon[i_lon+1],id_lat[i_lat]] lt 0 ) )
            endelse
            ; If the contour levels are different then plot a contour line
            if id_level_1 ne id_level then begin
              plots, temp_lon[[1,2]], temp_lat[[1,2]], color=outline_color, $
                  thick=thick
              ; Plot a down-/up-hill tick if requested
              if keyword_set( outline_point ) then begin
                d_lon_tick = [ 0, ( temp_lon[2] - temp_lon[3] ) / 4. ]
                lat_tick = [ 0, 0 ] + mean( temp_lat[[1,2]] )
                temp = outline_point * sign( id_level_1 - id_level )
                oplot, temp_lon[2]+temp*d_lon_tick, lat_tick, $
                    color=outline_color, thick=thick
              endif
            endif
            ; Determine the contour level of the cell to the north
            if i_lat eq n_id_lat - 1 then begin
              id_level_1 = id_level
            endif else begin
              id_level_1 = max( where( outline_levels $
                  - outline_data[id_lon[i_lon],id_lat[i_lat+1]] lt 0 ) )
            endelse
            ; If the contour levels are different then plot a contour line
            if id_level_1 ne id_level then begin
              plots, temp_lon[[2,3]], temp_lat[[2,3]], color=outline_color, $
                  thick=thick
              ; Plot a down-/up-hill tick if requested
              if keyword_set( outline_point ) then begin
                lon_tick = [ 0, 0 ] + mean( temp_lon[[2,3]] )
                d_lat_tick = [ 0, ( temp_lat[3] - temp_lat[0] ) / 4. ]
                temp = outline_point * sign( id_level_1 - id_level )
                oplot, lon_tick, temp_lat[3]+temp*d_lat_tick, $
                    color=outline_color, thick=thick
              endif
            endif
            ; Determine the contour level of the cell to the west if this is 
            ; the westernmost cell
            if ( i_lon eq 0 ) and ( wrapopt eq 1 ) then begin
              id_level_1 = max( where( outline_levels $
                  - outline_data[id_lon[n_id_lon-1],id_lat[i_lat]] lt 0 ) )
              ; If the contour levels are different then plot a contour line
              if id_level_1 ne id_level then begin
                plots, temp_lon[[3,0]], temp_lat[[3,0]], color=outline_color, $
                    thick=thick
                ; Plot a down-/up-hill tick if requested
                if keyword_set( outline_point ) then begin
                  d_lon_tick = [ 0, ( temp_lon[2] - temp_lon[3] ) / 4. ]
                  lat_tick = [ 0, 0 ] + mean( temp_lat[[0,3]] )
                  temp = outline_point * sign( id_level_1 - id_level )
                  oplot, temp_lon[0]-temp*d_lon_tick, lat_tick, $
                      color=outline_color, thick=thick
                endif
              endif
            endif
          endelse
        endif
      endfor
    endfor
  endfor
; If we have regularly gridded data for a smooth contour style of plot
endif else begin
  if wrapopt then begin
    ; Wrapped global data
    contour, [data,data[0,*]], [lon,lon[0]], lat, levels=levels_contour, $
        c_colors=c_colors, cell_fill=1, overplot=1
    ; Draw outline contours if requested
    if keyword_set( outline_data ) then begin
      contour, [outline_data,outline_data[0,*]], [lon,lon[0]], lat, $
          levels=outline_levels, overplot=1, color=outline_color, thick=thick, $
          downhill=downhill_opt
    endif
  endif else begin
    ; Regional data
    contour, data, lon, lat, levels=levels_contour, c_colors=c_colors, $
        cell_fill=1, overplot=1
    ; Draw outline contours if requested
    if keyword_set( outline_data ) then begin
      contour, outline_data, lon, lat, levels=outline_levels, overplot=1, $
          color=outline_color, thick=thick, downhill=downhill_opt
    endif
  endelse
endelse

; Draw coastlines
if not( keyword_set( nolines_opt ) ) then begin
  map_continents, hires=hiresopt, coasts=coastsopt, rivers=riversopt, $
      countries=countriesopt, color=color, thick=thick
endif

; Draw axes.
; This is simple only if a cylindrical projection is used
if cylindricalopt and axisopt then begin
  xrange = [ min( lon ), max( lon ) ] $
      + 0.01 * [ -1., 1. ] * ( max( lon ) - min( lon ) )
  yrange = [ min( lat ), max( lat ) ] $
      + 0.01 * [ -1., 1. ] * ( max( lat ) - min( lat ) )
  axis, xrange[0], 0, yaxis=0, xmargin=xmargin, ymargin=ymargin, $
      ytitle=ytitle, ythick=ythick, charsize=charsize, $
      yrange=yrange, ystyle=1, noerase=1
  axis, xrange[1], 0, yaxis=1, xmargin=xmargin, ymargin=ymargin, $
      ythick=ythick, charsize=charsize, yrange=yrange, ystyle=1, noerase=1, $
      ytickname=strarr(30)+' '
  axis, 0, yrange[0], xaxis=0, xmargin=xmargin, ymargin=ymargin, $
      xtitle=xtitle, xthick=xthick, charsize=charsize, $
      xrange=xrange, xstyle=1, noerase=1
  axis, 0, yrange[1], xaxis=1, xmargin=xmargin, ymargin=ymargin, $
      xthick=xthick, charsize=charsize, xrange=xrange, xstyle=1, noerase=1, $
      xtickname=strarr(30)+' '
endif

; Restore !p.multi settings
!p.multi = pmulti

;***********************************************************************
; Plot Legend

if legendopt then begin
  ; Plot legend
  contour_legend, legend[0]+[0.,0.4], legend[1]+[0.,0.03], $
      levels=levels, c_colors=c_colors, color=charcolor, charsize=charsize, $
      normal=1, horizontal=1, subtitle=units, nticks=nticks, $
      tickname=tickname, arrowend=arrowend_opt
  ; Ensure that plotting the legend has not forced us to a new sub-window
  if pmulti[1] + pmulti[2] gt 2 then begin
    if !p.multi[0] eq 0 then !p.multi[0] = !p.multi[1] * !p.multi[2]
    !p.multi[0] = !p.multi[0] - 1
  endif
  ; Ensure that mapping coordinates are retained in the plotting window
  map_set, centlat, centlon, 0, cylindrical=cylindricalopt, $
      mercator=mercatoropt, orthographic=orthographicopt, color=charcolor, $
      isotropic=1, noerase=1, title=title, charsize=charsize, $
      xmargin=xmargin, ymargin=ymargin, limit=limit
endif

;***********************************************************************
; Setdown

; Undo printing settings
if !d.name eq 'PS' then begin
  !p.font = -1
endif

;***********************************************************************
; The End

return
END
