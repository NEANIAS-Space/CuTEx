;+
; NAME:
;	LAND_MASK
;
; PURPOSE:
;	This function creates and returns a global gridded land mask of
;	the desired resolution.
;
; CATEGORY:
;	Geographical
;
; CALLING SEQUENCE:
;	Result = LAND_MASK( )
;
; KEYWORD PARAMETERS:
;	AFRICA:  If set the function returns a mask of the African
;	          landmass only.  Note this may not properly isolate
;	          Africa from Eurasia at some resolutions.
;	ANTARCTICA:  If set the function returns a mask of the Antarctic
;	             landmass only.
;	AUSTRALIA:  If set the function returns a mask of the Australian
;	            landmass only.
;	EURASIA:  If set the function returns a mask of the Eurasian
;	          landmass only.  Note this may not properly isolate
;	          Eurasia from Africa at some resolutions.
;	LATITUDE:  Returns a vector of the latitude coordinates of the
;	           grid points.
;	LIMIT:  The coordinates of the corner points of the grid, of
;	        type floating point.  This vector is of the form
;	        [ westernmost longitude, southernmost latitude,
;	          easternmost longitude, northernmost latitude].
;	        The default is [ -180, -90, 180, 90 ].
;	LONGITUDE:  Returns a vector of the longitude coordinates of
;	            the grid points.
;	NAMERICA:  If set the function returns a mask of the North
;	           American landmass only.
;	RESOLUTION:  The scalar resolution of the uniform grid in degrees.
;	      If a 2-element vector it defines the resolution for
;	      [ longitude, latitude ].  The default is 1x1 degree.
;	SAMERICA:  If set the function returns a mask of the South
;	           American landmass only.
;	SEED:  The [lon,lat] coordinates of a single desired landmass.
;	       The function returns a mask of all contiguous land
;	       surrounding this point.
;	WALL:  A 4-element vector (or 4xN-element array) of coordinates
;	       of wall(s) to separate landmasses when using SEED (e.g.
;	       to separate South America from North America).  The
;	       coordinates are of the form [ lon1, lat1, lon2, lat2 ].
;
; OUTPUTS:
;	Result:  A longitude x latitude land mask grid.  Points
;	         corresponding to land are denoted by "1", "0"
;	         otherwise.
;	LATITUDE, LONGITUDE
;
; USES:
;	dimension.pro
;	sign.pro
;
; PROCEDURE:
;	This function plots a global map in a virtual window and
;	determines which points are coloured as land.  If SEED is set,
;	it uses an algorithm expanding from the SEED point to determine
;	contiguous points.
;
; EXAMPLE:
;	Return a 1x1-degree global land grid.
;	  Result = land_mask( )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-07-25.
;	Modified:	DAS, 2000-08-22 (killed grid size bug).
;	Modified:	DAS, 2000-08-29 (improved continent definitions;  
;			killed WALL bug).
;	Modified:	DAS, 2004-03-10 (altered to work with TrueColor).
;	Modified:	DAS, 2006-07-25 (modified documentation;  fixed bug 
;			where end LATITUDE values are on top of poles;  
;			removed use of constants.pro)
;-

;***********************************************************************

FUNCTION LAND_MASK, $
	LIMIT=limit, $
	RESOLUTION=resolution, $
	SEED=seed, $
	WALL=wall, $
	AFRICA=africaopt, ANTARCTICA=antarcticaopt, AUSTRALIA=australiaopt, $
	  EURASIA=eurasiaopt, NAMERICA=namericaopt, SAMERICA=samericaopt, $
	LONGITUDE=longitude, LATITUDE=latitude

;***********************************************************************
; Constants

; The number of degrees in a circle
ndeg = 360

; Virtual window dimensions
windim = [1000,500]

; Mask resolution
if keyword_set( resolution ) then begin
  lonres = resolution[0]
  if n_elements(resolution) eq 1 then begin
    latres = resolution[0]
  endif else begin
    latres = resolution[1]
  endelse
endif else begin
  lonres = 1
  latres = 1
endelse

; Grid coordinates
; Longitude
if keyword_set( limit ) then begin
  lon1 = limit[0]
  lon2 = limit[2]
  if ( lon1 ge lon2 ) and ( lon1 gt 0 ) then lon1 = lon1 - ndeg
  if ( lon1 ge lon2 ) and ( lon2 le 0 ) then lon2 = lon2 + ndeg
endif else begin
  lon1 = -ndeg / 2
  lon2 = ndeg / 2
endelse
nlon = round( 1. * ( lon2 - lon1 ) / lonres )
lon = findgen( nlon ) * lonres + lon1 + lonres / 2.
; Latitude
if keyword_set( limit ) then begin
  if limit[1] lt limit[3] then begin
    lat1 = limit[1]
  endif else begin
    lat1 = limit[3]
  endelse
  if limit[1] lt limit[3] then begin
    lat2 = limit[3]
  endif else begin
    lat2 = limit[1]
  endelse
endif else begin
  lat1 = -ndeg / 4
  lat2 = ndeg / 4
endelse
nlat = round( ( lat2 - lat1 ) / latres )
lat = findgen( nlat ) * latres + lat1 + latres / 2.

; Output
mask = intarr( nlon, nlat )

; Seed/continent options
if keyword_set( africaopt ) then begin
  seed = [20,5]
  wall = [ [-10,37,15,38], [15,38,35,32], [35,32,44,15], [44,15,55,15] ]
endif
if keyword_set( antarcticaopt ) then begin
  seed = [0,-80]
endif
if keyword_set( australiaopt ) then begin
  seed = [135,-25]
endif
if keyword_set( eurasiaopt ) then begin
  seed = [100,30]
  wall = [ [-10,34,9,37], [9,37,29,34], [29,34,42,11], [42,11,55,12] ]
endif
if keyword_set( namericaopt ) then begin
  seed = [-100,50]
  wall = [-79,5,-79,10]
endif
if keyword_set( samericaopt ) then begin
  seed = [-60,-15]
  wall = [-81,5,-81,10]
endif
seedopt = keyword_set( seed )

; Fit Seed and Wall into longitude limits
if seedopt then begin
  if seed[0] gt lon2 then seed[0] = seed[0] - ndeg
  if seed[0] lt lon1 then seed[0] = seed[0] + ndeg
  ;if keyword_set(wall) then begin
  ;  for i=0,n_elements(wall[0,*])-1 do begin
  ;    if wall[0,i] gt lon2 then wall[0,i] = wall[0,i] - ndeg
  ;    if wall[0,i] lt lon1 then wall[0,i] = wall[0,i] + ndeg
  ;    if wall[2,i] gt lon2 then wall[2,i] = wall[2,i] - ndeg
  ;    if wall[2,i] lt lon1 then wall[2,i] = wall[2,i] + ndeg
  ;  endfor
  ;endif
endif

; Kernel for seed growth
if seedopt then begin
  near = [ [-1,0,1,1,1,0,-1,-1], [1,1,1,0,-1,-1,-1,0] ]
  nnear = 8
endif

; Wall option
wallopt = keyword_set( wall ) and seedopt
if wallopt then begin
  if dimension( wall ) eq 1 then wall = reform( wall, 4, 1 )
endif

;***********************************************************************
; Draw Map

; Create virtual window
window, free=1, pixmap=1, xsize=windim[0], ysize=windim[1]

; Plot map
midlon = ( lon2 + lon1 ) / 2.
map_set, 0, midlon, 0, cylindrical=1, isotropic=1
map_continents, coast=1, fill_continents=1

;***********************************************************************
; Determine Mask

; Convert to device coordinates
pos1 = convert_coord( lon1, lat1, data=1, to_device=1 )
pos2 = convert_coord( lon2, lat2, data=1, to_device=1 )
posdim = [ pos2[0] - pos1[0], pos2[1] - pos1[1] ]

; Read colour from plot
pixcol = tvrd( pos1[0], pos1[1], round( posdim[0] ) + 1, $
    round( posdim[1]) + 1 )

; Determine mask
pixlon = round( ( lon - lon1 ) / ( lon2 - lon1 ) * posdim[0] )
for i = 0, nlat - 1 do begin
  pixlat = round( ( lat[i] - lat1) / ( lat2 - lat1 ) * posdim[1] )
  ; This line has had to be changed since it does not work with TrueColor.
  ;id = where( pixcol[pixlon,pixlat] eq !p.color, siz )
  id = where( pixcol[pixlon,pixlat] ne 0, siz )
  if siz ne 0 then mask[id,i] = 1
endfor

; Close virtual window
wdelete

;***********************************************************************
; Seed/Continent Options

if seedopt then begin
  ; Create wall
  if wallopt then begin
    for i = 0, n_elements( wall[0,*]) - 1 do begin
      nwall = sqrt( ( ( wall[2,i] - wall[0,i] ) / lonres ) ^ 2 $
          + ( ( wall[3,i] - wall[1,i] ) / latres ) ^ 2 )
      lonfract = 1. / nwall * ( wall[2,i] - wall[0,i] )
      latfract = 1. / nwall * sign( lonfract ) * ( wall[3,i] - wall[1,i] )
      for j = 0, ceil( nwall ) - 1 do begin
        walllon = wall[0,i] + j * lonfract
        if walllon gt lon2 then walllon = walllon - ndeg
        if walllon lt lon1 then walllon = walllon + ndeg
        walllon = walllon - lon
        walllat = wall[1,i] + j * latfract - lat
        idlon = ( where( abs( walllon ) eq min( abs( walllon ) ) ) )[0]
        if walllon[idlon] lt 0 then idlon = idlon - 1
       idlat = ( where( abs( walllat ) eq min( abs( walllat ) ) ) )[0]
        if walllat[idlat] lt 0 then idlat = idlat - 1
        mask[idlon+[0,0,1,1],idlat+[0,1,1,0]] = 0
      endfor
    endfor
    ; Revert to input format
    wall = reform(wall)
  endif
  newmask = 0 * mask
  oldmask = newmask
  seedlon = where( abs( seed[0] - lon ) eq min( abs( seed[0] - lon ) ) )
  seedlat = where( abs( seed[1] - lat ) eq min( abs( seed[1] - lat ) ) )
  newmask[seedlon,seedlat] = mask[seedlon,seedlat]
  if newmask[seedlon[0],seedlat[0]] eq 1 then begin
    good = 1
    while good ge 1 do begin
      id = where( newmask - oldmask eq 1, nid )
      oldmask1 = newmask
      glat = id / nlon
      glon = round( ( ( 1. * id / nlon ) - glat ) * nlon )
      good = 0
      for i = 0, nid - 1 do begin
        for j = 0, nnear - 1 do begin
          nearlon = glon[i] + near[j,0]
          ; Wrap around longitude
          if lon2 - lon1 gt ndeg - 1.5 * lonres then begin
            if nearlon eq -1 then nearlon = nlon - 1
            if nearlon eq nlon then nearlon = 0
          endif
          nearlat = glat[i] + near[j,1]
          if ( nearlat ge 0 ) and ( nearlat lt nlat ) then begin
            if ( nearlon ge 0 ) and ( nearlon lt nlon ) then begin
              if mask[nearlon,nearlat] - oldmask[nearlon,nearlat] eq 1 $
                  then begin
                newmask[nearlon,nearlat] = 1
                good = 1
              endif
            endif
          endif
        endfor    
      endfor
      oldmask = oldmask1
    endwhile
  endif
  mask = newmask
endif

;***********************************************************************
; The End

; Optional output
longitude = lon
latitude = lat

return, mask
END
