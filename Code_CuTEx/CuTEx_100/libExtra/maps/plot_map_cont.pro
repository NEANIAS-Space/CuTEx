;+
; Name: plot_map_contour
;
; Purpose: include file for plot_map
; Mod: Kim, 9-Jan-2001 - added c_charsize stuff.
; Modified, 31-Aug-2001, Zarro (EITI/GSFC) - added % check for levels
; Modified, 30-Oct-2003, Zarro (L-3/GSFC) - fixed call to DSCALE
; Modified, 20-Nov-2003, Kim - fixed bug with percentage contours on log plots
; Modified, 25-Nov-2003, Zarro (L-3Com/GSFC) - fixed bug with plot range
; Modified, 21-Jul-2005, Zarro (L-3Com/GSFC) - fixed bug with c_colors
; Modified, 3-May=2007, Kim - if clabel, init c_labels to integer 1 (prev = c_levels)
;-

if cont or surf or sh_surf or show3 then begin

;-- filter out any missing or non-positive data (for log scale plots)
;   by setting them outside the data plot range

 pic=dscale(pic,/no_copy,missing=missing,drange=prange,log=dlog,$
            crange=corange,err=err_msg)

 if err_msg ne '' then begin
  status = 0
  goto,done
 endif

;-- default is 10 contour levels between plot data range min/max

 if cont then begin

  tail=keyword_set(tail)
  percent=keyword_set(percent)
  nlev=10

;-- start with user input levels
;-- if levels not entered on command line, prompt if /tail

  if exist(levels) then begin
   lmin=min(levels,max=lmax)
   if (lmin ne lmax) or (lmax ne 0) then begin
    c_levels=levels
    if dlog then begin
     ok=where(c_levels gt 0,count)
     if count gt 0 then c_levels=c_levels[ok] else begin
      err_msg='Need at least one positive contour level for log plots'
      message,err_msg,/cont
      status = 0
      goto,done
     endelse
    endif
    if percent then begin
     if dlog then c_levels=alog10(c_levels*(10.^corange[1])/100.) else $
      c_levels=c_levels*(corange[1])/100.
    endif else begin
     if dlog then c_levels=alog10(c_levels)
    endelse
   endif
  endif else begin
   if tail then begin
    print,'* default is 10 equispaced contours between:'
    print,' MIN = ',corange[0],' and MAX = ',corange[1]
    print,'* enter explicit values for contour levels'
    clev='' & read,'----> ',clev
    if is_string(clev) then c_levels=float(str2arr(clev)) else begin
     nlev='' & read,'* enter number of equispaced contour levels [def=10]: ',nlev
     if is_number(nlev) then nlev=fix(nlev)
    endelse
   endif
  endelse

;-- construct default levels if not entered via keyword or prompt

  if not exist(c_levels) then $
   c_levels=corange[0]+findgen(nlev)*(corange[1]-corange[0])/(nlev-1.)

  dprint,'% PLOT_MAP_CONT: c_levels=',c_levels

;-- establish default contour properties

  if not exist(c_colors) then begin
   if wbuff or zbuff then c_colors=white else c_colors=black
  endif
  if not exist(c_thick) then c_thick=1
  if not exist(c_style) then c_style=0
  if exist(cthick) then c_thick=fix(cthick)
  if exist(cstyle) then c_style=fix(cstyle)
  if exist(lcolor) then c_colors=byte(lcolor)

  clabel=keyword_set(clabel)

;-- tailor contours

  if tail then begin
   ans='' & read,'* label contours [def=n]? ',ans
   if ans eq '' then ans='n' & ans=strupcase(strmid(ans,0,1))
   clabel=ans eq 'Y'
   c_style='' & read,'* enter contour linestyle [def=0, solid]: ',c_style
   c_style=fix(c_style)
   c_thick='' & read,'* enter contour thickness [def=1]: ',c_thick
   if c_thick eq '' then c_thick=1. & c_thick=fix(c_thick)
  endif

;-- sort the whole thing out

  cs=uniq([c_levels],sort([c_levels]))
  c_levels=c_levels[cs]
  nlev=n_elements(c_levels)
  c_labels=intarr(nlev)
  if clabel then begin
   c_labels=intarr(nlev)+1 & odd=where(findgen(nlev) mod 2,count)
   if count gt 0 then c_labels[odd]=0.
  endif

  c_thick=replicate(c_thick,nlev)
  c_style=replicate(c_style,nlev)
  c_colors=replicate(c_colors,nlev)
  c_charsize = 0.

; If c_charsize isn't explictly set, IDL is supposed to automatically set c_charsize to .75 of character size
; used for labels, but that doesn't seem to work.  So we'll do it ourselves.

  if exist (ccharsize) then c_charsize = ccharsize else begin
   if have_tag(extra,'charsize') then c_charsize = .75 * extra.charsize
  endelse

  contour,pic,xp,yp,/data,xstyle=5,ystyle=5,c_linestyle=c_style,$
  levels=c_levels,c_labels=c_labels,c_thick=c_thick,c_colors=c_colors,$
  max_value=corange[1],min_value=corange[0],over=(over gt 0),$
  /noeras,noclip=0,/follow,xrange=dxrange,yrange=dyrange,$
  clip=[dxrange[0],dyrange[0],dxrange[1],dyrange[1]],$
  c_charsize=c_charsize,_extra=extra

 endif

endif

