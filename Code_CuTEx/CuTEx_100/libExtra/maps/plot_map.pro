;+
; Project     : SOHO_CDS
;
; Name        : PLOT_MAP
;
; Purpose     : Plot an image map
;
; Category    : imaging
;
; Syntax      : plot_map,map
;
; Inputs      : MAP = image structure map created by MAKE_MAP
;               INDEX = optional index (if array of maps) [def=0]
;
; Keywords    :
;     /OVERLAY = overlay on previous image
;     /CONT = contour the image
;     SMOOTH_WIDTH = smoothing width (> 1)
;     FOV = [fx,fy] = field of view to be plotted
;     GRID_SPACING = grid spacing (deg) for latitude-longitude grid [def= 0, no grid]
;     GLABEL = label grid with coordinate values [def = 0, no labels]
;     GSTYLE = grid linestyle [def=0]
;     CENTER = [xc,yc] = center coordinates (arcsec) of FOV [def = center of image]
;            (if center is a valid map, then use its center)
;     DMIN,DMAX = min, max data for plot [def = data min,max]
;     BORDER = draw border around image [def = no]
;     /TAIL = allows user to tailor contours
;     /LOG  = log_10 scale image
;     WINDOW = window index to send plot to
;     /NOAXES = inhibit plotting axes
;     /NOTITLE = inhibit printing title
;     /NOLABELS = inhibit axis labels
;     /NOXTICKS = inhibit X-tick labels
;     /NOYTICKS = inhibit Y-tick labels
;     /DROTATE  = solar rotate image contour
;     LEVELS  = user specified contour levels
;     /POSITIVE_ONLY = plot positive data
;     /NEGATIVE_ONLY = plot negative data
;     XRANGE,YRANGE = cartesian plot limits
;     /INTERLACE = interlace two images when overlaying
;     /COMPOSITE = simultaneously plot two images when overlaying
;                = type of compositing:
;                  1: original , 2: latest(new), 3: max(new/old) 4: min(new/old)
;     /AVERAGE   = average two images when using /COMPOSITE
;     BOTTOM = lowermost color index to use in color scaling [def=0]
;     LAST_SCALE = use MIN/MAX from previous plot, or if LAST is a valid map
;                  use scaling from it
;     LIMB_PLOT = overplot solar limb
;     BTHICK = border thickness
;     CTHICK, C_THICK = contour thickness
;     BCOLOR = border line color (defaults to white)
;     LCOLOR, C_COLORS = contour line color (defaults to white)
;     GCOLOR = grid color  (defaults to white)
;     GTHICK = grid thickness
;     LMCOLOR = limb color (defaults to white)
;     LMTHICK = limb thickness
;     MULTI = set for multiple plots per page, e.g. mult=[2,2] (or 2) for 4
;             plots per page (!p.multi remains at this value until cleared)
;     NOERASE = don't erase previous plot
;     SQUARE_SCALE = force equal aspect ratio (by adjusting position)
;     CLABEL  = label contours
;     CSTYLE, C_STYLE  = contour linestyle
;     MARGIN  = margin around plot [normalized coords, used only if /SQUARE]
;     /SURFACE = show the image as a wire surface
;     /SHADE_SURF = show the image as a shaded surface
;     AX_SURF = Angle of rotation about X axis for surface (degrees)
;     AZ_SURF = Angle of rotation about Z axis for surface (degrees)
;     ERR_MSG = String error message if any
;     STATUS = 0/1 means failure/success
;     CBAR = 0/1 means draw colorbar on image plots (only works in > IDL 5.2)
;     CCHARSIZE = size of characters used for contour level labels.  (if not set, but charsize is
;        passed in extra, then c_charsize is .75 * charsize.  Otherwise 0.)
;     PERCENT = if levels are entered, they are in % of data max
;     MARK_POINT = if set to a 2-element array, it is the x,y data coords of a point to be marked
;        If point is not within plot, and arrow off edge of plot shows direction to point.
;     NEAREST = if maps is an array, then plot map nearest in time to this value
;     XSHIFT, YSHIFT = arcsec shifts to apply to map (same as TRANS)
;     DURTITLE = If set, plot title will include duration of image.  Title will be
;        map.id +  dd-mmm-yy hh:mm:ss-hh:mm:ss
;     DIMENSIONS = plot window dimensions (XSIZE, YSIZE entered as 2-d vector)
;
; Restrictions:
;      - do not set /GRID unless absolute coordinates of image are known
;      - do not set /OVERLAY unless a plot exists on the current device
;
; History     : Written 22 December 1996, D. Zarro, SAC/GSFC
;             : S.L.Freeland - let COMPOSITE have different interpretations
;             : Major modifications 15 Feb 1999, Zarro (SM&A/GSFC)
;                -- reorganized
;                -- sped-up sub-field extraction
;                -- fixed potential bug in contour levels scaling
;                -- fixed potential bug in image color scaling
;                -- fixed roll correction
;             : Zarro (SM&A/GSFC) 28 April 1999
;                -- fixed roll_center interpretation
;                (heaven help me for onlining this during Gopal's CDAW)
;             : Zarro (SM&A/GSFC) 3 May 1999
;                -- allowed overlaying images with different roll centers.
;             : Zarro (SM&A/GSFC) 5 Aug 1999
;                -- Made VIEW_ADJUST=1 the default
;             : Zarro (SM&A/GSFC) 25 Sep 1999
;                -- Made VIEW_ADJUST=0 the default
;             : Zarro (SM&A/GSFC) 1 Nov 1999
;                -- Added GSTYLE grid style keyword
;             : Zarro (SM&A/GSFC) 30 Nov 1999
;                -- Added OVERLAY=2 hidden feature
;             : Zarro (SM&A/GSFC) 14 Feb 2000
;                -- changed CONT keyword to CONTOUR, and added /EXTEND
;             : Zarro (SM&A/GSFC) 27 Mar 2000
;                -- changed CLABEL, removed old keywords
;             : Zarro (SM&A/GSFC) 7 Apr 2000
;                -- fixed roundoff errors causing edge pixels
;                   to spill over during hardcopying. Also improved smoothing.
;             : Zarro (SM&A/GSFC) 25 Apr 2000
;                -- fixed problem with image viewport falling outside plot
;                   limits; removed EXTENDS, VIEW_ADJUST keywords
;             : Zarro (EIT/GSFC) 10 May 2000, added optional INDEX argument
;             : Zarro (EIT/GSFC) 28 June 2000, added MARGIN keyword and call
;                to GET_ASPECT
;             : Kim 27 Sep 2000 - added surface and shade_surface options
;             : Kim 29 Sep 2000 - removed some keywords to stay under 64
;                argument limit in versions < 5.3.  Handle new keywords in
;                _extra
;             : Kim 1 Oct 2000 - added show3 option
;             : Kim 4 Oct 2000 - added status keyword.  err keyword became
;                err_msg.
;             : Zarro (EIT/GSFC): 6 Oct 2000 -- restored capability to overlay
;                images without using contours.
;                                13 Oct 2000 -- added rescale zoom logic
;             : Kim 9 Jan 2001 - added colorbar option (for > IDL V 5.2)
;                and ccharsize keyword
;             : Khan (MSSL/ISAS): 2001 Mar 30 -- Changed Kim's colorbar to
;                cbar to avoid conflict with color keyword
;             : Zarro (EITI/GSFC): 2001 Jun 18 -- Fixed Z-buffer bug
;             : Zarro (EITI/GSFC): 2001 Sept 1 -- added /PERCENT
;             : Kim: 2001 Sep 6 - don't draw limb for surface, shade_surf,
;                or show3
;             : Zarro (EITI/GSFC): 1 Dec 2001 - made /SQUARE the default
;             : Kim Tolbert: 24-Jan-2002 -- Added mark_point keyword
;             : Zarro (EITI/GSFC): 16 Mar 2002 - check for unnecessary call
;                to PLOT_HELIO when GRID/LIMB=0
;             : Zarro (LAC/GSFC): 8 Oct 2002 - fixed roll bug + added check
;                for subfields outside fov + better handling of off-limb
;                pixels during solar rotation
;             : Zarro (EER/GSFC): 4 Nov 2002 - added GCOLOR and LMCOLOR for
;                grid and limb colors
;             : Henney (SOLIS/NSO): 15 Nov 2002 - replaced black background
;                with !p.background
;             : A. Caspi (SSL): 16 Dec 2002 - added BCOLOR for border color
;             : Zarro (EER/GSFC), 17 Dec 2002 - added /NODATA option
;             : Zarro (EER/GSFC), 22 Dec 2002 - fixed yet another ROLL bug and a problem
;               with ZOOM. It just never ends.
;             : Zarro (EER/GSFC), 20 Jan 2003 - fixed bug with DMIN/DMAX
;               keywords being ignored.
;             : Zarro (EER/GSFC), 24 Feb 2003 - added BORDER option for
;               contour overlays
;             : Kim, 23-Apr-2003. Removed /xstyle, /ystyle from call to show3
;             : Kim, 22-May-2003. Added log keyword to plot_map_colorbar
;             : Kim, 10-June-2003. Added NEAREST keyword
;             : Zarro (L-3/GSFC), 23 November 2003 - added check for roll values
;               less than 1 degree. Treat these as effectively 0 degree roll.
;             : Zarro (L-3Com/GSFC), 13 January 2004 - fixed FOV/CENTER
;               keyword inconsistency.
;             : Kim, 23-Feb-2004, added lmthick keyword for limb thickness
;             : Zarro (L-3Com/GSFC), 24 January 2004 - fixed minor roll bug
;               and moved PLOT_HELIO keywords to _EXTRA
;             : Zarro (L-3Com/GSFC), 27 February 2004 - added XSHIFT/YSHIFT
;             : Zarro (L-3Com/GSFC), 24 March 2004 - fixed problem with
;               masking out pixels that spill outside plot window in PS plots
;             : Zarro (L-3Com/GSFC), 27 April 2004 - preserved bytescale range
;               when compositing images.
;             : Zarro (L-3Com/GSFC), 11 Jan 2005 - added check for b0 in map
;             : Metcalf, 24 Jan 2005 - use square brackets on arange
;               to avoid confusion with the arange function
;             : Kim, 17 Feb 2005, added durtitle keyword to put duration in title
;             : Zarro (L-3Com/GSFC) 17 July 2005 - added 
;               c_thick, c_style, c_colors, c_label for compatibility
;               with RSI
;             : Zarro (ADNET) 2 April 2007 - 
;               added DIMENSIONS keyword and check for bytescale tags
;               in map 
;             : Zarro (ADNET) 7 May 2007 - added DATA=DATA to pass in
;               external data array
;             : Zarro (ADNET) 25 July 2007 
;                - added check for solar radius RSUN in map structure
;             : Zarro (ADNET) 6 October 2007
;                - added checks for L0, B0 angles in map to support
;                  STEREO views
;             : Zarro (ADNET) 11 November 2007
;                - moved roll checks to drot_map
;               Zarro (ADNET) 23 November 2007
;                - added check for 2-d smoothing width input
;
; Contact     : dzarro@solar.stanford.edu
;-

pro plot_map_white,xb_dev,yb_dev,xr_dev,yr_dev,background=background

;-- utility routine to white-out "over the edge pixels" in postscript plots

 n_colors=!d.table_size
 white=n_colors-1
 if is_number(background) then white=  0 > nint(background) < (n_colors-1)
 left_x=min(xb_dev)
 right_x=max(xb_dev)
 sx=right_x-left_x+1.
 bot_y=min(yb_dev)
 top_y=max(yb_dev)
 sy=top_y-bot_y+1.
 white_out=bytarr(2,2)+byte(white)
 t1=left_x
 t2=min(xr_dev)
 if (t1 lt t2) and ((t2-t1) ge 1) then begin
  xsize=t2-t1 & ysize=sy
  tv,white_out,t1,yb_dev[0],xsize=xsize,ysize=ysize,/device
 endif
 t1=right_x
 t2=max(xr_dev)
 if (t1 gt t2) and ((t1-t2) ge 1) then begin
  xsize=t1-t2 & ysize=sy
  tv,white_out,t2+1,yb_dev[0],xsize=xsize,ysize=ysize,/device
 endif
 t1=bot_y
 t2=min(yr_dev)
 if (t1 lt t2) and ( (t2-t1) ge 1) then begin
  xsize=sx & ysize=t2-t1
  tv,white_out,xb_dev[0],t1,xsize=xsize,ysize=ysize,/device
 endif
 t1=top_y
 t2=max(yr_dev)
 if (t1 gt t2) and ( (t1-t2) ge 1) then begin
  xsize=sx & ysize=t1-t2
  tv,white_out,xb_dev[0],t2+1,xsize=xsize,ysize=ysize,/device
 endif
 return & end

;------------------------------------------------------------------------------------------

pro plot_map,map,index,contour=cont,overlay=overlay,smooth_width=smooth_width,border=border,$
 fov=fov,grid_spacing=grid_spacing,center=center,$
 tail=tail,log_scale=log_scale,notitle=notitle,title=title,$
 lcolor=lcolor,window=window,noaxes=noaxes,nolabels=nolabels,$
 xsize=xsize,ysize=ysize,new=new,levels=levels,$
 missing=missing,dmin=dmin,dmax=dmax,$
 top=top,quiet=quiet,square_scale=square_scale,$
 trans=trans,positive_only=positive_only,$
 negative_only=negative_only,dimensions=dimensions,$
 offset=offset,time=time,bottom=bottom,$
 cstyle=cstyle,cthick=cthick,date_only=date_only,nodate=nodate,$
 last_scale=last_scale,composite=composite,$
 interlace=interlace,xrange=xrange,yrange=yrange,$
 average=average,ncolors=ncolors,drange=drange,$
 limb_plot=limb_plot,truncate=truncate,$
 duration=duration,bthick=bthick,bcolor=bcolor,drotate=drotate,$
 multi=multi,noerase=noerase,_extra=extra,clabel=clabel,margin=margin, $
 status=status, err_msg=err_msg,xshift=xshift,yshift=yshift,$
 data=data

status = 1
err_msg = ''

error=0
catch,error
if error ne 0 then begin
 err_msg=!err_string
 message,err_msg,/cont
 catch,/cancel
 return
endif

;-- determine extra keywords

more_keywords = ['surface', 'shade_surf', 'ax_surf', 'az_surf', 'show3', 'nodata', $
                 'nearest','rescale_image', 'cbar', 'ccharsize','percent',$
                 'mark_point','original_time','mask_value',$
                 'no_mask','background','durtitle','c_label','c_thick','c_colors','c_style']

@extra_keywords

;-- some variables saved in memory for overlay

@plot_map_com

;-- color controls

zbuff=!d.name eq 'Z'
wbuff=(!d.name eq 'X') or (!d.name eq 'WIN')
post=(!d.name eq 'PS')
n_colors=!d.table_size
white=byte(n_colors-1) & black=0b

if not exist(bottom) then bottom=0 else bottom=bottom > 0
if not exist(top) then begin
 if exist(ncolors) then top=(bottom+ncolors-1L) else top=n_colors-1L
endif
top=top < (n_colors-1L)

if not exist(bthick) then bthick=1
if not exist(bcolor) then bcolor=white

;-- examine keywords

if is_number(rescale_image) then rescale_zoom=(0b > rescale_image < 1b) else $
 rescale_zoom=0b

nodata=keyword_set(nodata)              ;-- no data option
noaxes=keyword_set(noaxes)              ;-- no axes option
quiet=keyword_set(quiet)
loud=1-quiet
surf=keyword_set(surface)		;-- surface image
sh_surf=keyword_set(shade_surf) 	;-- shade_surf image
show3=keyword_set(show3)		;-- image, surface, contour
border=keyword_set(border)              ;-- plot image border
dlog=keyword_set(log_scale)             ;-- log scale image
dtrans=exist(trans) or exist(xshift) or exist(yshift)   ;-- translate image
if not exist(grid_spacing) then grid_spacing=0  ;-- no grid
limb_plot = keyword_set(limb_plot)
if surf or sh_surf or show3 then begin
 limb_plot = 0
 grid_spacing=0
endif

;-- always overlay as a contour unless /interlace, /composite, or cont=0, are set

if is_number(overlay) then over = (0 > overlay < 1) else over=0
if is_number(cont) then cont= (0 > cont < 1) else cont=over
if over and (not cont) then composite=2
if is_number(noerase) then noerase= (0 > noerase < 1) else noerase=over
take_average=keyword_set(average)
interlace=keyword_set(interlace)
if keyword_set(composite) then comptype=composite   ; intercept COMPTYPE
composite=keyword_set(composite)
if composite or interlace then begin
 over=1 & cont=0
endif

;-- open a new window if one doesn't exist
;-- else get viewport from previous plot

if (over eq 0) then begin

 if n_elements(dimensions) eq 2 then begin
  xsize=dimensions[0] & ysize=dimensions[1]
 endif
 get_xwin,window,xsize=xsize,ysize=ysize,new=new,err=err_msg
 if wbuff and (err_msg ne '') then return

endif else begin

 if exist(window) and exist(last_window) and (not zbuff) then begin
  if window ne last_window then begin
   err_msg='WARNING - overlaying on other than last plot window. Cannot guarantee coalignment'
   print,'---------------------------------------------------------------------'
   message,err_msg,/cont
   print,'% window, last_window: ',window,last_window
   print,'---------------------------------------------------------------------'
  endif
 endif
 ok=1

 if over then begin
  if not exist(last_window) then begin
   if not exist(window) then begin
    status = 0
    err_msg='No previous window on which to overlay'
    message,err_msg,/cont
    return
   endif else last_window=window
  endif
  smart_window,last_window,status=ok,draw=draw
 endif

 if not ok then begin
  if exist(last_window) then app=trim(string(last_window))
  status = 0
  err_msg='Could not open window '
  if exist(app) then err_msg=err_msg+app
  message,err_msg,/cont
  return
 endif

endelse

;-- overlay limb and/or grid

if over and limb_plot then begin
 if not exist(last_time) then begin
  err_msg='No previous image on which to overlay limb/grid'
  message,err_msg,/cont
  if not valid_map(map) then begin
   status = 0
   return
  endif
 endif
 plot_helio,last_time,roll=last_roll,grid_spacing=grid_spacing,$
  /over,soho=last_view,rcenter=last_rcenter,$
  limb_plot=limb_plot,b0=last_b0,l0=last_l0,rsun=last_rsun,_extra=extra
 if not valid_map(map) then return
endif

;-- check input map

if not exist(map) then begin
 status = 0
 err_msg = 'plot_map,map'
 pr_syntax,err_msg
 return
endif

if not valid_map(map) then begin
 status = 0
 err_msg='Invalid input image map'
 message,err_msg,/cont
 return
endif

nmaps=n_elements(map)
if not is_number(index) then index=0 else index= 0 > index < (nmaps-1)
if nmaps gt 1 then begin
 if valid_time(nearest) or valid_map(nearest) then begin
  if valid_time(nearest) then tnear=anytim2tai(nearest) else $
   tnear=anytim2tai(nearest.time)
  diff=abs(tnear-anytim2tai(map.time))
  chk= where(diff eq min(diff))
  index=chk[0]
  message,'plotting nearest map at '+map[index].time,/cont
 endif else begin
  err = 'input map is an array, plotting index '+trim(index)
  message, err, /cont
 endelse
endif

;-- keep track of plot location for multi-page plots
;-- clear page if !p.multi changed

if exist(multi) then begin
 pmulti=[multi[0],multi[n_elements(multi)-1]]
 !p.multi[[1,2]]=pmulti
endif
pnx=!p.multi[1]
pny=!p.multi[2]
if n_elements(last_multi) lt 3 then last_multi=!p.multi
if (last_multi[1] ne pnx) or (last_multi[2] ne pny) then begin
 erase & !p.multi[0]=0
endif

;-- go to previous image if an overlay

sp=!p.multi[0]
if over then begin
 !p.multi[0]=(!p.multi[0]+1)
 if !p.multi[0] gt pnx*pny then !p.multi[0]=0
 sp=!p.multi[0]
endif

;-- deduce image center
;   (start with that of image, then FOV keyword, then CENTER keyword)

icenter=get_map_center(map[index])
dcenter=icenter
if valid_map(fov) then dcenter=get_map_center(fov)
if exist(center) then begin
 if valid_map(center) then dcenter=get_map_center(center) else begin
  if valid_range(center,/allow) then dcenter=float(center)
 endelse
endif

;-- deduce actual ranges
;   (start with image, then FOV, then XRANGE/YRANGE keywords)

ixrange=get_map_xrange(map[index])
iyrange=get_map_yrange(map[index])
dx=map[index].dx
dy=map[index].dy
dx2=dx/2. & dy2=dy/2.

def_xmin=min(ixrange)-dx2
def_xmax=max(ixrange)+dx2
def_ymin=min(iyrange)-dy2
def_ymax=max(iyrange)+dy2
dxrange=[def_xmin,def_xmax]
dyrange=[def_ymin,def_ymax]
oxrange=dxrange
oyrange=dyrange

if exist(fov) then begin
 if valid_map(fov) then dfov=get_map_fov(fov) else begin
  nfov=n_elements(fov)
  dfov=60.*float([fov[0],fov[nfov-1]])
 endelse
 half_fov=dfov/2.
 dxrange=[dcenter[0]-half_fov[0],dcenter[0]+half_fov[0]]
 dyrange=[dcenter[1]-half_fov[1],dcenter[1]+half_fov[1]]
endif

if exist(center) and (not exist(fov)) then begin
 dxrange=dxrange+dcenter[0]-icenter[0]
 dyrange=dyrange+dcenter[1]-icenter[1]
endif

if valid_range(xrange) then dxrange=float(xrange)
if valid_range(yrange) then dyrange=float(yrange)

;-- if overlaying, match with previous viewport

if over then begin
 if valid_range(last_xrange) then begin
  dxrange[0]=last_xrange[0] > dxrange[0]
  dxrange[1]=last_xrange[1] < dxrange[1]
 endif
 if valid_range(last_yrange) then begin
  dyrange[0]=last_yrange[0] > dyrange[0]
  dyrange[1]=last_yrange[1] < dyrange[1]
 endif
endif

;-- def viewport

xmin=min(dxrange) & xmax=max(dxrange)
ymin=min(dyrange) & ymax=max(dyrange)

if (xmin eq xmax) or (ymin eq ymax) then begin
 status = 0
 err_msg='Plot scale MIN/MAX must differ'
 message,err_msg,/cont
 goto,clean_up
endif

;-- get some map properties

curr_view=get_map_prop(map[index],/soho,def=0b)
curr_roll=get_map_prop(map[index],/roll_angle,def=0.)
curr_rcenter=get_map_prop(map[index],/roll_center,def=icenter)

;-- if solar rotating, check that we are rotating relative to last time image
;   rotated

atime=get_map_time(map[index],/tai)
rtime=atime

;-- plot labels

otime=get_map_time(map[index],/tai,/original)
mtitle=get_map_prop(map[index],/id,def='')
if (over eq 0) then begin
 err=''
 mtime=rtime
 if keyword_set(original_time) then mtime=otime
 if keyword_set(durtitle) then begin
   s_time = anytim (anytim2utc(mtime),/yohkoh,err=err,/truncate)
   if err eq '' then mtitle = mtitle+' '+s_time
   e_time = anytim (anytim2utc(mtime+get_map_prop(map[index],/dur)),/yohkoh,/time_only,/truncate,err=err)
   if err eq '' then mtitle=mtitle+'-'+e_time
 endif else begin
   date_obs=anytim2utc(mtime,/vms,err=err,time_only=keyword_set(nodate),$
    date_only=date_only,truncate=truncate)
   if (1-keyword_set(date_only)) then field=' UT' else field=''
   if err eq '' then mtitle=mtitle+' '+date_obs+field
 endelse
endif
mtitle=trim(mtitle)
if is_string(title,/blank) then mtitle=title
if keyword_set(notitle) then mtitle=''

;-- check if translating

xoff=0. & yoff=0.
if dtrans then begin
 ntrans=n_elements(trans)
 if (ntrans eq 1) or (ntrans eq 2) then xoff=trans[0]
 if (ntrans eq 2) then yoff=trans[1]
 if exist(xshift) then xoff=xshift
 if exist(yshift) then yoff=yshift
endif

;-- don't extract sub-region if contouring, since contour procedure
;   takes care of it via CLIP keyword
;-- check if external data array is passed on command-line

ext_data=same_size(data,map[index].data)
if cont or surf or sh_surf or show3 then begin
 if ext_data then pic=data else pic=map[index].data
endif else begin
 pic=get_map_sub(map[index],xrange=dxrange-xoff,yrange=dyrange-yoff,arange=arange,$
                 count=vcount,err=err_msg,irange=irange)
 if vcount eq 0 then nodata=1b else begin
  if ext_data then pic=data[irange[0]:irange[1],irange[2]:irange[3]]
 endelse
endelse

if (err_msg ne '') then begin
 status = 0
 goto,clean_up
endif

;-- plot axes & viewport
;-- try to preserve aspect ratio (won't work if multi is set)

@plot_map_aspect

;-- make an empty plot to establish scaling

units=get_map_prop(map[index],/units,def='arcsecs')
units='('+units+')'
xunits=units & yunits=units
if tag_exist(map[index],'xunits') then xunits='('+(map[index]).xunits+')'
if tag_exist(map[index],'yunits') then yunits='('+(map[index]).yunits+')'
xtitle='X '+xunits & ytitle='Y '+yunits
if keyword_set(nolabels) then begin
 xtitle='' & ytitle=''
endif

if (over eq 0) then begin
 if have_tag(extra,'noxt') then begin
  xticks=replicate(' ',n_elements(!x.tickname)-1)
  extra=rep_tag_value(extra,xticks,'xtickname')
 endif
 if have_tag(extra,'noyt') then begin
  yticks=replicate(' ',n_elements(!y.tickname)-1)
  extra=rep_tag_value(extra,yticks,'ytickname')
 endif
 !p.multi[0]=sp
 plot,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/data, $
  xstyle=5,ystyle=5,noerase=noerase,/nodata,xrange=dxrange,yrange=dyrange,$
  _extra=extra
 smart_window,window,/set,draw=draw
endif

;-- check for angles in map

ang=float(pb0r(atime,/arcsec,soho=curr_view))
b0=ang[1]
l0=0.
rsun=ang[2]
if have_tag(map,'b0') then b0=map[index].b0
if have_tag(map,'l0') then l0=map[index].l0
if have_tag(map,'rsun') then rsun=map[index].rsun

;-- if contouring and not overlaying, then check for roll

if cont or over then begin
 ilimb=-1 & olimb=-1
 if cont and (over eq 0) then begin
  dum=drot_map(map[index],err=err_msg,$
               xp=xp,yp=yp,/no_data,_extra=extra,trans=[xoff,yoff])
 endif else begin
  dum=drot_map(map[index],tend=last_time,_extra=extra,trans=[xoff,yoff],$
      b0=last_b0,l0=last_l0,rsun=last_rsun,roll=last_roll,olimb=olimb,$
      rcenter=last_rcenter,err=err_msg,xp=xp,yp=yp,/no_data,ilimb=ilimb)
 endelse
 if err_msg ne '' then begin
  status = 0
  goto,clean_up
 endif
 in_view=where( (xp le max(dxrange)) and (xp ge min(dxrange)) and $
                (yp le max(dyrange)) and (yp ge min(dyrange)), vcount)
endif

;-- get data limits

if cont or surf or sh_surf or show3 then begin
 min_x=min(xp,max=max_x) 
 min_y=min(yp,max=max_y)
endif else begin
 arange[0:1]=arange[0:1]+xoff
 arange[2:3]=arange[2:3]+yoff
 min_x=arange[0] & max_x=arange[1]
 min_y=arange[2] & max_y=arange[3]
endelse

;-- define outer edges of extracted data
;   (for heaven's sake don't roundoff here)

emin_x=(min_x-dx2)
emax_x=(max_x+dx2)
emin_y=(min_y-dy2)
emax_y=(max_y+dy2)
xedge = [emin_x,emax_x,emax_x,emin_x,emin_x]
yedge = [emin_y,emin_y,emax_y,emax_y,emin_y]

;-- make empty plot if no data

if nodata then goto,done

;-- smoothing?

if exist(smooth_width) then begin
 smo=smooth_width[0]
 if n_elements(smooth_width) gt 1 then smo=[smo,smooth_width[1]]
 if max(smo) gt 1 then pic=smooth(temporary(pic),smo,/edge,/nan)
endif

;-- get data value limits
;-- start with actual data, then dmin/dmax keywords, then last scale, and
;   finally drange.

zoom= (min(dxrange) ne min(oxrange)) or (min(dyrange) ne min(oyrange)) or $
      (max(dxrange) ne max(oxrange)) or (max(dyrange) ne max(oyrange))

dprint,'% zoom, rescale_zoom: ',zoom,rescale_zoom

omin=min(pic,max=omax,/nan)
zmin=omin & zmax=omax
if exist(in_view) and (vcount gt 0) then $
 zmin=min(pic[in_view],max=zmax,/nan) 

if rescale_zoom then prange=[zmin,zmax] else prange=[omin,omax]

if exist(dmin) then prange[0]=dmin
if exist(dmax) then prange[1]=dmax

if exist(last_scale) then begin
 if valid_map(last_scale) then prange=get_map_drange(last_scale) else begin
  if valid_range(last_drange) then prange=last_drange
  if dlog then if valid_range(last_lrange) then prange=last_lrange
 endelse
endif

if exist(drange) then begin
 if valid_map(drange) then prange=get_map_drange(drange) else $
  if valid_range(drange) then prange=float(drange)
endif

if (min(prange) eq max(prange)) then prange=[0,0]

if n_elements(positive_only) eq 1 then if (positive_only[0] ne 0) then begin
 prange[0]=0 & missing=0
endif

if keyword_set(negative_only) then begin
 prange[1]=0 & missing=0
endif

;-- send limb points to infinity

if cont or surf or sh_surf then begin
 !p.multi[0]=sp
 if ilimb[0] ne -1 then begin
  xp[ilimb]=100*max(dxrange)
  yp[ilimb]=100*max(dyrange)
  pic[ilimb]=100*max(pic)
 endif
 if olimb[0] ne -1 then begin
  xp[olimb]=100*max(dxrange)
  yp[olimb]=100*max(dyrange)
  pic[olimb]=100*max(pic)
 endif

;-- set up and plot contours

@plot_map_cont

;-- plot surface plot

 if surf then begin
  font_sav = !p.font
  if post then if !p.font eq 0 then !p.font = -1
  surface, pic, xp, yp, xrange=dxrange, yrange=dyrange, $
 	  xtitle=xtitle, ytitle=ytitle, /xstyle,/ystyle, $
 	  ax=ax_surf, az=az_surf, zlog=dlog,$
          noclip=0,$
          max_value=corange[1],min_value=corange[0], _extra=extra
  xyouts, .5, .95, mtitle, align=.5, /norm, _extra=extra
  !p.font = font_sav
 endif

;-- plot shaded surface

 if sh_surf then begin
  background_sav = !p.background
  font_sav = !p.font
  if post then begin
   if !p.background eq 0 then !p.background=white
   if !p.font eq 0 then !p.font = -1
  endif
  shade_surf, pic, xp, yp, xrange=dxrange, yrange=dyrange, $
 	      xtitle=xtitle, ytitle=ytitle, /xstyle,/ystyle,$
 	      ax=ax_surf, az=az_surf, zlog=dlog, $
              max_value=corange[1],min_value=corange[0],_extra=extra
  xyouts, .5, .95, mtitle, align=.5, /norm, _extra=extra
  !p.background = background_sav
  !p.font = font_sav
 endif

;-- plot image

endif else begin

;-- bail out if trying to image at the sub-pixel level

 diff_x=(max(dxrange)-min(dxrange))
 diff_y=(max(dyrange)-min(dyrange))
 if (zbuff or wbuff) then begin
  if (diff_x lt dx2) or (diff_y lt dy2) then begin
   status = 0
   err_msg='cannot display below half pixel resolution limit'
   message,err_msg,/cont
   return
  endif
 endif

;-- outer device pixels of extracted data

 xb_dev=float(!d.x_size)*(float(!x.s[0])+float(!x.s[1])*xedge)
 yb_dev=float(!d.y_size)*(float(!y.s[0])+float(!y.s[1])*yedge)

;-- dimensions of scaled image

 sx=(abs(max(xb_dev)-min(xb_dev))+1.) > 1.
 sy=(abs(max(yb_dev)-min(yb_dev))+1.) > 1.

;-- device pixels of range window

 xr_dev=float(!d.x_size)*(float(!x.s[0])+float(!x.s[1])*dxrange)
 yr_dev=float(!d.y_size)*(float(!y.s[0])+float(!y.s[1])*dyrange)

 xscale=diff_x/(max(xr_dev)-min(xr_dev))
 yscale=diff_y/(max(yr_dev)-min(yr_dev))

;-- rebin image for X-windows or Z-buffer (!d.name = 'X' or 'Z')
;-- or plot in Postscript using scalable pixels (!d.name = 'PS')

 if have_tag(map,'bottom') and have_tag(map,'top') then begin
  bottom=map[index].bottom & top=map[index].top
  crange=[bottom,top] & prange=crange
 endif else begin
  pic=cscale(pic,top=top,bottom=bottom,drange=prange,/no_copy,$
            missing=missing,err=err_msg,log=dlog,crange=crange)
  if err_msg ne '' then begin
   status = 0
   goto,done
  endif
  if dlog then prange=10.^crange else prange=crange
 endelse
 brange=[bottom,top]

;-- check if composite/interlace is requested

 !p.multi[0]=sp

 if zbuff or wbuff then begin
  words=data_chk((map[index]).data,/type) ne 1         ; boolean for tv/tvrd
  true=!d.n_colors gt 256

  pic=congrid(temporary(pic),sx,sy)
  sx=(size(pic))[1]
  sy=(size(pic))[2]

;-- set pixels outside plot window range to black

  xpic=xb_dev[0]+findgen(sx)
  xleft=where(xpic lt xr_dev[0],lcount)
  xright=where(xpic gt xr_dev[1],rcount)

  ypic=yb_dev[0]+findgen(sy)
  ybot=where(ypic lt yr_dev[0],bcount)
  ytop=where(ypic gt yr_dev[1],tcount)

  if lcount gt 0 then pic[xleft[0]:xleft[lcount-1],*]= !p.background
  if rcount gt 0 then pic[xright[0]:xright[rcount-1],*]= !p.background
  if bcount gt 0 then pic[*,ybot[0]:ybot[bcount-1],*]= !p.background
  if tcount gt 0 then pic[*,ytop[0]:ytop[tcount-1],*]= !p.background

  if (composite or interlace) and over then begin
   ok=(xb_dev[0] gt 0) and (xb_dev[0] lt !d.x_size) and $
      (yb_dev[0] gt 0) and (yb_dev[0] lt !d.y_size) and $
      (xb_dev[0]+sx lt !d.x_size) and $
      (yb_dev[0]+sy lt !d.y_size)

;-- just create a blank bytarr if underlying image cannot be read

   if ok then begin
    if zbuff then $
     base=tvrd(xb_dev[0],yb_dev[0],sx,sy,channel=words,words=words) else $
      base=tvrd(xb_dev[0],yb_dev[0],sx,sy)
   endif else base=bytarr(sx,sy)
   base=cscale(base,top=last_top,bottom=last_bottom,drange=brange,/no_copy)

;-- combine underlying and overlaying images

   if interlace then begin
    base=swiss_cheese(base,last_bottom,/shift,/no_copy)
    pic=swiss_cheese(pic,bottom,/no_copy)
    pic=temporary(base)+temporary(pic)
   endif else begin                             ; COMPOSITE set
    case comptype of                            ; setable option
       1: begin                                 ; backwardly compatible
	   bfac=([1,2])[take_average]
           pic=temporary(base)/bfac+temporary(pic)/bfac
          end
       3: begin
           pic=temporary(base) > temporary(pic)        ; 'largest'  pixel
          end
       4: pic=temporary(base) < temporary(pic)        ; 'smallest' pixel
       else: do_nothing=1
    endcase
   endelse
  endif
 endif

;-- call TV
;   SX and SY used only in postscript mode

 if show3 then begin
  	xarr = xmin + indgen(sx) * (xmax-xmin) / sx
  	yarr = ymin + indgen(sy) * (ymax-ymin) / sy
  	e_contour = {xrange: dxrange, yrange: dyrange, _extra: extra}
 	e_surface = {xrange: dxrange, yrange: dyrange, $
 		ax: ax_surf, az: az_surf, zlog: dlog, _extra: extra}
 endif

if wbuff then begin
  if show3 then begin
	show3, pic, xarr, yarr, sscale=sx/50>1, top=top, bottom=bottom, $
		e_contour=e_contour, e_surface=e_surface
  endif else tv,pic,xb_dev[0],yb_dev[0],/device
 endif

;-- if Postscript, we literally "white-out" pixels outside viewport

 if post then begin
  if show3 then begin
    status = 0
    err_msg = "Can't make PostScript file for show3 plot."
    message, err_msg, /cont
    return
  endif else tv,pic,xb_dev[0],yb_dev[0],xsize=sx,ysize=sy,/device
   if (1-keyword_set(no_mask)) then begin
    if is_number(mask_value) then background=mask_value
    plot_map_white,xb_dev,yb_dev,xr_dev,yr_dev,background=background
   endif
 endif

;-- if Z-buffer, we have to physically avoid pixels outside viewport

 if zbuff then begin
  inx=where( (xpic le !d.x_size) and (xpic ge 0) ,xcount)
  iny=where( (ypic le !d.y_size) and (ypic ge 0) ,ycount)
  if (xcount eq 0) or (ycount eq 0) then begin
   status = 0
   err_msg='image overflows Z-buffer'
   message,err_msg,/cont
   goto,clean_up
  endif
  pic=temporary(pic[inx[0]:inx[xcount-1],iny[0]:iny[ycount-1]])
  ux=xpic[inx[0]]
  uy=ypic[iny[0]]
  dprint,'% ux, uy',ux,uy
  if show3 then begin
  	show3, pic, xarr, yarr, sscale=sx/50>1, top=top, bottom=bottom, $
		e_contour=e_contour, e_surface=e_surface
  endif else tv,pic,ux,uy,/device
 endif

 if since_version('5.2') then $
 	if keyword_set(cbar) then plot_map_colorbar, prange, bottom, ncolors, log=dlog, _extra=extra
endelse

done:

;-- plot axes and labels

if (over eq 0 and not (surf or sh_surf or show3) ) then begin
 !p.multi[0]=sp
 plot,[xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],/data,noclip=0, $
  xstyle=([1,5])[noaxes],ystyle=([1,5])[noaxes],/noeras,/nodata,xrange=dxrange,$
  yrange=dyrange,xtitle=xtitle,ytitle=ytitle,title=mtitle,$
  _extra=extra,$
  clip=[dxrange[0],dyrange[1],dxrange[1],dyrange[1]]
endif

;-- overlay a solar latitude-longitude grid

grid_limb=keyword_set(grid_spacing) or limb_plot
if grid_limb and ((over eq 0) or composite or interlace) then begin
 !p.multi[0]=sp
 plot_helio,atime,roll=curr_roll,grid_spacing=grid_spacing,$
  /over,soho=curr_view,rcenter=curr_rcenter,$
  limb_plot=limb_plot,b0=b0,l0=l0,rsun=rsun,_extra=extra
endif

;-- mark point

mark_point,mark_point

;-- plot border edges

if border then begin
 !p.multi[0]=sp
 oplot,xedge,yedge,thick=bthick,color=bcolor
endif

;-- save last settings

if (over eq 0) then begin
 if exist(window) then last_window=window
 if exist(prange) then begin
  last_drange=prange
  if dlog then last_lrange=prange
 endif
 if exist(dxrange) then last_xrange=dxrange
 if exist(dyrange) then last_yrange=dyrange
 if exist(rtime) then last_time=rtime else last_time=atime
 if exist(top) then last_top=top
 if exist(bottom) then last_bottom=bottom
 if exist(curr_view) then last_view=curr_view
 if exist(curr_roll) then last_roll=curr_roll
 if exist(curr_rcenter) then last_rcenter=curr_rcenter
 if exist(b0) then last_b0=b0
 if exist(l0) then last_l0=l0
 if exist(rsun) then last_rsun=rsun
endif

!p.multi[0]=(!p.multi[0]-1)
if !p.multi[0] lt 0 then !p.multi[0]=(pnx*pny-1)
last_multi=!p.multi

clean_up:

if nodata then message,'No data to display',/cont

delvarx,xp,yp,pic,xpic,ypic

return & end
