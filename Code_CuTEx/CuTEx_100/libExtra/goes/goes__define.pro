;+
; Project     : HESSI
;
; Name        : GOES__DEFINE
;
; Purpose     : Define a GOES lightcurve object
;
; Category    : synoptic objects
;
; Explanation : Object to read GOES data, and return or plot the two flux channels or
;               the temperature and emission measure.  Reads either the SDAC or the
;               YOHKOH archive of GOES data.  Can be used from the command line (via
;               set, get, getdata, plot, plotman methods), or from a gui (o->gui).
;
; Syntax      : IDL> new=obj_new('goes')
;
; History     : Written 31 Dec 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;
; Modifications:
;   24-Sep-2003, Zarro (GSI/GSFC) - fixed timerange plotting bug
;   21-Oct-2003, Zarro (GSI/GSFC) - fixed timeunits bug
;   17-Nov-2005, Kim Tolbert, lots of changes to enable the following:
;     1. subtracting background
;     2. cleaning glitches out of data
;     3. using either SDAC or YOHKOH archive of GOES data
;     4. calculating temperature and emission measure
;     5. adding plotman capability
;     6. adding a gui interface (in goes__gui.pro).  Access via o->gui.
;   7-Dec-2005, Kim.  In getdata, if timerange is not passed in, use
;     timerange=[tstart,tend] in case tstart,tend is a subset of data read in
;     last read operation.
;   21-Dec-2005, Kim.  Added a /yohkoh switch.  Same as sdac=0.
;   23-Dec-2005, Kim.  Speed up getdata. Also use current plot for select_background.
;     And made yohkoh the default.
;   23-Dec-2005, Zarro. Added a few calls to 'temporary'
;   9-Jan-2006, Kim.  Modified display_message to output to widget only when GUI is running
;   9-Jan-2006, Kim.  Added bkfunc and bk properties.  Got rid of avback.  Background
;     is now computed by fitting an exponential or a polynomial of order 0,1,2,or 3 to
;     the data in the background intervals, and is stored as an array vs time, not 2 numbers.
;     Also added option to compute and plot the total energy loss in flare, and the integral.
;   11-Jan-2006, Kim.  Fix disabling integration times (setting itimes to -1)
;   12-Jan-2006, Kim.  Added ps keyword to plot method to generate PostScript file
;   17-Jan-2006, Kim.  Changed update checking method.  Instead of setting an update flag
;     in set method as params are getting set, check just before new plot or getdata whether we
;     need to reread data.  This requires saving 5 params that show what was read in last time,
;     and new need_update method.
;     Got rid of search, verbose, and update properties.  Replaced get_sat method with sat2index
;     method - covers more types of input.
;   18-Jan-2006, Zarro, reconciled _ref_extr
;   20-Jan-2006, Kim.  Added lx stuff (X-ray energy loss), and added /times keyword to getdata
;   20-Jun-2006, Kim.  Times input as seconds are interpreted as UTC/TAI (rel to 1958).  Fixed
;     inconsistencies so this is true for btimes and itimes too.  Always use get(/btimes) instead
;     of self.btimes (similarly for itimes).  All times are stored in properties as TAI seconds,
;     and are converted to vms ascii format by the get method.  Note that internally, in some
;     cases, ascii times may be temporarily converted to sec since 1979, e.g.  for
;     compatibility with plotman.
;    20-Aug-2006, Zarro (ADNET/GSFC) - added check for GOES FITS files
;                                     in $GOES_FITS
;    8-Sep-2006, Kim.  Added LX (X-ray energy loss rate) to items saved in save file.
;    7-Nov-2006, Zarro (ADNET/GSFC)
;     - moved check for valid $GOES_FITS directory to RD_GOES_SDAC
;    16-Nov-2006, Zarro (ADNET/GSFC) - removed recursive read call in ::READ
;    11-Feb-2007, Zarro (ADNET/GSFC) - added .lremote property to
;                                      re-read when switching between remote=0 or 1
;    19-Apr-2007, Kim - added goes 11 to sat_times
;     6-May-2007, Zarro - fixed passing of sat_num to RD_GOES and RD_GOES_SDAC
;    12-Nov-2007, Kim. Added derivative of flux to items that can be plotted (not returned
;      separately from getdata since can be computed so easily).
;-
;---------------------------------------------------------------------------

function goes::init,_ref_extra=extra

if not self->utplot::init(_extra=extra) then return,0

if is_string(extra) then self -> set,_extra=extra

;-- default to start of current day

self->def_times,tstart,tend

self->set,tstart=tstart,tend=tend

recompile,'sock_goes' ; contains socket-capable version of rd_week_file
self.gdata=ptr_new(/all)
self.numstat = -1
self.tstat=ptr_new(/all)
self.stat=ptr_new(/all)
self.sdac = 0b
self.clean = 1b
self.markbad = 1b
self.showclass = 1b
self.bsub = 0b
self.btimes=ptr_new(/all)
self.bfunc='0poly'
self.bfunc_options = ['0Poly', '1Poly', '2Poly', '3Poly', 'Exp']
self.itimes=[-1.d0,-1.d0]
self.abund_options = ['Coronal', 'Photospheric', 'Meyer']
self.abund = 0

return,1

end

;------------------------------------------------------------------------------
;-- flush out old temp files

pro goes::flush,days

if not is_number(days) then return

old_files=file_since(older=days,patt='g*',count=count,path=goes_temp_dir())

if count gt 0 then file_delete,old_files,/quiet

return & end

;-----------------------------------------------------------------------------
pro goes::cleanup

message,'cleaning up...',/cont
ptr_free,self.gdata

self->flush,10
self->utplot::cleanup

return
end

;----------------------------------------------------------------------------
;-- trap cases of missing GOES databases or network

function goes::allow_goes,err=err

; if can get to GOES directory, then that's all we need
if self->have_goes_dir() then return,1b

; otherwise, we'll need socket capability to copy data over network
if not allow_sockets(err=err) then return,0b

return,1b

end

;---------------------------------------------------------------------------
;-- default start and end times for GOES plot

pro goes::def_times,tstart,tend

get_utc,tstart
tstart.mjd = tstart.mjd-2
tstart.time=0
tend=tstart
tend.mjd=tend.mjd+1

tstart=anytim2tai(tstart)
tend=anytim2tai(tend)

return & end

;---------------------------------------------------------------------------
;-- check whether local sdac or yohkoh data directories exist

function goes::have_goes_dir

if self->get(/sdac) then return,is_dir('$GOES_FITS') else return,is_dir('$DIR_GEN_G81')

end

;----------------------------------------------------------------------------
;-- send progress message to user

pro goes::output,sdac=sdac

if self->have_goes_dir() then return

server=goes_server(network=network, sdac=sdac)
if network then $
 output='Please wait. Searching archives across network...' else $
  output='Network connection currently unavailable. Checking cached lightcurves...'

message,output,/cont

return & end

;---------------------------------------------------------------------------
; getdata function returns requested data or structure with everything
; timerange - time interval (subset of full interval) to return data for
; temperature - if set, return temperature
; emission - if set, return emission measure
; struct - if set, then return structure with flux, temp, em, and everything
; quick_struct - if set, then return structure with just the items that are set
;    by parameters

function goes::getdata, timerange=timerange, $
   times=times, $
   temperature=temperature, emission=emission, $
   lrad=lrad, lx=lx, integrate=integrate, bk_overlay=bk_overlay, $
   low=low, high=high, $
   struct=struct, quick_struct=quick_struct, err=err, _extra=extra

if keyword_set(extra) then self -> set, _extra=extra

self -> read, _extra=extra, err=err
if err ne '' then return, -1

do_tem = keyword_set(temperature) or keyword_set(emission) or keyword_set(struct) or $
	keyword_set(lrad)

; ydata and tarray will be the full data and time array from the last read operation

ydata = self->get(/data)
tarray = self->get(/times)
utbase_ascii = self->get(/utbase)
utbase = anytim(utbase_ascii)

; tarray may contain a bigger interval than the currently set tstart/tend, so
; need to get the subset of data. If timerange passed in, use that interval
; instead of tstart/tend

checkvar, timerange, [self->get(/tstart), self->get(/tend)]
if valid_range(timerange,/time) then begin
   trstart = anytim(timerange[0]) - utbase
   trend =   anytim(timerange[1]) - utbase
   if long(trstart) ne min(tarray,max=maxt) or long(trend) ne maxt then begin
	   chk=where( (tarray le anytim(trend)) and (tarray ge anytim(trstart)),count)
	   if count lt 2 then begin
	      err='No lightcurve data during specified times'
	      message,err,/cont
	      return, -1
	   endif
	   if (count gt 0) then begin
	      tarray=tarray[chk]
	      ydata=ydata[chk, *]
	   endif
   endif
endif

if keyword_set(times) then return, tarray

sat_num = goes_sat(self.sat,/num)

yclean = -1
bad0 = -1
bad1 = -1
tem = -1
em = -1
rad_loss = -1
rad_loss_x = -1
integrate_times = [-1.d0,-1.d0]

; Call clean even if clean flag isn't set when markbad is selected so  we'll
; have bad0 and bad1 (stat variable are only defined for sdac data, but clean_goes
; will find glitches in both yohkoh and sdac data.) Also call clean if struct
; is set, because then we want everything.

do_clean = self.clean or self.markbad or keyword_set(struct)

; yes_clean will be true if actually did clean.  Even if requested, may get error.
yes_clean = 0

if do_clean and n_elements(tarray) gt 5 then begin
  clean_goes, tarray = tarray, yarray = ydata, $
     yclean = yclean, bad0 = bad0, bad1 = bad1, numstat=self.numstat, $
     satellite=sat_num, tstat=*self.tstat, stat=*self.stat, error=error
  if error then begin
     ;only print this message if user actually asked for clean data.
     if self.clean then message, 'Error in clean_goes. Using non-cleaned data.',/cont
  endif else begin
     yes_clean = 1
  endelse
endif

yes_clean = yes_clean and self.clean

yuse = yes_clean ? yclean : ydata

; yes_bsub will be true if actually subtracted bk.  Even if requested, may get error.
yes_bsub = 0
ybsub = -1
bk = -1
if self.bsub or keyword_set(bk_overlay) then bk = self -> calc_bk(tarray+utbase, yuse)
if self.bsub then begin
	if bk[0] ne -1. then begin
		if n_elements(bk) eq 2 then begin
			yuse[*,0] = yuse[*,0] - bk[0]
			yuse[*,1] = yuse[*,1] - bk[1]
		endif else yuse = yuse - bk
		ybsub = yuse
		yes_bsub = 1
	endif
endif

if keyword_set(low) then return, yuse[*,0]
if keyword_set(high) then return, yuse[*,1]

; Now yuse incorporates clean and bsub changes if requested and possible

if do_tem then begin
   ; pass in unbackground-subtracted data (cleaned if cleaning was successful)
   goes_tem, tarray=tarray, yclean=(yes_clean? yclean : ydata), tempr=tem, emis=em, $
      savesat=sat_num, date=utbase_ascii, bkarray=bk, abund=self.abund

   if keyword_set(temperature) then yuse = tem
   if keyword_set(emission)  then yuse = em

endif

if keyword_set(integrate) then begin
	ind = [0,n_elements(tarray)-1]
	if self.itimes[0] ne -1.d0 then begin
		itimes = anytim(self->get(/itimes))
		istart = itimes[0]-utbase > min(tarray) < max(tarray)
		iend = itimes[1]-utbase < max(tarray)
		if istart ge iend then print, 'Invalid integration times.  Using full range.' else $
			ind = value_locate(tarray, [istart,iend])
	endif
	integrate_times = anytim(tarray[ind]+utbase,/vms)
endif

if keyword_set(lrad) or keyword_set(struct) then begin
	rad_loss = calc_rad_loss(em, tem*1.e6)
	if keyword_set(integrate) then begin
		if ind[0] gt 0 then rad_loss[0:ind[0]-1] = 0.
		if ind[1] lt n_elements(tarray)-1 then rad_loss[ind[1]:*] = 0.
		rad_loss = total(rad_loss,/cumulative)
	endif
endif

if keyword_set(lx) or keyword_set(lrad) or keyword_set(struct) then begin
	rad_loss_x = goes_lx(yes_bsub ? ybsub : yclean)
	if keyword_set(integrate) then begin
		if ind[0] gt 0 then rad_loss_x[0:ind[0]-1] = 0.
		if ind[1] lt n_elements(tarray)-1 then rad_loss_x[ind[1]:*] = 0.
		rad_loss_x = total(rad_loss_x, /cumulative)
	endif
endif

if keyword_set(struct) or keyword_set(quick_struct) then begin
   return, { $	;return everything for flux data
      utbase: utbase_ascii, $
      tarray: tarray, $
      ydata: ydata, $
      yclean: yclean, $
      ybsub: ybsub, $
      bk: bk, $
      bad0: bad0, $
      bad1: bad1, $
      tem: tem, $
      em: em, $
      lrad: rad_loss, $
      lx: rad_loss_x, $
      integrate_times: integrate_times, $
      yes_clean: yes_clean, $
      yes_bsub: yes_bsub }
endif else begin
   if keyword_set(temperature) then return, tem
   if keyword_set(emission)  then return, em
   if keyword_set(lrad) then return, rad_loss
   if keyword_set(lx) then return, rad_loss_x
   return, yuse
endelse

end

;---------------------------------------------------------------------------
; Function to calculate background
; Fit all points in all background intervals to an exponential or a polynomial of
; order 0, 1, 2, or 3.  For exponential, use polynomial of order 1 with log of data, and
; take exponential of that.
; bfunc will have values that are defined in self.bfunc_options
; ybk returned will be an array with values for each time for both channels: (ntime,2)
function goes::calc_bk, times, yuse

ybk = -1

if self->valid_btimes() then begin

	bck_intervals=anytim(self->get(/btimes))
	nbk = n_elements(bck_intervals) / 2

	; gather indices of all points in all background time intervals
	ind = -1
	for i=0, nbk-1 do $
	   ind = [ind, where ((times ge bck_intervals[0,i]) and (times le bck_intervals[1,i]))]
	ind = get_uniq(ind)	;eliminate overlap, and multiple -1's if where returned none

	if n_elements(ind) gt 1 then begin
	   ind = ind[1:*]   ; get rid of leading -1
	   ny = n_elements(yuse[*,0])
	   ybk = make_array(size=size(yuse))
	   ; default to 1st order polynomial.  Otherwise get order from first character of string.
	   ; but if doing exponential, order will be 1.
	   order=1
	   do_poly = not stregex(self.bfunc, 'exp', /boolean, /fold_case)
	   if do_poly and stregex(self.bfunc, '^[0-3]', /boolean) then order = fix(strmid(self.bfunc,0,1))

	   if do_poly then begin
	      ; polynomial fit
	 	  for ich=0,1 do ybk(*,ich) = fit_backgrnd (times-min(times), yuse[*,ich], $
	   		                       		fltarr(ny), order, selected=ind, ltime=fltarr(ny)+1.)
	   endif else begin
	   	; exponential fit uses exponential of 1st order polynomial fit of log of data
	 	  for ich=0,1 do ybk(*,ich) = exp( fit_backgrnd (times-min(times), alog(yuse[*,ich]), $
	   		                       		fltarr(ny), order, selected=ind, ltime=fltarr(ny)+1.) )
	   endelse

	endif else begin
		message,'No valid background times are defined, or no data in background time intervals.', /cont
	endelse

endif else message, 'No valid background times are defined.', /cont

return, ybk

end

;---------------------------------------------------------------------------

function goes::get,_extra=extra,data=data,low=low,high=high,$
                   times=times,utbase=utbase,no_copy=no_copy,tai=tai,secs79=secs79, $
                   tstart=tstart, tend=tend, $
                   btimes=btimes, itimes=itimes, $
                   sat=sat

if keyword_set(sat) then return, goes_sat(self->getprop(/sat))  ;returns GOESxx format
if keyword_set(tstart) then return, anytim2utc(self.tstart,/vms)
if keyword_set(tend) then return, anytim2utc(self.tend,/vms)
if keyword_set(btimes) then return, (*self.btimes)[0] eq -1 ? -1. : anytim2utc(*self.btimes,/vms)
if keyword_set(itimes) then return, self.itimes[0] eq -1 ? -1. : anytim2utc(self.itimes,/vms)

ktime=keyword_set(times) or arg_present(times)
kbase=keyword_set(utbase) or arg_present(utbase)
klow=keyword_set(low) or arg_present(low)
khigh=keyword_set(high) or arg_present(high)
kdata=keyword_set(data) or arg_present(data)

ktime2=keyword_set(times) and (not arg_present(times))
kbase2=keyword_set(utbase) and (not arg_present(utbase))
klow2=keyword_set(low) and (not arg_present(low))
khigh2=keyword_set(high) and (not arg_present(high))
kdata2=keyword_set(data) and (not arg_present(data))

utbase=self->getprop(/utbase)

if ktime or kdata or khigh or klow  then begin

 if not self->have_gdata() then begin
  message,'No GOES data yet read in',/cont
  return,''
 endif

 if ktime then begin
  times=(*self.gdata).time
  utbase=self->getprop(/utbase)
  if keyword_set(tai) then times=temporary(times)+anytim(utbase,/tai)
  if keyword_set(secs79) then times=temporary(times)+anytim(utbase)
 endif

 if kdata or khigh or klow then begin
  if keyword_set(no_copy) then data=temporary(*self.gdata) else data=*self.gdata
  if klow or kdata then low=data.lo
  if khigh or kdata then high=data.hi
  if kdata then data=[[data.lo],[data.hi]]
 endif

endif

if ktime2 then return,times
if kbase2 then return,utbase
if klow2 then return,low
if khigh2 then return,high
if kdata2 then return,data

if is_struct(extra) then return,self->utplot::get(_extra=extra)

return,''

end

;---------------------------------------------------------------------------
;-- GOES set method

pro goes::set,tstart=tstart,tend=tend, $
              mode=mode,sat=sat,_extra=extra,$
              sdac=sdac, yohkoh=yohkoh, $
              clean=clean,markbad=markbad,showclass=showclass, $
              bsub=bsub,btimes=btimes,bfunc=bfunc,remote=remote,$
              itimes=itimes,abund=abund, plotman_obj=plotman_obj



;-- user can select GOES satellite by number (e.g. sat = 12) , name (sat = 'GOES12'),
;   or keyword (/goes12).  Store in self.sat as index 0 (means goes12), 1 (goes10), etc.
if exist(sat) then begin
   index = self->sat2index(sat)
   if index gt -1 then self.sat = index else message,'No such satellite - '+trim(sat),/cont
endif
if have_tag(extra,'goe',/start,ind) then begin
   index = self->sat2index(extra)
   if index gt -1 then self.sat = index else message,'No such satellite - '+(tag_names(extra))[ind],/cont
end

if is_number(remote) then self.remote=remote
if is_number(sdac) then self.sdac = sdac
if is_number(yohkoh) then self.sdac = (yohkoh eq 0)

;-- user can select time resolution mode either by number (e.g. mode=1) or keyword (e.g. /three)
if is_number(mode) then self.mode = 0>mode<2 else self.mode = self->get_mode(extra)

if is_struct(extra) then self->utplot::set,_extra=extra

if exist(tstart) then begin
   if valid_time(tstart) then self.tstart=anytim2tai(tstart) else message, /cont,'Invalid time: ', tstart
endif
if exist(tend) then begin
   if valid_time(tend) then self.tend=anytim2tai(tend) else message, /cont,'Invalid time: ', tend
endif

if is_number(clean) then self.clean = 0b > clean < 1b

if is_number(markbad) then self.markbad = 0b > markbad < 1b

if is_number(showclass) then self.showclass = 0b > showclass < 1b

if is_number(bsub) then self.bsub = 0b > bsub < 1b

if is_string(bfunc) then begin
	q = where (strpos(strlowcase(self.bfunc_options), strlowcase(bfunc)) ne -1, count)
	if count gt 0 then self.bfunc=self.bfunc_options[q[0]] else $
		message,/cont,'Invalid function.'
endif

;-- user can select abundance model either by number (e.g. abund=1) or string (abund='Photospheric')
if is_number(abund) then self.abund = 0b > abund < 2b
if is_string(abund) then begin
	q = where (strpos(strlowcase(self.abund_options),  strlowcase(abund)) ne -1, count)
	if count gt 0 then self.abund = q[0] else message, /cont, 'Invalid abundance model: ' + abund
endif

if exist(btimes) then begin
	if n_elements(btimes) ge 2 then *self.btimes = anytim2tai(btimes) else $
		if btimes[0] eq -1 then *self.btimes = -1 else $
			message, /cont, 'Background time intervals should be an array of start/ends, or -1 for none.'
endif

if exist(itimes) then begin
	if n_elements(itimes) eq 2 then self.itimes = anytim2tai(itimes) else $
		if itimes[0] eq -1 then self.itimes=-1 else $
			message, /cont, 'Integration time interval should be a single start,end, or -1 to disable.'
endif

if is_class(plotman_obj, 'plotman', /quiet) then self.plotman_obj=plotman_obj

return
end

;------------------------------------------------------------------------
;-- remove duplicate keywords

pro goes::fix_keywords,extra

if is_struct(extra) then begin
 tags=tag_names(extra)
 chk=where(stregex(tags,'goe|one|thr|fiv',/fold) ne -1,count)
 if count gt 0 then extra=rem_tag(extra,chk)
 if not is_struct(extra) then delvarx,extra
endif

sat=self->getprop(/sat)
mode=self->getprop(/mode)
goes_res=['three_sec','one_min','five_min']
extra=add_tag(extra,1,goes_sat(sat))
extra=add_tag(extra,1,goes_res[mode])

return & end

;---------------------------------------------------------------------------
;-- GOES plot method

pro goes::prepare_plot,tstart,tend, timerange=timerange, $
              derflux=derflux, $
              temperature=temperature, emission=emission, $
              lrad=lrad, integrate=integrate, bk_overlay=bk_overlay, $
              err=err,_extra=extra,$
              file_id=file_id, new_utplot_obj=new_utplot_obj

err=''
if not self->allow_goes(err=err) then return

struct = self -> getdata(tstart=tstart, tend=tend, timerange=timerange, $
   temperature=temperature, emission=emission, lrad=lrad, integrate=integrate, $
   bk_overlay=bk_overlay, $
   _extra=extra, err=err, /quick_struct)
if err ne '' then return

title=self->title()
showclass = self -> getprop(/showclass)
markbad = self -> getprop(/markbad)
file_id=self->mk_file_id()

do_bk_plot = 0

case 1 of
   keyword_set(temperature): begin
      ydata = struct.tem
      data_unit = 'MegaKelvin'
      label = 'Model: ' + self->get_abund_name()
      ylog = 0
      showclass = 0
      dim1_use = 0
      dim1_ids = ''
      dim1_unit = ''
      title = 'Temperature  ' + title
      ybad = get_uniq([struct.bad0,struct.bad1])
      file_id = file_id + ' Temp'
   end
   keyword_set(emission): begin
      ydata = struct.em
      data_unit = 'cm!u-3!n x 10!u49!n'
      label = 'Model: ' + self->get_abund_name()
      ylog = 1
      showclass = 0
      dim1_use = 0
      dim1_ids = ''
      dim1_unit = ''
      title = 'Emission Measure  ' + title
      ybad = get_uniq([struct.bad0,struct.bad1])
      file_id = file_id + ' Emis'
   end
   keyword_set(lrad): begin
      integ = keyword_set(integrate)
      ydata = [[struct.lrad], [struct.lx]]
      data_unit = integ ? 'erg' : 'erg s!u-1!n'
      label = 'Model: ' + self->get_abund_name()
      if integ then begin
      	itimes = anytim(struct.integrate_times, /vms, /time_only, /truncate)
     	label = [label, $
     		'Integration times: ' + itimes[0] + ' to ' + itimes[1], $
     		'Total Energy Loss: ' + trim(max(struct.lrad),'(e9.2)') + ' erg' ]
      endif
      ylog = 1
      showclass = 0
      dim1_use = [0,1]
      dim1_ids = ['Total loss', 'X-ray loss']
      dim1_unit = ''
      title = (integ ? 'Radiative Energy Loss ' : 'Radiative Energy Loss Rate ') + title
      ybad = get_uniq([struct.bad0,struct.bad1])
      ybad = [ [ybad], [ybad] ]
      file_id = file_id + (integ ? ' Int Eloss' : ' Eloss')
      end
   else: begin
      do_deriv = keyword_set(derflux)
      data_unit='watts m!u-2!n'
      label = ''
      ylog = 1
      dim1_ids=['1.0-8.0 A','0.5 - 4.0 A']
      dim1_unit='Wavelength (Ang)'
      nbad = n_elements(struct.bad0) > n_elements(struct.bad1)

      do_bk_plot = keyword_set(bk_overlay) and (struct.bk[0] ne -1)
      if do_bk_plot then begin
         bk_is_single = (n_elements(struct.bk) eq 2) ; single bk value for each channel
         ny = n_elements(struct.ydata[*,0])
         ydata = [ [struct.yes_clean ? struct.yclean : struct.ydata], $
                 [ bk_is_single ? transpose(rebin(struct.bk,2,ny)) : struct.bk] ]
         dim1_use = [0,1,2,3]
         dim1_ids = [ [dim1_ids], [dim1_ids]+' Background' ]
         title = 'Flux and Background  ' + title
         ybad = lonarr(nbad,4) - 1
         ybad[0,0] = struct.bad0
         ybad[0,1] = struct.bad1
         ybad[0,2] = struct.bad0
         ybad[0,3] = struct.bad1
         file_id = file_id + ' Flux and Bk'
      endif else begin
         ydata = struct.yes_clean ? struct.yclean : struct.ydata
         ydata = struct.yes_bsub  ? struct.ybsub: ydata
         if do_deriv then begin
         	ydata[*,0] = deriv(struct.tarray, ydata[*,0])
         	ydata[*,1] = deriv(struct.tarray, ydata[*,1])
         	title='Derivative of Flux  ' + title
         	file_id = file_id + ' Deriv Flux'
         	data_unit = data_unit + ' derivative'
         endif else begin
         	title = 'Flux  ' + title
         	file_id = file_id + ' Flux'
         endelse
         dim1_use = [0,1]
         ybad = lonarr(nbad,2) - 1
         ybad[0,0] = struct.bad0
         ybad[0,1] = struct.bad1
      endelse
	end
endcase

label = append_arr(label, 'GOES archive: ' + (self.sdac ? 'SDAC' : 'Yohkoh'))
if struct.yes_clean then label = append_arr(label, 'Cleaned')
if struct.yes_bsub and not do_bk_plot then  label = append_arr(label, 'Background subtracted')

self->set,xdata=struct.tarray, ydata=ydata
self->set, $
       ylog=ylog, dim1_use=dim1_use, dim1_ids=dim1_ids, dim1_unit=dim1_unit,$
       label=label, id=title, $
       data_unit=data_unit, /no_copy, filename=file_id, /dim1_sel

if showclass or markbad then begin
   tarray=self->get(/times)
   addplot_arg = {markbad: markbad, showclass:showclass, $
      tarray:tarray, ydata: ydata, ybad: ybad}
   addplot_name = 'goes_oplot'
endif else addplot_name = ''

self -> utplot::set, addplot_name=addplot_name, addplot_arg=addplot_arg


if arg_present(new_utplot_obj) then begin
  new_utplot_obj = obj_new('utplot', struct.tarray, ydata, utbase=self->get(/utbase), $
    status=status, err_msg=err_msg)
  new_utplot_obj -> set, $
       ylog=ylog, dim1_use=dim1_use, dim1_ids=dim1_ids, dim1_unit=dim1_unit,$
       label=label, id=title, $
       data_unit=data_unit, /no_copy, filename=file_id, /dim1_sel
  new_utplot_obj -> set, addplot_name=addplot_name, addplot_arg=addplot_arg
endif

end

;---------------------------------------------------------------------------

; Need err_msg keyword (not err) because when called from plotman, has err_msg
; in extra already, and gets confused if there's an 'err' too
pro goes::plot, tstart, tend, timerange=timerange, err_msg=err, ps=ps, _extra=extra

;if user passed in start time as arg, but no end time, assume they want full day
if exist(tstart) and not exist(tend) then tend = anytim2tai(tstart)+86400.

self -> prepare_plot, tstart, tend, $
   timerange=timerange, err=err, _extra=extra
if err ne '' then return

if keyword_set(ps) then begin
	savedev = !d.name
	savefont = !p.font
	tvlct,/get,r,g,b
	set_plot,'ps'
	!p.font = 0
	device, /color, bits=8, /landscape, filename='goesplot.ps'
	linecolors
	self->utplot::plot,_extra=extra,err=err,timerange=timerange, dim1_colors=[0,3,7,9], thick=2
	device,/close
	set_plot, savedev
	tvlct,r,g,b
	!p.font = savefont
endif else begin

	self->utplot::plot,_extra=extra,err=err,timerange=timerange

endelse

return & end

;---------------------------------------------------------------------------
;-- check whether have plotman software in path

function goes::have_plotman_dir

return, have_proc('plotman__define')

end

;---------------------------------------------------------------------------
;-- GOES get_plotman function method - returns plotman object reference.
; Output Keywords:
; valid - 1 if successful, 0 otherwise.

function goes::get_plotman, valid=valid, nocreate=nocreate, quiet=quiet, _extra=extra

err_msg = ''

valid = 0

if is_class(self.plotman_obj, 'PLOTMAN',/quiet) then valid = 1 else begin

	if not keyword_set(nocreate) then begin

		; first make sure plotman directories are in path
		if self->have_plotman_dir() then begin

		   plotman_obj = obj_new('plotman', /multi_panel,  $
		                   error=err, _extra = extra)
		   if err then err_msg = 'Error creating plotman object.' else begin
		      self.plotman_obj = plotman_obj
		      valid = 1
		   endelse

		endif else begin
		   err_msg = 'Please include HESSI in your SSW IDL path if you want to use plotman.'
		endelse

		if err_msg ne '' and not keyword_set(quiet) then self->display_message, err_msg
	endif

endelse

return, self.plotman_obj

end

;---------------------------------------------------------------------------
;-- GOES PLOTMAN method
; Previously passed the entire GOES object to plotman, but this is unnecessary.  Added new_utplot_obj
; keyword to prepare_plot - this is a utplot object only containing what plotman needs - send this to plotman.

pro goes::plotman, tstart, tend, plotman_obj=plotman_obj, desc=desc, _extra=extra

if keyword_set(plotman_obj) then self->set, plotman_obj=plotman_obj
plotman_obj = self -> get_plotman (valid=valid)
if not valid then return

;if user passed in start time as arg, but no end time, assume they want full day
if exist(tstart) and not exist(tend) then tend = anytim2tai(tstart)+86400.

self -> prepare_plot, tstart, tend, file_id=desc, err=err, new_utplot_obj=new_utplot_obj, _extra=extra
if err ne '' then return

stat = plotman_obj -> setdefaults (input=new_utplot_obj, plot_type='utplot', _extra=extra)
plotman_obj->new_panel, desc, /replace

end

;---------------------------------------------------------------------------
;-- GOES read method
; Stores data and times in gdata structure property.  If new tstart/tend is
; within the last full time read, and sat, sdac, and mode (resolution) didn't change,
; we return.  get_data method will handle getting the correct subset of times.

pro goes::read,tstart,tend, $
              err=err,file_id=file_id,_extra=extra,$
              force=force

err=''
if not self->allow_goes(err=err) then return

;if user passed in start time as arg, but no end time, assume they want full day
if exist(tstart) and not exist(tend) then tend = anytim2tai(tstart)+86400.

; set any changed parameters into the object
self->set,tstart=tstart,tend=tend,_extra=extra

; only read new data if force is set, or need_update returns 1

if not ( keyword_set(force) or self->need_update() ) then return

sdac = self.sdac

dstart=anytim2utc(self->getprop(/tstart),/vms)
dend=anytim2utc(self->getprop(/tend),/vms)

 dprint,'% GOES::READ: reading data...'
 self->fix_keywords,extra

 self->output,sdac=sdac
 sat = self.sat

 sat_num = goes_sat(sat,/num) ; rd_goes_sdac wants sat num, not index (e.g. 12, not 0)
 if sdac then begin
    rd_goes_sdac, tarray=times, yarray=data, $
 	  stime=dstart, etime=dend, error=error, $
 	  sat=sat_num, numstat=numstat, tstat=tstat, stat=stat, $
 	  err_msg=err, /sdac, base_sec=base_sec, _extra=extra
    if err eq '' and exist(times) then begin
       times = temporary(times) + base_sec
       if numstat gt 0 then tstat = temporary(tstat) + base_sec
    endif
 endif else begin
    rd_goes,times,data,trange=[dstart,dend],_extra=extra,err=err,$
      type=type,gdata=gdata,sat=sat_num
 endelse
 sat = sat_num	; get the sat actually retrieved

 if (is_string(err)) then begin
    self->display_message, err
    return
 endif

 count=n_elements(times)
 if count lt 2 then begin
  err='No lightcurve data during specified times'
  message, err, /cont

  return
endif

tmin=times[0]
times=temporary(times)-tmin
utbase=anytim(tmin,/vms)

if sdac then begin
   b = anytim(base_sec, /ints)
   if (is_number(numstat)) then begin
      self.numstat = numstat
      ptr_empty, self.tstat
      ptr_empty, self.stat
      self.tstat   = ptr_new(temporary(tstat)-tmin,/no_copy)  ; make relative to utbase too
      self.stat    = ptr_new(stat,/no_copy)
   endif else self.numstat = -1

   gbo_struct, gxd_data=data_ref

   gdata      = make_array(n_elements(times),value=data_ref)
   gdata.time = temporary(times)
   gdata.day  = b.day
   gdata.lo   = temporary(data[*,0])
   gdata.hi   = temporary(data[*,1])

endif else begin

    self.numstat = -1
    ptr_empty, self.tstat
    ptr_empty, self.stat
    gdata.time = temporary(times)

endelse

*self.gdata=temporary(gdata)

self->set,sat=sat,utbase=utbase

; store last accumulation parameters
self.lstart=anytim2tai(dstart)
self.lend=anytim2tai(dend)
self.lsat = self->sat2index(sat)
self.lsdac = sdac
self.lmode = self.mode  ;need to check if this mode was actually used (type from rd_goes?) ?????????
self.lremote=self->getprop(/remote)
return & end

;--------------------------------------------------------------------------
; Function to return GOES satellite index from string or number or extra structure, i.e. input
; equal to 12 or 'GOES12' or is structure with tag goes12 returns 0

function goes::sat2index, val

index = -1

input = val

if is_struct(input) then begin
   if have_tag(input,'goe',/start,ind) then begin
      gsat = (tag_names(input))[ind]
      input = stregex(gsat,'[0-9]+',/sub,/extra)
   endif
endif

number = is_number(input)

chk=where(strup(input) eq goes_sat(number=number),count)
if count gt 0 then index = chk[0]

return, index
end


;--------------------------------------------------------------------------
; Function to check whether we need to read data files again.
; If satellite, sdac/yohkoh, mode (for yohkoh) changed, or new time is not within last
; time accumulated, then return 1

function goes::need_update

if not self->have_gdata() then return, 1

if self.sat ne self.lsat then return, 1
if self.sdac ne self.lsdac then return, 1
if self.remote ne self.lremote then return,1
if (not self.sdac) and self.mode ne self.lmode then return, 1

if not ( (self.tstart ge self.lstart) and (self.tstart le self.lend) and $
       (self.tend ge self.lstart) and (self.tend le self.lend) ) then return,1

return, 0

end

;--------------------------------------------------------------------------

function goes::title

res=['3 sec','1 min','5 min']
return,goes_sat(self.sat)+' '+res[self.sdac ? 0 :self.mode]

end

;--------------------------------------------------------------------------
;-- make unique identifier (for plotman panel description)

function goes::mk_file_id

t1=trim(anytim2utc(self->getprop(/tstart),/vms,/trunc))
t2=trim(anytim2utc(self->getprop(/tend),/vms,/trunc))

file_id=self->title()+' '+trim(t1)+' to '+trim(t2)

return,file_id
end

;----------------------------------------------------------------------------
;-- extract GOES mode from keyword extra

function goes::get_mode,extra

modes=['thr','one','fiv']
nmodes=n_elements(modes)
if is_struct(extra) then begin
 for i=0,nmodes-1 do if have_tag(extra,modes[i],/start) then return,i
endif

if is_string(extra) then begin
 for i=0,nmodes-1 do begin
  textra=strup(extra)
  chk=where(strpos(extra,modes[i]) eq 0,count)
  if count gt 0 then return,i
 endfor
endif

return,self.mode

end

;----------------------------------------------------------------------------
;-- get full string for abundance model used.  If photospheric or coronal, put chianti
;   version number in string.

function goes::get_abund_name

abund_name = self.abund_options[self.abund]
if self.abund lt 2 then abund_name = abund_name + ' (' + goes_get_chianti_version() + ')'
return, abund_name
end

;----------------------------------------------------------------------------
;-- list time range for each GOES satellite

pro goes::sat_times, out=out

out=['GOES 6  -  04-Jan-1980   to   18-Aug-1994', $
     'GOES 7  -  01-Jan-1994   to   03-Aug-1996', $
     'GOES 8  -  21-Mar-1996   to   18-Jun-2003', $
     'GOES 9  -  20-Mar-1996   to   24-Jul-1998', $
     'GOES 10 -  10-Jul-1998   to   present', $
     'GOES 11 -  21-Jun-2006   to   present', $
     'GOES 12 -  13-Dec-2002   to   present' ]

prstr, out, /nomore
end

;---------------------------------------------------------------------------
;-- show properties

pro goes::help, widget=widget

out = [' ', $
	' GOES parameter values:', $
	'   Last data interval read:']
if self->have_gdata() then begin
 out = [out, '     ' + anytim2utc(self.lstart,/vms) + ' to ' + anytim2utc(self.lend,/vms)]
endif else out = [out, '     None.']

if valid_time(self.tstart) and valid_time(self.tend) then begin
 out = [out,'   Current TSTART / TEND:', $
 	'     ' + self->get(/tstart) + ' to ' + self->get(/tend)]
endif

; + 0 in following lines is to convert byte to fix, otherwise prints weird character
out = [out, $
	self.sdac ? '   SDAC DATA: 1' : '   YOHKOH DATA: 1', $
	'   MODE: ' + trim(self.mode), $
	'   DATA TYPE:  ' + self->title(), $
	'   NEED_UPDATE: ' + trim(self->need_update()+0), $
	'   CLEAN: ' + trim(self.clean+0), $
	'   MARKBAD: ' + trim(self.markbad+0), $
	'   SHOW CLASS: ' + trim(self.showclass+0), $
	'   SUBTRACT BACKGROUND: ' + trim(self.bsub+0), $
	'   BACKGROUND TIMES: ']
if self ->valid_btimes() then begin
   str_btimes = self->get(/btimes)
   for i=0,n_elements(str_btimes)/2-1 do $
      out = [out,'     ' + str_btimes[0,i] + ' to ' +str_btimes[1,i]]
endif else $
   out = [out, '     None']
out = [out, '   BACKGROUND FUNCTION: ' + self.bfunc]

if self.itimes[0] eq -1 then str_itimes = 'None' else begin
	itimes = self->get(/itimes)
	str_itimes = anytim(itimes[0],/vms) + ' to ' + anytim(itimes[1],/vms)
endelse

out= [out, $
	'   INTEGRATION TIMES: ', $
	'     ' + str_itimes, $
	'   ABUNDANCE: ' + self->get_abund_name(), $
	' ']

if keyword_set(widget) then a = dialog_message (out, /info) else prstr, out, /nomore

return & end

;------------------------------------------------------------------------------
;-- get GOES data times

pro goes::times,t1,t2

t1=0.
t2=0.
if not self->have_gdata() then return
np=n_elements(*self.gdata)
t1=anytim((*self.gdata)[0],/tai)
t2=anytim((*self.gdata)[np-1],/tai)

return & end

;------------------------------------------------------------------------------
;-- have GOES data?

function goes::have_gdata,count=count

count=0l
chk=ptr_exist(self.gdata)

if chk then count=n_elements(*self.gdata)

return,chk

end

;-----------------------------------------------------------------------------------------
;-- Display message in IDL log.  If running from GUI, also display in a widget.

pro goes::display_message, msg

prstr, msg, /nomore

if xregistered('goes') gt 0 then r=dialog_message(msg)

end

;-----------------------------------------------------------------------------------------
;--  Interactive background selection using plotman intervals method

pro goes::select_background, full_options=full_options, _extra=extra

bins = -99

intervals = self->valid_btimes() ? anytim(self->get(/btimes)) : -1
type = 'Background'
title='Select Time Intervals for Background'
valid_range = anytim([self->get(/tstart), self->get(/tend)])

plotman_obj = self -> get_plotman(valid=valid)

if not valid then begin
	a=dialog_message (['To use the interactive methods for selecting background ',$
		'time intervals, you must include HESSI in your SSW IDL path for now.', $
		'', $
		'You can select background times by setting the btimes parameter directly: ', $
		"a->set,btimes=['1-Jun-2002 07:53:39.000', '1-Jun-2002 08:34:36.000']" ], /error)
	return
endif

if keyword_set(full_options) then begin

	; use call_function because if hessi isn't in path, this won't compile
	bins = call_function (xsel_intervals,  $
	                       input_intervals=intervals, $
	                       plotman_obj=plotman_obj, $
	                       group=plotman_obj->get(/plot_base), $
	                       valid_range=valid_range, $
	                       title=title, $
	                       type=type, $
	                       /show_start, $
	                       /force, $
	                       _extra=extra )

endif else begin

	bsub_sav = self->getprop(/bsub)

    ; if there's not a utplot currently showing in plotman, plot one
	if not plotman_obj->valid_window(/ut) then self -> plotman, bsub=0, _extra=extra

    plotman_obj -> intervals, title=title, $
                            type=type, $
                            intervals=intervals, $
                            /show_start, $
                            /force, $
                            _extra=extra

    bins = plotman_obj->get( /intervals )

    self -> set, bsub=bsub_sav

endelse

if bins[0] ne -99 then self -> set, btimes=bins[0] eq -1 ? -1 : anytim(bins,/vms)

end

;-----------------------------------------------------------------------------------------
;-- Draw boundaries of background interval(s)
pro goes::show_background

if self->valid_btimes()then begin
	intervals = anytim(self->get(/btimes))
	plotman_obj = self -> get_plotman(valid=valid, /nocreate)
	if valid then $
		plotman_draw_int,'all',{plotman_obj:plotman_obj}, intervals=intervals, type='Background'
	atimes = format_intervals(intervals, /ut)
	out = ['Background time intervals: ', atimes]
endif else out = 'No background time intervals defined.'
prstr, out, /nomore

end

;-----------------------------------------------------------------------------------------
;-- Return 1 if background intervals are defined and they're OK for current times

function goes::valid_btimes

if (ptr_valid(self.btimes)) then begin
   if (n_elements(*self.btimes) gt 1) then $
      return, 1b
endif

return, 0b

end

;-----------------------------------------------------------------------------------------
;--  Interactive integration time selection using plotman intervals method

pro goes::select_integration_times, full_options=full_options,_extra=extra

bins = -99

intervals = (self.itimes[0] eq -1) ? -1  : anytim(self->get(/itimes))
type = 'Integration'
title='Select a Single Time Interval for Integration'
valid_range = anytim([self->get(/tstart), self->get(/tend)])

plotman_obj = self -> get_plotman(valid=valid)

if not valid then begin
	a=dialog_message (['To use the interactive methods for selecting integration ',$
		'time intervals, you must include HESSI in your SSW IDL path for now.', $
		'', $
		'You can select integration times by setting the itimes parameter directly: ', $
		"a->set,itimes=['1-Jun-2002 07:53:39.000', '1-Jun-2002 08:34:36.000']" ], /error)
	return
endif

if keyword_set(full_options) then begin

	; use call_function because if hessi isn't in path, this won't compile
	bins = call_function (xsel_intervals,  $
	                       input_intervals=intervals, $
	                       plotman_obj=plotman_obj, $
	                       group=plotman_obj->get(/plot_base), $
	                       valid_range=valid_range, $
	                       title=title, $
	                       type=type, $
	                       /show_start, $
	                       /force, $
	                       _extra=extra )

endif else begin

    ; if there's not a utplot currently showing in plotman, plot one
	if not plotman_obj->valid_window(/ut) then self -> plotman, _extra=extra

    plotman_obj -> intervals, title=title, $
                            type=type, $
                            intervals=intervals, $
                            max_intervals=1, $
                            /show_start, $
                            /force, $
                            _extra=extra

    bins = plotman_obj->get( /intervals )

endelse

if bins[0] ne -99 then self -> set, itimes=bins[0] eq -1 ? -1 : anytim(bins,/vms)

end

;-----------------------------------------------------------------------------------------
;-- Draw boundaries of integration interval(s)
pro goes::show_integration_times

if self.itimes[0] ne -1 then begin
	intervals = anytim(self->get(/itimes))
	plotman_obj = self -> get_plotman(valid=valid, /nocreate)
	if valid then $
		plotman_draw_int,'all',{plotman_obj:plotman_obj}, intervals=intervals, type='Analysis'
	atimes = format_intervals(self->get(/itimes), /ut)
	out = ['Integration time intervals: ', atimes]
endif else out = 'No integration time intervals defined.'
prstr, out, /nomore

end

;---------------------------------------------------------------------------
;-- Write IDL save file with everything in it.
; filename - output filename string. If not passed, and nodialog is set, then
;    output file is autonamed to idlsave_goes_yyyymmdd_hhmm.sav
; nodialog - don't prompt for filename when not provided

pro goes::savefile, filename=filename, nodialog=nodialog, _extra=extra

if keyword_set(extra) then self -> set, _extra=extra

if not is_string(filename) then begin
	if not keyword_set(nodialog) then begin
		filename = dialog_pickfile (path=curdir(), filter='*.sav', $
			file='idlsave_goes.sav', title = 'Select output save file name',  get_path=path)
		if filename eq '' then begin
			self->display_message,'No output file selected.  Aborting.'
			return
		endif
	endif
endif

readme = ['Variables stored in GOES save file: ', $
   '', $
   'SATELLITE - satellite: GOES 6, 7, 8, 9, 10, 11, 12...', $
   'ASCIIBASE - base time in ASCII format  	',$
   'UTBASE - base time in sec since 79/1/1,0',$
   'TARRAY - time in sec since base time     ',$
   '', $
   'YCLEAN - channels 1,2 with gain change spikes smoothed out in watts m^-2',$
   '', $
   'EMIS   - Emission measure in 10^49 cm^-3', $
   '', $
   'TEMPR  - Temperature in MegaKelvin', $
   '', $
   'LRAD   - Total energy loss rate in erg s^-1', $
   'LX     - X-Ray energy loss rate in erg s^-1', $
   '', $
   'CH0_BAD - element #s that were interpolated for Chan 1',$
   'CH1_BAD - element #s that were interpolated for Chan 2', $
   '', $
   'BSUB    - If 1, then background was subtracted', $
   'BKTIMES - Background time intervals in ASCII', $
   'BFUNC   - Background function', $
   'BK      - Background in two channels in watts m^-2', $
   '', $
   'ABUND_MODEL - Abundance Spectral Model' ]

struct = self -> getdata(/struct)
satellite = self -> get(/sat)
asciibase = self -> getprop(/utbase)
utbase = anytim(asciibase)
abund_model = self->get_abund_name()

tarray = struct.tarray
yclean = struct.yclean
emis = struct.em
tempr = struct.tem
lrad = struct.lrad
lx = struct.lx
ch0_bad = struct.bad0
ch1_bad = struct.bad1
bsub = struct.yes_bsub
bktimes = self->valid_btimes() ? self->get(/btimes) : ''
bfunc = self.bfunc
bk = struct.bk

; if still no filename defined, autoname it (don't do this farther up because
; until we call getdata, we don't have the correct utbase)
if not is_string(filename) then $
	filename = 'idlsave_goes_' + time2file(self.utbase) + '.sav'

save, filename=filename, $
	readme, satellite, asciibase, utbase, tarray, yclean, emis, tempr, lrad, lx, $
	ch0_bad, ch1_bad, bsub, bktimes, bfunc, bk, abund_model, /xdr

msg = ['', $
   'Saved in IDL save file ' + filename, $
   '', $
   'To restore and get a list of variables restored, type:', $
   ' ', $
   "restore, '" + filename, $
   'prstr, readme']
self -> display_message, msg

end

;---------------------------------------------------------------------------
;-- define GOES object

pro goes__define



goes_struct={goes, $
	tstart:0.d0,$
	tend: 0.d0,$
	sat:0,$
	sdac: 0, $   ; 0/1 means use yohkoh/sdac archive of goes files
	mode: 0, $   ; mode = 0,1,2 = 3 sec, 1 min, 5 min
	gdata:ptr_new(),$
	numstat: 0L, $
	tstat: ptr_new(), $
	stat: ptr_new(), $
	lstart:0.d, $   ; lstart through lmode are settings of last accumulation
	lend:0.d, $
	lsat:0, $
	lsdac:0, $
        lremote:0b,$
	lmode:0, $
	showclass: 0b, $
	clean: 0b, $
	markbad: 0b, $
	bsub: 0b, $
	btimes: ptr_new(), $
	bfunc: '', $	; 0poly, 1poly, 2poly 3poly, or exp
	bfunc_options: strarr(5), $
	itimes: [0.d0, 0.d0], $
	abund: 0, $
	abund_options: strarr(3), $
	plotman_obj: obj_new(), $
        remote:0b,$
	inherits utplot}

return & end
