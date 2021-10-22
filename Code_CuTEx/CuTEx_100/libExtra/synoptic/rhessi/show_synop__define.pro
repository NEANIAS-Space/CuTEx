;+
; Project     : HESSI
;
; Name        : SHOW_SYNOP__DEFINE
;
; Purpose     : widget interface to Synoptic data archive
;
; Category    : HESSI, Synoptic, Database, widgets, objects
;
; Syntax      : IDL> obj=obj_new('show_synop',group=group)
;
; Keywords    : GROUP = widget ID of any calling widget
;               PTR = pointer to last displayed object
;
; History     : 12-May-2000,  D.M. Zarro (SM&A/GSFC) - written
;               11-Nov-2005, Zarro (L-3Com/GSI) - tidied up
;               20-Dec-2005, Kim Tolbert - changed dealing with goes object
;                4-Jan-2006, Zarro - removed old GOES references
;               13-Jan-2006, Zarro - added GROUP and NO_PLOTMAN keywords
;               24-Sep-2006, Zarro (ADNET/GSFC)
;                - moved directory selection from config to main widget
;                            
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

;----------------------------------------------------------------------------
;-- event handler for GOES lightcurve plots

pro plot_goes_eh,event

 widget_control,event.top,get_uvalue=self
 if not self->check_times() then return
 info=self->get_info()

;-- create GOES lightcurve object

 if not obj_valid(!show_synop.goes_obj) then begin
  !show_synop.goes_obj=ogoes()
  if not is_class(!show_synop.goes_obj,'goes',/quiet) then begin
   xack,'Error creating GOES object.  Aborting.' & return
  endif
 endif

;-- get GOES times 

 tstart=self->getprop(/tstart)
 tend=self->getprop(/tend)

 widget_control, event.id, get_value=button_val
 if (button_val eq 'Plot GOES Lightcurve') then begin

    if xregistered('goes') gt 0 then return

    ; if plotman obj already exists pass it into goes obj.  If it didn't
    ; exist, goes will create new one, and we need to set that
    ; into !show_synop.plotman_obj

    plotman_obj = !show_synop.plotman_obj
    !show_synop.goes_obj -> set, plotman_obj=plotman_obj
    !show_synop.plotman_obj = !show_synop.goes_obj -> get_plotman(/quiet)
 	!show_synop.goes_obj -> gui, tstart=tstart, tend=tend

    endif

 self->setprop,info=info

 return & end

;----------------------------------------------------------------------------

function show_synop::init,verbose=verbose,no_plotman=no_plotman,$
  plotman_obj=plotman_obj,reset=reset,_extra=extra,group=group,$
  messenger=messenger

;-- defaults

if not have_widgets() then begin
 message,'widgets unavailable',/cont
 return,0
endif

;-- only single copy allowed

self->setprop,verbose=verbose

if keyword_set(reset) then xkill,'show_synop::setup'
if (xregistered('show_synop::setup') ne 0) then return,0

ret=self->synop::init(_extra=extra)
if not ret then return,ret

;-- config file for saving state

if is_dir('$HOME',out=home) then save_dir=home else save_dir=get_temp_dir()

config_file=concat_dir(save_dir,'.show_synop_config')

;-- create !SHOW_SYNOP  for caching

defsysv,'!show_synop',exists=defined
if (1-defined) then begin
 temp={searched:0b,fifo:obj_new(),plotman_obj:obj_new(),goes_obj:obj_new()}
 defsysv,'!show_synop',temp
endif 

;-- reset state

if keyword_set(reset) then begin
 free_var,!show_synop
 !show_synop.searched=0b
 heap_gc
 file_delete,config_file,/quiet
endif

;-- save copied objects in FIFO object

if not obj_valid(!show_synop.fifo) then !show_synop.fifo=obj_new('fifo')

;-- create INFO state structure

mk_dfont,lfont=lfont,bfont=bfont

;--- if plotman object is passed on the command line, then send plots
;    to it, else use valid plotman object saved from last time
;    show_synop was called standalone


if obj_valid(plotman_obj) then begin
 if obj_valid(!show_synop.plotman_obj) then begin
  if plotman_obj ne !show_synop.plotman_obj then obj_destroy,!show_synop.plotman_obj
 endif
 !show_synop.plotman_obj=plotman_obj
endif

self.ptr=ptr_new(/all)

nmodes=4
info={tstart:0l,tend:0l,main:0l,lfont:lfont,bfont:bfont,brow:0l,srow:0l,$
      site_drop:0l,wave_drop:0l,type_drop:0l,vso_drop:0l,$
      timer:0l,timer2:0l,$
      slist:0l,slist2:0l,flist:0l,flist2:0l,cur_sel:'',cur_fsel:'',$
      site_base:0l,wave_base:0l,type_base:0l,vso_base:0l,sbuttons:[0l,0l,0l],$
      config_file:config_file,dtext:0l,mbuttons:replicate(0l,nmodes),$
      tbase:0l,hbase:0l,group:0l,messenger:0l,clear_time:systime(/sec),$
      wait_time:3600.}
if is_number(group) then info.group=group
if is_number(messenger) then info.messenger=messenger

self->setprop,info=info,smode=1

;-- setup widget interface

self->setup,_extra=extra

;-- inhibit PLOTMAN selection

if keyword_set(no_plotman) then self->setprop,plotman=-1

return,1

end



;-----------------------------------------------------------------------------
;-- validate user time inputs

function show_synop::check_times

 err1='' & err2='' & err3=''

 info=self->get_info()
 cur_tstart=anytim2utc(self->getprop(/tstart),/vms)

 widget_control,info.tstart,get_value=tstart
 if not valid_time(tstart,err1) then begin
  xack,err1,group=info.main
  widget_control,info.tstart,set_value=cur_tstart
 endif else begin
  tstart=anytim2utc(tstart,/vms)
  widget_control,info.tstart,set_value=tstart
  self->setprop,tstart=tstart,/no_order
 endelse

 cur_tend=anytim2utc(self->getprop(/tend),/vms)
 widget_control,info.tend,get_value=tend
 if not valid_time(tend,err2) then begin
  xack,err2,group=info.main
  widget_control,info.tend,set_value=cur_tend
 endif else begin
  tend=anytim2utc(tend,/vms)
  self->setprop,tend=tend,/no_order
  widget_control,info.tend,set_value=tend
 endelse

 if (err1 eq '') and (err2 eq '')  then begin
  if anytim2tai(tend) le anytim2tai(tstart) then begin
   err3='End time should be greater than Start time'
   xack,err3,group=info.main
  endif
 endif

 return,(err1 eq '') and (err2 eq '') and (err3 eq '')
 end

;----------------------------------------------------------------------------

pro show_synop::setup,timerange=timerange,_extra=extra

;-- create widgets

self->create_widgets,_extra=extra

info=self->get_info()

self->setprop,copy=1

;-- reconcile times

secs_per_day=24l*3600l
week=7l*secs_per_day

get_utc,tstart & tstart.time=0
tstart=(utc2tai(tstart)-3.*secs_per_day) > utc2tai(anytim2utc('2-dec-95'))
tend=anytim2utc(!stime)
tend.time=0
tend.mjd=tend.mjd+1
tend=utc2tai(tend)
tstart=anytim2utc(tstart,/vms)
tend=anytim2utc(tend,/vms)

;-- restore settings from last saved object
;   (catch any errors in case saved object has a problem)

reset=0
error=0
catch,error
if error ne 0 then  reset=1

if reset then file_delete,info.config_file,/quiet

xhour
chk=loc_file(info.config_file,count=count)
verbose=self.verbose
if count gt 0 then begin
 restore,file=chk[0]
 if is_struct(props) then begin
  if have_tag(props,'last_count') and have_tag(props,'save_time') then begin
   now=anytim2tai(!stime)
   day_secs=24.d*3600.d
   if (now - props.save_time) le day_secs then begin
    message,'restoring configuration from - '+info.config_file,/cont
    self->setprop,_extra=props
    ldir=self->getprop(/ldir)
    self->setprop,ldir=fix_dir_name(ldir)
   endif
  endif
 endif
 self->setprop,verbose=verbose
endif

catch,/cancel

;-- ensure that remote listing returns file sizes

self->setprop,/pair,/bytes

;-- use last saved times

last_time=self->getprop(/last_time)
if last_time and is_struct(props) then begin
 tstart=props.tstart
 tend=props.tend
endif

;-- restore last files being downloaded in background

if have_tag(props,'bstruct') then begin
 if is_struct(props.bstruct) then $
  widget_control,info.flist2,set_uvalue=props.bstruct
endif

;-- override with command-line times

if n_elements(timerange) eq 2 then begin
 chk=valid_time(timerange)
 if (min(chk) ne 0) then begin
  tstart=timerange[0] & tend=timerange[1]
 endif
endif

widget_control,info.tstart,set_value=anytim2utc(tstart,/vms)
widget_control,info.tend,set_value=anytim2utc(tend,/vms)
self->setprop,tstart=tstart,tend=tend

;-- last file selection

if have_tag(props,'cur_fsel') then begin
 info=rep_tag_value(info,props.cur_fsel,'cur_fsel')
 self->setprop,info=info
endif

;-- set working directory

;if self->getprop(/current) then self->setprop,ldir=curdir()

;-- update sort mode

sort_mode=self->getprop(/sort_mode) < 2
widget_control,info.sbuttons[sort_mode],/set_button

;-- update search mode type

search_mode=self->getprop(/mode)
if not widget_valid(info.mbuttons[3]) then begin
 if search_mode eq 3 then begin
  search_mode=0
  self->setprop,mode=0
 endif
endif

widget_control2,info.mbuttons[search_mode],/set_button
widget_control,info.site_base,map=search_mode eq 0
widget_control,info.type_base,map=search_mode eq 1
widget_control,info.wave_base,map=search_mode eq 2
widget_control,info.vso_base,map=search_mode eq 3

;-- update site name

curr_site=self->getprop(/site)
names=self->list_synop_names(abbr)
chk=str_match(abbr,curr_site,count=count,found=found)
if count gt 0 then widget_control,info.site_drop,set_droplist_select=found[0]

names=self->list_vso_names(abbr)
chk=str_match(abbr,curr_site,count=count,found=found)
if count gt 0 then widget_control,info.vso_drop,set_droplist_select=found[0]

;-- update wave type

curr_wave=self->getprop(/wave)
waves=self->list_synop_waves(abbr)
chk=str_match(abbr,curr_wave,count=count,found=found)
if count gt 0 then widget_control,info.wave_drop,set_droplist_select=found[0]

;-- update data type

curr_type=self->getprop(/types)
type=self->list_synop_dirs()
chk=str_match(type,curr_type,count=count,found=found)
if count gt 0 then widget_control,info.type_drop,set_droplist_select=found[0]

self->flist

;-- don't produce initial listing on startup

if (!show_synop.searched eq 0b) or (self.last_count eq 0) then begin
 search_mess='*** Press "Search" to list latest data files ***'
 widget_control,info.slist,set_value=search_mess
 widget_control,info.slist,set_uvalue=''
endif else self->slist,/quiet

;-- start  XMANAGER

xmanager,'show_synop::setup',info.main,/no_block,event='show_synop_event',$
 cleanup='show_synop_cleanup'

return & end

;-----------------------------------------------------------------------------
;-- create widgets

pro show_synop::create_widgets,group=group,modal=modal,no_plot=no_plot

info=self->get_info()

;-- load fonts

lfont=info.lfont
bfont=info.bfont

modal=keyword_set(modal)
info.main = widget_mbase(title = 'SHOW_SYNOP',group=group,$
                   modal=modal,/column,uvalue=self)

;-- setup timers

info.timer=widget_base(info.main,map=0)
widget_control,info.timer,set_uvalue='timer'

info.timer2=widget_base(info.main,map=0)
widget_control,info.timer2,set_uvalue='timer2'

;-- operation buttons

curr=anytim2utc(!stime,/vms,/date)

widget_control,info.main, tlb_set_title ='SHOW SYNOP: '+curr

row1=widget_base(info.main,/row,/frame)
exitb=widget_button(row1,value='Done',uvalue='exit',font=bfont)
;relistb=widget_button(row1,value='Relist',uvalue='reload',font=bfont)
conb=widget_button(row1,value='Configure',font=bfont,uvalue='config')

;-- date/time fields

row2=widget_base(info.main,/column,/frame)

trow=widget_base(row2,/row)
info.tstart=cw_field(trow,title= 'Start Time:  ',value=' ',xsize = 20,font=lfont)

info.tend=cw_field(trow,title='Stop Time:   ', value=' ',xsize = 20,font=lfont)

srow=widget_base(row2,/row)

left=widget_base(srow,/row)

vso_support=0b
supp=['Site','Type','Subtype']
suvalue=['by_site','by_type','by_subtype']
if chklog('VSO_SUPPORT') then begin
 vso_support=1b
 supp=[supp,'VSO']
 suvalue=[suvalue,'by_vso']
endif

xmenu2,supp,left,/row,/exclusive,font=lfont,/no_rel,$
      buttons=mbuttons,uvalue=suvalue,$
      title='Search by: ',lfont=lfont
info.mbuttons=mbuttons

right=widget_base(srow)

search_mode=self->getprop(/mode)
names=self->list_synop_names(abbr)
info.site_base=widget_base(right,/row,map=0,space=10)
info.site_drop=widget_droplist(info.site_base,value=trim(names),font=bfont,uvalue=abbr,/dynamic)

info.type_base=widget_base(right,/row,map=0)
types=self->list_synop_dirs()
info.type_drop=widget_droplist(info.type_base,value=types,font=bfont,uvalue=types)

info.wave_base=widget_base(right,/row,map=0)
waves=self->list_synop_waves(abbr)
info.wave_drop=widget_droplist(info.wave_base,value=waves,font=bfont,uvalue=abbr,/dynamic)

info.vso_base=widget_base(right,/row,map=0)
names=self->list_vso_names(abbr)
info.vso_drop=widget_droplist(info.vso_base,value=names,font=bfont,uvalue=abbr,/dynamic)

;-- search button

junk=widget_base(row2,/row, space=20)

left=widget_base(junk,/row)

slabel1=widget_label(left,font=lfont,value='Press: ')
searchb=widget_button(left,value='Search',uvalue='search',font=bfont)
slabel2=widget_label(left,font=lfont,value=' to list data files')

right=widget_base(junk,/row, space=10)
guig=widget_button(right,value='Plot GOES Lightcurve',font=bfont,event_pro='plot_goes_eh')

;--  files list

row3=widget_base(info.main,/column,/frame)
blabel=widget_label(row3,value='Data Archive Search Results',font=lfont)


;-- sort buttons

xmenu2,['Filename','Decreasing Date','Increasing Date'],row3,/row,/exclusive,font=lfont,/no_rel,$
      buttons=sbuttons,uvalue=['by_file','by_date_d','by_date_i'],$
      title='Sort by: ',lfont=lfont

info.sbuttons=sbuttons
info.srow=widget_base(row3,/row)
dlabel1=widget_label(info.srow,font=lfont,value='Press: ')
downb=widget_button(info.srow,value='Download',font=bfont,uvalue='download')
dlabel2=widget_label(info.srow,font=lfont,value=' to copy selected file(s), and ')
detb=widget_button(info.srow,value='Details',font=bfont,uvalue='details')
dlabel3=widget_label(info.srow,font=lfont,value=' to display remote file information')


slabel='FILENAME                                    DATE_OBS              TYPE                 SIZE'
label=widget_list(row3,value=slabel,ysize=1,/frame,font='fixed')
info.slist=widget_list(row3,value='   ',ysize=8,font='fixed',/multiple)
info.slist2=widget_base()

;-- downloaded files list

info.brow=widget_base(info.main,/row,/frame)
headb=widget_button(info.brow,value='View Header',font=bfont,uvalue='head')
plotb=widget_button(info.brow,value='Display',font=bfont,uvalue='plot')
;refb=widget_button(info.brow,value='Refresh',font=bfont,uvalue='refresh')
remb=widget_button(info.brow,value='Delete',font=bfont,uvalue='delete')

row4=widget_base(info.main,/column,/frame)
drow=widget_base(row4,/row)
ldir=self->getprop(/ldir)
dlabel=widget_label(/align_left,drow,value='Currently downloaded files in: ',font=lfont)
info.dtext=widget_text(drow,value=ldir,xsize=30,/editable,uvalue='directory')
dbutt=widget_button(drow,value='Change',uvalue='change',font=bfont)
     
info.flist=widget_list(row4,value='',uvalue='',ysize=8,xsize=70,font='fixed',/multiple)
info.flist2=widget_base()

;-- realize widgets and start timers

widget_control,info.main,/realize
widget_control,info.timer,timer=1.
widget_control,info.timer2,timer=10.

self->setprop,info=info

return & end

;-----------------------------------------------------------------------------
;-- set INFO structure

pro show_synop::setprop,info=info,sort_mode=sort_mode,err=err,$
                        save_time=save_time,_extra=extra

err=''
if is_struct(extra) then self->synop::setprop,_extra=extra,err=err

if is_number(sort_mode) then self.sort_mode =  0 > sort_mode < 2

if valid_time(save_time) then self.save_time=anytim2tai(save_time)

if is_struct(info) then *self.ptr=info

return & end

;-----------------------------------------------------------------------------
;-- return widget state INFO

function show_synop::get_info

if not ptr_valid(self.ptr) then return,-1
if not exist(*self.ptr) then return,-1
return,*self.ptr
end

;----------------------------------------------------------------------------
;-- object event handler. Since actual methods cannot be event-handlers,
;   we shunt events thru this wrapper

pro show_synop_event,event
widget_control,event.top,get_uvalue=object
if obj_valid(object) then object->event,event
return & end

;-----------------------------------------------------------------------------
;-- main event handler

 pro show_synop::event,event

;-- retrieve object reference from uvalue of main widget

 widget_control,event.top,get_uvalue=self
 widget_control,event.id, get_uvalue=uvalue
 if not exist(uvalue) then uvalue=''
 bname=''
 if is_string(uvalue) then bname=trim(uvalue[0])
 info=self->get_info()

;-- timer

 if bname eq 'timer' then begin
  widget_control,info.timer,timer=1.

  widget_control,info.slist,get_uvalue=hold
  sens=is_string(info.cur_sel) and is_string(hold)
  widget_control,info.srow,sensitive=sens

  sens=is_string(info.cur_fsel)
  widget_control,info.brow,sens=sens

;-- clear cache if longer than wait time

  cur_time=systime(/sec)
  if (abs(cur_time - info.clear_time) gt info.wait_time) then begin
   self->list_cache,/clear
   message,'clearing cache...',/cont
   info.clear_time=cur_time
   self->setprop,info=info
  endif
  return
 endif

 if bname eq 'timer2' then begin
  widget_control,info.timer2,timer=10.

;-- update file download status

  widget_control,info.flist2,get_uvalue=bstruct
  if is_struct(bstruct) then self->flist,/refresh
 
  return
 endif

;-- quit here

 if bname eq 'exit' then begin
  show_synop_cleanup,event.top
  xkill,event.top
  return
 endif

;-- sort mode

 chk=where(strlowcase(bname) eq ['by_file','by_date_d','by_date_i'],count)
 if count gt 0 then begin
  sort_mode=chk[0]
  self->setprop,sort_mode=sort_mode
  widget_control,info.slist2,get_uvalue=stc,/no_copy
  if is_struct(stc) then begin
   fdata=stc.fdata
   fnames=stc.fnames
   times=stc.times
   widget_control,info.slist,get_uvalue=files
   self->display,fdata,files,fnames,times
  endif
  return
 endif

;-- search mode

 chk=where(strlowcase(bname) eq ['by_site','by_type','by_subtype','by_vso'],count)

;-- restore last drop-list selections

 if count gt 0 then begin
  mode=chk[0]
  self->setprop,mode=mode
  widget_control,info.site_base,map=mode eq 0
  widget_control,info.type_base,map=mode eq 1
  widget_control,info.wave_base,map=mode eq 2
  widget_control,info.vso_base,map=mode eq 3
  self->setprop,info=info
 endif

;-- check time inputs and search pattern

 if (bname eq 'search') or (bname eq 'reload') then begin

;-- validate times

  if not self->check_times() then return

;-- drop list selections

  mode=self->getprop(/mode)

;-- search by site

  if (mode eq 0) then begin
   widget_control,info.site_drop,get_uvalue=duvalue
   drop_index=widget_info(info.site_drop,/droplist_select)
   new_site=trim(duvalue[drop_index])
   self->setprop,site=new_site
  endif

;-- search by type

  if (mode eq 1) then begin
   widget_control,info.type_drop,get_uvalue=duvalue
   type_index=widget_info(info.type_drop,/droplist_select)
   types=duvalue[type_index]
   self->setprop,types=types
  endif

;-- search by wave (or subtype)

  if (mode eq 2) then begin
   widget_control,info.wave_drop,get_uvalue=duvalue
   drop_index=widget_info(info.wave_drop,/droplist_select)
   new_wave=trim(duvalue[drop_index])
   self->setprop,wave=new_wave
  endif

;-- search by VSO 

  if (mode eq 3) then begin
   widget_control,info.vso_drop,get_uvalue=duvalue
   drop_index=widget_info(info.vso_drop,/droplist_select)
   new_site=trim(duvalue[drop_index])
   self->setprop,site=new_site
  endif

  self->slist,reload=bname eq 'reload'
  !show_synop.searched=1b
  self->setprop,info=info
  return
 endif

;-- configure

 if bname eq 'config' then self->config,group=event.top,/no_types

;-- top list selection event

 if event.id eq info.slist then begin
  widget_control,info.srow,/sens
  new_sel=widget_selected(info.slist)
  info=rep_tag_value(info,new_sel,'cur_sel')
  self->setprop,info=info

;-- highlight first file in download list

  self->fbreak,new_sel[0],sdir,sname
  widget_control,info.flist,get_uvalue=files
  self->fbreak,files,cdir,cname
  sel=where(sname eq cname,scount)
  if scount gt 0 then begin
   widget_control,info.flist,set_list_select=sel[0]
   info=rep_tag_value(info,files[sel[0]],'cur_fsel')
   self->setprop,info=info
  endif
 endif

;-- bottom list selection event

 if event.id eq info.flist then begin
  widget_control,info.flist,get_uvalue=files
  sel_index=widget_selected(info.flist,/index)
  new_sel=files[sel_index]
  info=rep_tag_value(info,new_sel,'cur_fsel')
  self->setprop,info=info
 endif

;-- show selected file details

 if xalive(info.tbase) and (event.id eq info.slist) then begin
  if have_tag(event,'clicks') then begin
   if (event.clicks eq 1) then bname='details'
  endif
 endif

 if bname eq 'details' then self->file_info,info.cur_sel[0]

;-- download selected file

 if (event.id eq info.slist) then begin
  if have_tag(event,'clicks') then begin
   if (event.clicks eq 2) then bname='download'
  endif
 endif

 if bname eq 'download' then begin
  if is_blank(info.cur_sel) then return
  ldir=self->getprop(/ldir)
  if not write_dir(ldir) then begin
   xack,['Cannot download to: '+ldir,$
         '  -> No write access <-  ']
   return
  endif
  xhour

;-- check files that are still downloading

  self->check_background
  widget_control,info.flist2,get_uvalue=bstruct

;-- update with new files

  lfile=self->fetch(info.cur_sel,err=err,bstruct=bstruct)
  widget_control,info.flist2,set_uvalue=bstruct

  if is_string(err) then xack,err,group=info.main
  if is_string(lfile) then begin
   clobber=self->getprop(/clobber)
   if clobber then !show_synop.fifo->delete,lfile
  endif
  sfiles=info.cur_sel
  ldir=self->getprop(/ldir)
  self->fbreak,sfiles[0],sdir,sname
  fname=concat_dir(ldir,sname)
  info=rep_tag_value(info,fname,'cur_fsel')
  self->setprop,info=info
  self->flist
 endif

 if (bname eq 'change') or (bname eq 'directory') then begin
  old_dir=self->getprop(/ldir)
  if bname eq 'change' then $
   new_dir=dialog_pickfile(/directory,/must_exist,dialog_parent=event.top,path=old_dir) else $
    widget_control,info.dtext,get_value=new_dir
  new_dir=fix_dir_name(trim(new_dir))
  if new_dir eq '' then begin
   widget_control,info.dtext,set_value=old_dir
   return
  endif
  if old_dir eq new_dir then return

  if not is_dir(new_dir) then begin
   xack,'Non-existent directory: '+new_dir
   widget_control,info.dtext,set_value=old_dir
   return
  endif

  info=rep_tag_value(info,'','cur_fsel')
  self->setprop,ldir=new_dir,info=info
  self->flist
 endif

;-- refresh download list

 if bname eq 'refresh' then self->flist,/refresh

;-- delete from download list

 if bname eq 'delete' then begin
  widget_control,info.flist2,get_uvalue=bstruct
  have_files=is_string(info.cur_fsel,cfiles)
  if have_files then begin
   for i=0,n_elements(cfiles)-1 do begin
    dfile=cfiles[i]

;-- shouldn't delete if in the middle of downloading

    ans=0b
    if is_struct(bstruct) then begin
     chk=where(dfile eq bstruct.files,count)
     if count gt 0 then begin
      dsize=(file_info(dfile)).size
      bsize=(bstruct.sizes)[chk[0]]
      if (dsize gt 0) and (dsize le bsize) then begin
       ans=xanswer('Deleting a file whilst still downloading can cause instabilities',$
                  instruct='Continue delete ? ',space=1)
       if not ans then begin
        self->flist,/refresh
        return
       endif
      endif
     endif
    endif

    self->fbreak,dfile,fdir,fname
    if not ans then begin
     ans=xanswer('Delete '+fname+' from local directory?',$
                message_supp='Do not request confirmation for future deletes',$
                /suppre,/check,instruct='Delete ? ',space=1)
    endif

;-- delete local file, compressed copy, and cached copy

    if ans then begin
     file_delete,dfile,/quiet
     dprint,'..deleting '+dfile
;     compressed=find_compressed(dfile,/look)
;     file_delete,compressed,/quiet
     !show_synop.fifo->delete,dfile
    endif
   endfor
   info=rep_tag_value(info,'','cur_fsel')
   self->setprop,info=info
   self->flist
  endif
 endif

 if (event.id eq info.flist) and have_tag(event,'clicks') then $
  if (event.clicks eq 2) then bname='plot'

;-- read header only

 if (bname eq 'head') then begin
  file_id=trim(info.cur_fsel)
  if (file_id eq '') then return

;-- check if header already read

  header=''
  !show_synop.fifo->get,file_id,data
  if obj_valid(data) then header=data->get(/header)

;-- otherwise read it

  if is_blank(header) then begin
   mrd_head,file_id,header,err=err
   if is_string(err) and is_blank(header) then begin
    xack,err,group=info.main
    return
   endif
  endif

  if is_string(header) then begin
   hbase=info.hbase
   desc=['File: '+file_id,' ']
   xpopup,[desc,header],wbase=hbase,group=info.main,tfont=info.lfont,bfont=info.bfont,$
           title='File Header Information',xsize=80
   info.hbase=hbase
   self->setprop,info=info
  endif else xack,['No header in: ',file_id],group=info.main,/info
 endif

;-- read & plot downloaded file

 if (bname eq 'plot') then begin

  use_plotman=(self->getprop(/plotman) eq 1)

  file_id=trim(info.cur_fsel[0])
  if (file_id eq '') then return

  if widget_info(info.messenger,/valid) then begin
   widget_control,info.messenger,set_uvalue='SYNOP'+file_id,timer=1
   return
  endif

  nf=n_elements(info.cur_fsel)
  if not use_plotman then nf=1

  for i=0,nf-1 do begin
   file_id=info.cur_fsel[i]

;-- check if this selection already cached

   status=0b
   if self->getprop(/cache) then begin
    self->setprop,info=info
    !show_synop.fifo->get,file_id,data
    status=0
    if (obj_valid(data))[0] then begin
     if have_method(data,'has_data') then status=data->has_data()
    endif
   endif

;-- if not, read it

   new_file=0b
   if not status then begin
    xtext,'Please wait. Reading file...',wbase=tbase,/just_reg,/center,$
         group=info.main
    xhour
    r=obj_new('reader')
    r->read,file_id,data,err=err
    obj_destroy,r
    xkill,tbase
    if is_string(err) then begin
     if obj_valid(data) then obj_destroy,data else $
      err=[err,'File probably still downloading, or an invalid FITS file.']
     xack,err,group=info.main
     break
    endif
    new_file=1b
   endif

;-- if using PLOTMAN, then disable channel selection options, and
;   automatic color scaling

   cancel=0b
   plot_type=data->get(/plot_type)

   if plot_type eq 'utplot' then begin
    data->set,dim1_sel=(1-use_plotman)
    data->options,cancel=cancel
   endif

   if not cancel then begin
    self->plot_data,data
   endif

   if new_file then !show_synop.fifo->set,file_id,data

  endfor
 endif

 return & end

;-----------------------------------------------------------------------------
;-- main plotter

 pro show_synop::plot_data,data

 if not obj_valid(data) then return

;-- use data's internal plot method if not using PLOTMAN

 xhour
 use_plotman=(self->getprop(/plotman) eq 1)
 if not use_plotman then begin
  data->plot,err=err
  if is_string(err) then xack,err
  return
 endif

;-- create new plotman object if not already done so

 if not obj_valid(!show_synop.plotman_obj) then begin
  !show_synop.plotman_obj=obj_new('plotman',input=data, /multi)
 endif else status = !show_synop.plotman_obj->setdefaults(input=data)

 filename = data->get(/filename)
 desc = filename eq '' ? data->get(/id) : filename
 !show_synop.plotman_obj->new_panel, desc, /nodup

return & end

;-------------------------------------------------------------------------
;-- check background files for any that are still downloading

pro show_synop::check_background

info=self->get_info()
widget_control,info.flist2,get_uvalue=bstruct
if (1-is_struct(bstruct)) then return
bfiles=bstruct.files
bsizes=bstruct.sizes

;-- Check latest list of downloaded files.
;   Check last saved size. If equal to remote size, then download is
;   complete.
 
nfiles=n_elements(bfiles)
for i=0,nfiles-1 do begin
 if file_test(bfiles[i]) then begin
  fstat=file_info(bfiles[i])
  fsize=fstat.size
  if (fsize lt bsizes[i]) then begin
   new_bfiles=append_arr(new_bfiles,bfiles[i],/no_copy)
   new_bsizes=append_arr(new_bsizes,bsizes[i],/no_copy)
  endif
 endif 
endfor

if is_string(new_bfiles) then begin
 new_bstruct={files:new_bfiles,sizes:new_bsizes}
 widget_control,info.flist2,set_uvalue=new_bstruct
endif else begin
 widget_control,info.flist2,set_uvalue=''
endelse

return & end

;-------------------------------------------------------------------------
;-- check current file listing for any that are still downloading

pro show_synop::check_flist,files,dfiles

dfiles=''
if is_blank(files) then return
dfiles=file_break(files)
self->check_background
info=self->get_info()
widget_control,info.flist2,get_uvalue=bstruct
if (1-is_struct(bstruct)) then return
bfiles=bstruct.files
bsizes=bstruct.sizes

;message,'Current files being downloaded: ',/cont
;print,bfiles+' '+string(strtrim(bsizes))+' bytes'
;print,''

dtag='     Downloading...'
match,files,bfiles,p,q
if p[0] gt -1 then begin
 dsizes=(file_info(bfiles[q])).size
 perc=100.*float(dsizes)/float(bsizes[q])
 perc=string(perc,'(I3)')
 dfiles[p]=dfiles[p]+dtag+' '+perc+' %'
endif

return & end

;--------------------------------------------------------------------------
;-- list currently downloaded files

pro show_synop::flist,refresh=refresh,no_highlight=no_highlight

info=self->get_info()

relist=1-keyword_set(refresh)
if relist then begin
 ldir=self->getprop(/ldir)
 widget_control,info.dtext,set_value=ldir
 xhour
 search_ext='\.fts|\.fits|\.ltc|.\spc|\.fit|\.[0-9]+'
 files=loc_file('*.*',path=ldir,count=count)
 chk=stregex(files,search_ext,/fold)
 ok=where(chk gt -1,count)
 if count eq 0 then begin
  widget_control,info.flist,set_value='',set_uvalue=''
  return
 endif
 files=files[ok]
 files=get_uniq(files)
endif else begin
 widget_control,info.flist,get_uvalue=files
endelse

;-- check list files against download list

self->check_flist,files,dfiles
widget_control,info.flist,set_value=dfiles,set_uvalue=files

if keyword_set(no_highlight) then return

self->fbreak,files,cdir,cnames
self->fbreak,info.cur_fsel[0],sdir,sname
sel=where(sname eq cnames,scount)
if scount eq 0 then sel=0
if sel[0] gt -1 then begin
 widget_control,info.flist,set_list_select=sel[0]
 info=rep_tag_value(info,files[sel[0]],'cur_fsel')
 self->setprop,info=info
endif

return & end

;-------------------------------------------------------------------------
;-- sort output

function show_synop::sort,fnames,times

sort_mode=self->getprop(/sort_mode) < 2

sorder=0
if (n_elements(fnames) gt 1) then begin
 if sort_mode eq 0 then sorder=bsort(fnames) else $
  sorder=bsort(times,reverse=sort_mode eq 1)
endif

return,sorder & end

;-------------------------------------------------------------------------
;-- list data archive

pro show_synop::slist,reload=reload,quiet=quiet

;-- initialize

info=self->get_info()

;-- start listing

verbose=1-keyword_set(quiet)
if verbose then $
 xtext,'Please wait. Searching archives...',wbase=tbase,/just_reg,/center,$
        group=info.main
old_cache=self->getprop(/cache)
if keyword_set(reload) then self->setprop,cache=0

xhour
widget_control,info.main,sensitive=0

self->list,files,sizes=sizes,times=times,count=count,cats=cats,stimes=stimes,err=err

if is_string(err) then message,err,/cont
self->setprop,cache=old_cache

widget_control,info.main,sensitive=1
xkill,tbase

if count eq 0 then begin
 mode=self->getprop(/mode)
 site=self->getprop(/site)
 wave=self->getprop(/wave)
 types=self->getprop(/types)
 case mode of
  0:  begin
       names=self->list_synop_names(abbr)
       chk=where(site eq abbr,count)
       no_files=names[chk[0]]
      end
  3:  begin
       names=self->list_vso_names(abbr,cat)
       chk=where(site eq abbr,count)
       no_files=names[chk[0]]
      end
  1:  no_files='type "'+types+'"'
  else: no_files='subtype "'+wave+'"'
 endcase
 no_files='No files matching '+no_files+' during specified time range'
 widget_control,info.slist,set_value=no_files
 widget_control,info.slist,set_uvalue=''
 widget_control,info.slist2,set_uvalue=''
 return
endif

;-- format output

fnames=file_break(files)
mcat=str_cut(cats,20,pad=22)
fcat=str_cut(fnames,36,pad=40)
fdata=temporary(fcat)+temporary(stimes)+'      '+temporary(mcat)+sizes

;-- display output

self->display,fdata,files,fnames,times

return & end

;-------------------------------------------------------------------------
;-- display list output

pro show_synop::display,fdata,files,fnames,times

info=self->get_info()
sorder=self->sort(fnames,times)
widget_control,info.slist,set_value=fdata[sorder]
widget_control,info.slist,set_uvalue=files[sorder]
widget_control,info.slist2,set_uvalue={fdata:fdata[sorder],fnames:fnames[sorder],times:times[sorder]}

chk=where(info.cur_sel[0] eq files[sorder],count)
if count gt 0 then widget_control,info.slist,set_list_select=chk[0]

return & end

;---------------------------------------------------------------------------
;-- widget cleanup

pro show_synop_cleanup,id

dprint,'% show_synop_cleanup...'

widget_control,id,get_uvalue=object

if obj_valid(object) then begin
 info=object->get_info()
 if is_struct(info) then begin
  xtext_reset,info
  obj_destroy,object
 endif
endif
return & end

;----------------------------------------------------------------------------
;-- object cleanup

pro show_synop::cleanup

dprint,'% show_synop::cleanup...'

info=self->get_info()
if not is_struct(info) then return

xhour

self->setprop,save_time=!stime

props=self->getprop(/all_props)

;-- clean up GOES files

if obj_valid(!show_synop.goes_obj) then !show_synop.goes_obj->flush,2

;-- save some useful configuration info

ptags=tag_names(props)
for i=0,n_tags(props)-1 do begin
 type=size(props.(i),/tname)
 if (type eq 'POINTER') or (type eq 'OBJREF') then $
  rtags=append_arr(rtags,ptags[i])
endfor
props=rem_tag(props,rtags)

props=add_tag(props,info.cur_fsel,'cur_fsel')
props=add_tag(props,self->getprop(/ldir),'ldir')
widget_control,info.flist2,get_uvalue=bstruct
if is_struct(bstruct) then props=add_tag(props,bstruct,'bstruct')
self->fbreak,info.config_file,fdir
if write_dir(fdir,/quiet) then begin
 message,'saving configuration to - '+info.config_file,/cont
 save,file=info.config_file,props
endif

self->synop::cleanup

ptr_free,self.ptr
xkill,'show_synop::setup'

return & end

;----------------------------------------------------------------------------
;-- read header of remote file

pro show_synop::rhead,file,header

header=''
if is_blank(file) then return

robj=obj_new('hfits')
robj->read,file[0],header=header,/nodata
obj_destroy,robj

return & end

;--------------------------------------------------------------------------
;-- display site info based on filename

pro show_synop::file_info,file

if not is_string(file) then return

xhour

mode=self->getprop(/mode)
if mode eq 3 then desc='' else $
 desc=['File: '+file_break(file),'Source: '+self->file_site(file),self->file_desc(file)]

self->rhead,file,header
if is_string(header) then desc=[desc,header]

;-- create pop-up

info=self->get_info()
tbase=info.tbase
xpopup,desc,wbase=tbase,group=info.main,tfont=info.lfont,bfont=info.bfont,$
            xsize=80,title='Remote File Information'
info.tbase=tbase
self->setprop,info=info

return
end


;----------------------------------------------------------------------------
;-- match file site abbr with full site name

function show_synop::file_site,file

if not is_string(file) then return,''

self->fbreak,file,dir,fname

sites=self->list_synop_sites()
nsites=n_elements(sites)
i=-1 & count=0
while (i lt (nsites-1)) and (count eq 0) do begin
 i=i+1
 look=where(strpos(strlowcase(fname),strlowcase(sites[i].abbr)) gt -1,count)
endwhile

if count gt 0 then return,sites[i].name else return,'Unrecognized site name'

end

;----------------------------------------------------------------------------
;-- match file desc abbr with full description

function show_synop::file_desc,file

if not is_string(file) then return,''

self->fbreak,file,dir,fname

desc=self->list_synop_desc()
ndesc=n_elements(desc)
for i=0,ndesc-1 do begin
 look=where(strpos(strlowcase(fname),'_'+strlowcase(desc[i].abbr)) gt -1,count)
 if (count gt 0) then out=append_arr(out,desc[i].desc)
endfor

if exist(out) then return,out else return,''

end


;------------------------------------------------------------------------------
;-- SHOW_SYNOP definition

pro show_synop__define

self={show_synop,save_time:0.d,sort_mode:0,ptr:ptr_new(), inherits synop}

return & end
