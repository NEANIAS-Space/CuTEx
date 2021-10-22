; Project     : SOLAR MONITOR
;
; Name        : SOLMON__DEFINE
;
; Purpose     : Define a SOLAR MONITOR data object
;
; Category    : Ancillary Synoptic Objects
;
; Syntax      : IDL> solmon=obj_new('solmon')
;
; Example     : IDL> solmon=obj_new('solmon')
;               IDL> solmon->plot_campare,instrument=['eit','eit','eit'],filter=['171','195','304']
;
; History     : Written 28-Jun-2007, Paul Higgins, (ARG/TCD)
;               Beta Version 9-Jul-2007, Paul Higgins, (ARG/TCD)
;               Version 0 12-Jul-2007, Paul Higgins, (ARG/TCD)
;
; Tutorial    : 
;
; Contact     : P.A. Higgins: era {at} msn {dot} com
;               P. Gallagher: peter.gallagher {at} tcd {dot} ie
;-->
;----------------------------------------------------------------------------->

;----------------------------------------------->
;-- The help procedure listing all of SOLMON's object commands

pro solmon::help

print,' '
print,'*** The Solar Monitor - SOLMON Instrument Object ***'
print,'Astrophysics Research Group - Trinity College Dublin'
print,' '
print,'Version: Alpha - June 28 2007 - Paul Higgins'
print,' '
print,'General Object Commands:'
print,' '
print,"IDL> solmon = obj_new('solmon')					;-- Creates the SOLMON object."
print,"IDL> solmon->set,instrument='xrt'				;-- Specifies the instrument to be analyzed.
print,"IDL> files = solmon->list(time='4-jun-2007')		;-- Lists the files in the given range."
print,"IDL> solmon->read,time='4-jun-2007'				;-- Reads the files in the given range into the object."
print,"IDL> data = solmon->getdata('4-jun-2007')		;-- Retrieves the data and, optionally, the headers and" 
print,"IDL>												;-- file names saved in the object"
print,"IDL> maps = solmon->getmap('4-jun-2007')			;-- Retrieves the data maps saved in the object."
print,"IDL> time = solmon->get(/time)					;-- Retrieves the value of the specified keyword."
print,"IDL> solmon->latest								;-- Reads the latest data file available for SOLMON.
print,"IDL> solmon->plot,time='20-apr-2007 04:20:00'	;-- Plots the data set with the date closest to that specified."
print,"IDL> obj_destroy,solmon							;-- Destroys the object, freeing precious memory."
print,' '

return
end

;-------------------------------------------------------------------->

FUNCTION solmon::INIT, SOURCE = source, _EXTRA=_extra


RET=self->Framework::INIT( CONTROL = xrt_control(), $

                           INFO={xrt_info}, $

                           SOURCE=source, $

                           _EXTRA=_extra )

self.data = ptr_new(/allocate)
self.map = ptr_new(/allocate)
self.headers = ptr_new(/allocate)
self.filelist = ptr_new(/allocate)
self.filescopied=ptr_new(/allocate)
self.filesread = ptr_new(/allocate)
self.setstart = ptr_new(/allocate)
self.setend = ptr_new(/allocate)

self._explot = ptr_new(/allocate)
self.plot_prop = ptr_new(/allocate)
*(self.plot_prop) = obj_new('plot_prop')

self.sat_prop = ptr_new(/allocate)
self.filter_prop = ptr_new(/allocate)

dummy=self->solmon_config::init()

self->set,instrument='eit'
self->set,filter='195'

self->server


RETURN, RET



END

;-------------------------------------------------------------------->



PRO solmon::Set, $
       instrument=instrument, $
       filter=filter, $
       use_config=use_config, $

       PARAMETER=parameter, $

       _EXTRA=_extra

if keyword_set(use_config) then *(self.use_config)=use_config

if keyword_set(instrument) then begin

	self.map=ptr_new(/allocate)
	self.filesread=ptr_new(/allocate)
	self.filelist=ptr_new(/allocate)
	self.filescopied=ptr_new(/allocate)

;--<< BEGIN INSTRUMENT DEFINITIONS >>

;--<< XRT PROPERTIES >>


	if instrument eq 'xrt' then begin
		filter_prop=''
	
		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'http://sohowww.nascom.nasa.gov',ftype:'*.fits',path:'/sdb/hinode/xrt/l1q_synop'}, $
		xstd:2100,ystd:2100,loadct:3,hasfilter:0,unisize:0,arch_type:1}
	endif

;--<< GONG PROPERTIES >>

	if instrument eq 'gong' then begin
		filter_prop=''
	
		sat_prop={explot:{log:'',grid:1,center:1}, $
		plot_prop:{log:'',grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org',ftype:'*_fd_*.fts*',path:['/data/','insert','/fits/gong']}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:0,unisize:0,arch_type:2}
	endif

;--<< BBSO PROPERTIES >>

	if instrument eq 'bbso' then begin
		filter_prop=''
		
		sat_prop={explot:{log:'',grid:1,center:1}, $
		plot_prop:{log:'',grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org',ftype:'*_fd_*.fts*',path:['/data/','insert','/fits/bbso']}, $
		xstd:2100,ystd:2100,loadct:3,hasfilter:0,unisize:0,arch_type:2}
	endif

;--<< SXI PROPERTIES >>

	if instrument eq 'sxi' then begin
		filter_prop=''

		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org',ftype:'*_fd_*.fts*',path:['/data/','insert','/fits/gsxi']}, $
		xstd:1100,ystd:1100,loadct:3,hasfilter:0,unisize:0,arch_type:2}
	endif

;--<< EIT PROPERTIES >>

	if instrument eq 'eit' then begin
		filter_prop={f195:['8','195',''],f171:['1','171',''],f304:['3','304',''],f284:['8','284','']}
	
		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org', $
			ftype:['*','insert', '*_fd_','*.fts*'], $
			path:['/data/','insert','/fits/seit']}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:1,def_filt:'195',unisize:0,arch_type:2}
	endif
	
;--<< SMDI PROPERTIES >>

	if instrument eq 'smdi' then begin
		filter_prop={fmaglc:['0','maglc',''],figram:['1','igram','']}
		
		sat_prop={explot:{grid:1,center:1}, $
		plot_prop:{grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org', $
			ftype:['*','','*_fd_','*.fts*'], $
			path:['/data/','insert','/fits/smdi']}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:1,def_filt:'maglc',unisize:0,arch_type:2}
	endif
	
;--<< MDI PROPERTIES >>

	if instrument eq 'mdi' then begin
		filter_prop={fmaglc:['0','fd','/mdi_mag'],figram:['1','int','/mdi_int']}
	
		sat_prop={explot:{dmin:1,dmax:1,grid:1,center:1}, $
		plot_prop:{dmin:-200,dmax:200,grid:15,center:[0,0]}, $
		fspan:{url:'http://solarmonitor.org', $
			ftype:['*_','insert','_*.fits'], $
			path:'insert'}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:1,def_filt:'maglc',unisize:0,arch_type:1}
	endif
	
;--<< TRACE PROPERTIES >>

	if instrument eq 'trace' then begin
		filter_prop=''
	
		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org',ftype:'*_fd_*.fts*',path:['/data/','insert','/fits/trce']}, $
		xstd:1100,ystd:1100,loadct:8,hasfilter:0,unisize:0,arch_type:2}
	endif

;--<< END INSTRUMENT DEFINITIONS >>

;--<< Configuration File >>
;	files=file_search('solmon_config__define.pro')
	use_config=*(self.use_config)
;	if files[0] eq 'solmon_config__define.pro' then begin
		if use_config eq 1 then self->solmon_config::config,sat_prop,filter_prop,instrument=instrument
;	endif
;--<< >>

	if datatype(sat_prop) ne 'STC' then begin
		print,' '
		print,strcompress(instrument,/remo)+' is not a SOLMON instrument'
		print,' '
		return
	endif

	*(self.sat_prop) = sat_prop
	*(self.filter_prop) = filter_prop
	
	if sat_prop.hasfilter eq 1 then self->set,filter=sat_prop.def_filt
	
	*(self._explot) = sat_prop.explot
	plot_prop = *(self.plot_prop)
	plot_prop->set,_extra=sat_prop.plot_prop
	self->restoreplot

endif


IF Keyword_Set(instrument) THEN BEGIN

	self->Framework::Set,instrument=instrument
	self->server

ENDIF 

;--<< Instrument Specific! >>
;--<< May cause problems!! >>
if keyword_set(filter) then begin
	sat_prop=*(self.sat_prop)
	if sat_prop.hasfilter eq 1 then begin
		self.map=ptr_new(/allocate)
		self.filesread=ptr_new(/allocate)
		self.filelist=ptr_new(/allocate)
		self.filescopied=ptr_new(/allocate)
		self->Framework::Set,filter=strcompress(filter,/remo)
	endif
endif
;--<< >>


IF Keyword_Set( _EXTRA ) THEN BEGIN

;--<< Set the plot keywords in the PLOT_PROP object. >>

	plot_prop=*(self.plot_prop)
	plot_prop->set, _EXTRA = _extra

;--<< Remember which plot keywords have been set. >>

	zero={zero:''}
	_explot=*(self._explot)
	extratag=tag_names(_extra)
	plottag=tag_names(_explot)

	for i=0,n_elements(extratag)-1 do begin 

		thisprop=CREATE_STRUCT(extratag[i], 1, zero)
		thisprop=rem_tag(thisprop,'zero')
		testprop=plot_prop->get(_extra=thisprop)
		
		wtag=where(extratag[i] eq plottag)
;--> May cause problems, since I'm assuming testprop=-1 means that prop doesn't exist...
		if wtag[0] eq -1 and testprop[0] ne -1 then _explot=CREATE_STRUCT(extratag[i], 1, _explot)
	endfor

	*(self._explot)=_explot


	self->Framework::Set, _EXTRA = _extra

ENDIF



END

;----------------------------------------------->

function solmon::filter_path,filter_prop=filter_prop,fcolor=fcolor,fpath=fpath,ftype=ftype;instrument=instrument,filter=filter

sat_prop=*(self.sat_prop)

if not keyword_set(filter_prop) then filter_prop=*(self.filter_prop)
if datatype(filter_prop) ne 'STC' then return,''

filt_tags=strlowcase(tag_names(filter_prop))

for i=0,n_elements(filt_tags)-1 do begin
filt_tags[i] = STRMID(filt_tags[i], 1, STRLEN(filt_tags[i]))
endfor

wfilt=where(filt_tags eq self->get(/filter))

if wfilt[0] eq -1 then begin
	print,' '
	print,'Filter name not found.'
	print,' '
	self->set,filter=sat_prop.def_filt
	value=self->filter_path(filter_prop=filter_prop,fcolor=fcolor,fpath=fpath,ftype=ftype)
	return,value
endif

filteproprarr=filter_prop.(wfilt[0])

if keyword_set(fpath) then return, filteproprarr[2]
if keyword_set(ftype) then return, filteproprarr[1]
if keyword_set(fcolor) then return, filteproprarr[0]

return,''

end

;----------------------------------------------->

FUNCTION solmon::Get, $

                  NOT_FOUND=NOT_found, $

                  FOUND=found, $
                  use_config=use_config, $

                  PARAMETER=parameter, $

                  _EXTRA=_extra 

if keyword_set(use_config) then begin
	return,*(self.use_config)
endif

RETURN, self->ultimon::Get( $;PARAMETER = parameter, $

                              ;NOT_FOUND=not_found, $

                              FOUND=found, _EXTRA=_extra )
end

;----------------------------------------------->

function solmon::data_process,maparr

if self->get(/use_config) eq 1 then begin
	maparr_process=self->solmon_config::data_process(maparr)
endif else maparr=maparr_process

return,maparr_process

end

;----------------------------------------------->

pro solmon::plot_compare,instrument=instrument,filter=filter,_extra=_extra,timerange=timerange,pmulti=pmulti,wsize=wsize,wset=wset

if not keyword_set(instrument) then instrument=['eit','eit','mdi']
if not keyword_set(filter) then filter=strarr(n_elements(instrument))
if not keyword_set(timerange) then timerange=self->get(/timerange)
if not keyword_set(pmulti) then pmulti=[0,n_elements(instrument),1]
timerange=anytim(timerange[0],/vms)

if n_elements(instrument) ne n_elements(filter) then begin
	print,' '
	print,'The number of elements of FILTER and INSTRUMENT must be equal.'
	print,' '
	return
endif

self->set,instrument=instrument[0]
if filter[0] ne '' then self->set,filter=filter[0]
self->multiplot,_extra=_extra,pmulti=pmulti,wsize=wsize,wset=wset, timerange=timerange

if n_elements(instrument) gt 1 then begin
	for i=1,n_elements(instrument)-1 do begin
	
		self->set,instrument=instrument[i]
		if filter[i] ne '' then self->set,filter=filter[i]
		maparr=self->getmap(_extra=_extra, timerange=timerange)
		
		if datatype(maparr[0]) eq 'STC' then self->setplotspecs, maparr[0],_extra=_extra
	endfor
endif

return

end

;----------------------------------------------->
;-- Calls a plot manager for the specified input image.

pro solmon::plotman,timerange=timerange,filelist=filelist,_extra=exset

sat_prop = *(self.sat_prop)

plottype='image'

if keyword_set(timerange) then timerange=anytim(timerange[0],/vms)
if keyword_set(filelist) then filelist=filelist[0]
if not keyword_set(timerange) and not keyword_set(filelist) then begin
	timerange=self->get(/timerange)
	timerange=anytim(timerange[0],/vms)
endif
map=self->getmap(timerange=timerange,filelist=filelist,_extra=exset)
if self->findnumimgs() eq 0 then return
map=map[0]

;--->
self->set, _EXTRA = exset
plot_prop=*(self.plot_prop)
plot_prop->set, _EXTRA = exset
_explot=*(self._explot)
ex = plot_prop->get(_EXTRA=_explot)
;--->

if not keyword_set(colortable) and sat_prop.hasfilter eq 0 then ex = CREATE_STRUCT('colortable', sat_prop.loadct, ex)

;--<< Instrument specific... >>
;filterct=self->get_loadct()
filterct=self->filter_path(/fcolor)
if not keyword_set(colortable) and sat_prop.hasfilter eq 1 then ex = CREATE_STRUCT('colortable', filterct, ex)
;--<< Loadsct color for eit >>

if not keyword_set(xtitle) then ex = CREATE_STRUCT('xtitle', map.xunits, ex)
if not keyword_set(ytitle) then ex = CREATE_STRUCT('ytitle', map.yunits, ex)
if not keyword_set(title) then ex = CREATE_STRUCT('title', map.id+' '+map.time, ex)

;--<< PROCESS DATA >>
map=self->data_process(map)
;--<< >>

plotman_obj = plotman(input=map[0], plot_type=plottype,_extra=ex)

return

end

;----------------------------------------------->
;-- Animate a series of images

pro solmon::movie,timerange=timerange,filelist=filelist,rescale=rescale

if not keyword_set(rescale) then rescale=4

sat_prop = *(self.sat_prop)

xstd=sat_prop.xstd
ystd=sat_prop.ystd
xrebin=xstd/rescale
yrebin=ystd/rescale

data=self->getdata(timerange=timerange,filelist=filelist)

if data[0] eq '' then return 

;--<< Instrument specific... >>
	if sat_prop.hasfilter eq 0 then loadct,sat_prop.loadct
	if sat_prop.hasfilter eq 1 then loadct,self->filter_path(/fcolor)
;--<< >>

	mapset=*(self.map)
	testmap=mapset[0]
	testdata=testmap.data

	data=*(self.data)

;help,data
	numimg=self->findnumimgs()
	if sat_prop.unisize eq 1 then imgsz=size(testdata) else imgsz=size(data[*,*,0])

	imagearr=fltarr(imgsz[1],imgsz[2],numimg)
	for i=0,numimg-1 do begin
		if sat_prop.unisize eq 1 then $
			imagearr[*,*,i]=self->extractplot(data[*,*,i]) else $
			imagearr[*,*,i]=data[*,*,i]
	endfor
	data=imagearr

;help,data

	plot_prop=sat_prop.plot_prop

	plot_tag=tag_names(plot_prop)
	wlog=where(plot_tag eq 'log')
	if wlog[0] ne -1 then begin
		if plot_prop.log eq 1 then datacal=alog(abs(data)+1) else datacal=data
	endif else begin
		datacal=data
	endelse
	
	datacal=self->movie_process(datacal)

;help,data
	
	imgsz=size(data)
	;images=datacal
	;images=rebin(datacal,xrebin,yrebin,numimg)
	images = CONGRID( datacal,xrebin,yrebin,numimg, /CENTER, /INTERP)

;help,images

	XMOVIE, IMAGES, 50

return

end

;----------------------------------------------->
;-- Outputs a 3D (if more than one set) array of the data sets contained in the object.

function solmon::getdataext,dataset

sat_prop=*(self.sat_prop)

numimgs=self->findnumimgs()
testmap=*(self.map[0])
imgsz=size(testmap.data)

if sat_prop.unisize eq 1 or numimgs eq 1 then begin
	extractset=fltarr(imgsz[1],imgsz[2],numimgs)
	for i=0,numimgs-1 do begin
		extractset[*,*,i]=self->extractplot(dataset[*,*,i])
	endfor

endif else extractset=dataset

return,extractset

end

;----------------------------------------------->
;-- Routine for setting the plot color and secifics.

pro solmon::setplotspecs,imagemap,_extra=exset

sat_prop=*(self.sat_prop)

self->set, _EXTRA = exset
plot_prop=*(self.plot_prop)
plot_prop->set, _EXTRA = exset
_explot=*(self._explot)
ex = plot_prop->get(_EXTRA=_explot)

;--<< Instrument specific >>
if sat_prop.hasfilter eq 1 then loadct,self->filter_path(/fcolor)
;--<< Loadsct color for eit >>

;--<< PROCESS DATA >>
imagemap=self->data_process(imagemap)
;--<< >>

plot_map,imagemap[0],_extra=ex

return

end

;----------------------------------------------->
;-- Converts a fits filelist to a map structure and saves it to the object.

pro solmon::savefits2map,filelist

fits2map,filelist,maparr
*(self.map)=maparr

return

end

;----------------------------------------------->

pro solmon::copy,_extra=_extra

sat_prop=*(self.sat_prop)

if sat_prop.arch_type eq 0 then begin

endif

if sat_prop.arch_type ne 0 then begin
	self->ultimon::copy,_extra=_extra
endif


return

end

;----------------------------------------------->
;-- Converts a list of full path file name to a list of local file names.

function solmon::fullpath2filename,filelist

sat_prop=*(self.sat_prop)

;--<< might cause problems >>
wblank=where(filelist ne '')
if wblank[0] eq -1 then return,'' else filelist=filelist[wblank]

;--<< Archive Specific >>
if sat_prop.arch_type eq 0 then return,filelist
;--<< >>

filenamelist=strsplit(filelist[0],'/',/extract)
testfname=filenamelist

if n_elements(filenamelist) gt 1 then begin

	for i=1,n_elements(filelist)-1 do begin
		nextfname=strsplit(filelist[i],'/',/extract)
		filenamelist=[[filenamelist],[nextfname]]
	endfor
	sizelist=size(filenamelist)
	filenamelist=filenamelist[n_elements(testfname)-1,*]

endif else begin
	filenamelist=filelist
endelse

return,filenamelist

end

;----------------------------------------------->

function solmon::filter,filelist,sttime,entime,_extra=exfilt

minelem=8

wfilt1=where(filelist ne '')
if wfilt1[0] eq -1 then begin
	print,' '
	print,'No files were found within the specified range.'
	print,' '
flistfilt=''
return,flistfilt
endif else begin
	filelist=filelist[wfilt1]
endelse

tlist=self->flist2tlist(filelist)
wfilt=where(tlist ge sttime and tlist le entime)

if wfilt[0] ne -1 then begin
	flistfilt=filelist[wfilt]
endif else begin
	print,' '
	print,'No files were found within the specified range.'
	print,' '
flistfilt=''
return,flistfilt
endelse

flistfilt=self->checkfilerepeat(flistfilt)

return,flistfilt

end

;----------------------------------------------->

function solmon::listgen,tstart,tend,range=range,settime=settime

sat_prop=*(self.sat_prop)

fspan=self->fullpath(tstart,tend)

ftype=fspan.ftype
fpath=fspan.fpath

;--<< Select files for a specific filter. >>
if sat_prop.hasfilter eq 1 then begin
	wputfilt=where(ftype eq 'insert')
	if wputfilt[0] ne -1 then begin
		ftype[wputfilt]=self->filter_path(/ftype)
	endif
	ftype=strjoin(ftype,'')
endif
;--<< >>

ftype=strjoin(ftype,'')

;--<< Solarmonitor, date based archive structure. >>
fpath=self->alterpath(fpath)

if sat_prop.arch_type eq 2 then begin
	filelist=''
	for i=0,n_elements(fpath)-1 do begin
		filelist=[filelist,sock_find(fspan.url,ftype,path=fpath[i])]
	endfor
	filelist=self->checkfilerepeat(filelist)
	return,filelist
endif
;--<< >>

;--<< Select a path for a specific filter. >>
if self->filter_path(/fpath) ne '' then begin
	wputfilt=where(fpath eq 'insert')
	if wputfilt[0] ne -1 then begin
		fpath[wputfilt]=self->filter_path(/fpath)
	endif
	fpath=strjoin(fpath,'')
endif
;--<< >>

fpath=strjoin(fpath,'')

;--<< Archive of files on a local directory. >>
if sat_prop.arch_type eq 0 then begin
	filelist=file_search(fpath,strjoin(ftype,''))
	return,filelist
endif
;--<< >>

;--<< Simple, single directory, remote archive. >>
if sat_prop.arch_type eq 1 then begin
	filelist=sock_find(fspan.url,ftype,path=fpath)
	return,filelist
endif

;--<< >>

return,''
end

;----------------------------------------------->

function solmon::fullpath,tstart,tend

sat_prop = *(self.sat_prop)
fspan=sat_prop.fspan

tstartarr=self->pathogen(tstart)
tendarr=self->pathogen(tend)

fstart=time2file(anytim(tstart,/vms,/date),/date)
fend=time2file(anytim(tend,/vms,/date),/date)

;---->
url=fspan.url
ftype=fspan.ftype
if sat_prop.arch_type eq 0 then path=fspan.path
if sat_prop.arch_type eq 1 then path=fspan.path
if sat_prop.arch_type eq 2 then path=self->timearrgen(tstart,tend)
;---->
fspan={fstart:fstart,fend:fend,url:url,ftype:ftype,fpath:path}

return, fspan

end

;----------------------------------------------->

pro solmon::server,status,time

sat_prop=*(self.sat_prop)
if sat_prop.arch_type eq 0 then begin
	time=1
	status=1
	return
endif

fspan=self->fullpath(systim(/utc),systim(/utc))
sock_ping,fspan.url,status,time=time

if status eq 1 then begin

endif
if status eq 0 then begin
	print,' '
	print,'The server '+fspan.url+' is down.'
	print,' '
	wait,.5
endif

return

end

;----------------------------------------------->

PRO solmon__Define

self = {solmon, filter_prop:ptr_new(), INHERITS ultimon, INHERITS solmon_config}

END

;----------------------------------------------->