; Project     : SOLAR MONITOR
;
; Name        : SOLMON_CONFIG__DEFINE
;
; Purpose     : Define a SOLAR MONITOR instrument configuration object
;
; Category    : Ancillary Synoptic Objects
;
; Syntax      : IDL> solmon=obj_new('solmon_config')
;
; History     : Written 11-Jul-2007, Paul Higgins, (ARG/TCD)
;               Beta Version 12-Jul-2007, Paul Higgins, (ARG/TCD)
;                  --Added SOLMON_CONFIG::DATA_PROCESS
;
; Contact     : P.A. Higgins: era {at} msn {dot} com
;               P. Gallagher: peter.gallagher {at} tcd {dot} ie
;-->
;----------------------------------------------------------------------------->

;-------------------------------------------------------------------->

FUNCTION solmon_config::INIT

self.use_config = ptr_new(/allocate)

;--<< CHANGE THIS VALUE TO 1 FOR THIS CONFIG FILE TO BE USED OR 0 TO TURN IT OFF. >>
*(self.use_config) = 1

return,1
end

;-------------------------------------------------------------------->

PRO solmon_config::config,instrument=instrument,sat_prop,filter_prop

if keyword_set(instrument) then begin

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
	
;--<< NSOGONG PROPERTIES >>

	if instrument eq 'nsogong' then begin
		filter_prop=''
	
		sat_prop={explot:{dmin:1,dmax:1,log:'',grid:1,center:1}, $
		plot_prop:{dmin:-200,dmax:200,log:'',grid:15,center:[0,0]}, $
		fspan:{url:'http://gong.nso.edu',ftype:'bb*.fits*',path:['/Daily_Images/bb/fits/','insert','']}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:0,unisize:0,arch_type:2}
	endif

;--<< BBSO PROPERTIES >>

	if instrument eq 'bbso' then begin
		filter_prop=''
		
		sat_prop={explot:{log:'',grid:1,center:1}, $
		plot_prop:{log:'',grid:15,center:[0,0]}, $
		fspan:{url:'http://www.solarmonitor.org',ftype:'*_fd_*.fts*',path:['/data/','insert','/fits/bbso']}, $
		xstd:2100,ystd:2100,loadct:0,hasfilter:0,unisize:0,arch_type:2}
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

;--<< DEIT PROPERTIES >>


	if instrument eq 'deit' then begin
		filter_prop={f195:['8','195',''],f171:['1','171',''],f304:['3','304',''],f284:['8','284','']}
	
		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'', $
			ftype:['*','insert','*_fd_','*.fts*'], $
			path:['~/data/eit']}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:1,def_filt:'195',unisize:0,arch_type:0}
	endif
	
;--<< DXRT PROPERTIES >>

	if instrument eq 'dxrt' then begin
		filter_prop=''
	
		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'',ftype:'*.fits',path:'~/data/xrt'}, $
		xstd:2100,ystd:2100,loadct:3,hasfilter:0,unisize:0,arch_type:0}
	endif
	
;--<< DDXRT PROPERTIES >>

	if instrument eq 'ddxrt' then begin
		filter_prop=''
	
		sat_prop={explot:{log:1,grid:1,center:1}, $
		plot_prop:{log:1,grid:15,center:[0,0]}, $
		fspan:{url:'',ftype:'*.fits',path:''}, $
		xstd:2100,ystd:2100,loadct:3,hasfilter:0,unisize:0,arch_type:0}
	endif
	
;--<< DMDI PROPERTIES >>

	if instrument eq 'dmdi' then begin
		filter_prop={fmaglc:['0','fd',''],figram:['1','int','']}

		sat_prop={explot:{dmin:1,dmax:1,grid:1,center:1}, $
		plot_prop:{dmin:-200,dmax:200,grid:15,center:[0,0]}, $
		fspan:{url:'', $
		ftype:['*_','insert','_*.fits'],path:'~/data/mdi'}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:1,def_filt:'maglc',unisize:0,arch_type:0}
	endif

;--<< DEITTEMP PROPERTIES >>

	if instrument eq 'deittemp' then begin
		filter_prop=''

		sat_prop={explot:{dmin:1,center:1}, $
		plot_prop:{dmin:900000,center:[0,0]}, $
		fspan:{url:'', $
		ftype:['solmon_eit_temp*.fits'],path:'~/idl/solarmonitor'}, $
		xstd:1100,ystd:1100,loadct:0,hasfilter:0,unisize:1,arch_type:0}
	endif

;--<< END INSTRUMENT DEFINITIONS >>

endif

print,' '
print,'Configuration Complete.'
print,' '

return
end

;----------------------------------------------->

function solmon_config::data_process,maparr

for i=0,n_elements(maparr)-1 do begin
	map=maparr[i]
	data=map.data

;--<< Process MDI-MAGLC >>
	if self->get(/instrument) eq 'mdi' and self->get(/filter) eq 'maglc' then begin

;		data=YOUR_FAVORITE_PROCESS(data)

	endif
;--<< >>

;--<< Process MDI-IGRAM >>
	if self->get(/instrument) eq 'mdi' and self->get(/filter) eq 'igram' then begin

;		data=YOUR_FAVORITE_PROCESS(data)

	endif
;--<< >>

;--<< Process DMDI-IGRAM >>
	if self->get(/instrument) eq 'dmdi' and self->get(/filter) eq 'igram' then begin

	endif
;--<< >>

;--<< Process EIT-284 >>
	if self->get(/instrument) eq 'eit' and self->get(/filter) eq '284' then begin

		eit_colors,284
;		data=exp(data)

	endif
;--<< >>

;--<< Process XRT >>
	if self->get(/instrument) eq 'xrt' then begin

		map=self->xrt::compensate(map)

	endif
;--<< >>

	map.data=data
	maparr[i]=map
endfor

maparr_process=maparr

return,maparr_process

end

;----------------------------------------------->

function solmon_config::alterpath,fpath

;--<< Alter NSOGONG >>
if self->get(/instrument) eq 'nsogong' then begin
		
	npath=n_elements(fpath)
	for i=0,npath-1 do begin
		thisfpath=fpath[i]
		thispath=strsplit(fpath[i],'/',/extract)
		thisdate=thispath[n_elements(thispath)-1]
		thispath[n_elements(thispath)-1]=time2file(file2time(thisdate),/date,/year2digit)
		fpath[i]=strjoin(thispath,'/')
	endfor
	
endif
;--<< >>

fpathnew=fpath

return, fpathnew

end

;----------------------------------------------->

function solmon_config::movie_process,dataset

;--<< Process DEITTEMP >>
if self->get(/instrument) eq 'deittemp' then begin
	
	dataset=dataset > 900000

endif
;--<< >>

dataset_process=dataset

return,dataset_process

end

;----------------------------------------------->

PRO solmon_config__Define

self = {solmon_config, use_config: ptr_new()}

END

;----------------------------------------------->