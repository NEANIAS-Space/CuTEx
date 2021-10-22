;+
; Project     : SXI
;
; Name        : sxi_latest
;
; Purpose     : Searches, downloads, and plots latest GOES12/SXI Level-1 image
;               from NGDC. 
;
; Category    : Synoptic display
;
; Syntax      : IDL> sxi_latest,file
;
; Inputs      : None
;
; Outputs     : FILE = name of FITS file found and plotted.
;               Plot to current window, or PNG file (/PNG)
;
; Keywords    : PNG = set to plot PNG
;               OUT_DIR = directory to download FITS file and create PNG file
;               BACK = # of days to look back [def = 3]
;               CLEANUP = cleanup downloaded files
;               NOPLOT = skip plotting to screen
;               GIF = set to plot GIF
;               HOURS = limit search to HOURS [def= 24]
;               NO_ROLL = do not correct for ROLL
;
; History     : Written 8 March 2003, Zarro (EER/GSFC)
;               Modified, 13-Aug-2003, William Thompson
;                      - Use SSW_WRITE_GIF instead of WRITE_GIF
;               Modified, 3-Jan-2006, Zarro (L-3Com/GSFC)
;                      - remove COMMON block
;               Modified, 28-Jul-2006, Zarro (ADNET/GSFC)
;                      - added SATELLITE keyword
;
; Contact     : dzarro@solar.stanford.edu
;-

pro sxi_latest,file,png=png,_ref_extra=extra,label=label,cleanup=cleanup,$
               help=help,noplot=noplot,gif=gif,no_roll=no_roll,err=err,$
               satellite=satellite

;-- create SXI object

sxi=obj_new('sxi')

if keyword_set(help) then begin
 sxi->filters
 obj_destroy,sxi
 return
endif

;-- search and download latest SXI FITS file at NGDC

sxi->hset,satellite=satellite
sxi->latest,file,err=err,_extra=extra,/no_warn
if is_string(err) or (sxi->get(/count) eq 0) then begin
 obj_destroy,sxi
 return
endif

;-- restore initial plot device in case of error

psave=!d.name
error=0
catch,error
if error ne 0 then begin
 set_plot,psave
 catch,/cancel
 obj_destroy,sxi
 return
endif

;-- plot it

if keyword_set(gif) then begin
 if not allow_gif(err=err) then begin
  message,err,/cont
  message,'Switching to PNG',/cont
  gif=0b & png=1b
 endif
endif

png_gif=keyword_set(png) or keyword_set(gif)

if (1-keyword_set(no_roll)) then sxi->rotate,roll=0
if (not png_gif) and (not keyword_set(noplot)) then begin
 sxi->plot,limb=0,/log,/noaxes,font=0,grid=0,_extra=extra
endif

if png_gif then begin
 set_plot,'z',/copy
 device,set_resolution=[512,512]
 sxi->plot,/log,/noaxes,font=0,grid=0,_extra=extra,limb=0
 data=tvrd()
 tvlct,r,g,b,/get
 break_file,file,dsk,dir,name
 if keyword_set(gif) then begin
  file=dsk+dir+name+'.gif'
  ssw_write_gif,file,data,r,g,b
  message,'Wrote GIF file - '+file,/cont
 endif else begin
  file=dsk+dir+name+'.png'
  write_png,file,data,r,g,b
  message,'Wrote PNG file - '+file,/cont
 endelse
 set_plot,psave
endif

label=sxi->get(/id)+' Filter '+sxi->get(/time)

if keyword_set(cleanup) and (not keyword_set(png)) then file_delete,file,/quiet

obj_destroy,sxi

return & end
