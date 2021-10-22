;-- Overlay LASCO C2/C3 and EIT images into an image file (def = png)
;   Written, 20-Feb-2007, Zarro (ADNET)

pro eit_lasco_over,struct,nsize=nsize,window=window,reprocess=reprocess,format=format,$
            files=files,out_dir=out_dir,_extra=extra


files=''
window=keyword_set(window)

if not window then begin
 if is_blank(out_dir) then out_dir=curdir()
 if ~write_dir(out_dir,err=err) then begin
  if is_string(err) then message,err,/cont
  return
 endif
endif

if ~is_struct(struct) then return
if is_blank(format) then format='png'         ;-- def image format
if ~is_number(nsize) then nsize=1024          ;-- def window size

nstruct=n_elements(struct)
fobj=obj_new('fits')
eobj=obj_new('eit')

;-- read each C3/C2/EIT triplet and create corresponding maps
;-- use Z-buffer unless /window is set

if window then begin
 set_plot,'x' & window,ysize=nsize,xsize=nsize,retain=2
endif else begin
 set_plot,'z'
 device,/close,set_resolution=[nsize,nsize]
endelse

for i=0,nstruct-1 do begin
 c3=struct[i].(0)
 c2=struct[i].(1)
 eit=struct[i].(2)

;-- Check if this overlay has already been done by checking if filename exists. 
;   Filename format is: yymmdd_c3_hhmm_c2_hhmm_eitwave_hhmm_nexp.format
;   where nexp= overlay image size (as a power of 2)

 if ~window then begin
  nexp=trim(exponent(nsize))
  ymd=(stregex(c3,'_([0-9]{8})_',/ext,/sub))[1]
  c3time=(stregex(c3,'_([0-9]{4})\.',/ext,/sub))[1]
  c2time=(stregex(c2,'_([0-9]{4})\.',/ext,/sub))[1]
  eittime=(stregex(eit,'_([0-9]{4})\.',/ext,/sub))[1]
  eitwave=(stregex(eit,'([0-9]{3})_fd',/ext,/sub))[1]
  filename=ymd+'_c3_'+c3time+'_c2_'+c2time+'_eit'+eitwave+'_'+eittime+'_'+nexp+'.'+format
  filename=concat_dir(out_dir,filename)
  ofile=append_arr(ofile,filename)
  if ~keyword_set(reprocess) and file_test(filename) then begin
   message,filename+' already processed',/continue
   continue
  endif
 endif

 fobj->read,c3
 c3map=fobj->get(/map)
 fobj->read,c2
 c2map=fobj->get(/map)
 eobj->read,eit
 eitmap=eobj->get(/map)

;-- center LASCO images on EIT

; c3map.xc=eitmap.xc
; c3map.yc=eitmap.yc
; c2map.xc=eitmap.xc
; c2map.yc=eitmap.yc

;-- plot each map

 loadct,8,bottom=0,ncolors=85
 plot_map,c3map,bottom=0,top=84,/noaxes,margin=.001

 loadct,1,bottom=85,ncolors=85
 plot_map,c2map,bottom=85,top=169,_extra=extra,comp=3

 eit_colors,eitwave,bottom=170,ncolors=86
 plot_map,eitmap,bottom=170,top=255,comp=3,/log,_extra=extra

;-- write image file

 if (!d.name eq 'Z') and ~window then begin
  message,'writing image to '+filename,/cont
  tvlct,r,g,b,/get
  image=tvrd()
  write_image,filename,format,image,r,g,b,_extra=extra,quality=100
 endif

endfor

;-- cleanup

obj_destroy,fobj
obj_destroy,eobj

if is_string(ofile) then files=ofile

return & end
