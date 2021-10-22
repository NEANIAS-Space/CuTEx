function lasco_time2nrlwww, time0 , time1, c2=c2, c3=c3, $
   e171=e171, e195=e195, e284=e284, e304=e304, naxis1=naxis1, nx=nx, $
   small=small, medium=medium, large=large, refresh=refresh, $
   difference=difference, $
   times=times, count=count

;+
;   Name: lasco_time2nrlwww
;
;   Purpose: map times & instrument & resolution -> NRL graphics URLs
;
;   Input Parameters:
;      time0,time1 - desired time range (only last few months, I think)
;
;   Keyword Parameters:
;      c2,c3,e171,195,e284,e304 - desired instrument
;      small, medium, large - (switches) resolution =>{256,512,1024}
;      naxis1 & nx - optional way to state resolution in pixel#  256,512,1024
;      difference (switch) - if set, difference images
;      refresh - force relist/reparse of nrl subdirectory 
;                (default caches->common for each resolution)
;      times (output) - times corresponding to returned urls
;
;   Calling Context:
;      ; Read most recent 6 hours of C2 images NRL -> 'data'
;      IDL> urls=lasco_time2nrlwww(reltime(hours=-6),reltime(/now),/c2,/small)
;      IDL> ssw_jsurl2data,dummy,data,ingraphics=urls ,times=tt,/get_time
;      --------------------------OUT-------------IN---------OUT---------
;      IDL> help,tt,data
;      TT              STRING    = Array[12]
;      DATA            BYTE      = Array[256, 256, 12]
;      
;      
;    Restrictions:
;       (more of a comment) - first call for any given resolution does
;       nrl listing/parsing - subsequent calls use cache so are much faster
;       Use /REFRESH to force reread/recache useful for NRT applications
;-

common lasco_time2nrl_graphics_blk1,ggb,tggb, ggs,tggs, ggt,tggt

if n_elements(time0) eq 0 then time0=reltime(days=-1)
if n_elements(time1) eq 0 then time1=reltime(time0,/days)

top='http://lasco-www.nrl.navy.mil/javagif/'

case 1 of
   keyword_set(naxis1): size=naxis1
   keyword_set(nx): size=nx
   else: size=-1
endcase

case 1 of
   size eq 1024 or keyword_set(large): type='gifs' 
   size eq 512 or keyword_set(medium): type='gifs_small'
   size eq 256 or keyword_set(small):  type='gifs_tiny'
   else: type='gifs_small'
endcase 


case 1 of 
   keyword_set(c2): inst='c2'
   keyword_set(c3): inst='c3'
   keyword_set(e195): inst='eit_195'
   keyword_set(e171): inst='eit_171'
   keyword_set(e284): inst='eit_284'
   keyword_set(e304): inst='eit_304'
   else: inst='c3'
endcase

if keyword_set(difference) then inst='d'+strtrim(strmid(inst,1,10),2)

types=strmid(type,5,1)  ; {'','s','t'}

tcom='gg'+types
tcomt='t'+tcom
estat=execute('need=n_elements('+tcom+') eq 0')
retval=''
gurl=top+type+'/'
if need or keyword_set(refresh) then begin
   sock_list,gurl,nrll
   ssg=where(strpos(nrll,'.gif') ne -1 and strpos(nrll,'href=') ne -1,gcnt)
   if gcnt eq 0 then begin 
      box_message,['No images found for url:',gurl]
      return,''
   endif
   gifs=strextract(nrll(ssg),' href="','.gif')
   gifs=strarrcompress(gifs)+'.gif'
   gifs=gifs(where(strlen(gifs) gt 14 and strlen(gifs) lt 30))
   estat=execute(tcom+'=gifs')
   estat=execute(tcomt+'=anytim(file2time(gifs))')

endif

estat=execute('tint='+tcomt)
estat=execute('gint='+tcom)
ssg=where(strpos(gint,inst+'.gif') ne -1,gcnt)
if gcnt eq 0 then begin
   box_message,'No files for instrument='+inst
   return,''
endif

gint=gint(ssg)
tint=tint(ssg)

t0=anytim(time0)
t1=anytim(time1)
sst=where(tint ge t0 and tint le t1,count)

if count eq 0 then begin
   box_message,'No files for instrument '+inst + ' it time range...'
   retval=''
endif else begin 
   retval=gurl+gint(sst)
   ftimes=anytim(tint,/ecs)
endelse

return,retval
end   
   
