;+
; Project     : TRACE
;
; Name        : TRACE_JSMOVIE
;
; Category    : movies
;
; Purpose     : create Javascript HTML file for latest TRACE special movie
;
; Syntax      : IDL> trace_jsmovie,file,url
;
; Inputs      : FILE = Javascript movie file name
;
; Outputs     : URL = URL of latest movie
;
; Keywords    : None (for now)
;
; History     : 1-Oct-2005, Zarro (L-3Com/GSFC). Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-


pro trace_jsmovie,file,url,_extra=extra

common trace_jsmovie,http
if not obj_valid(http) then http=obj_new('http')

;-- find latest JS movies at Lockheed

 url=''
 movie_dir=get_temp_dir()
 top_http='http://trace.lmsal.com'
 gen_file=top_http+'/last_movies/last_movie_log.geny'
 sock_copy,gen_file,_extra=extra,/clobber,out_dir=movie_dir,copy=copy

 mklog,'top_http',top_http
 f=trace_movies_prioritize2(moviedir=movie_dir,/java)

;-- find first good URL
 
 for i=0,n_elements(f)-1 do begin
  if is_string(f[i]) then begin
   status=http->file_found(f[i])
   if status then begin
    url=f[i]
    break
   endif
  endif
 endfor

 if is_blank(url) then begin
  message,'No recent TRACE movies available',/cont
  return
 endif

;-- load URL into Web page

 openw,lun,file,/get_lun,error=err
 if err ne 0 then begin
  message,err_state(),/cont
  return
 endif

 a=['<html>',$
    '<head>',$
    '<title>TRACE Special Movie</title>',$
    '<meta http-equiv="Expires" content="-1">',$
    '<meta http-equiv="Pragma" content="no-cache">',$
    '<meta http-equiv="Cache-Control" content="no-cache">',$
    '<meta http-equiv="Refresh" content="3600">',$
    '</head>',$
    '<frameset rows="100%,*">',$
    '<frame src="'+url+'">',$
    '</frameset>',$
    '<meta http-equiv="Expires" content="-1">',$
    '<meta http-equiv="Pragma" content="no-cache">',$
    '<meta http-equiv="Cache-Control" content="no-cache">',$
    '</head>',$
    '</html>']

 printf,lun,a
 close_lun,lun
 message,'Completed Javascript movie for: '+url,/cont
 return
 end



