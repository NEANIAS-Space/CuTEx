function trace_movies_prioritize2, mordered, mcount=mcount, $
    wave_len=wave_len, recent=recent, frequency=frequency, moviedir=moviedir, $
    javascript=javascript, gif_animate=gif_animate, mpeg=mpeg, $
    anchors=anchors
;
;+
;   Name: trace_movies_prioritize
;  
;   Purpose: return priortized movie URLs 
;  
;   Input Parameters:
;      NONE:
;
;   Output:
;      Function returns movie URLS, most->least priority
;
;   Output Paramteres:
;      mordered - movie info structures, most->least priority
;  
;   Keyword Parameters:
;
;      wave_len  - one or more wavelenths (returns 1st matching)
;      recent    -if set, timeliness is most important for given WAVE_LEN
;      frequency - if set, frequency is most important param for WAVE_LEN  
;      mcount (output) - number of movies matching criteria (zero if no match)
;      javascript/gif_animate/mpeg - type of URL to return
;      anchors (output) - corresponding top level html anchor point
;                         associated with output urls
;
;   Calling Sequence:
;      urls=trace_movies_prioritize(wave_len='xxx' [,/frequency]  [,/recent] $
;                                   [mcount=mcount],  $
;                                   [,/javascript] [,/mpeg] [,/gif_animate]  
;
;   History:
;      10-May-2000 - S.L.Freeland - for auto WWW->display
;      18-May-2000 - S.L.Freeland - add ANCHORS output 
;      11-Oct-2005 - Zarro (L-3Com/GSFC) - expanded wavelength search
;
;-
mcount=0  
if not data_chk(moviedir,/string) then $
    moviedir=concat_dir('path_http','last_movies')
if not file_exist(moviedir(0)) then begin
    box_message,['Cannot find $path_http/last_movies or MOVIEDIR, returning..']
    return,''
endif    

if not data_chk(wave_len,/string) then wave_len='171,195,1550,1600,1700'

if n_elements(wave_len) eq 1 then waven=str2arr(wave_len) else waven=wave_len

genxfile=(concat_dir(moviedir,'last_movie_log.geny'))(0)
if not file_exist(genxfile) then begin
   box_message,'No LAST MOVIE log: '  + genxfile + ', returning...'
   return, ''
endif

restgenx,file=genxfile,t0,t1,movieinfo

nwaves=n_elements(waven)

;wcnt=0
;repeat begin
;   ss=where(gt_tagval(movieinfo,/WAVE_LEN,missing='') eq waven(wcnt),sscnt)
;   wcnt=wcnt+1
;endrep until (sscnt gt 0) or wcnt eq nwaves

match,movieinfo.wave_len,waven,ss

if ss[0] eq -1 then begin
   box_message,'No movies for desired wavelenth(s) found, returning...'
   return,''   
endif

movieinfo=movieinfo[ss]

nmovie=n_elements(movieinfo)

freq=gt_tagval(movieinfo,/NSS,missing=0)

case 1 of
   nmovie eq 1: mordered=movieinfo
   keyword_set(frequency): mordered=movieinfo(reverse(sort(movieinfo.nss)))
   keyword_set(recent): begin
      stoptimes=fltarr(nmovie)
      for i=0,nmovie-1 do begin
         time_window,gt_tagval(movieinfo(i),/CATALOG),t0,t1,out='tai'   ; m start/stop
         stoptimes(i)=t1
      endfor
      mordered=movieinfo(reverse(sort(stoptimes)))
   endcase
   else: mordered=movieinfo(reverse(sort(movieinfo.nss)))
endcase

tophttp=get_logenv('top_http')

roots=gt_tagval(mordered,/UNAME)

case 1 of
   keyword_set(mpeg): exten='_m.mpg'
   keyword_set(gif_animate): exten='_g.gif'
   keyword_set(javascript): exten='_j.html'
   else: exten='.html'
endcase

; generate fully qualified movie urls, 'highest->lowest'
unames=roots+exten
urls=concat_dir(concat_dir('top_http','last_movies'),unames)

; corresponding movie page anchor point
anchors=concat_dir(concat_dir('top_http','last_movies'),         $
     'LATEST_TRACE_'+ gt_tagval(mordered,/OBS_PROG,missing='') + $
      '.html#' + roots)   

mcount=nmovie
return, urls

end

