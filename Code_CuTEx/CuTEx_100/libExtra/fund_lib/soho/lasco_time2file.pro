function lasco_time2file, time0, time1, fcount, $
   nrl_url=nrl_url, gsfc_url=gsfc_url, confirm=confirm, $
   before=before, after=after, deltat=deltat, $
   level=level, quicklook=quicklook, debug=debug, $  
   c1=c1, c2=c2, c3=c3, c4=c4, ftimes=ftimes, force_times=force_times
;
;+
;  Name: lasco_time2file
; 
;  Purpose: map from SSW times[ or time range ->  LASCO urls
;
;  Input Parameters:
;      time0 - vector of times or start time of range
;      time1 - optional stop time of range
;   
;  Output Parameters:
;     fcount - number of files or urls returned
;
;  Keyword Parameters:
;     before - if set & one param, return closest BEFORE time0 (def=closest)
;     after  - if set & one param, return closest AFTER  time0 (def=closest) 
;     deltat - deltaT(desired:actual) in minutes (negative~before)
;     c1,c2,c3,c4 - desired telescope (default=C2)
;     level - processing level (default = 05)
;     nrl_url - if set, function returns full NRL URLs
;     gsfc_url     - if set, function returns full gsfc urls
;     confirm - if switch, only return files/urls which actually exist
;               on server; default is "idealized" list
;               (fcount always reflects number of elements returned) 
;     force_times - if set, force header read + update of FTIMES even if
;                   whole days are specified
;
;  Calling Sequence:
;      lasc2=lasco_time2file(time0,time1 ,/c2 ...   ) ; Time range
;      lasc3=lasco_time2file(time0, /AFTER , /c3,lev='05'     ) ; 1st After T0
;      lasc2=lasco_time2file(time0, /BEFORE, /c2,lev='2'{     ) ; 1st Before T0
;      lasc2l1=lasco_time2file(time0,level=1) ; Level 1     
;
;  History:
;     25-Nov-2003 S.L.Freeland - long planned breakout of of a
;                                local CoSEC/SSW socket "service"
;                 Some heritage from sxi_files.pro
;                 use standalone 
;     8-apr-2004 - S.L.Freeland - from mdi_time2file analog
;    19-jan-2005 - S.L.Freeland - allow mixed case html file listings@nrl
;    31-aug-2005 - J-P.Wuelser  - to get /c4 and /quicklook to work
;    20-sep-2006 - S.L.Freeland - allow .fts.gz (Level1 gzipped for example)
;
;  Restrictions:
;     Use of /CONFIRM requires IDL +5.4 (RSI socket utility)
;     If remote files are compressed (example is nrl level1 .gz files), 
;     then time trimming cannot be done - only full days urls returned 
;     in that case and user can copy/trim them in local sswidl session 
; 
;-
common lasco_time2file, last_day0, last_urls, last_times

debug=keyword_set(debug)
fcount=0                     ; assume failure...

urls=1 
case 1 of 
   keyword_set(nrl_url): urlp='http://lasco-www.nrl.navy.mil/'
   keyword_set(gsfc_url):     begin
      box_message,'Sorry, NRL only for now... '
      urlp='http://http://lasco-www.nrl.navy.mil'
   endcase
   else: urlp='http://lasco-www.nrl.navy.mil'
endcase 

case 1 of 
   keyword_set(c1): tel=1
   keyword_set(c3): tel=3
   keyword_set(c4): tel=4
   else: tel=2
endcase

stel='c'+strtrim(tel,2)


case 1 of 
   n_elements(level) eq 0: slev='05'
   str2number(level) eq 5: slev='05'
   else:slev=strtrim(str2number(level),2) ; problably 1 or 2...
endcase

confirm=keyword_set(confirm)
before=keyword_set(before)
after=keyword_set(after)

case n_params() of 
  1: tg=anytim(time0,/utc_int)
  2: tg=timegrid(time0,time1,out='utc_int',hours=24,/quiet)
  else: begin 
     box_message,'You need to supply times or a time range...'
     return,''
  endelse
endcase 

subdirs=time2file(tg,/year2,/date_only)
if n_params() eq 2 then begin 
   if anytim(time1) eq anytim(time1,/date_only) and $
      anytim(time0,/date_only) ne anytim(time1,/date_only) then $
      subdirs=subdirs(0:n_elements(subdirs)-2)
endif 
suburls=urlp+'/lz_data/' + 'level_' + slev + '/' + subdirs + '/' + stel+'/'
if keyword_set(quicklook) then $
suburls=urlp+'/ql/' + 'level_' + slev + '/' + subdirs + '/' + stel+'/'

retval=''

for i=0,n_elements(suburls)-1 do begin 
   sock_list,suburls(i),ll0
   if n_elements(ll0) ge 1 then begin 
   ll0=strlowcase(ll0)  
   ssf=where(strpos(ll0,'.fts') ne -1,fcnt)
   if fcnt gt 0 then begin 
      fpat='.fts'+(['','.gz'])(strpos(ll0(ssf(0)),'.gz') ne -1)
      fits=strarrcompress(strextract(ll0(ssf),'<a href="',fpat+'"'))
      fitsurls=suburls(i)+ fits + fpat 
      fcount=fcount+n_elements(fits)
      if n_elements(allfits) eq 0 then allfits=fitsurls else $
          allfits=[temporary(allfits),fitsurls]
      if debug then stop,'List>> ' + suburls(i)
   endif else box_message,['No FITS found at:', suburls(i)] 
   endif ; (listing ~ok)
endfor
if n_elements(allfits) gt 1 then begin 
   retval=allfits(1:*)
   if anytim(time0) ne anytim(time0,/date_only) or $ 
                     keyword_set(force_times) then begin 
    if fpat ne '.fts' then begin 
      box_message,['Times requested/implied but remote files are compressed',$
                  'Cannot trim times, so returning full days urls...'] 
      endif else begin 
      box_message,'Not a whole day, need to check headers via WWW so please be patient...'
     times=strarr(n_elements(retval))
     a=obj_new('hfits')
     for i=0,n_elements(retval)-1 do begin 
       a->hread,retval(i),head
       get_fits_time,fitshead2struct(head),timex
       times(i)=timex
       box_message,[retval(i),times(i)] 
     endfor
       ftimes=times               ; -> output keyword 
       atimes=anytim(times)       ; anytim number version
       dttimes=atimes-anytim(time0)
       case 1 of 
          n_elements(time1) gt 0: ss=sel_timrange(times,anytim(time0,/int), $
               anytim(time1,/int), between=1-keyword_set(before) )
          keyword_set(before): begin
             befbool=atimes le anytim(time0) 
             ss=max(where(dttimes*befbool))
          endcase
          keyword_set(after): begin 
             aftbool=atimes ge anytim(time0)
             ss=min(where(dttimes*aftbool))
          endcase
          else: begin
             abdt=abs(dttimes)
             ss=(where(abdt eq min(abdt)))(0)
          endcase
       endcase
       if ss(0) ne -1 then begin 
           retval=retval(ss) 
           ftimes=ftimes(ss)
       endif else $
          box_message,'No LASCO files within your time range...' 
    endelse
   endif
endif else begin 
   box_message,'No LASCO files found matching your times/criteria'
endelse

if confirm then begin 
   box_message,['/CONFIRM not yet implemented...',$
                'Try again ' + reltime(days=2,/day_only)]
;
;  file_exist (nfs) or sock_pings done here to separate 
;  ideal from actual
;
endif 

return, retval
end
