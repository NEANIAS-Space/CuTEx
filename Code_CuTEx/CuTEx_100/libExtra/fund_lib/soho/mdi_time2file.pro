function mdi_time2file, time0, time1, fcount, $
   stanford_url=stanford_url, gsfc_url=gsfc_url, confirm=confirm, $
   before=before, after=after, deltat=deltat
;
;+
;  Name: mdi_time2file
; 
;  Purpose: map from SSW times[ or time range -> mdi magnetogram fnames/URLS
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
;     stanford_url - if set, function returns full stanford URLs
;     gsfc_url     - if set, function returns full gsfc urls
;     confirm - if switch, only return files/urls which actually exist
;               on server; default is "idealized" list
;               (fcount always reflects number of elements returned) 
;
;  Calling Sequence:
;      mdimags=mdi_time2file(times [,/stanford] [,/confirm] ) ; Time vector
;      mdimags=mdi_time2file(time0,time1 [,/stanford] ...   ) ; Time range
;      mdimags=mdi_time2file(time0, /AFTER , /stanford)     ) ; 1st After T0
;      mdimags=mdi_time2file(time0, /BEFORE, /stanford)     ) ; 1st Before T0
;
;  History:
;     25-Nov-2003 S.L.Freeland - long planned breakout of of a
;                                local CoSEC/SSW socket "service"
;                 Some heritage from sxi_files.pro
;                 Will soon call from older mdi_files.pro but may 
;                 use standalone 
;
;  Restrictions:
;     Use of /CONFIRM requires IDL +5.4 (RSI socket utility)
;     Proto-version - not all keywords implemented as of
;       today but maybe pre Fall AGU 2003... 
; 
; 
;-
fcount=0                     ; assume failure...

urls=1 
case 1 of 
   keyword_set(stanford_url): urlp='http://soi.Stanford.EDU/magnetic/mag/'
   keyword_set(gsfc_url):     begin
      box_message,'Sorry, mdi-mags not yet available via GSFC HTTP'
      urlp='http://soi.Stanford.EDU/magnetic/mag'
   endcase
   else: urlp='$MDI_MAGS'      ; local nfs mount?
endcase 

confirm=keyword_set(confirm)
before=keyword_set(before)
after=keyword_set(after)

day0='1-jan-93'
tref=anytim(day0,/utc_int)
; tg=utc_int

case n_params() of 
  1: tg=anytim(time0,/utc_int)
  2: tg=timegrid(time0,time1,out='utc_int',min=96,/quiet)
  else: begin 
     box_message,'You need to supply times or a time range...'
     return,''
  endelse
endcase 
ddays=tg.mjd - tref.mjd
 
dmins=tg.time / 1000. / 60.    ; ut time -> minutes

dbin=after + ([0,-1])(before) 

funct='round'

mbin=call_function(funct,dmins/96.)   ; relative "bin" (15 per day)

mabin=mbin + dbin 

mlo=where(mabin lt 0,lcnt)
mhi=where(mabin gt 14,hcnt)

case 1 of                            ; littl vectorization possible...
   lcnt gt 0: begin 
      ddays(mlo)=ddays(mlo)-1
      mabin(mlo)=14
   endcase
   hcnt gt 0: begin 
      ddays(mhi)=ddays(mhi)+1
      mabin(mhi)=0
   endcase
   else:
endcase

retval=strarr(n_elements(tg))

; derive 'idealized' day and time dependent (and cryptic) 
; SOI derived file/url names...

fname=  'fd_M_96m_01d.' +  $
        fstring(ddays,format='(I6.6)')  + '/fd_M_96m_01d.' + $ 
        fstring(ddays,format='(I4.4)')  + '.'              + $ 
        fstring(mabin,format='(I4.4)')  + '.fits'

retval=urlp+'/'+fname

fcount=n_elements(retval)

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
