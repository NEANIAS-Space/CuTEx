function sxi_files, time0, time1, nfiles=nfiles,  $ 
  level0=level0, level1=level1, level2=level2, graphics=graphics, $
  ngdc=ngdc, goesn=goesn, full_path=full_path, verbose=verbose,_extra=extra
;+
;   Name: sxi_files
;
;   Purpose: return sxi filnames
;
;   Input Paramters:
;      time0 - vector of desired times or start time of range
;      time1 - stop time if range desired
;
;   Keyword Parameters:
;      nfiles (output) - number of files returned
;      level0 (switch) - if set, return level0 names
;      level1 (switch) - if set, return level1 names (default)
;      graphics (switch) - if set, return graphics file names (*.png)
;      ngdc (switch) - if set, use direct NGDC socket listing (not catalog)
;      goesn - GOES sat# desired (default=12) 
;      full_path (switch) - if set, return full path (def is just file name)
; 
;   Calling Sequence:
;      sxifiles=sxi_files(time0,time1 [/level0, /graphics, nfiles=nfiles]
;
;   Calling Examples:
;      sxifiles=sxi_files('12:30 2-dec-2000','14:00 3-dec-2000') ; level1 in range
;      sxifiles=sxi_files(eitindex) ; sxi closest to input EIT index (times)
;
;   History:
;      11-Jan-2003 - S.L.Freeland - from mdi_files/eit_files analog
;      12-Jan-2003 - Zarro (EER/GFSC), added obj_destroy for cleanup
;      13-Jan-2003 - S.L.Freeland - add /PARSE_TIME switch to file2time call
;      14-Jan-2003 - Zarro (EER/GSFC) - added call to SXI_SERVER() and optimized
;                    for Windows
;      17-Jan-2003 - Zarro (ERR/GSFC) - anytim->parse_time, unix delim(server)
;      17-Jan-2003 - S.L.Freeland - remove xtra day pad from range stop time
;                                   for time vector option, only uniq subdirs
;      20-Jan-2003 - Zarro - removed extra directory search,
;                            redundant PARSE_TIME call, and
;                            extra time range search for date only input times 
;      26-Jan-2003 - Zarro - added check for text file listing
;                            in remote directory
;       4-aug-2003 - S.L.Freeland - fixed problem when start and stop times
;                    were on a day boundry (incorrect subscripting)
;                    enhanced filter to reject non image lines in html html index 
;       5-jun-2004 - S.L.Freeland - additonal parse required for ngdc change
;      16-mar-2005 - S.L.Freeland - add /LEVEL2 keyword (coronal hole product for example)
;       1-aug-2006 - S.L.Freeland - pass GOESN->sxi_server.pro (top_path sat#dependent)
;
;   Method:
;      Two options:
;         Use SSW sxi catalog 
;         Direct listing via sockets->ngdc (via DMZarro socket package)
;
;  Restrictions:
;    Only /NGDC (direct socket listing) option available for now
;    (which I'll use to generate and enable catalog option asap...) 
;-

secs_day=3600.*24.d
ngdc=keyword_set(ngdc)
ssw_sxi_catalog=1-ngdc            ; default - use catalog
;                                 ; (maybe default ngdc for time0/time1 ~recent? tbd)
full_path=keyword_set(full_path)  ; include full path (def=just filename)
verbose=keyword_set(verbose)      ;

nfiles=0                          ; initialize success/match count to failure...
if (1-ngdc) or (1-since_version('5.4')) then begin
   box_message,'Currently requires /NGDC switch (and IDL V>=5.4) '
   return,''
endif

if not keyword_set(goesn) then goesn=12              ; which GOES sat?
sgn=strtrim(goesn,2)

sxidata=concat_dir('$SSWDB','sxig'+sgn)              ; future use

sxilog='SXIG'+strtrim(goesn,2)+'_TOP_HTTP'           ; future use

if (1-ngdc) then begin
      box_message,'Only /NGDC or $SXIGn_TOP_HTTP supported, returning
      return,'' 
endif
      
; type of file desired (level0, level1, or graphics)
fexten='.FTS'
tfilter=''
case 1 of 
   keyword_set(level0):   tfilter='A*_'+sgn
   keyword_set(level1):   tfilter='B*_'+sgn
   keyword_set(level2):   tfilter='C*_'+sgn
   keyword_set(graphics): fexten='.PNG' 
   else: tfilter='B*_'+sgn                 ; default Level1 FITS
endcase

sfilter='*SXI_*'+tfilter+'*'+fexten+'*'          ; implied search pattern
;
; generate time grid (maps to subdirectories YYYY/MM/DD )

case n_params() of 
   1: begin
       tgrid=anytim(time0,/ecs,/date_only)
       if n_elements(tgrid) gt 1 then tgrid=tgrid(uniq(tgrid,sort(tgrid)))
      end
   2: begin
       tgrid=anytim(timegrid(time0,time1,/day,/quiet),/date_only,/ecs)
       ng=n_elements(tgrid)
       if (ng gt 1) then if (anytim(tgrid[ng-1]) eq anytim(time1)) then $
        tgrid=tgrid[0:ng-2]
      end
   else: begin
      box_message,'Supply time(s) or time range...'
      return,''
   endcase
endcase

case 1 of
   ssw_sxi_catalog:                        ; not yet implemented
   else: begin 
      server=sxi_server(network=network,path=toppath,/verb,sat=goesn)
      if not network then return,''
      pathgrid=toppath+'/'+tgrid+'/'            ; uniq subdirectories ..YYYY/MM/DD
      a=obj_new('http') 
      a->open,server,/gateway
      listall='' & np=n_elements(pathgrid)
      for i=0,np-1 do begin 
         if verbose then box_message,'Listing>> ' + pathgrid[i]

;-- Check for file listing in directory: sxi_yyyymmdd.txt
;-- If found, faster to slurp it than full remote listing

         tfile=pathgrid[i]+'sxi_'+str_replace(tgrid[i],'/','')+'.txt'
         status=a->file_found(tfile)
         if status then begin
          a->list,tfile,listx
         endif else begin
          a->list,pathgrid[i],output,_extra=extra
          listx='SXI'+strextract(output,'<A HREF="SXI',fexten+'">')+fexten
         endelse
         listall=[temporary(listall),temporary(listx)]
      endfor

      obj_destroy,a
      ss=where(strmatch(listall,sfilter),nfiles)

      if nfiles eq 0 then begin 
         box_message,'No SXI files match your times and/or filters, returning...'
         return,''
      endif
      listall=listall[ss]

      no_search=0b
      ftimes=file2time(listall,out='int',/parse_timex,ymd=ymd)        
      case n_params() of
         1: begin                            ; time(s) - get closest matches
             ss=tim2dset(ftimes,anytim(time0,/int))
            end
         2: begin                            ; range 
             
;-- skip range search if time0/1 on day boundaries

             no_search=(anytim(tgrid[0]) eq anytim(time0)) and $
                       (anytim(tgrid[np-1]) eq (anytim(time1)-secs_day))
             dprint,'% no_search: ',no_search
             if not no_search then ss=sel_timrange(ftimes,anytim(time0,/int),anytim(time1,/int),/between)
            end

         else: box_message,'No way to get here...'
      endcase

      if ss[0] eq -1 then begin
       box_message,'No SXI files match your times, returning...'
       return,''
      endif

;-- return full path if requested (could use CONCAT_DIR but it has loops)

      if full_path then begin
       delim='/'
       listall=toppath+delim+temporary(ymd)+delim+temporary(listall) 
       if (1-ngdc) then listall=local_name(listall)

      endif
       ssx=where(strpos(listall,'SXIicons') ne -1,ssxcnt)
       if ssxcnt gt 0 then begin
           fss=listall(ssx)
           filess=ssw_strsplit(fss,'href="',/tail,head=iconss)
           pathss=ssw_strsplit(fss,'SXIicons',/head)
           listall(ssx)=pathss+filess
       endif
      if exist(ss) and (1-no_search) then listall=listall[ss]
      nfiles=n_elements(listall)
   endcase
endcase

return,listall
end
