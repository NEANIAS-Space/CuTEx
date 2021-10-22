pro sxi_genxcat, time0, time1, sxicat, nmatches, goesn=goesn, $
   level0=level0, level1=level1, filenames=filenames
;
;   Name: sxi_genxcat
;
;   Purpose: read/return sxi catalog
;
;   Input Parameters:
;      time0, time1 - desired time range
;
;   Output Parameters:
;      sxicat - output vector of sxi header-structures within time range
;      nmatches - number of records (sxicat) returned (0=if problem or no match)
;
;   Keyword Parameters:OA
;      goesn - GOES satellite number (default=12)
;      level0 - if set, read level0 catalog  (default is level1)
;      level1 - if set, read level1 catalog (default)
;      filenames - filenames (1:1 w/resp to sxicat records)
;
;   29-Sep-2003 - S.L.Freeland - written
;   
;   Method: setup and call read_genxcat for sxi catalog application
;
;   Restrictions:
;      assumes sxi genxcatalog is included in $SSWDB tree
;      (default $SSWDB/goes/sxigNN/genxcat/... )
;     
;   TODO: if no local genxcatalog and IDL Version >=5.4, access an 
;         http version (gsfc/lmsal/ or other SSW/SSWDB host www)
; 
nmatches=0                                   ; assume failure...

if not keyword_set(goesn) then goesn=12

sgn=string(str2number(goesn),format='(i2.2)')

prefix='SXI'
exten ='.FTS'

slevel='BB'           ; default level1

case 1 of
   keyword_set(level0): box_message,'Sorry, only level 1 catalog for now'   
   else:  ; reserved... 
endcase

sswdb=get_logenv('SSWDB')
gsswdb=concat_dir(sswdb,'goes')        ; Mission level $SSWDB
gnsswdb=concat_dir(gsswdb,'sxig'+sgn)  ; Instrument sxigNN
catdir=concat_dir(gnsswdb,'genxcat')   ; genxcatalogs

if not file_exist(catdir) then begin 
   box_message,['No local $SSWDB/goes/'+'sxig'+sgn + ' genxcat catalog...',$
               'Remote access not yet enabled (call Sam...), returning...'] 
   return
endif

; read is via read_genxcat.pro
read_genxcat,time0,time1,topdir=catdir, $
                         sxicat,count=nmatches     ; times -> catalog

if nmatches gt 0 then begin 
   if not required_tags(sxicat,'FILENAME') then begin 
      filenames=prefix+'_'+$
          time2fid(anytim(sxicat,/utc_int),/time,/milli,/full) + $
          '_'+ slevel+ '_' + sgn + exten
   endif else filenames=gt_tagval(sxicat,/FILENAME)
endif

return
end




 

