pro ssw_jsurl2data, jsurl, data, r,g,b, $
    times=times, get_times=get_times, out_style=out_style, $ 
    nodelete=nodelete, outdir=outdir, ingraphics=ingraphics
;
;+
;    Name: ssw_jsulr2data
;
;    Purpose: transfer/read implied 3D from JS movie url -or- graphics urls 
;
;    Input Paramters:
;       jsurl - URL of a JavaScript movie  
;
;    Output Parameters:
;       data - the 3D image cube
;       r,g,b, - color table (if applicable)
;
;    Keyword Parameters:
;       nodelete - if set, do not delete the "temporary" files (gifs/pngs...)
;       outdir - optional output directory for graphics files 
;                (default will use get_temp_dir.pro)
;       get_times (switch) - if set, assume file names include image times
;       times (output) - if /GET_TIMES set (and valid), contains image times
;       out_style - optional time format per anytim.pro (default='ECS')
;       ingraphics - graphics explicitly passed (jsurl is ignored!!)
;
;    Calling Sequence:
;       ssw_jsurl2data, jsurl, data [,/get_times,times=times,outstyle=fmt]
;       -ORr-
;       ssw_jsurl2data, dummy, data , ingraphics=graphicsURLS (png/gif..)
;
;    History:
;      11-Jan-2006 - S.L.Freeland 
;
;-
;
if data_chk(ingraphics,/string) then begin 
   gurls=ingraphics
endif else begin 
   if not data_chk(jsurl,/string) then begin 
      box_message,'Need URL of JavaScript movie'
      return
   endif

   break_url,jsurl,host,subdir,jsname,http=http
   if (1-http) or strpos(jsname,'.htm') eq -1 then begin 
      box_message,'Input does not look like JavaScript URL...
      return
    endif
    gurls=ssw_jsurl2imagelist(jsurl)
endelse
ngurls=n_elements(gurls)

break_url,gurls,host,rsubdir,gfiles
if gurls(0) eq '' then begin 
   box_message,'Problem reading remote JavaScript...'
   return
endif
 
if n_params() gt 1 then begin
   box_message,'Transferring ' + strtrim(gurls,2) + ' graphics files'
   if n_elements(outdir) eq 0 then outdir=get_temp_dir()
   sock_copy,gurls,out_dir=outdir
   lfiles=concat_dir(outdir,gfiles)
   lexist=file_exist(lfiles)
   nexist=total(lexist)
   case 1 of
      nexist eq ngurls: 
      nexist eq 0: begin 
          box_message,'None of the files were transferred, bailing...
          return
      endcase
      else: begin 
         box_message,'Problem with ' + strtrim(ngurls-nexist,2) + ' out of ' + $
            strtrim(ngurls,2) + ' transfers.. returning subset'
         ss=where(nexist)
         lfiles=lfiles(ss)
         gfiles=gfiles(ss)
      endcase
   endcase 
   data=files2data(lfiles,r,g,b) ; read graphics -> data
   if not keyword_set(nodelete) then ssw_file_delete,lfiles ; delete local
endif else box_message,'No output DATA parameter requested...

if keyword_set(get_times) then begin 
   if n_elements(out_style) eq 0 then out_style='ecs'
   times=file2time(gfiles,out_style=out_style)
endif 

return
end
