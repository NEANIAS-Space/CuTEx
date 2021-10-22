;+
; Project     : HESSI
;
; Name        : db_gbo
;
; Purpose     : IDL database for Max Millennium Campaign
;
; Category    : Database
;
; Inputs      : See component routines
;
; Outputs     : Database save file in $GBO_DB
;
; Keywords    : See component routines
;
; History     : 2-Aug-1999,  D.M. Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-
;-----------------------------------------------------------------------------

;-- plot latest search results in PNG file 

pro gbo_png,fid,_extra=extra

;-- return existing png file if present, else create new one

if is_blank(fid) then return
cache_gbo,fid,records,trange=trange,/get

png='/tmp/gbo'+trim2(fid)+'.png'

chk=loc_file(png,count=count)
if count ne 0 then return

;-- check for latest search results in cache

plot_gbo,records,png=png,_extra=extra,trange=trange

return
end

;----------------------------------------------------------------------------------

pro plot_gbo,ids,trange=trange,_extra=extra,png=png,gsize=gsize

common gbo_com,gbo_records

np=n_elements(ids)
if np eq 0 then return
if is_struct(ids) then records=ids else begin
 if ids[0] eq -1 then return
 records=gbo_records[ids]
endelse

;-- establish time range

temp={mjd:0l,time:0l}
tstart=replicate(temp,np)
tend=tstart
tstart.mjd=records.dstart
tstart.time=records.tstart
tend.mjd=records.dend
tend.time=records.tend

tstart=anytim(tstart)
tend=anytim(tend)

chk=where(valid_time(trange),count)
if count eq 2 then begin
 dmin=anytim(trange[0])
 dmax=anytim(trange[1])
endif else begin
 dmin=min(tstart)
 dmax=max(tend)
endelse


;-- make plot window at least 1 day wide


day_sec=24.*3600.d
if abs(dmax-dmin) le day_sec then dmax=dmin+day_sec

;-- read GOES

gev=get_gev(anytim(dmin,/utc_int),anytim(dmax,/utc_int),count=gcount)
dmin=anytim(dmin,/utc_int)
dmin.time=0
dmin=anytim(dmin)
tmin=dmin
utbase=anytim(tmin,/vms)
message,'UTBASE: '+utbase,/cont

tstart=tstart-tmin
tend=tend-tmin
dmax=dmax-tmin
dmin=dmin-tmin

;-- establish observatories

obs=records.observatory
uobs=get_uniq(obs,count=count)
uobs=['GOES',uobs]
count=count+1
nticks=count+1
yrange=[0,nticks]
ytick=replicate(' ',nticks+1)

;-- establish Z-buffer

do_png=0b
if is_string(png) then begin
 break_file,png,dsk,dir
 do_png=test_dir(dsk+dir)
 if do_png then begin
  dsave=!d.name & psave=!p.color
  zsize=[750,550]
  if exist(gsize) then zsize=[gsize[0],gsize[n_elements(gsize)-1]]
  ncolors=!d.table_size
  set_plot,'z',/copy
  device,/close,set_resolution=zsize,set_colors=ncolors
  !p.color=ncolors-1
 endif
endif

;-- establish plot window

utplot,[dmin,dmax],[1,1],utbase,yrange=yrange,/ystyle,ytickname=ytick,$
                         yticks=nticks,/nodata,/xstyle,$
                         title='Max Millennium Search Results'

mtime=600.
width=!x.crange[1]-!x.crange[0]
mtime=width*.01
set_line_color,count=ncolors

lcolor=0
for i=0,count-1 do begin
 message,'Plotting '+uobs[i],/cont
 chk=where(uobs[i] eq obs,cobs)

 if (cobs gt 0) or (gcount gt 0) then begin

;-- GOES plug-in

  if (uobs[i] eq 'GOES') then begin
   if (gcount gt 0) then begin
    cstart=anytim(gev)-tmin
    cend=cstart+gev.duration
    cobs=gcount
   endif
  endif else begin
   if cobs gt 0 then begin
    cstart=tstart[chk]
    cend=tend[chk]
   endif
  endelse

  diff=cend-cstart
  zero=where(diff le mtime,zcount)
  if zcount gt 0 then cend[zero]=cstart[zero]+mtime
  ctime=(transpose( [[cstart],[cend]]))[*]
  black=replicate(0,cobs)
  lcolor=lcolor+1
  if lcolor gt (ncolors-1) then lcolor=lcolor-ncolors+1
  color=replicate(lcolor,cobs)
  ycol=(transpose( [[black],[color]]))[*]
  yval=replicate(i+1,2*cobs)
  xyouts,[!x.crange[0],!x.crange[0]]+.05*width,[i+1.15,i+1.15],uobs[i],/data,$
        charthick=1,font=0,_extra=extra

  plots,ctime,yval,color=ycol,_extra=extra,noclip=0,thick=6
 endif
endfor

if do_png then begin
 temp=tvrd()
 device,/close
 tvlct,rs,gs,bs,/get
 if idl_release(upper=5.3,/inc) then $
  write_png,png,rotate(temp,7),rs,gs,bs else $
   write_png,png,temp,rs,gs,bs
 chmod,png,/g_write,/g_read
 set_plot,dsave
 !p.color=psave
endif

return & end

;-----------------------------------------------------------------------------------
;-- setup commons

pro com_gbo

common gbo_com,gbo_records

return & end

;-----------------------------------------------------------------------------
;-- server root

pro gbo_server,server

server='http://orpheus.nascom.nasa.gov/~zarro'

return & end

;-----------------------------------------------------------------------------
;-- get lock file name

function gbo_lock_file

lock_file=''
db_file=chklog('GBO_DB')
if db_file eq '' then return,''
break_file,db_file,disk,dir
gbo_dir=disk+dir
if not test_dir(gbo_dir) then return,''
lock_file=concat_dir(gbo_dir,'gbo_lock.dat')
return,lock_file & end

;-----------------------------------------------------------------------------
;-- check if database locked

function gbo_locked,err=err,_extra=extra

locked=0b
caller=get_caller()
lock_file=gbo_lock_file()
if not check_lock(lock_file,/quiet,err=err,_extra=extra) then begin
 message,err,/cont
 return,1b
endif

return,0b
end

;----------------------------------------------------------------------------
;-- lock database

pro lock_gbo,err=err,_extra=extra

lock_file=gbo_lock_file()
if lock_file eq '' then return
apply_lock,lock_file,err=err,_extra=extra
if err ne '' then message,err,/cont

return & end

;----------------------------------------------------------------------------
;-- unlock database

pro unlock_gbo,err=err,_extra=extra

err=''
lock_file=gbo_lock_file()
rm_lock,lock_file,err=err,/quiet,_extra=extra
if err ne '' then message,err,/cont

return & end

;------------------------------------------------------------------------------

pro gbo_time,gbo,gstart,gend,verbose=verbose

gstart={mjd:gbo.dstart,time:gbo.tstart}
gstart=anytim2tai(gstart)

if n_params() eq 3 then begin
 gend={mjd:gbo.dend,time:gbo.tend}
 gend=anytim2tai(gend)
endif

if keyword_set(verbose) then begin
 print,anytim2utc(gstart,/vms)
 if n_params() eq 3 then print,anytim2utc(gend,/vms)
endif

return & end

;------------------------------------------------------------------------------

pro obs_gbo,file=file

if is_string(file) then begin
 openw,unit,file,/get_lun,error=error
 if error ne 0 then begin
  message,'Error opening '+file,/cont
  return
 endif
endif else unit=-1

message,'Updating catalog statistics file...',/cont

restore_gbo
com_gbo
common gbo_com

ok=where( (gbo_records.file ne '') and (gbo_records.deleted eq 0b),count)

ok2=where( (gbo_records.file eq '') and (gbo_records.deleted eq 0b),count2)


printf,unit,'<html>'
printf,unit,'<head>'
printf,unit,'<meta http-equiv="Expires" content="-1">'
printf,unit,'<meta http-equiv="Pragma" content="no-cache">'
printf,unit,'<meta http-equiv="Cache-Control" content="no-cache">'
printf,unit,'<title> Max Millennium Catalog Statistics </title></head>'
printf,unit,'<body bgcolor="white"><pre>'
printf,unit,'Max Millennium Catalog Statistics'
printf,unit,'Last updated: ',!stime
printf,unit,''
printf,unit,''
printf,unit,'SITE                                  FIRST ENTRY     LAST ENTRY     TOTALS'


if count gt 0 then begin
 printf,unit,''
 printf,unit,'Automatic input entries:'
 printf,unit,'------------------------'
 printf,unit,''
 printf_gbo,unit,ok
endif


if count2 gt 0 then begin
 printf,unit,''
 printf,unit,'Manual input entries:'
 printf,unit,'---------------------'
 printf,unit,''
 printf_gbo,unit,ok2
endif
printf,unit,'</pre></body>'

printf,unit,'<head>'
printf,unit,'<meta http-equiv="Expires" content="-1">'
printf,unit,'<meta http-equiv="Pragma" content="no-cache">'
printf,unit,'<meta http-equiv="Cache-Control" content="no-cache">'
printf,unit,'</head></html>'

if unit gt -1 then close_lun,unit
return & end 
  
;----------------------------------------------------------------------------

pro printf_gbo,unit,index

common gbo_com

robs=gbo_records[index].observatory
rstart=gbo_records[index].dstart
rend=gbo_records[index].dend

obs=get_uniq(robs)
nobs=n_elements(obs)

for i=0,nobs-1 do begin
 chk=where(robs eq obs[i],count)
 if count gt 0 then begin
  dmin=min(rstart[chk])
  dmax=max(rend[chk])
  tstart={mjd:dmin,time:0l}
  tend={mjd:dmax,time:0l}
  tstart=anytim2utc(tstart,/vms,/date)
  tend=anytim2utc(tend,/vms,/date)  
  printf,unit,str_cut(obs[i],35,pad=37),tstart,'     ',tend,str_format(count,'(i8)')
 endif
endfor

return & end

;----------------------------------------------------------------------------
;-- define template structure
                          
pro def_gbo,gbo           

gbo= {db_gbo,id:-1l,dstart:0l,dend:0l,tstart:0l,tend:0l, $
       observatory:'',instrument:'',class:'',type:'',subtype:'',$
       center:'',fov:'',$
       format:'',campaign:'',goes:'',noaa:'',name:'',email:'',url:'',info:'',$
       submitted:0d,deleted:0b,password:'',file:''}

return & end

;---------------------------------------------------------------------------
;-- define GBO_DB env

pro set_gbo,file,err=err         

if is_string(file) then begin
 chk=loc_file(file,count=count,err=err)
 if count eq 0 then message,'Warning - '+err,/cont
 mklog,'GBO_DB',chk[0]
endif
return & end

;-----------------------------------------------------------------------------
;-- show GBO_DB env definition

pro show_gbo

print,'GBO_DB --> '+chklog('GBO_DB')

return & end

;-----------------------------------------------------------------------------
;-- purge database
                              
pro purge_gbo,err=err,save=save
err=''

com_gbo
common gbo_com
                     
restore_gbo,err=err
if err ne '' then return
                
if not is_struct(gbo_records) then return


bad=where( (gbo_records.observatory eq '') or $
           (gbo_records.class eq '') or (gbo_records.type eq '') or $
           (gbo_records.dstart eq 0l) or (gbo_records.dend eq 0l) or $
           (gbo_records.email eq '') or (gbo_records.format eq ''),count)

if count gt 0 then gbo_records[bad].deleted=1b
chk=where(gbo_records.deleted eq 0b,count)

if count eq n_elements(gbo_records) then begin
 if keyword_set(save) then save_gbo
 return
endif

if (count gt 0) then begin
 gbo_records=gbo_records[chk]
 gbo_records.id=lindgen(count)
endif else begin
 gbo_records=clear_struct(gbo_records[0])
 gbo_records.id=-1
endelse

gbo_records=stc_compress(gbo_records,/no_copy)
save_gbo

return & end

;----------------------------------------------------------------------------
;-- check if DB is in use

pro use_gbo

com_gbo
common gbo_com

return & end

;-----------------------------------------------------------------------------
;-- clear memory copy of database

pro clear_gbo

com_gbo
common gbo_com

use_gbo

delvarx,gbo_records

return & end

;-----------------------------------------------------------------------------

function valid_gbo,gbo     ;-- check for valid database record structure

if not is_struct(gbo) then return,0b
def_gbo,template_gbo
zero_gbo=clear_struct(gbo)
return,match_struct(template_gbo,zero_gbo,dtag=dtag,/type)
end

;------------------------------------------------------------------------------

pro restore_gbo,err=err,force=force,no_lock=no_lock     ;-- read database from file

err=''
if (1b-keyword_set(no_lock)) then begin
 if gbo_locked(err=err) then return
endif

com_gbo
common gbo_com

force=(not is_struct(gbo_records)) or keyword_set(force)

if force then begin
 use_gbo
 chk=loc_file('GBO_DB',count=count,err=err)
 if count eq 0 then return
 message,'Restoring from '+chk[0],/cont
 restore,chk[0]
endif

return & end

;-------------------------------------------------------------------------
;-- merge GBO_DB into new structure template

pro merge_gbo,template

if not is_struct(template) then return

com_gbo
common gbo_com
restore_gbo
np=n_elements(gbo_records)
gbo_new=replicate(template[0],np)
copy_struct,gbo_records,gbo_new
gbo_records=gbo_new
save_gbo

return & end


;--------------------------------------------------------------------------

pro save_gbo,err=err             ;-- save database to file

err=''
if gbo_locked(err=err) then return

com_gbo
common gbo_com

chk=chklog('GBO_DB')
if chk[0] eq '' then begin
 err='Environment variable GBO_DB needs to point to database filename'
 message,err,/cont
 return
endif

;-- test for write access

break_file,chk[0],dsk,dir
if not test_dir(dsk+dir,err=err) then return

if is_struct(gbo_records) then begin
 save_file=chk[0]
 temp_file=save_file+'_temp'
 message,'Saving to '+temp_file,/cont
 save,gbo_records,file=temp_file
 message,'Copying to '+save_file,/cont
 espawn,'cp '+temp_file+' '+save_file,/back
endif

return & end

;---------------------------------------------------------------------------
;-- remove duplicates from database

pro remdup_gbo

com_gbo
common gbo_com
                     
restore_gbo
                
if not is_struct(gbo_records) then return
nrec=n_elements(gbo_records)

if nrec lt 2 then return

excl=['id','file','submitted']
gbo_records=stc_uniq(gbo_records,excl=excl)

save_gbo

return & end

;----------------------------------------------------------------------------

;-- update database records

pro update_gbo,gbo,err=err,replace=replace,remove=remove,ofile=ofile,$
               no_save=no_save,check_dup=check_dup

err=''

;-- read latest gbo_records into memory

com_gbo
common gbo_com
restore_gbo,err=err

if err ne '' then begin
 output_gbo,err,ofile
 return
endif

remove=keyword_set(remove)
replace=keyword_set(replace)
edit=replace or remove
                      
;-- check if GBO record or ID # entered

cur_id=-1
if not exist(gbo) then return
if is_struct(gbo) then begin
 if valid_gbo(gbo) then cur_id=gbo.id
endif else begin
 if is_number(gbo) then cur_id=gbo
endelse

if (not valid_gbo(gbo)) and (not remove) then begin
 err='Invalid GBO record'
 output_gbo,err,ofile
 return
endif

;-- merge new record

if is_struct(gbo) then gbo=stc_compress(gbo)
                      
if (not is_struct(gbo_records)) and (not edit) then begin
 id=0 & gbo.id=id & gbo_records=gbo
endif else begin
 if cur_id eq -1 then edit=0
 if edit then begin
  chk=where(cur_id eq gbo_records.id,count)
  if count gt 0 then begin 
   if replace then begin
    gbo_records[chk[0]]=gbo
    if count gt 1 then gbo_records[chk[1:count-1]].deleted=1b
   endif else begin
    gbo_records[chk].deleted=1b
    if valid_gbo(gbo) then gbo.deleted=1b
   endelse
  endif else begin
   err='record '+num2str(cur_id)+' not found'
   output_gbo,err,ofile   
   return
  endelse
 endif else begin
  id=max(gbo_records.id)+1l & gbo.id=id
  if keyword_set(check_dup) then begin
   exclude=['submitted','id']
   chk=stc_where(gbo,gbo_records,exclude=exclude,count=count)
   if count gt 0 then begin
    err='Duplicate entry already in database'
    output_gbo,err,ofile
    return
   endif
  endif
  if is_struct(gbo_records) then gbo_records=[temporary(gbo_records),gbo] else gbo_records=gbo
 endelse
endelse

;-- update gbo_records file

if (err eq '') and (1b-keyword_set(no_save)) then begin
 output_gbo,'successful',ofile
 save_gbo
endif


return & end

;-----------------------------------------------------------------------------
;-- print message to terminal and optional file

pro output_gbo,mess,ofile

if is_blank(mess) then return
message,arr2str(mess),/cont
if is_string(ofile) then begin
 openw,lun,ofile,error=error,/get_lun
 if error ne 0 then return
 printf,lun,mess
 close_lun,lun
endif
return
end

;------------------------------------------------------------------------------
;-- get series of GBO records

pro get_gbo,ids,results,count=count,deleted=deleted

;-- initialize

count=0 & delvarx,results
if not exist(ids) then return 

db_gbo
common gbo_com
restore_gbo
if not exist(gbo_records) then return

;-- find records, filtering deleted ones

chk=where_arr(gbo_records.id,ids,count)
if count gt 0 then begin
 results=gbo_records[chk]
 if (1b-keyword_set(deleted)) then begin
  chk=where(results.deleted eq 0b,count)
  if count gt 0 then results=results[chk] else delvarx,results
 endif
endif

return & end

;-------------------------------------------------------------------------------
;-- search GBO_DB

pro search_gbo,results,ids=ids,dstart=dstart,dend=dend,class=class,type=type,$
               observatory=observatory,deleted=deleted,ifile=ifile,err=err,$
               campaign=campaign,$
               subtype=subtype,goes=goes,noaa=noaa,format=format,count=count

;-- initialize

err=''
count=0 & delvarx,results,ids

com_gbo
common gbo_com
restore_gbo,/no_lock
if not exist(gbo_records) then begin
 err='GBO DB is empty'
 return
endif
purge_gbo

;-- read inputs from file

if is_string(ifile) then begin
 data=rd_ascii(ifile,lun=lun)
 if (n_elements(data) eq 1) and (trim2(data[0]) eq "") then begin
  err='Null search request'
  return
 endif
 def_gbo,gbo
 tags=tag_names(gbo)
 np=n_elements(data)
 for i=0l,np-1 do begin
  colon=strpos(data[i],':')
  if colon gt -1 then begin
   pre=strmid(data[i],0,colon)
   chk=grep(pre,tags,index=index)
   post=trim2(strmid(data[i],colon+1,strlen(data[i])))
   pre=strlowcase(trim2(pre))
   status=execute(pre+"='"+post+"'")
  endif
 endfor
endif
 
;-- start with times

valid_tstart=valid_time(dstart)
valid_tend=valid_time(dend,tend)

if valid_tstart then tstart=anytim2utc(dstart)
if valid_tend then tend=anytim2utc(dend)

search_all=0b
case 1 of
 valid_tstart and (not valid_tend): begin
  chk=where(gbo_records.dstart ge tstart.mjd,count)
  if count eq 0 then return
 end

 valid_tend and (not valid_tstart): begin
  chk=where(gbo_records.dend le (tend.mjd+1),count)
  if count eq 0 then return
 end

 valid_tend and valid_tstart: begin

 d1= (gbo_records.dstart gt tend.mjd) 

 d2= (gbo_records.dend gt tend.mjd)

 d3= (gbo_records.dstart lt tstart.mjd) 

 d4= (gbo_records.dend lt tstart.mjd) 
  
 ochk=where( (d1 and d2) or (d3 and d4),ncomplement=count,complement=chk)
 if count eq 0 then return
 end
 else: search_all=1b
endcase

if search_all then results=gbo_records else results=gbo_records[chk] 
count=n_elements(results)

;-- starting searching for individual components

check_gbo,'observatory',observatory,results,count=count,delim='+',/exact
check_gbo,'class',class,results,count=count,delim=' '
check_gbo,'type',type,results,count=count,delim='+'
check_gbo,'subtype',subtype,results,count=count,delim='+'
check_gbo,'format',format,results,count=count,delim='+'
check_gbo,'goes',goes,results,count=count,delim='+'
check_gbo,'noaa',noaa,results,count=count,delim='+'
check_gbo,'campaign',campaign,results,count=count,delim='+'
if (1b-keyword_set(deleted)) and (count gt 0) then begin
 chk=where(results.deleted eq 0b,count)
 if count gt 0 then results=results[chk] else delvarx,results
endif

count=n_elements(results)
if count gt 0 then begin
 gbo_time,results,gstart
 s=bsort(gstart)
 results=results[s]
 if count eq 1 then results=results[0]
 ids=results.id
endif

return & end

;-----------------------------------------------------------------------------
;-- check GBO fields for specified values

pro check_gbo,field,value,results,count=count,exclusive=exclusive,$
              delim=delim,exact=exact

;-- check inputs

count=n_elements(results)
if not is_struct(results) then return
if is_blank(field) then return
if is_blank(value) then return
if (1-is_string(delim,/blank)) then delim=','

if tag_exist(results,field,index=tag_no) then begin
 pieces=trim2(str2arr(value,delim=delim)) 
 if is_blank(pieces) then return
 found_piece=0b
 for i=0l,n_elements(pieces)-1 do begin
  if pieces[i] ne '' then begin
   ok=grep(pieces[i],results.(tag_no),index=index,exact=exact)
   if index[0] gt -1 then begin
    found_piece=1b
    if exist(tresults) then tresults=[temporary(tresults),results[index]] else $
     tresults=results[index]
   endif else begin
    if keyword_set(exclusive) then begin
     delvarx,results & count=0 & return
    endif
   endelse
  endif
 endfor

 if not found_piece then begin
  delvarx,results & count=0 & return
 endif else begin
  sorder=uniq([tresults.id],sort([tresults.id]))
  results=temporary(tresults[sorder])
 endelse

 count=n_elements(results)
endif

return & end

;-----------------------------------------------------------------------------
;-- find latest observatory log files in $SYNOP_DATA/logs

pro logs_gbo,ldir,_extra=extra

;-- list log subdirs

log_dir=chklog('SYNOP_LOGS')
if is_blank(log_dir) then begin
 message,'$SYNOP_LOGS is undefined',/cont
 return
endif

dirs=findfile(log_dir,count=count)
if count eq 0 then return
keep=where( (dirs ne 'noaa') and (dirs ne 'backups'),count)
if count eq 0 then return else dirs=dirs[keep]

;-- find latest files and update new ones

log_dirs=concat_dir(log_dir,dirs)
for i=0l,count-1 do begin
 tdir=log_dirs[i]
 skip=0
 if is_string(ldir) then skip=ldir ne dirs[i]
 if not skip then begin
  message,'Processing '+tdir,/cont
  logs=list_file(tdir,count=lcount)
  if lcount gt 0 then begin
   for k=0l,lcount-1 do cat_gbo,logs[k],/no_check,_extra=extra,/no_purge
  endif
 endif
endfor

purge_gbo,/save

return & end

;----------------------------------------------------------------------------
;-- update GBO database using observatory catalog as input

pro cat_gbo,file,verbose=verbose,no_check=no_check,back=back,$
                 reprocess=reprocess,no_purge=no_purge

if (1b-keyword_set(no_check)) then begin
 chk=loc_file(file,count=count,/verb)
 if count eq 0 then return
endif

verbose=keyword_set(verbose)
reprocess=keyword_set(reprocess)

break_file,file,dsk,dir,sname,sext
fname=trim2(sname+sext)

;-- check if this file has been processed

com_gbo
common gbo_com                     
restore_gbo

reprocessed=0b
done=where( (gbo_records.file eq fname) and (gbo_records.deleted eq 0b),count)
if (count ne 0) and (not reprocess) then begin
 if verbose then message,file+' already updated in catalog',/cont
 return
endif

data=rd_ascii(file)

;-- look for DATE_OBS fields

g=grep('DATE_OBS',data,index=index)
if index[0] eq -1 then begin
 message,'Missing DATE_OBS in file: '+file,/cont
 return
endif

;-- read each DATE_OBS record

cat_tags=['DATE_OBS','DATE_END','SOURCE','OBSERVATORY','CLASS','INSTRUMENT','TYPE',$
          'SUBTYPE','SUB_TYPE','FORMAT','MM_OP','CENTER','FOV','URL',$
          'GOES','NOAA','ACTIVE_REG','EMAIL','INFO','CONT_NAME']

nrec=n_elements(index) & ncat=n_elements(cat_tags)
found=0b
for i=0l,nrec-1 do begin
 if i eq (nrec-1) then  rec=data[index[i]:*] else $
  rec=data[index[i]:index[i+1]-1]
 s=stregex(rec,'([^:]+)(:)(.+)',/subexp,/extr)
 def_gbo,gbo
 for k=0l,ncat-1 do begin
  err=''
  key=cat_tags[k]
  chk=where(stregex(s[1,*],key,/fold) gt -1,count)
  if count gt 0 then begin
   value=trim2(s[3,chk[0]])
   case 1 of
   (key eq 'DATE_OBS'): begin
    dstart=anytim2utc(value,err=err)    
    if is_string(err) then begin 
     message,err,/cont
     break
    endif
    gbo.dstart=dstart.mjd
    gbo.tstart=dstart.time
   end
   (key eq 'DATE_END'): begin
    dend=anytim2utc(value,err=err)
    if is_string(err) then begin 
     message,err,/cont
     break
    endif
    gbo.dend=dend.mjd
    gbo.tend=dend.time
   end
   (key eq 'SOURCE') or (key eq 'OBSERVATORY') : begin
                          if stregex(value,'Yohkoh',/bool) then begin
                           if not stregex(value,'SXT',/bool) then value=value+'/SXT'
                          endif
                          gbo.observatory=trim(value)
                         end
   (key eq 'NOAA') or (key eq 'ACTIVE_REG') : begin
                          chk=stregex(value,'[0-9]+',/extra)
                          if is_string(chk) then begin
                           chk=trim(chk)
                           lp=strpos(chk,'0')
                           if lp eq 0 then chk=strmid(chk,1,strlen(chk))
                           gbo.noaa=chk
                          endif
                         end
   (key eq 'NAME') or (key eq 'CONT_NAME') : gbo.name=trim(value)

   (key eq 'MM_OP')    : gbo.campaign=value
   (key eq 'SUBTYPE') or (key eq 'SUB_TYPE'): gbo.subtype=trim(value)

   (key eq 'INFO')     : begin
   gbo.info=value
    nrec2=n_elements(rec)
    if (chk[0]+1) lt nrec2 then $
     gbo.info=gbo.info+' '+arr2str(trim2(rec[chk[0]+1:*]),delim= ' ')
   end
   else: begin
    status=execute('gbo.'+key+'=trim(value)')
   end
   endcase
   if is_string(err) then continue
  endif

 endfor

 if is_blank(gbo.observatory) then begin
  if verbose then begin
   message,'Missing Observatory name in '+file,/cont
   if stregex(file,'hasta',/bool,/fold) then gbo.observatory='C.U. Cesco Station (OAFA), Solar Division'
  endif
 endif

;-- skip if no GOES events occurred during observation time or DSTART is
;   earlier than !STIME-BACK days

 secs_day=24.*3600.d
 tstart=anytim2tai(dstart)
 if exist(dend) then tend=anytim2tai(dend) else tend=tstart
 if tend lt tstart then tend=tstart
 get_utc,stime
 ctime=anytim2tai(stime)

 if (tstart gt ctime) or (tend gt ctime) then begin
  message,'Skipping because of illogical observation times',/cont
  message,anytim2utc(tstart,/vms),/cont
  message,anytim2utc(tend,/vms),/cont
  continue
 endif

 if not exist(back) then back=30
 if exist(back) then begin
  if back gt 0 then begin
   if tstart lt (ctime-secs_day*back) then continue
  endif
 endif

 skip=0
 rd_gev,anytim2utc(tstart,/vms),anytim2utc(tend,/vms),gev,status=status
 if (status gt 0) or (status eq -1) then begin
  if verbose then message,'Warning - No GOES events during times in '+file,/cont
  skip=1
 endif

 if reprocess and (done[0] gt -1) and (not reprocessed) then begin
  gbo_records[done].deleted=1b
  if verbose then message,'Reprocessing '+file,/cont
  reprocessed=1b
 endif

;-- figure out GOES events

 if (not skip) and is_blank(gbo.goes) then begin
  if verbose then print,string(gev.st$class)
  class=trim2(string(gev.st$class))
  day=trim2(gt_day(gev,/str)) & fstart=strmid(trim2(gt_time(gev,/str)),0,5)
  result=[day[*]+','+fstart[*]+','+class[*]]
  result=arr2str(result,delim='+')   
  gbo.goes=result
 endif

;-- figure out fov 
                               
 fov=gbo.fov
 def_fov=2*960.
 reg='( *[0-9]*)( *\,? *)([0-9]*)'
 chk=stregex(fov ,reg,/fold,/extr,/sube)
 fx=trim2(chk[1])
 fy=trim2(chk[3])
 if fx eq '' then fov=def_fov else begin
  if fy eq '' then fy=fx
  fov=fx+','+fy
 endelse
 
;-- figure out NOAA info

 if is_blank(gbo.noaa) then begin
  noaa=list_nar(dstart,dend,gbo.center,fov)
  gbo.noaa=noaa
  if verbose then message,'Using inferred NOAA: '+gbo.noaa,/cont
 endif else begin
  noaa=list_nar(dstart,dend)
  if is_string(noaa) then begin 
   temp=str2arr(gbo.noaa,delim=',')
   nar=stregex(temp,'[0-9]+',/ext)
   if is_string(nar) then begin  
    temp=str2arr(noaa,delim='+')
    delvarx,new_nar
    for j=0l,n_elements(nar)-1 do begin   
     chk=where(strpos(temp,nar[j]) gt -1,count)
     if count gt 0 then new_nar=append_arr(new_nar,temp(chk[0]))
    endfor
    if exist(new_nar) then begin
     gbo.noaa=arr2str(new_nar,delim='+')
     if verbose then message,'Using supplied NOAA: '+gbo.noaa,/cont
    endif
   endif
  endif
 endelse

 gbo.submitted=anytim2tai(!stime)
 gbo.file=fname
 update_gbo,gbo,/no_save
 found=1b

endfor

purge=1b-keyword_set(no_purge)
if found and purge then purge_gbo,/save

return & end


;-----------------------------------------------------------------------------
;-- add GBO record from a file

pro add_gbo,file,gbo,pid=pid

if is_blank(file) then return
data=rd_ascii(file)
def_gbo,gbo

tags=tag_names(gbo)
np=n_elements(data)
for i=0l,np-1 do begin
 colon=strpos(data[i],':')
 if colon gt -1 then begin
  pre=strmid(data[i],0,colon)
  chk=grep(pre,tags,index=index)
  post=strmid(data[i],colon+1,strlen(data[i]))
  pre=strlowcase(trim2(pre))

;-- handle times

  if pre eq 'submitted' then post=anytim2tai(!stime) 
  if pre eq 'dstart' then begin
   dstart=anytim2utc(post) & post=dstart.mjd
  endif
  if pre eq 'dend' then begin
   dend=anytim2utc(post) & post=dend.mjd
  endif

;-- handle info

  if pre eq 'info' then begin   
   if (i lt (np-1)) then post=post+' '+arr2str(data[i+1:np-1],delim=' ')
  endif

  if index[0] gt -1 then gbo.(index[0])=post
 endif
endfor

update_gbo,gbo,err=err

if is_string(pid) then begin
 err=trim2(err)
 if err eq '' then mess='Record successfully added' else $
  mess=err
 openw,lun,pid,/get_lun,error=error
 printf,lun,mess
 close_lun,lun
 if error eq 0 then chmod,pid,/a_write
endif 

message,mess,/cont

return & end

;-----------------------------------------------------------------------------
;-- help on GBO records

pro help_gbo,index,last=last,first=first

com_gbo
common gbo_com
restore_gbo,/no_lock

np=n_elements(gbo_records)
if np eq 0 then begin
 help,gbo_records
 return
endif

if exist(index) then begin
 if (index lt 0) or (index ge np) then begin
  message,'Input index out of range',/cont
  return
 endif
endif else begin
 if keyword_set(first) then index=0
 if keyword_set(last) then index=np-1
endelse
 
if exist(index) then help,/st,gbo_records[index] else help,gbo_records


return & end

;----------------------------------------------------------------------------
;-- cache search results

pro cache_gbo,fid,records,get=get,reset=reset,trange=trange

; FID      :unique FILEID  associated with search
; RECORDS  :records: search records found
; TRANGE    : tstart,tend of search

common cache_gbo,fid_records

if keyword_set(reset) then begin
 delvarx,fid_records
 return
endif

if keyword_set(get) then records=-1
if not exist(fid) then return
fid=trim2(string(fid))

;-- check what is already cached

have_cache=is_struct(fid_records)
in_cache=0
if have_cache then begin
 chk=where(trim2(fid) eq strtrim(fid_records.fid,2),count)
 in_cache=count gt 0
endif

;-- retrieve search records

if keyword_set(get) then begin
 if in_cache then begin
  str_records=fid_records[chk[0]].records
  records=long(str2arr(str_records,delim=','))
  trange=fid_records[chk[0]].trange
 endif else records=-1
 message,'Retrieving from cache',/cont
 return
endif

;-- cache search records

if not exist(records) then return
str_records=trim2(arr2str(records,delim=','))
new_record={fid:fid,records:str_records,trange:['','']}
vtime=valid_time(trange,count=count) 
if count eq 2 then $
 new_record.trange=[anytim2utc(trange[0],/vms),anytim2utc(trange[1],/vms)] 
if in_cache then begin
 fid_records(chk[0]).records=new_record.records
 fid_records(chk[0]).trange=new_record.trange 
endif else begin
 if exist(fid_records) then fid_records=[temporary(fid_records),new_record] else $
  fid_records=new_record
endelse

return & end

;-----------------------------------------------------------------------------
;-- print GBO search results in HTML format

pro print_search,records,istart=istart,ofile=ofile,$
     chunks=chunks,fid=fid,view=view,iend=iend,iref=iref

com_gbo
common gbo_com

restore_gbo

@html_tags

ndb=n_elements(gbo_records)
if ndb eq 0 then return

;-- check if records are in cache (identified by fid)

ids=-1
if not exist(records) then begin
 if exist(fid) then cache_gbo,fid,records,/get
endif else begin
 if is_string(records) then begin
  openr,lun,records,/get_lun
  np=0
  readf,lun,np
  if np gt 0 then begin
   records=intarr(np)
   readf,lun,records
  endif
  close_lun,lun
 endif
endelse

if is_struct(records) then ids=records.id else ids=records
np=n_elements(ids)


if not exist(iref) then iref=0 else iref=0 > iref < (np-1)
if not exist(chunks) then chunks=15 else chunks=fix(chunks) > 1
if not exist(istart) then istart=0 else istart=fix(istart) < (np-1)
if not exist(iend) then iend=-1 else iend=fix(iend)
if iend lt 0 then iend=(istart+chunks-1)
iend=iend < (np-1)

if is_string(ofile) then begin                           
 if file_test(ofile,/write) then openu,lun,ofile,/get_lun else $
  openw,lun,ofile,/get_lun
endif else lun=-1                                               

title="Max Millennium Catalog Search Results"
gbo_server,server
base=server+'/'
title1='<base href="'+base+'"><title>'+title+'</title>'

body='<body bgcolor="white" vlink="purple" link="blue" alink="blue" leftmargin="0" rightmargin="0" >'

opening='<center><h1>'+title+'</h1></center>'
space=space+space+space+space

if exist(fid) then begin
 fid=trim2(num2str(fid))
 gbo_search='gbo_search.pl?fid='+fid
endif else begin
 gbo_search='gbo_search.pl'
 fid='""'
endelse

printf,lun,'<html>'
printf,lun,'<head>'
printf,lun,title1
printf,lun,'<script language="Javascript" src="js_lib/gbo_form.js"></script>'
printf,lun,'<script language="Javascript" src="js_lib/utility.js"></script>'
printf,lun,transpose(style)
printf,lun,'</head>'

printf,lun,body


blank='<img src="../img/space.gif" width="80" height="1" border="0">'

printf,lun,'<table border=0 width="100%" height="100%"  cellpadding="0" cellspacing="0" >'
printf,lun,'<tr>'
printf,lun,'<td width="15" bgcolor="lightskyblue">'+blank+'</td>'
printf,lun,'<td width="100%" valign="top">'

printf,lun,opening


printf,lun,'<br><p> &nbsp <a href="/cgi-bin/gbo_form.pl">'
printf,lun,'<img src="img/writit2.gif" height=30 width=30 border=0><b>Add entry</b></a>'
printf,lun,' &nbsp '
printf,lun,'<a href="/cgi-bin/'+gbo_search+'">'
printf,lun,'<img src="img/ffinder.gif" width=30 height=30 border=0>'
printf,lun,'<b>New search</b></a><br><p>'


printf,lun,hr+br

;-- handle zero match result

istart_s=num2str(istart+1)
iend_s=num2str(iend+1)

form=''

if (np eq 1) and (ids[0] eq -1) then begin
 matches='No matches found'
 found=0
endif else begin
 if np eq 1 then match=' match.' else match=' matches.'
 if iend gt istart then begin
  matches=' &nbsp Results &nbsp '+istart_s+' - '+iend_s+' &nbsp of &nbsp '+num2str(np)+match
 endif else matches=' &nbsp Result '+istart_s+' &nbsp of &nbsp '+num2str(np)+match
 found=1
 gbo_match="gbo_match(this.form,"+trim2(chunks)+","+trim2(np)+","+trim2(fid)+");"
 button='&nbsp<input type="button" onClick="'+gbo_match+'" value="Go">&nbsp'
 go='<a href="javascript:void '+gbo_match+'"> number </a>'
 form='&nbsp Starting match # &nbsp <input type="text" name="istart" size="4">'+button
endelse


;-- link to search results

chart=''
if (found gt 0) then begin

 url="'/cgi-bin/gbo_plot.pl?fid="+fid+"','650','750','png'"
 java='javascript:void popup('+url+');'

 gbo_server,server
 url_wait="'"+server+"/gbo/gbo_wait.html','700','900','png'"
 java_wait='javascript:void popup('+url_wait+');'

 chart='&nbsp <b> <a href="'+java+'"> Plot </a> search results chart </b><br><br> '
endif
printf,lun,bold+'<form onSubmit="return false;">'+matches+form+'</form>'+ebold+chart+hr+br

nstart=0
if found then begin

 next_gbo,np,iref,lun=lun,chunks=chunks,fid=fid,current=istart,view=view
 printf,lun,'<br>'
 cur_color="white"
 for i=istart,iend do begin
  chk=where(ids[i] eq gbo_records.id,count)
  if count gt 0 then begin
   color="lightyellow"
   if color eq cur_color then color="white" 
   printf,lun,'<table border=0 cellpadding=1 cellspacing=0>'
   cur_color=color
   printf,lun,'<tr><td align=right> &nbsp '+num2str(i+1)+'. &nbsp </td>'
   printf,lun,'<td align=left bgcolor='+color+'>'
   index=chk[0]
   id='rec='+num2str(ids[i])

   url=id
   if fid ne '' then url=id+'&amp;fid='+fid 

   temp=gbo_records[index]
   obs=temp.observatory
   tstart=anytim2utc({time:temp.tstart,mjd:temp.dstart},/vms,/trun)
   tstop=anytim2utc({time:temp.tend,mjd:temp.dend},/vms,/trun)
   type=str_replace(temp.type,'+',', ')
   line="<b>Observatory: </b>"+obs+br
   line=line+"<b>Observations Start: </b>"+tstart
   line=line+"<b>&nbsp &nbsp End : </b>"+tstop+br  
   line=line+"<b>Data Type(s): </b>"+type
   subtype=str_replace(temp.subtype,'+',', ')
   if is_string(subtype) then line=line+', '+subtype
   line=line+br
   line=line+"<a href='/cgi-bin/gbo_print.pl?"+url+"'><b><font size=-1>More details...</font></b></a>"+br
   printf,lun,line
   printf,lun,'</td></tr></table><br>'
  endif
 endfor
 if abs(istart-iend+1) gt 5 then $
  next_gbo,np,iref,lun=lun,chunks=chunks,fid=fid,current=istart,view=view

endif

if found then printf,lun,br+hr

printf,lun,'</td><td width="50"> &nbsp </td></tr></table>'

printf,lun,'</body>'
;printf,lun,'<head>'
;printf,lun,meta
;printf,lun,'</head>'
printf,lun,'</html>'


if lun gt -1 then begin
 close_lun,lun
 chmod,ofile,/a_write
endif

return & end

;-----------------------------------------------------------------------------
;-- show GBO record in HTML format

pro print_gbo,rec,ofile=ofile

@html_tags

;-- GBO header include file

db_gbo
common gbo_com
restore_gbo

;-- open file or pipe for output

if is_string(ofile) then begin                           
 if file_test(ofile,/write) then openu,lun,ofile,/get_lun else $
  openw,lun,ofile,/get_lun
endif else lun=-1      

;-- print content header

print_content,lun

;-- catch errors 

error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 print_html,lun,'Unable to process catalog request: '+err_state()
 return
endif

if not is_struct(gbo_records) then begin
 err='No records found in catalog database'
 print_html,lun,err
 return
endif

if not exist(rec) then rec=-1
get_gbo,rec,result,count=count
if count eq 0 then begin
 err='Non-existent catalog record: '+num2str(rec)
 print_html,lun,err
 return
endif

gbo_time,result,cstart,cend
tstart=anytim2utc(cstart,/vms,/trun)
tend=anytim2utc(cend,/vms,/trun)

yellow='<td bgcolor="lightyellow">'
blue='<tr><td bgcolor="lightskyblue">'

;-- format result in a table

src='js_lib/'
id=num2str(result.id)
gbo_server,server
base=server+'/'
action='/cgi-bin/gbo_form.pl/'+id
title='Catalog entry: '+id
printf,lun,'<html>'
printf,lun,'<head><base href="'+base+'">'
printf,lun,'<title>'+title+'</title>'
printf,lun,'<script language="Javascript" src="'+src+'gbo_form.js">'
printf,lun,'</script>'
printf,lun,'<script language="Javascript" src="'+src+'utility.js">'
printf,lun,'</script>'

printf,lun,transpose(style)
printf,lun,'</head>'
printf,lun,'<body bgcolor="white"><p>'

printf,lun,'<h2>'+title+'</h2>'
                                                           
printf,lun,'<p><form name="gbo_entry" action='+action+' method="post">'
printf,lun,'<input type="hidden" value="0" name="delete">'
printf,lun,'<table border=0 cellpadding=1 cellspacing=1><tr>'
printf,lun,'<td><b><input type="submit" Value="Edit Entry"></b>'
temp="this.form.elements['delete'].value=1;"
printf,lun,'<td><b><input type="submit" Value="Delete Entry" onClick="'+temp+'"></b>'
printf,lun,'<td><b> &nbsp &nbsp Password: <input size=10 name="password" type="password" value=""></b>'
printf,lun,'</table>'
printf,lun,'<br>'


printf,lun,'<table border=0 cellspacing=1 cellpadding=1>'

printf,lun,blue
printf,lun,bold+' Observatory: &nbsp&nbsp&nbsp&nbsp&nbsp '+ebold
printf,lun,yellow
printf,lun,space+result.observatory
                    
printf,lun,blue
printf,lun,'<b> Start Date: </b>'
printf,lun,yellow
printf,lun,space+tstart

printf,lun,blue
printf,lun,'<b> End Date: </b>'
printf,lun,yellow
printf,lun,space+tend

printf,lun,blue
printf,lun,bold+' Instrument: '+ebold
printf,lun,yellow
inst=result.instrument
if is_blank(inst) then inst='N.A'
printf,lun,space+trim2(inst)
                    
printf,lun,blue
printf,lun,bold+' Data classification(s): '+ebold
printf,lun,yellow
printf,lun,space+str_replace(strcompress(trim2(result.class)),' ',', ')

printf,lun,blue
printf,lun,bold+' Data type(s): '+ebold
printf,lun,yellow
printf,lun,space+str_replace(strcompress(trim2(result.type)),'+',', ')

printf,lun,blue
printf,lun,bold+' Data sub-type(s): '+ebold
printf,lun,yellow
printf,lun,space+str_replace(strcompress(trim2(result.subtype)),'+','<br>'+space)

printf,lun,blue
printf,lun,bold+' Data format(s): '+ebold
printf,lun,yellow
printf,lun,space+str_replace(strcompress(trim2(result.format)),'+',', ')

printf,lun,blue
printf,lun,bold+' Max Millennium OP: '+ebold
printf,lun,yellow
if is_blank(result.campaign) or stregex(result.campaign,'(000|N/A)',/bool) then printf,lun,space+' N.A ' else $
 printf,lun,space+str_replace(strcompress(trim2(result.campaign)),'+','<br>'+space)

printf,lun,blue
printf,lun,bold+' Observed GOES events: '+ebold
printf,lun,yellow

cid="'"+num2str(rec)+"'"
pre=trim2(result.goes)
if pre ne '' then begin
 id="'GOES'"
 arg=cid+','+id
 link='<a href="javascript:void gbo_display('+arg+');"> <b><font size=-1>Available</font></b></a>'
 printf,lun,space+link
endif else begin
 printf,lun,space+' N.A '
endelse
                                         
printf,lun,blue
printf,lun,bold+' Observed NOAA active regions: '+ebold
printf,lun,yellow

pre=trim2(result.noaa)
if pre ne '' then begin
 id="'NOAA'"
 arg=cid+','+id
 link='<a href="javascript:void gbo_display('+arg+');"> <b><font size=-1>Available</font></b></a>'
 printf,lun,space+link
endif else begin
 printf,lun,space+'N.A'
endelse

name=trim2(result.name)
if (name eq '') then name='N.A'
printf,lun,blue
printf,lun,bold+' Contact name: '+ebold
printf,lun,yellow
printf,lun,space+name+'</a>'

printf,lun,blue
printf,lun,bold+' Contact e-mail: '+ebold
printf,lun,yellow
email=str_replace(result.email,'<','')
email=str_replace(email,'>','')

printf,lun,space+'<a href=mailto:'+email+'><b><font size=-1>'+email+'</font></b></a>'

printf,lun,blue
printf,lun,bold+' Data URL: '+ebold
printf,lun,yellow
if trim2(result.url) ne '' then begin
 printf,lun,space+'<a href="'+result.url+'"><b><font size=-1>'+result.url+'</font></b></a>'
endif else begin
 printf,lun,space+' N.A '
endelse

if trim2(result.info) ne '' then begin
 temp=str2lines(result.info,length=80)
 add_info=1
endif else begin
 temp=''
 add_info=0
endelse

if add_info then begin
 printf,lun,blue
 printf,lun,bold+' Additional information: '+ebold
;printf,lun,yellow
 printf,lun,'<td bgcolor="lightyellow">'

 line=space+'<textarea rows=7 cols=80 name="info" wrap="virtual" '
 line=line+' onFocus="this.blur();" >'
 printf,lun,line
 for i=0,n_elements(temp)-1 do printf,lun,temp[i]
 printf,lun,'</textarea>'
endif
                                                            
printf,lun,'</table>'

printf,lun,'<p><em><font size=-1>Submitted: '+anytim2utc(result.submitted,/vms)+'</font></em>'

printf,lun,'</form>'

printf,lun,closing

if lun gt -1 then begin
 close_lun,lun
 chmod,ofile,/a_write
endif
return & end

;-----------------------------------------------------------------------------

pro list_gbo,fid,ifile,ofile=ofile

search_gbo,results,ids=ids,ifile=ifile,err=err,count=count,$
          dstart=dstart,dend=dend

header='Successful'
if count eq 0 then begin
 ids=-1
 count=1
endif
if err ne '' then header=err

;-- cache search records

if exist(fid) then begin
 if valid_time(dstart) and valid_time(dend) then begin
  day_sec=24.*3600.
  trange=[anytim2tai(dstart),anytim2tai(dend)+day_sec]
 endif
 cache_gbo,fid,ids,trange=trange
endif

if is_string(ofile) then begin
 if file_test(ofile,/write) then openu,lun,ofile,/get_lun else $
  openw,lun,ofile,/get_lun
 printf,lun,header
 printf,lun,count
 if count gt 0 then printf,lun,ids
 close_lun,lun
 chmod,ofile,/a_write
endif

;-- refresh results chart

if exist(fid) then begin
 png='/tmp/gbo*'+trim2(fid)+'.png'
 file_delete,png,/quiet
endif

return & end


;-----------------------------------------------------------------------------
;-- Display GOES and NOAA selections for particular record

pro display_gbo,rec,id,ofile=ofile
 
@gbo_header

if is_string(id) then begin
 id=strupcase(trim2(id))
 if id ne 'NOAA' then id='GOES'
endif else id='GOES'

if id eq 'GOES' then begin
 prev=trim2(result.goes)
 proc='print_gev'
endif else begin
 prev=trim2(result.noaa)
 proc='print_nar'
endelse

call_procedure,proc,tstart,tend,prev=prev,/no_select,entry=rec,out=ofile

return & end

;------------------------------------------------------------------------------
;-- load GBO record fields into cookie string

pro load_gbo,rec

get_gbo,rec,result
if not exist(result) then begin
 mess='non-existent database record -> '
 if exist(rec) then mess=mess+trim2(rec)
 message,mess,/cont
 return
endif

mon=['January','February','March','April','May','June','July','August',$
         'September','October','November','December']

fld=string(1b)
cookie=fld
cookie2=fld
cookie3=fld
cookie4=fld
if exist(result) then begin
 tstart=anytim2utc({time:0,mjd:result.dstart},/ext)
 tend=anytim2utc({time:0,mjd:result.dend},/ext)
 cookie=cookie+'day1='+trim2(tstart.day)+fld
 cookie=cookie+'month1='+trim2(mon(tstart.month-1))+fld
 cookie=cookie+'year1='+trim2(tstart.year)+fld
 cookie=cookie+'day2='+trim2(tend.day)+fld
 cookie=cookie+'month2='+trim2(mon(tend.month-1))+fld
 cookie=cookie+'year2='+trim2(tend.year)+fld
 cookie=cookie+'observatory='+trim2(result.observatory)+fld
 classes=trim2(str2arr(result.class,delim=''))
 for i=0,n_elements(classes)-1 do begin
  field=classes[i]+'='+classes[i] 
  if i eq 0 then vec=field else vec=vec+field
 endfor
 cookie=cookie+vec+fld
 cookie=cookie+'type='+trim2(result.type)+fld
 cookie=cookie+'subtype='+trim2(result.subtype)+fld
 cookie=cookie+'format='+trim2(result.format)+fld
 cookie=cookie+'ops='+trim2(result.campaign)+fld
 cookie=cookie+'url='+trim2(result.url)+fld
 cookie=cookie+'email='+trim2(result.email)
 cookie=cookie+'NOAA='+trim2(result.noaa)
 cookie3=fld+'GOES='+trim2(result.goes)
 cookie4=fld+'info='+trim2(result.info)+fld
endif
print,cookie
print,cookie2
print,cookie3
print,cookie4

return & end

;-----------------------------------------------------------------------------

pro next_gbo,np,istart,lun=lun,chunks=chunks,view=view,$
    action=action,fid=fid,current=current

if not exist(np) then return
if np eq 0 then return
if not exist(view) then view=5 else view = fix(view) > 1
if not exist(chunks) then chunks=15 else chunks=fix(chunks) > 1
if not exist(istart) then istart=0
if not exist(lun) then lun=-1

if is_blank(action) then action="/cgi-bin/gbo_result.pl?"
if not is_number(fid) then fid='' else fid=num2str(fid)
fid=trim2(fid)
amp='&amp;'
full=''
nobreak=' &nbsp '

;-- add a link to return to previous page


if istart gt 0 then begin
 iref=num2str(istart)
 count=0
 kend=(istart-1) > 0 
 repeat begin
  kstart=(kend-chunks+1) > 0
  count=count+1
  quit=(count eq view) or (kstart eq 0)
  if not quit then kend=(kstart-1) > 0  
 endrep until quit 
 kend=kstart+chunks-1
 url=action+'istart='+num2str(kstart)+amp+'iend='+num2str(kend)+amp+'iref='+num2str(kstart)
 if fid ne '' then url=url+amp+'fid='+fid
 line='<a href="'+url+'">'+'&nbsp Previous '+'</a>'
 full=line
endif

istart1=istart
count=0
repeat begin
 iend1=(istart1+chunks-1) < (np-1)
 url=action+'istart='+num2str(istart1)
 if iend1 gt istart1 then url=url+amp+'iend='+num2str(iend1)
 if fid ne '' then url=url+amp+'fid='+fid
 if (count+1) gt view then begin
  line=' Next &nbsp'
  url=url+amp+'iref='+num2str(istart1)
 endif else begin
  line=num2str(istart1+1)
  if iend1 gt istart1 then line=line+'-'+num2str(iend1+1)
  if (istart gt 0) then url=url+amp+'iref='+iref
 endelse
 href='<a href="'+url+'"> '
 no_link=0b
 if exist(current) then begin
  no_link=(current le iend1) and (current ge istart1) 
 endif
 if no_link then line='<font color="red">'+line+'</font>' else $
  line=href+line+'</a>'
 istart1=iend1+1
 if full(0) eq '' then full=line else full=append_arr(full,line,/no_copy)
 count=count+1
endrep until ((istart1 ge np) or count gt view) 

if count gt 0 then begin
 printf,lun,'<b><center>'
 printf,lun,nobreak+'[&nbsp'+trim2(full)+'&nbsp]'
 printf,lun,'</b></center>'
endif

return & end

;-----------------------------------------------------------------------------
;-- setup commons

pro db_gbo

com_gbo

return & end



