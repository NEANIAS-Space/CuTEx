;+
; Project     : IHY
;
; Name        : ihy_db
;
; Purpose     : IDL database for International Heliophysical Year Participants 
;
; Category    : Database
;
; Inputs      : See component routines
;
; Outputs     : Database save file in $IHY_DB
;
; Keywords    : See component routines
;
; History     : 2-Aug-1999,  D.M. Zarro.  Written
;             : 6-July-2004, B.J. Thompson, modified for IHY
;
; Contact     : BARBARA.J.THOMPSON@NASA.GOV
;-
;-----------------------------------------------------------------------------
;-- ** setup commons

pro ihy_com

common ihy_com,ihy_records

return & end

;-----------------------------------------------------------------------------
;-- ** server root

pro ihy_server,server

server='http://orpheus.nascom.nasa.gov/~zarro'

return & end

;-----------------------------------------------------------------------------
;-- ** get lock file name

function ihy_lock_file

lock_file=''
db_file=chklog('IHY_DB')
if db_file eq '' then return,''
break_file,db_file,disk,dir
ihy_dir=disk+dir
if not test_dir(ihy_dir) then return,''
lock_file=concat_dir(ihy_dir,'ihy_lock.dat')
return,lock_file & end

;-----------------------------------------------------------------------------
;-- ** check if database locked

function ihy_locked,err=err,_extra=extra

locked=0b
caller=get_caller()
lock_file=ihy_lock_file()
if not check_lock(lock_file,/quiet,err=err,_extra=extra) then begin
 message,err,/cont
 return,1b
endif

return,0b
end

;----------------------------------------------------------------------------
;-- ** lock database

pro ihy_lock,err=err,_extra=extra

lock_file=ihy_lock_file()
if lock_file eq '' then return
apply_lock,lock_file,err=err,_extra=extra
if err ne '' then message,err,/cont

return & end

;----------------------------------------------------------------------------
;-- **  unlock database

pro ihy_unlock,err=err,_extra=extra

err=''
lock_file=ihy_lock_file()
rm_lock,lock_file,err=err,/quiet,_extra=extra
if err ne '' then message,err,/cont

return & end

;------------------------------------------------------------------------------
; ** 
pro  ihy_time,ihy,gstart,gend,verbose=verbose

gstart={mjd:ihy.dstart,time:ihy.tstart}
gstart=anytim2tai(gstart)

if n_params() eq 3 then begin
 gend={mjd:ihy.dend,time:ihy.tend}
 gend=anytim2tai(gend)
endif

if keyword_set(verbose) then begin
 print,anytim2utc(gstart,/vms)
 if n_params() eq 3 then print,anytim2utc(gend,/vms)
endif

return & end
  
;----------------------------------------------------------------------------
; **

pro ihy_printf,unit,index

common ihy_com

robs=ihy_records[index].observatory
rstart=ihy_records[index].dstart
rend=ihy_records[index].dend

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
;-- ** define template structure
                          
pro ihy_def,ihy           

ihy= {ihy_db,id:-1l, lastname:'',$
       firstname:'', middleinit:'',title:'',institution:'', country:'', $
       sciorgs:'',sciints:'', observatory:'', obsrep:'', dataexp:'',$ 
       email:'', url:'',phone:'',$
       address:'',comments:'',submitted:'',deleted:0b}

return & end

;-----------------------------------------------------------------------------
;-- map IHY internal database names to actual names

pro ihy_map,out

ihy_def,ihy

ihy.firstname='first name'
ihy.lastname='last name'
ihy.middleinit='middle initial'
ihy.title='title'
ihy.institution='institution'
ihy.country='country'
ihy.sciorgs='scientific organizations'
ihy.sciints='science interests'
ihy.address='contact address'
ihy.phone='contact telephone'
ihy.email='contact email'
ihy.url='url'
ihy.dataexp='data expert'
ihy.obsrep='observatory representative'
ihy.observatory='observatory/instrument'
ihy.comments='comments'
ihy.submitted='submitted'

tags=tag_names(ihy)
np=n_elements(tags)
out=strarr(np)
for i=0,np-1 do out[i]=ihy.(i)

return
end

;---------------------------------------------------------------------------
;-- **  define IHY_DB env

pro ihy_set,file,err=err         

if is_string(file) then begin
 chk=loc_file(file,count=count,err=err)
 if count eq 0 then message,'Warning - '+err,/cont
 mklog,'IHY_DB',file
endif
return & end

;-----------------------------------------------------------------------------
;-- ** show IHY_DB env definition

pro ihy_show

print,'IHY_DB --> '+chklog('IHY_DB')

return & end

;-----------------------------------------------------------------------------
;-- ** purge database
                              
pro ihy_purge,err=err,save=save
err=''

ihy_com
common ihy_com
                     
ihy_restore,err=err
if err ne '' then return
                
if not is_struct(ihy_records) then return


bad=where( (ihy_records.lastname eq '') or $
           (ihy_records.email eq '') ,count)

if count gt 0 then ihy_records[bad].deleted=1b
chk=where(ihy_records.deleted eq 0b,count)

if count eq n_elements(ihy_records) then begin
 if keyword_set(save) then ihy_save
 return
endif

if (count gt 0) then begin
 ihy_records=ihy_records[chk]
 ihy_records.id=lindgen(count)
endif else begin
 ihy_records=clear_struct(ihy_records[0])
 ihy_records.id=-1
endelse

ihy_records=stc_compress(ihy_records,/no_copy)
ihy_save

return & end

;----------------------------------------------------------------------------
;-- ** check if DB is in use

pro ihy_use

ihy_com
common ihy_com

return & end

;-----------------------------------------------------------------------------
;-- ** clear memory copy of database

pro ihy_clear

ihy_com
common ihy_com

ihy_use

delvarx,ihy_records

return & end

;-----------------------------------------------------------------------------
; **
function ihy_valid,ihy     ;-- check for valid database record structure

if not is_struct(ihy) then return,0b
ihy_def,ihy_template
ihy_zero=clear_struct(ihy)
return,match_struct(ihy_template,ihy_zero,dtag=dtag,/type)
end

;------------------------------------------------------------------------------
; **

pro ihy_restore,err=err,force=force,no_lock=no_lock     ;-- read database from file

err=''
if (1b-keyword_set(no_lock)) then begin
 if ihy_locked(err=err) then return
endif

ihy_com
common ihy_com

force=(not is_struct(ihy_records)) or keyword_set(force)

if force then begin
 ihy_use
 chk=loc_file('IHY_DB',count=count)
 if count eq 0 then return
 message,'Restoring from '+chk[0],/cont
 restore,chk[0]
endif

return & end

;-------------------------------------------------------------------------
;-- merge IHY_DB into new structure template
; **
pro ihy_merge,template

if not is_struct(template) then return

ihy_com
common ihy_com
ihy_restore
np=n_elements(ihy_records)
ihy_new=replicate(template[0],np)
copy_struct,ihy_records,ihy_new
ihy_records=ihy_new
ihy_save

return & end


;--------------------------------------------------------------------------
; **
pro ihy_save,err=err             ;-- save database to file

err=''
if ihy_locked(err=err) then return

ihy_com
common ihy_com

chk=chklog('IHY_DB')
if chk[0] eq '' then begin
 err='Environment variable IHY_DB needs to point to database filename'
 message,err,/cont
 return
endif

;-- test for write access

break_file,chk[0],dsk,dir
if not test_dir(dsk+dir,err=err) then return

if is_struct(ihy_records) then begin
 save_file=chk[0]
 temp_file=save_file+'_temp'
 message,'Saving to '+temp_file,/cont
 save,ihy_records,file=temp_file
 message,'Copying to '+save_file,/cont
 espawn,'cp '+temp_file+' '+save_file,/back
endif

return & end

;---------------------------------------------------------------------------
;-- remove duplicates from database

; **
pro ihy_remdup

ihy_com
common ihy_com
                     
ihy_restore
                
if not is_struct(ihy_records) then return
nrec=n_elements(ihy_records)

if nrec lt 2 then return

excl=['id','file','submitted']
ihy_records=stc_uniq(ihy_records,excl=excl)

ihy_save

return & end

;----------------------------------------------------------------------------

; **
;-- update database records

pro ihy_update,ihy,err=err,replace=replace,remove=remove,ofile=ofile,$
               no_save=no_save,check_dup=check_dup

err=''

;-- read latest ihy_records into memory

ihy_com
common ihy_com
ihy_restore,err=err

if err ne '' then begin
 ihy_output,err,ofile
 return
endif

remove=keyword_set(remove)
replace=keyword_set(replace)
edit=replace or remove
                      
;-- check if IHY record or ID # entered

cur_id=-1
if not exist(ihy) then return
if is_struct(ihy) then begin
 if ihy_valid(ihy) then cur_id=ihy.id
endif else begin
 if is_number(ihy) then cur_id=ihy
endelse

if (not ihy_valid(ihy)) and (not remove) then begin
 err='Invalid IHY record'
 ihy_output,err,ofile
 return
endif

;-- merge new record

if is_struct(ihy) then ihy=stc_compress(ihy)
                      
if (not is_struct(ihy_records)) and (not edit) then begin
 id=0 & ihy.id=id & ihy_records=ihy
endif else begin
 if cur_id eq -1 then edit=0
 if edit then begin
  chk=where(cur_id eq ihy_records.id,count)
  if count gt 0 then begin 
   if replace then begin
    ihy_records[chk[0]]=ihy
    if count gt 1 then ihy_records[chk[1:count-1]].deleted=1b
   endif else begin
    ihy_records[chk].deleted=1b
    if ihy_valid(ihy) then ihy.deleted=1b
   endelse
  endif else begin
   err='record '+num2str(cur_id)+' not found'
   ihy_output,err,ofile   
   return
  endelse
 endif else begin
  id=max(ihy_records.id)+1l & ihy.id=id
  if keyword_set(check_dup) then begin
   exclude=['submitted','id']
   chk=stc_where(ihy,ihy_records,exclude=exclude,count=count)
   if count gt 0 then begin
    err='Duplicate entry already in database'
    ihy_output,err,ofile
    return
   endif
  endif
  if is_struct(ihy_records) then ihy_records=[temporary(ihy_records),ihy] else ihy_records=ihy
 endelse
endelse

;-- update ihy_records file

if (err eq '') and (1b-keyword_set(no_save)) then begin
 ihy_output,'successful',ofile
 ihy_save
endif


return & end

;-----------------------------------------------------------------------------
;-- print message to terminal and optional file
; **

pro ihy_output,mess,ofile

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
;-- get series of IHY records

; **** what about .id? Change that? 

pro ihy_get,ids,results,count=count,deleted=deleted

;-- initialize

count=0 & delvarx,results
if not exist(ids) then return 

ihy_db
common ihy_com
ihy_restore
if not exist(ihy_records) then return

;-- find records, filtering deleted ones

chk=where_arr(ihy_records.id,ids,count)
if count gt 0 then begin
 results=ihy_records[chk]
 if (1b-keyword_set(deleted)) then begin
  chk=where(results.deleted eq 0b,count)
  if count gt 0 then results=results[chk] else delvarx,results
 endif
endif

return & end

;-------------------------------------------------------------------------------
;-- search IHY_DB

; ****   fix variables!
pro ihy_search,results,ids=ids,dstart=dstart,dend=dend,class=class,type=type,$
               observatory=observatory,deleted=deleted,ifile=ifile,err=err,$
               campaign=campaign,$
               subtype=subtype,goes=goes,noaa=noaa,format=format,count=count

;-- initialize

err=''
count=0 & delvarx,results,ids

ihy_com
common ihy_com
ihy_restore,/no_lock
if not exist(ihy_records) then begin
 err='IHY DB is empty'
 return
endif
ihy_purge

;-- read inputs from file

if is_string(ifile) then begin
 data=rd_ascii(ifile,lun=lun)
 if (n_elements(data) eq 1) and (trim2(data[0]) eq "") then begin
  err='Null search request'
  return
 endif
 ihy_def,ihy
 tags=tag_names(ihy)
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
  chk=where(ihy_records.dstart ge tstart.mjd,count)
  if count eq 0 then return
 end

 valid_tend and (not valid_tstart): begin
  chk=where(ihy_records.dend le (tend.mjd+1),count)
  if count eq 0 then return
 end

 valid_tend and valid_tstart: begin

 d1= (ihy_records.dstart gt tend.mjd) 

 d2= (ihy_records.dend gt tend.mjd)

 d3= (ihy_records.dstart lt tstart.mjd) 

 d4= (ihy_records.dend lt tstart.mjd) 
  
 ochk=where( (d1 and d2) or (d3 and d4),ncomplement=count,complement=chk)
 if count eq 0 then return
 end
 else: search_all=1b
endcase

if search_all then results=ihy_records else results=ihy_records[chk] 
count=n_elements(results)

;-- starting searching for individual components

; ****

ihy_check,'observatory',observatory,results,count=count,delim='+',/exact
ihy_check,'class',class,results,count=count,delim=' '
ihy_check,'type',type,results,count=count,delim='+'
ihy_check,'subtype',subtype,results,count=count,delim='+'
ihy_check,'format',format,results,count=count,delim='+'
ihy_check,'goes',goes,results,count=count,delim='+'
ihy_check,'noaa',noaa,results,count=count,delim='+'
ihy_check,'campaign',campaign,results,count=count,delim='+'
if (1b-keyword_set(deleted)) and (count gt 0) then begin
 chk=where(results.deleted eq 0b,count)
 if count gt 0 then results=results[chk] else delvarx,results
endif

count=n_elements(results)
if count gt 0 then begin
 ihy_time,results,gstart
 s=bsort(gstart)
 results=results[s]
 if count eq 1 then results=results[0]
 ids=results.id
endif

return & end

;-----------------------------------------------------------------------------
;-- check IHY fields for specified values

pro ihy_check,field,value,results,count=count,exclusive=exclusive,$
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
;--  ** add IHY record from a file

pro ihy_add,file,ihy,pid=pid,err=err

if is_blank(file) then return
chk=loc_file(file,err=err,count=count)
if count eq 0 then return

data=rd_ascii(file)
ihy_def,ihy
ihy_map,map
np=n_elements(data)
for i=0l,np-1 do begin
 chk=stregex(data[i],'<b>(.+): *</b>(.*)',/sub,/extract)
 if is_string(chk[1]) then begin
  pre=trim(strlowcase(chk[1]))
  post=trim(chk[2])
  index=where(pre eq map,count)
  if count gt 0 then begin

;-- handle info

   if pre eq 'comments' then begin   
    if (i lt (np-1)) then post=post+' '+arr2str(data[i+1:np-1],delim=' ')
   endif
   
   ihy.(index[0])=post
  endif 
 endif
endfor

ihy_update,ihy,err=err

if is_string(pid) then begin
 err=trim2(err)
 if err eq '' then mess='Record successfully added' else $
  mess=err
 openw,lun,pid,/get_lun,error=error
 printf,lun,mess
 close_lun,lun
 if error eq 0 then chmod,pid,/a_write
endif 

if is_string(mess) then message,mess,/cont

return & end

;-----------------------------------------------------------------------------
;-- ** help on IHY records

pro ihy_help,index,last=last,first=first

ihy_com
common ihy_com
ihy_restore,/no_lock

np=n_elements(ihy_records)
if np eq 0 then begin
 help,ihy_records
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
 
if exist(index) then help,/st,ihy_records[index] else help,ihy_records


return & end

;----------------------------------------------------------------------------
;-- cache search results
; ** later
pro ihy_cache,fid,records,get=get,reset=reset,trange=trange

; FID      :unique FILEID  associated with search
; RECORDS  :records: search records found
; TRANGE    : tstart,tend of search

common ihy_cache,fid_records

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
;-- print IHY search results in HTML format
; **** later

pro ihy_print_search,records,istart=istart,ofile=ofile,$
     chunks=chunks,fid=fid,view=view,iend=iend,iref=iref

ihy_com
common ihy_com

ihy_restore

@html_tags

ndb=n_elements(ihy_records)
if ndb eq 0 then return

;-- check if records are in cache (identified by fid)

ids=-1
if not exist(records) then begin
 if exist(fid) then ihy_cache,fid,records,/get
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


if not exist(iref) then iref=0 else iref=0 > iref <  (np-1)
if not exist(chunks) then chunks=15 else chunks=fix(chunks) > 1
if not exist(istart) then istart=0 else istart=fix(istart) <  (np-1)
if not exist(iend) then iend=-1 else iend=fix(iend)
if iend lt 0 then iend=(istart+chunks-1)
iend=iend <  (np-1)

if is_string(ofile) then begin                           
 if file_test(ofile,/write) then openu,lun,ofile,/get_lun else $
  openw,lun,ofile,/get_lun
endif else lun=-1                                               

title="Max Millennium Catalog Search Results"
ihy_server,server
base=server+'/'
title1='< base href="'+base+'">< title>'+title+'< /title>'

body='< body bgcolor="white" vlink="purple" link="blue" alink="blue" leftmargin="0" rightmargin="0" >'

opening='< center>< h1>'+title+'< /h1>< /center>'
space=space+space+space+space

if exist(fid) then begin
 fid=trim2(num2str(fid))
 ihy_search='ihy_search.pl?fid='+fid
endif else begin
 ihy_search='ihy_search.pl'
 fid='""'
endelse

printf,lun,'< html>'
printf,lun,'< head>'
printf,lun,title1
printf,lun,'< script language="Javascript" src="js_lib/ihy_form.js">< /script>'
printf,lun,'< script language="Javascript" src="js_lib/utility.js">< /script>'
printf,lun,transpose(style)
printf,lun,'< /head>'

printf,lun,body


blank='< img src="../img/space.gif" width="80" height="1" border="0">'

printf,lun,'< table border=0 width="100%" height="100%"  cellpadding="0" cellspacing="0" >'
printf,lun,'< tr>'
printf,lun,'< td width="15" bgcolor="lightskyblue">'+blank+'< /td>'
printf,lun,'< td width="100%" valign="top">'

printf,lun,opening


printf,lun,'< br>< p>   < a href="/cgi-bin/ihy_form.pl">'
printf,lun,'< img src="img/writit2.gif" height=30 width=30 border=0>< b>Add entry< /b>< /a>'
printf,lun,'   '
printf,lun,'< a href="/cgi-bin/'+ihy_search+'">'
printf,lun,'< img src="img/ffinder.gif" width=30 height=30 border=0>'
printf,lun,'< b>New search< /b>< /a>< br>< p>'


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
  matches='   Results   '+istart_s+' - '+iend_s+'   of   '+num2str(np)+match
 endif else matches='   Result '+istart_s+'   of   '+num2str(np)+match
 found=1
 ihy_match="ihy_match(this.form,"+trim2(chunks)+","+trim2(np)+","+trim2(fid)+");"
 button=' < input type="button" onClick="'+iHo_IHYCH+'" value="Go"> '
 go='< a href="javascript:void '+gbo_match+'"> number < /a>'
 form='  Starting match #   < input type="text" name="istart" size="4">'+button
endelse


;-- link to search results

chart=''
if (found gt 0) then begin

 url="'/cgi-bin/gbo_plot.pl?fid="+fid+"','650','750','png'"
 java='javascript:void popup('+url+');'

 gbo_server,server
 url_wait="'"+server+"/gbo/gbo_wait.html','700','900','png'"
 java_wait='javascript:void popup('+url_wait+');'

 chart='  < b> < a href="'+java+'"> Plot < /a> search results chart < /b>< br>< br> '
endif
printf,lun,bold+'< form onSubmit="return false;">'+matches+form+'< /form>'+ebold+chart+hr+br

nstart=0
if found then begin

 next_gbo,np,iref,lun=lun,chunks=chunks,fid=fid,current=istart,view=view
 printf,lun,'< br>'
 cur_color="white"
 for i=istart,iend do begin
  chk=where(ids[i] eq gbo_records.id,count)
  if count gt 0 then begin
   color="lightyellow"
   if color eq cur_color then color="white" 
   printf,lun,'< table border=0 cellpadding=1 cellspacing=0>'
   cur_color=color
   printf,lun,'< tr>< td align=right>   '+num2str(i+1)+'.   < /td>'
   printf,lun,'< td align=left bgcolor='+color+'>'
   index=chk[0]
   id='rec='+num2str(ids[i])

   url=id
   if fid ne '' then url=id+'&fid='+fid 

   temp=gbo_records[index]
   obs=temp.observatory
   tstart=anytim2utc({time:temp.tstart,mjd:temp.dstart},/vms,/trun)
   tstop=anytim2utc({time:temp.tend,mjd:temp.dend},/vms,/trun)
   type=str_replace(temp.type,'+',', ')
   line="< b>Observatory: < /b>"+obs+br
   line=line+"< b>Observations Start: < /b>"+tstart
   line=line+"< b>    End : < /b>"+tstop+br  
   line=line+"< b>Data Type(s): < /b>"+type
   subtype=str_replace(temp.subtype,'+',', ')
   if is_string(subtype) then line=line+', '+subtype
   line=line+br
   line=line+"< a href='/cgi-bin/gbo_print.pl?"+url+"'>< b>< font size=-1>More details...< /font>< /b>< /a>"+br
   printf,lun,line
   printf,lun,'< /td>< /tr>< /table>< br>'
  endif
 endfor
 if abs(istart-iend+1) gt 5 then $
  next_gbo,np,iref,lun=lun,chunks=chunks,fid=fid,current=istart,view=view

endif

if found then printf,lun,br+hr

printf,lun,'< /td>< td width="50">   < /td>< /tr>< /table>'

printf,lun,'< /body>'
;printf,lun,'< head>'
;printf,lun,meta
;printf,lun,'< /head>'
printf,lun,'< /html>'


if lun gt -1 then begin
 close_lun,lun
 chmod,ofile,/a_write
endif

return & end

;-----------------------------------------------------------------------------
;-- show IHY record in HTML format
; **

pro ihy_print,rec,ofile=ofile

@html_tags

;-- IHY header include file

ihy_db
common ihy_com
ihy_restore

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

if not is_struct(ihy_records) then begin
 err='No records found in catalog database'
 print_html,lun,err
 return
endif

if not exist(rec) then rec=-1
ihy_get,rec,result,count=count
if count eq 0 then begin
 err='Non-existent catalog record: '+num2str(rec)
 print_html,lun,err
 return
endif

ihy_time,result,cstart,cend
tstart=anytim2utc(cstart,/vms,/trun)
tend=anytim2utc(cend,/vms,/trun)

yellow='< td bgcolor="lightyellow">'
blue='< tr>< td bgcolor="lightskyblue">'

;-- format result in a table

src='js_lib/'
id=num2str(result.id)
ihy_server,server
base=server+'/'
action='/cgi-bin/ihy_form.pl/'+id
title='Catalog entry: '+id
printf,lun,'< html>'
printf,lun,'< head>< base href="'+base+'">'
printf,lun,'< title>'+title+'< /title>'
printf,lun,'< script language="Javascript" src="'+src+'ihy_form.js">'
printf,lun,'< /script>'
printf,lun,'< script language="Javascript" src="'+src+'utility.js">'
printf,lun,'< /script>'

printf,lun,transpose(style)
printf,lun,'< /head>'
printf,lun,'< body bgcolor="white">< p>'

printf,lun,'< h2>'+title+'< /h2>'
                                                           
printf,lun,'< p>< form name="ihy_entry" action='+action+' method="post">'
printf,lun,'< input type="hidden" value="0" name="delete">'
printf,lun,'< table border=0 cellpadding=1 cellspacing=1>< tr>'
printf,lun,'< td>< b>< input type="submit" Value="Edit Entry">< /b>'
temp="this.form.elements['delete'].value=1;"
printf,lun,'< td>< b>< input type="submit" Value="Delete Entry" onClick="'+temp+'">< /b>'
printf,lun,'< td>< b>     Password: < input size=10 name="password" type="password" value="">< /b>'
printf,lun,'< /table>'
printf,lun,'< br>'


printf,lun,'< table border=0 cellspacing=1 cellpadding=1>'

printf,lun,blue
printf,lun,bold+' Observatory:       '+ebold
printf,lun,yellow
printf,lun,space+result.observatory
                    
printf,lun,blue
printf,lun,'< b> Start Date: < /b>'
printf,lun,yellow
printf,lun,space+tstart

printf,lun,blue
printf,lun,'< b> End Date: < /b>'
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
printf,lun,space+str_replace(strcompress(trim2(result.subtype)),'+','< br>'+space)

printf,lun,blue
printf,lun,bold+' Data format(s): '+ebold
printf,lun,yellow
printf,lun,space+str_replace(strcompress(trim2(result.format)),'+',', ')

printf,lun,blue
printf,lun,bold+' Max Millennium OP: '+ebold
printf,lun,yellow
if is_blank(result.campaign) or stregex(result.campaign,'(000|N/A)',/bool) then printf,lun,space+' N.A ' else $
 printf,lun,space+str_replace(strcompress(trim2(result.campaign)),'+','< br>'+space)

printf,lun,blue
printf,lun,bold+' Observed GOES events: '+ebold
printf,lun,yellow

cid="'"+num2str(rec)+"'"
pre=trim2(result.goes)
if pre ne '' then begin
 id="'GOES'"
 arg=cid+','+id
 link='< a href="javascript:void ihy_display('+arg+');"> < b>< font size=-1>Available< /font>< /b>< /a>'
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
 link='< a href="javascript:void ihy_display('+arg+');"> < b>< font size=-1>Available< /font>< /b>< /a>'
 printf,lun,space+link
endif else begin
 printf,lun,space+'N.A'
endelse

name=trim2(result.name)
if (name eq '') then name='N.A'
printf,lun,blue
printf,lun,bold+' Contact name: '+ebold
printf,lun,yellow
printf,lun,space+name+'< /a>'

printf,lun,blue
printf,lun,bold+' Contact e-mail: '+ebold
printf,lun,yellow
email=str_replace(result.email,'< ','')
email=str_replace(email,'>','')

printf,lun,space+'< a href=mailto:'+email+'>< b>< font size=-1>'+email+'< /font>< /b>< /a>'

printf,lun,blue
printf,lun,bold+' Data URL: '+ebold
printf,lun,yellow
if trim2(result.url) ne '' then begin
 printf,lun,space+'< a href="'+result.url+'">< b>< font size=-1>'+result.url+'< /font>< /b>< /a>'
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
 printf,lun,'< td bgcolor="lightyellow">'

 line=space+'< textarea rows=7 cols=80 name="info" wrap="virtual" '
 line=line+' onFocus="this.blur();" >'
 printf,lun,line
 for i=0,n_elements(temp)-1 do printf,lun,temp[i]
 printf,lun,'< /textarea>'
endif
                                                            
printf,lun,'< /table>'

printf,lun,'< p>< em>< font size=-1>Submitted: '+anytim2utc(result.submitted,/vms)+'< /font>< /em>'

printf,lun,'< /form>'

printf,lun,closing

if lun gt -1 then begin
 close_lun,lun
 chmod,ofile,/a_write
endif
return & end

;-----------------------------------------------------------------------------

; ** later
pro ihy_list,fid,ifile,ofile=ofile

ihy_search,results,ids=ids,ifile=ifile,err=err,count=count,$
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
 ihy_cache,fid,ids,trange=trange
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
 png='/tmp/ihy*'+trim2(fid)+'.png'
 file_delete,png,/quiet
endif

return & end

;------------------------------------------------------------------------------
;-- load IHY record fields into cookie string
; **** need to customize 

pro ihy_load,rec

ihy_get,rec,result
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
if exist(result) then begin
; tstart=anytim2utc({time:0,mjd:result.dstart},/ext)
; tend=anytim2utc({time:0,mjd:result.dend},/ext)
 cookie=cookie+'lastname='+trim2(result.lastname)+fld
 cookie=cookie+'firstname='+trim2(result.firstname)+fld
 cookie=cookie+'middleinitial='+trim2(result.middleinitial)+fld
 cookie=cookie+'title='+trim2(result.title)+fld
 cookie=cookie+'institution='+trim2(result.institution)+fld
 cookie=cookie+'country='+trim2(result.country)+fld
; cookie=cookie+vec+fld
 cookie=cookie+'sciorgs='+trim2(result.sciorgs)+fld
 cookie=cookie+'observatory='+trim2(result.observatory)+fld
 cookie=cookie+'obsrep='+trim2(result.obsrep)+fld
 cookie=cookie+'dataexp='+trim2(result.dataexp)+fld
 cookie=cookie+'sciints='+trim2(result.sciints)+fld
 cookie=cookie+'address='+trim2(result.address)+fld
 cookie=cookie+'phone='+trim2(result.phone)+fld
 cookie=cookie+'email='+trim2(result.email)+fld
 cookie=cookie+'url='+trim2(result.url)+fld
 cookie=cookie+'comment='+trim2(result.comment)+fld
 cookie=cookie+'pid='+trim2(result.pid)
endif
print,cookie

return & end

;-----------------------------------------------------------------------------
; **  

pro ihy_next,np,istart,lun=lun,chunks=chunks,view=view,$
    action=action,fid=fid,current=current

if not exist(np) then return
if np eq 0 then return
if not exist(view) then view=5 else view = fix(view) > 1
if not exist(chunks) then chunks=15 else chunks=fix(chunks) > 1
if not exist(istart) then istart=0
if not exist(lun) then lun=-1

if is_blank(action) then action="/cgi-bin/ihy_result.pl?"
if not is_number(fid) then fid='' else fid=num2str(fid)
fid=trim2(fid)
amp='&'
full=''
nobreak='   '

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
 line='< a href="'+url+'">'+'  Previous '+'< /a>'
 full=line
endif

istart1=istart
count=0
repeat begin
 iend1=(istart1+chunks-1) <  (np-1)
 url=action+'istart='+num2str(istart1)
 if iend1 gt istart1 then url=url+amp+'iend='+num2str(iend1)
 if fid ne '' then url=url+amp+'fid='+fid
 if (count+1) gt view then begin
  line=' Next  '
  url=url+amp+'iref='+num2str(istart1)
 endif else begin
  line=num2str(istart1+1)
  if iend1 gt istart1 then line=line+'-'+num2str(iend1+1)
  if (istart gt 0) then url=url+amp+'iref='+iref
 endelse
 href='< a href="'+url+'"> '
 no_link=0b
 if exist(current) then begin
  no_link=(current le iend1) and (current ge istart1) 
 endif
 if no_link then line='< font color="red">'+line+'< /font>' else $
  line=href+line+'< /a>'
 istart1=iend1+1
 if full(0) eq '' then full=line else full=append_arr(full,line,/no_copy)
 count=count+1
endrep until ((istart1 ge np) or count gt view) 

if count gt 0 then begin
 printf,lun,'< b>< center>'
 printf,lun,nobreak+'[ '+trim2(full)+' ]'
 printf,lun,'< /b>< /center>'
endif

return & end

;-----------------------------------------------------------------------------
;--  ** setup commons

pro ihy_db

ihy_com

return & end

