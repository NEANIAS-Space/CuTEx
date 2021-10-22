;+
; Project     : HESSI
;
; Name        : MDI_COPY
;
; Purpose     : copy daily MDI magnetograms and intensitygrams to SYNOP_DATA
;
; Category    : synoptic gbo
;
; Syntax      : IDL> mdi_copy
;
; Keywords    : clobber = clobber existing files
;               BACK = # of days back to copy
;             
; Restrictions: Unix only
;
; History     : Written 8 March 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro mdi_copy,clobber=clobber,back=back,verbose=verbose,_extra=extra

if not test_dir('$SYNOP_DATA') then return

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return
endif

;-- copy new summary files 

chk=is_dir('$SOHO_PRIVATE/data/planning/mdi',out=out)
if not chk then begin
 err='Non-existent MDI summary directory'
 message,err,/cont
 return
endif

;-- look for files newer than BACK days

verbose=keyword_set(verbose)
if is_number(back) then tback=trim(string(back)) else tback='30'

exp1='find '+out+' -name \*igram\*  -mtime -'+tback
espawn,exp1,igrams,count=icount,/noshell

if icount eq 0 then begin
 err='No recent MDI intensitygrams found'
 message,err,/cont
endif

exp2='find '+out+' -name \*maglc\*  -mtime -'+tback
espawn,exp2,maglc,count=mcount,/noshell

if mcount eq 0 then begin
 err='No recent MDI magnetograms found'
 message,err,/cont
endif

if (mcount eq 0) and (icount eq 0) then return

if mcount gt 0 then files=maglc
if icount gt 0 then files=append_arr(files,igrams,/no_copy)

spos=strpos(files,'smdi')
inames=strmids(files,spos+1,1000)

;-- construct command to spawn copy

get_ymd,inames,ymd

synop_dir=chklog('$SYNOP_DATA')
tdir=concat_dir(synop_dir,'images')
mk_sub_dir,tdir,ymd

snames=inames+'.gz'
sdir=concat_dir(tdir,ymd)

;-- if clobber is set then overwrite existing files

clobber=keyword_set(clobber)
nfiles=n_elements(inames)

mdi=obj_new('mdi')
mdi->set,verbose=verbose
count=0

znames=concat_dir(sdir,snames)
onames=concat_dir(sdir,inames)

for i=0,nfiles-1 do begin
 
 process=1b

 if not clobber then begin
  chk1=loc_file(znames[i],count=zcount)
  chk2=loc_file(onames[i],count=ocount)
  process=(zcount eq 0) and (ocount eq 0)
 endif

 error=0
 catch,error
 if error ne 0 then begin
  message,err_state(),/cont
  catch,/cancel
  continue
 endif

 if process then begin
  count=count+1
  if verbose then message,'Processing '+files[i],/cont
  mdi->read,files[i],err=err,_extra=extra,/no_prep
  if err ne '' then begin
   message,err,/cont
   continue
  endif
  index=mdi->get(/index)
  if stregex(files[i],'mag',/bool) then index=rep_tag_value(index,'Magnetogram','type')
  if stregex(files[i],'igram',/bool) then index=rep_tag_value(index,'Intensitygram','type')
  mdi->set,index=index
  mdi->earth_correct
  mdi->write,inames[i],out=sdir[i]
 endif

 chk2=loc_file(onames[i],count=ocount)
 zip=ocount gt 0
 if zip then zip_files=append_arr(zip_files,onames[i],/no_copy)

endfor

obj_destroy,mdi

if is_string(zip_files) then begin
 chmod,zip_files,/g_read,/g_write
 if verbose then message,'Compressing files...',/cont
 espawn,'gzip -f '+zip_files,/unique,/back,/noshell
endif

message,'Processed '+trim(count)+' files',/cont


return & end
