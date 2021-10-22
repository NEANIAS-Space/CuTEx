;+
; Project     : SOHO-CDS
;
; Name        : FTP_SYNOP
;
; Purpose     : Driver to ftp synoptic images for a given date
;
; Category    : planning
;
; Explanation : calls appropriate FTP function 
;
; Syntax      : ftp_synop,site,date
;
; Examples    :
;
; Inputs      : SITE = site name to search
;
; Opt. Inputs : DATE = date to retrieve
;
; Outputs     : 
;
; Opt. Outputs: None
;
; Keywords    : MAIL = send some mail
;               FITS = copy FITS files (normally GIF)
;               LINK = link created files to $SYNOP_DATA/site
;               BACK = # of days back to search
;
; Common      : None
;
; Restrictions: Unix only 
;
; Side effects: None
;
; History     : Written 14 May 1998 D. Zarro, SAC/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro ftp_synop,site,date,mailto=mailto,err=err,$
           fits=fits,link=link,back=back

on_error,1

err=''

if datatype(site) ne 'STR' then begin
 err='site name missing'
 pr_syntax,'ftp_synop,site,date,back=back'
 return
endif

;-- check for supported sites

supp=['bbso','kisf','mlso','mwso']
nsite=strlowcase(trim(site))
chk=where(nsite eq supp,sc)
if sc eq 0 then begin
 err='unsupported site '+'"'+site+'"'
 message,err,/cont
 return
endif


ftper='ftp_'+nsite
type=strupcase(nsite)

;-- test for write access

curr=curdir()
synop_data=chklog('SYNOP_DATA')
if synop_data eq '' then begin
 err='SYNOP_DATA environment variable undefined - using current'
 message,err,/cont
 synop_data=curr
endif
if not test_dir(synop_data,err=err) then return

;-- which date?

cdate=anytim2utc(date,err=err)
if err ne '' then get_utc,cdate

;-- spawn FTP
 
fits=keyword_set(fits)
call_procedure,ftper,files=files,cdate,out_dir='/tmp',/kill,$
  count=count,err=err,fits=fits,back=back

;-- move to SYNOP_DATA

if fits then out_dir='fits' else out_dir='gif'
if count gt 0 then begin
 out_dir=concat_dir(synop_data,out_dir)
 if not is_dir(out_dir) then mk_dir,out_dir,/a_write
 file2fid,files,out_dir,out_files=out_files  
endif
 
;-- create links (FITS files only)

count=n_elements(out_files)
if keyword_set(link) and fits and (count gt 0) then begin
 site_dir=concat_dir(synop_data,site)
 if not is_dir(site_dir) then mk_dir,site_dir,/a_write
 for i=0,count-1 do begin
  chk=loc_file(out_files(i),count=nc)
  if nc gt 0 then espawn,'ln -sf '+chk(0)+' '+site_dir
 endfor
endif

if exist(mailto) then begin
 run_id=['Results of FTP_SYNOP run on '+anytim2utc(cdate,/vms)+':','']
 if count gt 0 then $
  mess=['Following '+type+' files created: ','',files] else $
   mess=['No new '+type+' files created']
 if datatype(mailto) eq 'STR' then recip=mailto else recip=get_user_id()
 send_mail,array=[run_id,mess],address=recip
endif


return & end

