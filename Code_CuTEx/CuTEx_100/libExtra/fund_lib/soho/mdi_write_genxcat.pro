pro mdi_write_genxcat, pattern=pattern, clean=clean, catdir=catdir, $
   testing=testing
;+
;   Name: mdi_write_genxcat
;
;   Purpose: map mdi files (from SOI WWW requests) -> a time based catalog
;
;   Input Parameters:
;      NONE
;
;   Keyword Paramters:
;      pattern - directory file pattern to match
;      clean   - if set, remove all mdi*genx in catalog directory (!)
;                prior to writing
;      testing - if set, only do this many directories (infrastructure test)
;
;   History:
;      Circa 15-Nov-1998 - S.L.Freeland
;       2-feb-1999 - S.L.Freeland - a little documentation
;      19-Mar-2001 - S.L.Freeland - allow multiple environmentals
;      09-May-2003, William Thompson - Use ssw_strsplit instead of strsplit
;
;   Restrictions:
;      $MDI_MAGS  - points to top of mdi data tree  
;                   (also $MDI_MAGS_0, $MDI_MAGS_1... $MDI_MAGS_9 optional)
;
;   Side Effects:
;      Writes catalog files (genx format) in $MDI_MAGS/mdi_genxcat  
;
;   Assumptions - data files are from a Stanford MDI data request 
;-

if not keyword_set(pattern) then pattern='fd_M_96m_01d*'

gendir=get_logenv('MDI_MAGS_GENXCAT')

case 1 of 
   data_chk(catdir,/string):
   is_dir(gendir): catdir=gendir
   else: catdir=concat_dir('$MDI_MAGS','mdi_genxcat')
endcase

if not file_exist(catdir) then begin
  box_message,'Directory ' + catdir + ' does not exist, creating it..'
  spawn,['mkdir','-p',catdir],/noshell
endif  

if keyword_set(clean) then begin
  box_message,'Cleaning up old catalogs..'
  files=file_list(catdir,'mdi*genx',/cd)
  file_delete,files
endif 

mdienv=get_logenv('MDI_MAGS'+['','_'+strtrim(indgen(10),2)],outenv=menvs)
ssok=where(mdienv ne '',okcnt)

if okcnt eq 0 then begin 
   box_message,'No MDI_MAG* environmentals defined, exiting'
   return
endif
 
mdienv=mdienv(ssok)
menvs=menvs(ssok)


for env=0,n_elements(mdienv)-1 do begin
   box_message,'Processing: ' + menvs(env)
   dirs=file_list(menvs(env),pattern,/cd)
   fddirs=ssw_strsplit(dirs,'/',/last,/tail)
   if keyword_set(testing) then ndirs=testing else ndirs=n_elements(dirs)

   for i=0, (ndirs<n_elements(dirs)) -1 do begin
      box_message,'Processing:>> ' + fddirs(i)
      files=file_list(dirs(i),'*.fits',/cd)
      flen=strlen(files)
      imgfiles=files(where(flen eq min(flen)))
      delvarx,mdiind,newind,sswstr
      rd_mdi, imgfiles, mdiindex
      sswstr=struct2ssw(mdiindex)
      sswstr.solar_r=mdiindex.r_sun
      sswstr.solar_l0=mdiindex.l0
      sswstr.solar_b0=mdiindex.b0
      sswstr.object='Full Disk Magnetogram'
      sswstr.telescop='SOHO'
      sswstr.instrume='MDI Magnetogram'
      break_file,imgfiles,ll,pp,ff,ee,vv
      newind=add_tag(sswstr,'','FILENAME')
      sswstr.filename=concat_dir(concat_dir(menvs(env),fddirs(i),/notrans),+ff+ee+vv)
      substr=str_subset(sswstr,$
        'time,mjd,day,date_obs,filename,naxis1,naxis2, cdelt1,cdelt2,crpix1,crpix2,xcen,ycen,solar_r,solar_l0,solar_b0')
   
      write_genxcat,substr,topdir=catdir, prefix='mdifd_', $
         /nelem
   endfor
endfor

return

end

