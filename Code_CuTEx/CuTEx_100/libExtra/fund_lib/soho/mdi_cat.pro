pro mdi_cat, t0 , t1, catout, refresh=refresh, quiet=quiet
;+
;   Name: mdi_cat
;
;   Purpose: return MDI header/structures between time0 and time1
;
;   Input Parameters:
;      time0, time1 - time range desired
;
;   Output Parameters:
;      catout - output structure vector
;
;   History:
;      7-November-1998 - S.L.Freeland - simple interface to read_genxcat  
;      5-March-2001 - S.L.Freeland - allow catalog files in 
;                     $MDI_MAGS_GENXCAT
;
;-  
catout=-1
loud=1-keyword_set(quiet)
common mdi_cat, mdicat_full, startt, stopt

if n_params() lt 3 then begin
  box_message,['Need start time, stop time and output parmeter', $
	       'IDL> mdi_cat,t0, t1, outcat']
  return
endif  

genxcats=[get_logenv('MDI_MAGS_GENXCAT'),concat_dir('MDI_MAGS','mdi_genxcat')]
whichdir=where(file_exist(genxcats),sscnt)

if sscnt eq 0 then begin 
   box_message,'Cannot find mdi file genxcatalog subdirectory'
   return
endif else catdir=genxcats(whichdir(0))

if keyword_set(refresh) or n_elements(mdicat_full) eq 0 then begin 
   box_message,'First call - initializing catalog'
   read_genxcat, tx0, tx1, mdicat_full, topdir=catdir
   fmt_timer,mdicat_full, startt, stopt
endif 

if n_elements(t0) eq 0 then t0=startt
if n_elements(t1) eq 0 then t1=stopt
ss=sel_timrange(mdicat_full, t0, t1,/between)

if ss(0) eq -1 then box_message,'No MDI records in time range' else $
       catout=mdicat_full(ss)

return
end
