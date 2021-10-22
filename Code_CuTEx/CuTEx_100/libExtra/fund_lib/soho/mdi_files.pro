function mdi_files, t0 , t1, ftimes=ftimes
;+
;   Name: mdi_files
;
;   Purpose: return MDI file names between time0 and time0
;  
;   Input Parameters:
;      time0  - start time or vector of times to match
;      time1  - stop  time of range
;
;   History:
;      7-November-1998 - S.L.Freeland - mimic eit_files/trace_files/cds_files   
;      5-March-2001 - S.L.Freeland - allow genx catalogs to reside in
;                     $MDI_MAGS_GENXCAT
;-

genxcats=[get_logenv('MDI_MAGS_GENXCAT'),concat_dir('MDI_MAGS','mdi_genxcat')]
whichdir=where(file_exist(genxcats),sscnt)

if sscnt eq 0 then begin 
   box_message,'Cannot find mdifile genx catalog'
   return,''
endif

retval=''

case n_params() of
   0: begin                                                   ; all files
        mdi_cat, tx0, tx1, mdicat        
        ftimes=mdicat
        retval=mdicat.filename
   endcase
   1:begin                                                    ; one or vector
     t0x=t0
     if n_elements(t0x) eq 1 then begin
         mdi_cat,timegrid(t0x,hour=-2,/string,/quiet),$
		 timegrid(t0x,hour=2,/string,/quiet), mdicat
     endif else begin
        mdi_cat,t0x(0),last_nelem(t0x), mdicat
     endelse
        if data_chk(mdicat,/struct) then begin
            ss=tim2dset(mdicat,t0x,delta=delta)
            ftimes=mdicat(ss)
	    retval=ftimes.filename
	endif
      endcase

   else: begin                                                 ; range
      ss=-1
      mdi_cat,t0,t1,mdicat      
      
      if data_chk(mdicat,/struct) then $
          ss=sel_timrange(mdicat,t0,t1,/between) else $
              box_message,'No mdi files in timerange' 
      if ss(0) ne -1 then begin
          ftimes=mdicat(ss)
	  retval=ftimes.filename
      endif
   endcase
endcase

return,retval
end
  
