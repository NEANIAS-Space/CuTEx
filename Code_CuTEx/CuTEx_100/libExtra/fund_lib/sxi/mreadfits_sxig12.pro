pro mreadfits_sxig12, sxifiles, index, data, $
   composite=composite, register=register,   $
   column_fix=column_fix, hotpix_fix=hotpix_fix, ref_index=ref_index, $
   debug=debug
;
;+ 
;   Name: mreadfits_sxig12
;
;   Purpose: read multi sxig12 2D -> 3D index,data w/opt. composite/registration
;
;   Input Parameters:
;      sxifiles - file list - composite option assumes Level One 
;
;   Keyword Parameters:
;      composite - (switch) - if set -and- L1 data includes satpix binary
;                             extension, then make the composite
;      register - (switch) - if set, register,de-roll
;      column_fix (switch) - if set, apply an (adhoc) column fix algorithm
;      hotpix_fix (switch) - if set, apply 'ssw_hot_pix' correction
;      ref_index - optional reference image 'index' for registration 
;
;   Calling Sequence:
;      mreadfits_sxig12,sxifiles,index,data[,/reg] [,/composite] [,/column_fix]
;                                          [,/hotpix_fix]
;
;   History:
;      9-Apr-2003 - S.L.Freeland - combine a few common functions 
;     30-Nov-2004 - S.L.Freeland - add column_fix keyword+function
;      6-Jan-2005 - S.L.Freeland - added hotpix_fix keyword+function
;     30-may-2006 - S.L.Freeland - add REF_INDEX -> ssw_register
;
;   Calls:
;      sxig12_read_one, optionally ssw_register
;
;   Restrictions: - currently assumes sxig12 in $SSW_INSTR
;
;-
version=1.5

; ================= default on for latest events operations ====
column_fix=keyword_set(column_fix) or get_logenv('ssw_latest_events') eq 1
hotpix_fix=keyword_set(hotpix_fix) or get_logenv('ssw_latest_events') eq 1
if n_elements(sigma) eq 0 then sigma=3
debug=keyword_set(debug)
; ============================================

if not data_chk(sxifiles,/string) then begin 
   box_message,'Need to supply file list'
   return
endif 

if n_params() eq 1 then begin 
   box_message,['You did not supply any output parameters, so I will not waste your time...',$
                'IDL> mreadfits_sxig12, sxifiles, index [,data] [,/composite] [,/register]']
   return
endif

composite=keyword_set(composite)
lev1=strpos(sxifiles,'BB') ne -1
wlev1=where(lev1,l1cnt)

ok2comp=composite and lev1          ; boolean which are ok/requested to composite

nfiles=n_elements(sxifiles)

case 1 of                            ; verify requestd composites all OK
   1-composite:                      ; none requested
   nfiles eq l1cnt:                  ; requested and all OK (level1)
   l1cnt eq 0: box_message,'Composite requested but that option requires Level1' 
   else: box_message,'Composite requested - I can only do that for your Level1 subset'
endcase 
   
simg0=sxig12_read_one(sxifiles(0),sxih,ind=indx)  ; first for template
mreadfits,sxifiles,index                 

nsats=gt_tagval(index,/sat_pix,missing=0)
ok2comp=ok2comp and nsats gt 0  ; don't bother composite if #sat=0

data=make_array(data_chk(simg0,/nx),data_chk(simg0,/ny),nfiles $
   ,type=data_chk(simg0,/type))
;index=replicate(indx,nfiles)

for i=0,nfiles-1 do begin 
   simg=sxig12_read_one(sxifiles(i),sxih,ind=indx, sat_pix_dat=sat_pix_dat)
if debug then stop,'ok2comp,sat_pix'
   if ok2comp(i) then begin 
      satpixs=long(sat_pix_dat(0,*))+long(sat_pix_dat(1,*))*512 ; subscripts of saturated pixels
      simg(satpixs)=sat_pix_dat(2<(data_chk(sat_pix_dat,/nx)-1),*)
   endif
   if i eq 0 then index=temporary(indx) else $
      index=concat_struct(index,temporary(indx))
   data(0,0,i)=temporary(simg)         ; insert 2D->3D
endfor

if composite and max(nsats) gt 0 then begin
   history_recs='COMPOSITING: Replaced ' + strtrim(nsats,2) + ' pixels'
   update_history,index,history_recs,/caller, version=version,/mode
endif

if keyword_set(column_fix) then begin 
   box_message,'Applying column fix...'
   nimg=data_chk(data,/nimage)
   for i=0,nimg-1 do begin ; can do with some vectorization if you have time...
      ncols=50             ; number of columns to average
      imi=data(*,*,i)
      bot50=imi(*,0:ncols-1)    ; 
      cavg=total(bot50,2)/ncols
      cimgi=rebin(cavg*(-1),512,512) ; corrective image
      newimi=temporary(imi) + temporary(cimgi)
      data(0,0,i)=temporary(newimi)
   endfor
endif

if keyword_set(hotpix_fix) then begin 
   box_message,'Applying HOTPIX fix...'
   ssw_hot_pix, index, data, /clobber, sigma=sigma
endif
 
if keyword_set(register) then begin 
   box_message,'Registering images...'
   ssw_register,index,data,oindex,odata,roll=0,ref_index=ref_index
   history_recs='REGISTRATION: Orig XCEN/YCEN/CROTA: ' + get_infox(index,'xcen,ycen,crota') 
   update_history,index,history_recs,version=version,/caller,/mode
   data=temporary(odata)
   index=temporary(oindex)
endif

return
end
