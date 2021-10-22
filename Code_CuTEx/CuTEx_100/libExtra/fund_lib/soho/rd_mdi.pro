pro rd_mdi, files, index, data, _extra=_extra,header=header
;+
;   Name: rd_mdi
;
;   Purpose: simple mdi front end to mreadfits; ssw-rationalize output
;
;   Input Parameters:
;      files - list of one or more mdi fits files
;
;   Output Paramters:
;      index - the structure equivilent header vector
;      data  - optional output 2D or 3D data
;
;   
;   Calling Sequence:
;     rd_mdi, filelist, index [,data ] [,outsize=outsize] [,/noscale]
;
;   History:
;      7-April-1998 - S.L.Freeland - handle the large SOI versions
;      24-April-1998 - Zarro, - added check for vector index
;       7-Nov-1998 - S.L.Freeland - pass NODATA->mreadfits based on n_param
;      12-nov-1998 - S.L.Freeland - added call to struct2ssw prior to exit
;      24-Nov-1999 - S.L.Freeland - pass unrecognized keywords->mreadfits
;                                   via inheritance.
;      28-Mar-2001 - Zarro (EITI/GSFC) - added catch for REFTIME if
;                                        DATE_OBS is blank
;-     
mreadfits, files, index,data,strtemp=mdi_struct(), nodata=(n_params() le 2),$
   _extra=_extra,header=header

if gt_tagval(index(0),/crpix1) eq 0  and  gt_tagval(index(0),/center_x) ne 0 then begin
; convert some SOI fields -> SSW standards
  index.cdelt1=gt_tagval(index,/xscale)
  index.cdelt2=gt_tagval(index,/yscale)
  index.crpix1=gt_tagval(index,/center_x)
  index.crpix2=gt_tagval(index,/center_y)
  index.solar_r=gt_tagval(index,/r_sun)
endif

;  these work for all styles of MDI that I know about

if strtrim(index(0).date_obs,2) eq '' then begin
 if have_tag(index(0),'reftime') then ints=anytim(index.reftime,/ints)
endif else begin 
 if strlen(index(0).date_obs) lt 18 then $
  ints=anytim(index.date_obs + ' ' + index.time_obs,/ints) else $
   ints=anytim(gt_tagval(index,/date_obs),/ints)
endelse

index.time=ints.time
index.day=ints.day

index=struct2ssw(index)                        ; rationalize some time/point
; -------------------------------------------

return
end
