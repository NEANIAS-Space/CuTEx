pro mreadfits_fixup, index, data, loud=loud
;+
;   Name: mreadfits_fixup
;
;   Purpose: adjust some standard fields after mreadfits rebinning
;
;   Input Parameters:
;      index (input/output) - structure array (see mreadfits.pro)
;      data  - corresponding data array (post-rebinning)
;
;   Output Parameters:
;      index - fields adjusted for previous rebinning
;     
;   Calling Sequence:
;      mreadfits_fixup, index, data [,/loud]
;      (by the time you read this, it may be called WITHIN mreadfits)
;  
;   History:
;      4-Jun-1997 - S.L.Freeland
;      5-Jun-1997 - S.L.Freeland - fix an 'off-by-1' error
;      5-may-2004 - S.L.Freeladn - relax constraint on all or nothing 
;     
;-
if n_params() ne 2 then begin
   prstr,strjustify('IDL> mreadfits_fixup, index, data',/box)
   return
endif

xn=float(gt_tagval(index,/naxis1))
yn=float(gt_tagval(index,/naxis2))
sdata=float(size(data))
fx=xn/sdata(1) & fy=yn/sdata(2)
ss=where(fx ne 1 or fy ne 1,ncnt)

; list of tags which are adjusted - add to list as required
tag_adjust=str2arr('cdelt1,cdelt2,solar_r,crpix1,crpix2')
nonemissing=(where(tag_index(index,tag_adjust) eq -1,mcnt))(0) eq -1

; dont adjust unless required - and ONLY if proper tags found
if (ncnt gt 0)  then begin 
   message,/info,"applying temporary mreadits parameter fixup..."
   if tag_exist(index,'cdelt1') then index.cdelt1=(fx)*index.cdelt1
   if tag_exist(index,'cdelt2') then index.cdelt2=(fx)*index.cdelt2
   if tag_exist(index,'crpix1') then index.crpix1  = ( (sdata(1)/float(index.naxis1)) * index.crpix1 )
   if tag_exist(index,'crpix2') then index.crpix2  = ( (sdata(2)/float(index.naxis2)) * index.crpix2 )
   if tag_exist(index,'solar_r') then index.solar_r = ( (sdata(2)/float(index.naxis2)) * index.solar_r )
   index.naxis1=sdata(1)
   index.naxis2=sdata(2)
endif 

return
end
