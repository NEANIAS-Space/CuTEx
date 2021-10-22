;+
; Project     : SXI
;
; Name        : SXI_COPY
;
; Purpose     : copy SXI file via HTTP sockets
;
; Category    : utility system sockets
;
; Syntax      : IDL> sxi_copy,file,outdir=outdir
;                   
; Inputs      : FILE = remote file name to copy with URL path (optional)
;
; Outputs     : None
;
; Keywords    : OUT_DIR = output directory to copy file
;               ERR   = string error message
;
; Example     :
;              First list remote files from NGDC server using 'sxi_files'  
;
;              IDL> f=sxi_files('1-dec-01','2-dec-01',/ngdc,/full)
;              IDL> sxi_copy,f,/progress
;              
;              Or copy without including path name:
;
;              IDL> sxi_copy,'SXI_20011201_000202041_BB_12.FTS'
;
; History     : 15-Jan-2003,  D.M. Zarro (EER/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro sxi_copy,url_file,out_file,_ref_extra=extra

common sxi_copy,sxi_obj

if not obj_valid(sxi_obj) then sxi_obj=obj_new('sxi')
sxi_obj->copy,url_file,out_file,_extra=extra

return & end


