;+
; Project     : HESSI
;
; Name        : SOCK_FITS
;
; Purpose     : read a FITS file via HTTP sockets
;
; Category    : utility sockets fits
;
; Syntax      : IDL> sock_fits,file,data,header,extension=extension
;                   
; Inputs      : FILE = remote file name with URL path attached
;
; Outputs     : DATA = FITS data
;               HEADER = FITS header
;
; Keywords    : ERR   = string error message
;
; Example     :
; IDL> f='smmdac.nascom.nasa.gov/synop_data/kanz/kanz_halph_fd_20000114_1016.fts
; IDL> sock_fits,f,data,header
;
; History     : 27-Dec-2001,  D.M. Zarro (EITI/GSFC)  Written
;               23-Dec-2005, Zarro (L-3Com/GSFC) - removed COMMON;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro sock_fits,file,data,header,err=err,_ref_extra=extra

err=''
if is_blank(file) then begin
 err='blank URL filename entered'
 message,err,/cont
 return
endif

hfits=obj_new('hfits',err=err)
if err ne '' then return

header=''
delvarx,data

hfits->hset,_extra=extra

hfits->readfits,file,data,header=header,err=err,_extra=extra,/verbose

obj_destroy,hfits

return

end


