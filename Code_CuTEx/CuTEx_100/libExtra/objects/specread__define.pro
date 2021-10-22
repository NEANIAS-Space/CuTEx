;+
; Project     : HESSI
;
; Name        : specread__DEFINE
;
; Purpose     : Define a general SPECTROGRAM reader object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('specread')
;
; History     : Written 18 Nov 2002, D. Zarro (EER/GSFC)
;               Modified 8 March 2007, Zarro (ADNET)
;                - made FITS and SITE into helper objects instead of
;                  inherited objects to avoid use of ADD_METHOD
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function specread::init,_ref_extra=extra

;-- define SITE object for downloading

self.site=obj_new('site',_extra=extra)
if not obj_valid(self.site) then return,0

;-- define FITS object for reading

self.fits=obj_new('fits',_extra=extra,/no_map)
if not obj_valid(self.fits) then return,0

if not self->specplot::init(_extra=extra) then return,0

return,1

end

;-----------------------------------------------------------------------

pro specread::cleanup

self->specplot::cleanup
obj_destroy,self.site
obj_destroy,self.fits

return & end

;--------------------------------------------------------------------------
;-- FTP search method

function specread::search,tstart,tend,_ref_extra=extra

return,self.site->flist(tstart,tend,_extra=extra)

end

;----------------------------------------------------------------------------
;-- FITS read method

pro specread::read,file,data,header=header,index=index,err=err,$
                     nodata=nodata,_extra=extra

err=''

if n_elements(file) gt 1 then begin
 err='Cannot read multiple files'
 message,err,/cont
 return
endif

;-- read main data

self.fits->read,file,data,header=header,index=index,$
          extension=0,err=err,nodata=nodata,_extra=extra

if is_string(err) then return
if keyword_set(nodata) then return

self.fits->read,file,data1,index=index1,$
          extension=1,err=err,_extra=extra

self->set_fits,file,index,data,index1,data1,header=header

return & end

;---------------------------------------------------------------------------
;-- placeholder method for passing FITS data to SPECTROGRAM object

pro specread::set_fits,file,index,data,index1,data1,header=header

return & end

;----------------------------------------------------------------------
pro specread::set,_ref_extra=extra

self.site->setprop,_extra=extra
self->specplot::set,_extra=extra

return & end

;------------------------------------------------------------------------------
;-- define class structure

pro specread__define                 

self={specread,site:obj_new(), fits:obj_new(), inherits specplot}

return & end

