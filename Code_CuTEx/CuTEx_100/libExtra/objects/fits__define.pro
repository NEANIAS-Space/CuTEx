;+
; Project     : HESSI
;
; Name        : FITS__DEFINE
;
; Purpose     : Define a FITS map object
;
; Category    : imaging maps objects
;               
; Syntax      : This procedure is invoked when a new FITS object is
;               created via:
;
;               IDL> new=obj_new('fits')
;
; History     : Written 19 May 1998, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

;-- FITS init

function fits::init,no_map=no_map,_ref_extra=extra

dprint,'% FITS::INIT'

if (1-keyword_set(no_map)) then return,self->map::init(_extra=extra)

return,1 

end

;--------------------------------------------------------------------------

pro fits::cleanup

dprint,'% FITS::CLEANUP'

self->map::cleanup

return & end

;---------------------------------------------------------------------------
;-- FITS reader (returns a linked list of maps)

pro fits::read,file,data,index=index,err=err,_ref_extra=extra,$
              nodata=nodata,no_copy=no_copy,verbose=verbose
err=''

verbose=keyword_set(verbose)

nfiles=n_elements(file)
if is_blank(file) or (nfiles eq 0) then begin
 err='Missing input FITS filename'
 message,err,/cont
 return
endif
file=strtrim(file,2)

;-- avoid making duplicate copies of data

copy=1b-keyword_set(no_copy)
if copy then no_copy=1b-arg_present(data)

;-- empty linkedlist

self->empty
k=-1
for i=0,nfiles-1 do begin
 terr=''
 if verbose then message,'Reading '+file[i],/cont
 self->readfits,file[i],data,index=index,_extra=extra,err=terr,$
                          nodata=nodata
 if (terr eq '') then begin
  tindex=merge_struct(tindex,index,/no_relax)
  if (not keyword_set(nodata)) and obj_valid(self.omap) then begin
   if (size(data,/n_dim) gt 1) then begin
    nindex=n_elements(index)
    for j=0,nindex-1 do begin
     k=k+1
     self->mk_map,index[j],data[*,*,j],k,no_copy=no_copy,err=merr,$
                       filename=file[i],_extra=extra
    endfor
   endif else begin
    err=err+'Data is not a simple image'
   endelse
  endif
 endif else err=err+trim(terr)
endfor

if is_struct(tindex) then index=temporary(tindex)

return & end

;--------------------------------------------------------------------------
;-- actual FITS reader

pro fits::readfits,file,data,index=index,_ref_extra=extra,err=err

self->mreadfits,file,data,index=index,_extra=extra,err=err

if err eq '' then index=self->index2fits(index,/no_copy,err=err)

return & end

;--------------------------------------------------------------------------
;-- make INDEX SSW/FITS compatible

function fits::index2fits,index,no_copy=no_copy,err=err
err=''
if exist(index) then return,index else return,-1
end

;-------------------------------------------------------------------------
;-- MREADFITS method

pro fits::mreadfits,file,data,header=header,index=index,_extra=extra,err=err,$
                        extension=extension,nodata=nodata

forward_function mrdfits

err=''

;-- manually decompress if MRDFITS can't do it

compressed=is_compressed(file,type)
have_mrdfits=have_proc('mrdfits')
dfile=strtrim(file,2)

decompress=(not have_mrdfits) or $
           ((type eq 'Z') and (os_family(/lower) eq 'windows')) or $
           (not since_version('5.3'))

if compressed and decompress then begin
 dprint,'% MREADFITS: decompressing manually...'
 dfile=find_compressed(file,err=err)
 if err ne '' then begin
  message,err,/cont
  return
 endif
endif

if not is_number(extension) then extension=0
if have_mrdfits then begin
 if compressed then dprint,'% MREADFITS: decompressing on the fly...'

;-- /NODATA?

 if keyword_set(nodata) then begin
  mrd_head,dfile,header,err=err,_extra=extra,extension=extension
  if err eq '' then index=fitshead2struct(header)
  return
 endif

 data=mrdfits(dfile,extension,header,_extra=extra,status=status,/fscale)
 if status ge 0 then begin
  index=fitshead2struct(header)
  sz=size(data)
  if sz[0] eq 3 then index=replicate(index,sz[3])
 endif else begin
  if status eq -2 then err='End of file during read.' else $ 
   err='Error during read - file not accessible.'
  message,err,/cont
 endelse
 return
endif

;-- otherwise use MREADFITS

if keyword_set(nodata) then begin
 mrd_head,dfile,header,err=err,_extra=extra
 if err eq '' then  index=fitshead2struct(header)
 return
endif

mreadfits,dfile,index,data,header=header,_extra=extra

return & end

;--------------------------------------------------------------------------                  
;-- define FITS object

pro fits__define                 

fits_struct={fits, inherits map}

return & end
