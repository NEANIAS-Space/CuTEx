;+
; Project     : HESSI
;
; Name        : BBSO__DEFINE

; Purpose     : Define a BBSO data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('bbso')
;
; History     : Written 7 June 2000, D. Zarro, EIT/GSFC
;               Modified 11 July 2007, Zarro (ADNET)
;                - fixed bug in SET
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function bbso::init,_ref_extra=extra

ret=self->fits::init(_extra=extra)

if not ret then return,ret

self.site=obj_new('site',_extra=extra)

if not obj_valid(self.site) then return,0
           
self.site->setprop,rhost='ftp.bbso.njit.edu',delim='/',/full,$
      org='day',topdir='/pub/archive',ftype='*_ha*',ext='.fts,.fits',smode=0

return,1

end

;----------------------------------------------------------------------------

pro bbso::cleanup

obj_destroy,self.site
self->fits::cleanup

return & end

;----------------------------------------------------------------------------
;-- convert BBSO index to FITS standard

function bbso::index2fits,index,no_copy=no_copy,err=err

err=''
if datatype(index) ne 'STC' then return,-1

if keyword_set(no_copy) then nindex=temporary(index) else nindex=index
if not have_tag(nindex,'cdelt1') then nindex=add_tag(nindex,1.0544,'cdelt1')
if not have_tag(nindex,'cdelt2') then nindex=add_tag(nindex,1.0544,'cdelt2')
nindex=rep_tag_value(nindex,0.,'crval1')
nindex=rep_tag_value(nindex,0.,'crval2')
if have_tag(nindex,'cenx') then nindex=rep_tag_value(nindex,nindex.cenx,'crpix1') 
if have_tag(nindex,'ceny') then nindex=rep_tag_value(nindex,nindex.ceny,'crpix2') 

if (not have_tag(nindex,'crpix1')) or (not have_tag(nindex,'crpix2')) then begin
 err='BBSO file does not contain standard FITS pointing headers'
 message,err,/cont
endif

return,nindex

end

;-------------------------------------------------------------------------------
;-- VSO search of BBSO archive

function bbso::list_vso,tstart,tend,count=count,_ref_extra=extra,times=times,$
              full_path=full_path,sizes=sizes,tai=tai

times='' & sizes='' & count=0l

;-- search VSO

dstart=get_def_times(tstart,tend,dend=dend,/vms)
tcat=vso_search(dstart,dend,source='bbso',wave='1-10000',_extra=extra)

;-- return if nothing found

if (1-is_struct(tcat)) then return,''
count = n_elements(tcat)

;-- parse out metadata

if keyword_set(tai) then times=anytim2tai(tcat.time.start) else times=tcat.time.start
sizes=strtrim(tcat.size,2)+' kB'
if keyword_set(full_path) then files=tcat.fileid else files=file_break(tcat.fileid)

return,files
end

;--------------------------------------------------------------------------
;-- search BBSO archive via FTP

function bbso::search,tstart,tend,_ref_extra=extra

return,self.site->flist(tstart,tend,_extra=extra)

end

;----------------------------------------------------------------------------

pro bbso::set,dummy,_ref_extra=extra

self.site->setprop,_extra=extra
self->fits::set,dummy,_extra=extra

return & end

;------------------------------------------------------------------------------------------
;-- BBSO site structure

pro bbso__define                 

self={bbso,site:obj_new(), inherits fits}

return & end
