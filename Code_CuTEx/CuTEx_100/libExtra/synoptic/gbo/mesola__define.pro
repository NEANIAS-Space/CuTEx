;+
; Project     : HESSI
;
; Name        : MESOLA__DEFINE
;
; Purpose     : Define a MESOLA data object
;
; Category    : Ancillary GBO Synoptic Objects
;
; Syntax      : IDL> c=obj_new('mesola')
;
; History     : Written 20 Dec 1999, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-
;-----------------------------------------------------------------------------
;-- init 

function mesola::init,_ref_extra=extra,err=err

ret=self->site::init(_extra=extra,err=err)
                     
if not ret then return,ret

ret=self->fits::init(_extra=extra,err=err)
           
self->setprop,rhost='ftpbass2000.obspm.fr',/rename,_extra=extra,err=err

return,err eq ''

end

;-----------------------------------------------------------------------------

pro mesola::cleanup

self->fits::cleanup
self->site::cleanup

return & end

;-----------------------------------------------------------------------------
;-- get remote directory id's based on file dates

function mesola::get_sdir

return,self->site::get_sdir(/no_day)

end

;-----------------------------------------------------------------------------
;-- parse file names into time

function mesola::parse_time,file,_ref_extra=extra

pref='([^\.\\/]+)'
dd='([0-9]{2})'
regex=pref+dd+dd+dd+'\.'+dd+dd+dd+dd+'?'+'\.(.+)'
return,parse_time(file,regex=regex,_extra=extra)

end

;-----------------------------------------------------------------------------
;-- rename files to IAU convention

function mesola::rename_files,files,err=err,count=count

err='' & count=0
times=self->parse_time(files,err=err,names=old_names,count=count,ss=ss)
if count eq 0 then return,''

old_names=old_names[ss]
names=time2fid(times[ss],/time,/full,/seconds)
req_ext=self->getprop(/ext)
if req_ext eq '.fits' then req_ext='.fts' 
names=names+req_ext

;-- append compressed extension

compressed=is_compressed(files,type)
chk=where(compressed,dcount)
if dcount gt 0 then names[chk]=names[chk]+'.'+type[chk]

ptypes=['mh','mk','na','nb']
ftypes=['meud_halph_fd_','meud_kline_fd_','nanc_164Mz_fd_','nanc_327Mz_fd_']

for i=0,n_elements(ptypes)-1 do begin
 chk=where(strpos(old_names,ptypes[i]) gt -1,pcount)
 if pcount gt 0 then names[chk]=ftypes[i]+names[chk]
endfor

if count eq 1 then names=names[0]

return,names

end


;------------------------------------------------------------------------------
;-- Mesola site structure

pro mesola__define                 

self={mesola,inherits site, inherits fits}

return & end


