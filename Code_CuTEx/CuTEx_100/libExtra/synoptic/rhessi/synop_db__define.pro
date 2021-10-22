;+
; Project     : HESSI
;
; Name        : SYNOP_DB__DEFINE
;
; Purpose     : Define database for Synoptic data achive
;
; Category    : HESSI, Synoptic, Database, widgets, objects
;
; History     : 13-Aug-2000,  D.M. Zarro (EIT/GSFC)  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-
                                             
;---------------------------------------------------------------------------
;-- init Synoptic DB

 function synop_db::init

 self->init_names

 self->init_desc

 dprint,'% SYNOP_DB::INIT ',1

 return,1 & end

;-----------------------------------------------------------------------------
;-- init site names

 pro synop_db::init_names

;'bbso   | Big Bear Solar Obs. | images | optical | bbso* | bbso',$

 t=['kanz   | Kanzelhohe Solar Obs.   | images | optical | kanz* |' ,$
    'kisf   | Kiepenheuer-Institute Obs. | images | optical | kisf* | ' ,$
    'meud   | Meudon Obs.| images |  optical | meud* | ' ,$
    'mdi    | SOHO/MDI| images | magnetic | mdi*maglc* | ',$
    'mdi    | SOHO/MDI | images | optical | mdi*igram* |',$
;    'mwso   | Mt. Wilson Solar Obs.| images | optical | mwso*igram* | ',$
;    'mwso   | Mt. Wilson Solar Obs.| images | magnetic | mwso*magmp* | ',$
;    'mwso   | Mt. Wilson Solar Obs.| images | doppler | mwso*dopp* | ',$ 
    'nobe   | Nobeyama Radioheliograph | images | radio | nobe* |' ,$
    'ovsa   | Owens Valley Solar Obs. | lightcurves | radio | ovsa*sp* |',$
    'ovsa   | Owens Valley Solar Obs. | lightcurves | radio | ovsa*lc* |',$
    'ethz   | Phoenix ETH Zurich | spectra | radio | phnx* | ethz' ,$
    'osra   | Potsdam Obs. of Solar Radioastronomy | spectra | radio | osra* | osra' ,$
;    'rstn   | Radio Solar Telescope Network | lightcurves | radio | rstn* |',$
    'trac   | TRACE | images | optical | trac*WL_* |',$
    'trac   | TRACE | images | euv | trac*1600_* |',$
    'trac   | TRACE | images | euv | trac*171_*  |' ,$
    'trac   | TRACE | images | euv | trac*195_*  |' ,$
    'trac   | TRACE | images | euv | trac*1216_*  |' ,$
    'nanc   | Nancay | images | radio | nanc* |',$
    'sxi    | GOES12/SXI | images | sxr | sxi* | sxi',$
    'bbso   | Big Bear Solar Observatory | images | optical | *_ha* | bbso ',$
    'eit    | SOHO/EIT | images  | euv  | eit* | ']

 
 db={abbr:'',name:'',dir:'',wave:'',patt:'',remote:''}
 site_db=self->make_db(t,db)

 ptr=self.site_db
 ptr_alloc,ptr
 *ptr=site_db
 self.site_db=ptr

return & end


;--------------------------------------------------------------------------
;-- get supported wave types and their directories

function synop_db::list_synop_waves,abbr

w=['Gamma Ray','Hard X-ray','Soft X-ray','EUV/UV','Optical','Radio',$
    'Magnetic','Doppler']

abbr=['gamr','hxr','sxr','euv','optical','radio','magnetic','doppler']

w=reverse(w)
abbr=reverse(abbr)

return,w

end 

;---------------------------------------------------------------------------
;-- supported SYNOP directories

function synop_db::list_synop_dirs,lower=lower

dirs=['Images','Lightcurves','Spectra']
if keyword_set(lower) then return,strlowcase(dirs) else return,dirs

end

;----------------------------------------------------------------------------
;-- supported VSO datasets

function synop_db::list_vso_names,abbr,cats

abbr=['eit','trace','euvi','cor','xrt']
cats=['euv/images','euv/images','euv/images','cor/images','sxr/images']
return,['SOHO/EIT','TRACE','STEREO/SECCHI-EUVI','STEREO/SECCHI-COR','HINODE/XRT']

end

;;----------------------------------------------------------------------------
;-- init data desc

 pro synop_db::init_desc

 t= ['fd   | Full Disk'        ,$
     'dopp | Dopplergram'      ,$
     'mag  | Magnetogram'      ,$
     'igra | Intensitygram'    ,$
     'halp | H-alpha'          ,$
     'kline| Ca II K'          ,$
     'Hz   | Radio'            ,$
     'lc   | Lightcurve'       ,$
     'sp   | Spectrum/Lightcurve'         ,$
     'AM   | Al/Mg'            ,$
     'A1   | Thin Al'          ,$
     '00195  | Fe XII'           ,$
     '00171  | Fe IX/X'          ,$
     '00284  | Fe XV'            ,$
     '00304  | He II'            ,$
     'ifa  | R+L Polarization' ,$
     'ifs  | R-L Polarization' ,$
     'ifz  | R+L Polarization'  ]

 db={abbr:'',desc:''}
 desc_db=self->make_db(t,db)

 ptr=self.desc_db
 ptr_alloc,ptr
 *ptr=desc_db
 self.desc_db=ptr

 return & end

;----------------------------------------------------------------------------
;-- convert string DB to structure

 function synop_db::make_db,sinput,sdb
 np=n_elements(sinput)
 if (np eq 0) or (n_elements(sdb) eq 0) then return,-1
 ntags=n_elements(tag_names(sdb))
 db=replicate(sdb,np)
 for i=0,np-1 do begin
  temp=str2arr(sinput[i],delim='|')
  ntemp=n_elements(temp)
  for j=0,(ntags < ntemp)-1 do begin
   var=strtrim((temp[j]),2)
   db[i].(j)=var
  endfor
 endfor  

 return,db
 end 
                
;-------------------------------------------------------------------------

 pro synop_db::cleanup

 dprint,'% SYNOP_DB::CLEANUP
 ptr_free,self.site_db
 ptr_free,self.desc_db

 return & end
                       
;--------------------------------------------------------------------------
;-- get desc DB

function synop_db::list_synop_desc

return,*self.desc_db

end                    
    
                    
;--------------------------------------------------------------------------
;-- get site name DB

function synop_db::list_synop_sites

return,*self.site_db

end
                   
 
;---------------------------------------------------------------------------
;-- get site names & abbr

function synop_db::list_synop_names,abbr

site_db=self->list_synop_sites()

names=site_db.name
abbr=site_db.abbr
names=reform(get_uniq(names,sorder))
abbr=reform((abbr)[sorder])                     

return,names

end

;---------------------------------------------------------------------------
;-- validate SYNOP directory

function synop_db::valid_synop_dirs,dirs

out_dirs=''
if not is_string(dirs) then return,out_dirs
synop_dirs=self->list_synop_dirs(/lower)

if n_elements(dirs) eq 1 then in_dirs=str2arr(dirs) else in_dirs=dirs
chk=where_vector(in_dirs,synop_dirs,count)

if count eq 1 then chk=chk[0]
if count gt 0 then out_dirs=synop_dirs[chk]

return,out_dirs

end

;--------------------------------------------------------------------------
;-- check if file is at a remote site

function synop_db::is_remote,file,class=class,count=count,have_url=have_url

count=0 & class=''
if is_blank(file) then return,0b

nfile=n_elements(file)
out=bytarr(nfile)
class=strarr(nfile)
sites=self->list_synop_sites()
chk=where(sites.remote ne '',count)
if count eq 0 then return,out

chk=stregex(strtrim(file,2),'[\\\/]?([a-z]+)_',/extract,/sub,/fold)
abbr=strlowcase(chk[1,*])
for i=0,nfile-1 do begin
 ok=where((abbr[i] eq sites.abbr) and (sites.remote ne ''),count)
 if count gt 0 then begin
  out[i]=1b & class[i]=(sites.remote)[ok[0]]
 endif
endfor

;-- check files with URL addresses

if keyword_set(have_url) then begin
 chk=where2(stregex(file,'ftp:|http:',/bool),fcount,comp=comp,ncomp=ncomp)
 if ncomp gt 0 then out[comp]=0b
 if fcount gt 0 then out[chk]=1b
endif

if nfile eq 1 then begin
 out=out[0] & class=class[0]
endif

remote=where(out,count)

return,out & end
              
;-----------------------------------------------------------------------------

pro synop_db__define                 

temp={synop_db,site_db:ptr_new(),desc_db:ptr_new()}

return & end
