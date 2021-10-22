;+
; Project     : HESSI
;
; Name        : synop_link
;
; Purpose     : make a link between SYNOP data files and 
;               $SYNOP_DATA/target_dir
;
; Category    : synoptic gbo
;
; Syntax      : IDL> synop_link,site,tstart,tend,back=back
;
; Inputs      : SITE = site abbreviation (e.g. bbso)
;               TSTART, TEND = start and end times to consider
;
; Outputs     : None
;
; Keywords    : BACK = days to look back
;               TARGET_DIR = target directory for links
;
; Restrictions: Unix only
;
; History     : Written 8 March 2001, D. Zarro (EITI/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-

pro synop_link,site,tstart,tend,back=back,err=err,verbose=verbose,$
               target_dir=target_dir,_extra=extra

if get_host() eq 'beauty.nascom.nasa.gov' then return

verbose=keyword_set(verbose)
err=''

;-- usual error checks
 
if os_family(/lower) ne 'unix' then begin
 err='sorry, Unix only'
 message,err,/cont
 return
endif

if is_blank(site) then begin
 err='site abbreviation not entered'
 message,err,/cont
 return
endif

synop_data=chklog('SYNOP_DATA')
if not write_dir(synop_data) then return

if is_blank(target_dir) then tdir=site else tdir=target_dir
tdir=concat_dir(synop_data,tdir)
mk_dir,tdir,/a_write

message,'creating links for '+site,/cont

;-- find files

s=obj_new('synop')
s->setprop,tstart=tstart,tend=tend,verbose=verbose,site=site,back=back,$
           mode=0,cache=0,smode=1,_extra=extra

s->list,files,count=count

if count eq 0 then begin
 message,'no files matching "'+site+'" found',/cont
 return
endif

s->fbreak,files,fdir,fnames
server=synop_server(path=synop_dir)

obj_destroy,s

;-- create link commands to spawn

if verbose then message,'constructing links...',/cont

sfiles=str_replace(files,synop_dir,'..')
fnames=str_replace(fnames,'.gz','')
cmd1='cd '+tdir
cmd2='rm -f '+fnames
cmd3='rm -f *xxx*'
cmd3='ln -sf '+sfiles+' . '
cmd=cmd1+' ; '+cmd2+' ; '+cmd3

espawn,cmd,out,count=count,/background
if count gt 0 then message,arr2str(out),/cont
                                                                    
return & end


