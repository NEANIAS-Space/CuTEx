function ssw_time2filelist, time0, time1, debug=debug, parent=parent, paths=paths, $
   in_files=in_files, pattern=pattern, recurse=recurse, _extra=_extra, fits=fits
;
;+
;   Name: ssw_time2filelist
;
;   Purpose: return files within time range, optionally listing
;
;   Input Paramters:
;      time0, time1 - time range desired
;
;   Keyword Parameters
;      parent - optional top level; assumed structured <parent>/yyyy/mm/dd
;      paths  - list of explicit paths to search (if not chron subdirs)
;      in_files - explicit file list or url list (skip listing segment)
;      pattern - optional file pattern
;      recurse - (switch) recursive search for all PATHS 
;      fits - (switch) - if set, imply PATTERN='*.fts' or '*.fits'
;      daily,weekly,monthly,year_2digit - in conjunction w/PARENT, define
;         directory structure - default=/DAILY -> <parent>/yyyy/mm/dd/<files>
;      flat (switch -> ssw_time2paths) - if subdirectories are not nested.
;              for example,  <parent>/yyyymmdd instead of <parent>/yyyy/mm/dd
;
;   Calling Examples:
;      IDL> eitqkl=ssw_time2filelist(reltime(hours=-6),reltime(/now),parent='$EIT_QKL')
;      IDL> help,eitqkl & more,[eitqkl(0),last_nelem(eitqkl)]
;           /service/eit_data/quicklook/2006/07/31/efr20060731.160009 ; YYYY/MM/DD org
;           /service/eit_data/quicklook/2006/07/31/efr20060731.202409
;
;      IDL> eitlz=ssw_time2filelist('12:00 15-mar-2001','06:00 16-mar-2001',$
;                     parent='$EIT_LZ',/MONTHLY)
;      IDL> help,eitlz & more,[eitlz(0),last_nelem(eitlz)]
;      IDL> /service/eit_data/lz/2001/03/efz20010315.120011 ; YYYY/MM org (/MONTHLY)
;      IDL> /service/eit_data/lz/2001/03/efz20010316.054810
;      
;
;   History:
;      27-Jul-2006 - S.L.Freeland - recast file/dir time search using
;                    sss_time2paths & RSI 'file_search' & 'strmatch'
;       4-Dec-2006 - S.L.Freeland - document /FLAT option, used by
;                    STEREO/SECCHI for example (via secchi_time2filelist)
;                    Fixed a type (temporarary mispell in heretofore unexplored path)
;
;   Methdod:
;      combine implied calls to ssw_time2paths, file_search & strmatch
;
;   Restrictions:
;      if 'in_files' supplied, assume ...[yy]yymmdd[delim[hhmm[ss[mss]]]]]....
;      Need at least V5.4 if internal listing is desired (RSI file_search)
;      Need at least V5.3 if IN_FILES supplied (ie, no listing - need RSI strmatch) 
;      If PARENT supplied, then we assume "standard" chronological 
;      subdirectory ordering per ssw_time2paths - in that case, PATHS
;      is derived from the implied and derived path list
;-
;
case 1 of 
   since_version(5.4): ;ok
   since_version('5.3') and data_chk(in_files): ; ok
   else: begin 
      box_message,['Sorry, you need at least:',$
                   'IDLV 5.3 if you supply IN_FILES (rsi strmatch) -or-',$
                   'IDLV 5.4 if you do not supply IN_FILES (rsi file_search)']
   endcase
endcase
debug=keyword_set(debug)

count=0
case n_params() of
   0: begin 
         box_message,'Need time or time range'
         return,''
   endcase
   1: begin 
         time1=reltime(time0,/days)
   endcase
   else:
endcase

t0=anytim(time0,/ecs)
t1=anytim(time1,/ecs)

; listing segment
if not data_chk(in_files,/string) then begin 
   case 1 of 
      data_chk(paths,/string):  ; user supplied paths 
      data_chk(parent,/string): paths=ssw_time2paths(t0,t1,parent,_extra=_extra)
      else: begin
         paths=curdir()
         box_message,'No PATHS or PARENT; assuming currend directory....'
      endcase
   endcase
   in_files=file_search(paths,'',/full,count=count)
endif

case 1 of 
   keyword_set(fits): matches=strmatch(in_files,'*.fts',/fold_case) or $
                              strmatch(in_files,'*.fits',/fold_case)
   keyword_set(pattern): matches=strmatch(in_files,pattern)
   else: matches=intarr(n_elements(in_files))+1
endcase

ssm=where(matches,mcnt)
if mcnt eq 0 then begin 
   box_message,'No files match your pattern...'
   count=0
   return,''
endif

tfiles=temporary(in_files(ssm))
tfiles=tfiles(sort(tfiles))
flen=strlen(tfiles)
hlen=histogram(flen,min=0)
ssok=where(flen eq (where(hlen eq max(hlen)))(0)) ; eliminate bogus files
tfiles=tfiles(ssok)
 
; time search
retval=''
fid=extract_fids(tfiles,fidfound=fidfound)
if fidfound then begin  ; 
   dpos=where(strspecial(fid(0)))
   fdelim=strmid(fid(0),dpos,1)
   tf0=time2file(t0,delim=fdelim,year2=dpos eq 6)
   tf1=time2file(t1,delim=fdelim,year2=dpos eq 6)
   sst=where(fid ge tf0 and fid le tf1,tcnt)
   if tcnt gt 0 then begin 
      retval=tfiles(sst) 
      if n_params() eq 1 then retval=(temporary(retval))(0) ; 
      count=tcnt
   endif else begin
      box_message,'No files in your time range...'
      retval=''
   endelse
endif else begin
stop
   box_message,'Problem parsing times in filenames
endelse
if debug then stop,'before return'

return,retval
end

