pro mreadfits_header,infits,index, debug=debug, only_tags=only_tags
;
;+
;   Name: mreadfits_header
;
;   Purpose: 3D FITS -> header structures - mreadfits helper for speed
;
;
;   Input Paramters:
;      fitsfiles - FITS file list 
;   
;   Output Parameters:
;      index - vector of header-structures
;
;   Keyword Parameters:
;      only_tags - optional tag list desired subset (for speed!)
;
;   Calling Sequence:
;      IDL> mreadfits_header, fitsfiles, index [,only_tags=taglist]
;
;   Calling Examples: 
;      IDL> mreadfits_header,fitsfiles, index ; all FITS -> index.TAGS
;      IDL> mreadfits_header,fitsfiles, index, $
;              only_tags='naxis1,naxis2,xcen,ycen,cdelt1,wavelen' ; much faster
;      IDL> help,index,/str ; show output of above
;
;   History:
;      11-July-2006 - S.L.Freeland - speed up the 3D FITS->structure process
;      17-July-2006 - S.L.Freeland - add ONLY_TAGS, online beta version
;      31-July-2006 - S.L.Freeland - in line comment protection...
;
;   Notes:
;      Preliminary tests indicate factor of at least 4 faster
;      Use of ONLY_TAGS may give another order of magnitude improvement
;
;   Restrictions:
;      escaped tags not yet implemented (DATE-OBS vs DATE_OBS for example)
;      (coming soon, I'm sure)
;      COMMENT and HISTORY not yet implemented - check tommorrow
;      Assumes that all files have same set of FITS keywords (1:1)
;      After testing, plan to integrate this ~transparently -> mreadfits.pro
;
;-
;
; use first for template
debug=keyword_set(debug)

      
case 1 of 
   n_params() eq 0: begin 
      box_message,'mreadfits_header,<fitsfiles>,index
      return
   endcase
   1-file_exist(infits(0)): begin 
      box_message,'first file does not exist, aborting...'
      return
   endcase
   else: begin 
      head=headfits(infits(0))
      mreadfits,infits(0),template ; use first for template
      ;template=rep_tag_value(template,'','SIMPLE')
      ;template=rep_tag_value(template,'','EXTEND')
      if keyword_set(only_tags) then begin 
         template=str_subset(template,only_tags)
      endif
   endcase
endcase

nf=n_elements(infits)
pad=10
nh=n_elements(head)+pad

allstr=strarr((nh+pad)*nf)  ; some padding * nFITS
      
for i=0,nf-1 do begin 
   pnt=i*nh
   allstr(pnt)=headfits(infits(i))
endfor

sscom=where(strpos(allstr,'/') gt 9 and strpos(allstr,'COMMENT') ne 0 and strpos(allstr,'HISTORY') ne 0,ccnt)
vals=allstr
if ccnt gt 0 then vals(sscom)=ssw_strsplit(allstr(sscom),'/',tail=comments)
vstr=strtrim(strmid(vals,9,100),2)
bool=where(vstr eq 'T' or vstr eq 'F',bcnt)
if bcnt gt 0 then vstr(bool)=vstr(bool) eq 'T'
keywords=strtrim(strmids(allstr,0,strpos(allstr,'=')),2)
vstr=strtrim(str_replace(vstr,"'"," "),2)

index=replicate(template,nf) 
ntags=n_tags(template)
tname=tag_names(template)
for i=0,ntags-1 do begin 
   delvarx,tempx
   tempx=index.(i)
   ;if is_member(tname(i),'SIMPLE,EXTEND') then tempx=strtrim(tempx,2)
   ss=where(tname(i) eq keywords,sscnt)
   if sscnt eq nf then begin 
      reads,vstr(ss),tempx
      if data_chk(tempx,/string) then tempx=str_replace(tempx,"'"," ")
      index.(i)=tempx
   endif else begin
      box_message,'#tags ne #files for tag: ' + tname(i)
   endelse
endfor

if debug then stop
return
end
   


