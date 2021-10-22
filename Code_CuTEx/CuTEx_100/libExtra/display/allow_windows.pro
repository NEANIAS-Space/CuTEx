;+
; Project     : HESSI
;                  
; Name        : ALLOW_WINDOWS
;               
; Purpose     : platform/OS independent check if current device
;               allows windows 
;                             
; Category    : system utility
;               
; Syntax      : IDL> a=allow_windows()
;                                        
; Outputs     : 1/0 if yes/no
;                   
; History     : Version 1,  4-Nov-1999, Zarro (SM&A/GSFC)
;               13 Dec 2001, Zarro (EITI/GSFC) - added DEVICE call
;               22 Oct 2002, Zarro (EER/GSFC) - added FSTAT check
;
; Contact     : dzarro@solar.stanford.edu
;-    

function allow_windows,dummy,quiet=quiet,verbose=verbose

;-- catch any open errors (vers > 3)

if since_version('3.0') then begin
 error=0
 catch,error
 if error ne 0 then begin
  err='Error opening window'
  if keyword_set(verbose) then message,err,/cont
  catch,/cancel
  check=0b
  return,check
 endif
endif

;-- check if we have a GUI or TTY (otherwise we must be in batch mode)

;stdin=fstat(0)
;tags=strupcase(tag_names(stdin))
;inter=strpos(tags,'INTERACTIVE')
;chk=where(inter gt -1,count)
;if count gt 0 then if stdin.interactive eq 0 then return,0b

;-- check if windows or widgets are even supported

if not have_windows() then return,0b
if not have_widgets() then return,0b

;-- try to create an invisible widget base

;s=widget_base(map=0)
;if widget_info(s,/valid) then widget_control,s,/destroy,/clear_events,$
; bad_id=destroyed

device,get_screen_size=s

check=1b

return,check

end
