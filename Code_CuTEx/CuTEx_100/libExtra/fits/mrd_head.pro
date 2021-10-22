;+
; Project     : HESSI
;
; Name        : MRD_HEAD
;
; Purpose     : wrapper around MRD_HREAD to simplify reading FITS file
;               headers
;
; Category    : FITS I/O
;
; Syntax      : IDL> hread,file,header
;
; Inputs      : FILE = input file name
;
; Outputs     : HEADER = string header
;
; Keywords    : EXTENSION = binary extension [def=0]
;               ERR = error string
;
; Written     : Zarro (EIT/GSFC), 13 Aug 2001
;
; Contact     : dzarro@solar.stanford.edu
;-

pro mrd_head,file,header,extension=extension,err=err

err='' & header=''

if is_blank(file) then begin
 err='invalid input file name'
 message,err,/cont
 return
endif

if n_elements(file) gt 1 then begin
 err='input file name must be scalar'
 return
endif


;-- check if need to manually decompress

compressed=is_compressed(file,type)
decompress=(not since_version('5.3')) or $
            ((type eq 'Z') and (os_family(/lower) eq 'windows'))

if compressed and decompress then dfile=find_compressed(file,err=err) else dfile=file
if err ne '' then return

if not is_number(extension) then extension=0
lun = fxposit(dfile,extension,/readonly)
if lun lt 0 then begin
 err='could not open '+file
 message,err,/cont
 return
endif

mrd_hread,lun,header,status
close_lun,lun

if status ne 0 then begin
 err='Invalid FITS file: '+file
 message,err,/cont
 return
endif
 
return
end
