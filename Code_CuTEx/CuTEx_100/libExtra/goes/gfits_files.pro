;+
; GFITS_FILES
; Finds GOES FITS files for a specified time interval.
; Based on user start and end times and satellite (GOES), create an 
; array of file names covering that time interval.
;
; Kim Tolbert 6/93
; Modification History:
; Version 2, Amy.Skowronek@gsfc.nasa.gov, modified findfile call
;	to get longer than two digit years for y2k
;-
pro gfits_files, stime_sec, etime_sec, satellite, files, count

sat = string(satellite,format='(i2.2)')

files = [' ']
count = 0

testtime = fix(stime_sec / 86400.d0) * 86400.d0
while testtime lt etime_sec do begin
   parse_atime, atime(testtime), year=year, month=month, day=day, /string
   files = [files, 'go' + sat + '*' + year + month + day + '.fits']
   count = count + 1
   testtime = testtime + 86400.d0
endwhile

if count gt 0 then files = files(1:count)  ; get rid of first blank file name

return & end
