;+
; Project     : HESSI
;
; Name        : GOES_FITS_FILES
;
; Purpose     : Return a list of possible SDAC GOES file names for specified times and satellite
;
; Explanation : The SDAC archive files are daily files, named either:
;    goxxyyyymmdd.fits for times >= 19-Jan-1999 (except for 21-Jan-1999)  or
;    goxxyymmdd.fits   for times <= 19-Jan-1999 (and for 21-Jan-1999)
;  where xx is the 2-digit satellite number, and yy, yyyy is a 2- or 4-digit year,
;  mm is the month, and dd is the day of month.
;  This routine returns the list of names constructed for each satellite for
;  each day requested.  It's called by rd_goes_sdac which will determine which
;  files actually exist.
;
;  We want to return the file names in the following order: all days requested for
;  the satellite requested, followed by all days requested for each of the remaining
;  satellites.
;
; Category    : synoptic gbo
;
; Syntax      : IDL> files=goes_fits_files(stime,etime,sat=sat,_extra=extra)
;
; Inputs      : STIME, ETIME = start/end times to search
;
; Outputs     : List of filenames
;
; Keywords    : SAT - satellite number of files we think we want
;               NO_COMPLEMENT = set to not include non-matching satellites
;
; History     : Written 12 July 2005, S. Bansal, (SSAI/GSFC)
; 15-Dec-2005, Kim. Rewrote and changed header doc.
; 26-Dec-2005, Zarro (L-3Com/GSFC) - trapped missing or non-existent satellite
; 27-Dec-2005, Zarro (L-3Com/GSFC) - added /NO_COMPLEMENT
; 21-Apr-2007, Kim. call goes_sat instead of making array of sat #'s here.
;
;-

function goes_fits_files, stime, etime, sat=satellite,$
         no_complement=no_complement, _extra=extra

;temp = sat

;-- set default to 12

if not is_number(satellite) then satellite=12

sat = trim(satellite, '(i2.2)')  ; make sure it's a string, with 2 digits

;sat_list = trim([6,7,8,9,10,12], '(i2.2)')
sat_list = trim(goes_sat(/number), '(i2.2)')

; dates will contain all the dates we need the files for
dates = timegrid (anytim(stime,/date_only), anytim(etime, /date_only), /days, /quiet, /utime)
; year2digit will be 1 or 0 for each date, telling us which should have 2-digit years
year2digit = dates le anytim('19-jan-1999') or dates eq anytim('21-jan-1999')

files = strarr(n_elements(dates), n_elements(sat_list))

; this will order the files by date, and then satellite number
for i=0,n_elements(dates)-1 do files[i,*] = 'go' + sat_list + $
   time2file(dates[i], /date_only, year2digit=year2digit[i]) + '.fits'


; now find the files for the requested satellite
q = where (strmid(files,0,4) eq 'go'+sat, complement=comp,$
           ncomplement=ncomp,count)

; return requested sat files first, then the rest of the files

if keyword_set(no_complement) then begin
 if count eq 0 then return,''
 return,files[q]
endif else begin
 if count eq 0 then return,files[comp]
 if ncomp eq 0 then return,files[q]
 return, [files[q], files[comp]]
endelse


;f = time2file(timegrid(stime,etime,/days, /quiet), /date_only)
;
;nsat = n_elements(sat_list)
;nf   = n_elements(f)
;files = strarr(nsat * nf)
;
;i = where(strmid(f,0,4) le 1999 and (strmid(f,4,2) eq 1 and strmid(f,6,2) le 19) or $
;                                    (strmid(f,4,2) eq 1 and strmid(f,6,2) eq 21))
;j = indgen(n_elements(f))
;k = where(i ne j)
;
;if (i[0] ne -1) then $
;   files[i] = 'go' + sat + strmid(f[i],2,strlen(f[0])-2) + '.fits'
;if (k[0] ne -1) then $
;   files[k] = 'go' + sat + f[k] + '.fits'
;
;
;j = 1
;while (n_elements(sat_list) gt 1) do begin
;   sat_list = sat_list(where_arr(/noteq,sat_list, sat))
;   sat = sat_list(0)
;   if (i[0] ne -1) then $
;      files[nf*j+i] = 'go' + strtrim(string(sat),2) + strmid(f[i],2,strlen(f[0])-2) + '.fits'
;   if (k[0] ne -1) then $
;      files[nf*j+k] = 'go' + strtrim(string(sat),2) + f[k] + '.fits'
;   ++j
;endwhile
;
;sat = temp
;
;
;return, files

end
