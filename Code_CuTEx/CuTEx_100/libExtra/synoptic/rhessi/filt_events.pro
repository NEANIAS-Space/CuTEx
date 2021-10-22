;+
; Project     : HESSI
;
; Name        : FILT_EVENTS
;
; Purpose     : filter times based on proximity to flare event
;
; Category    : synoptic gbo
;
; Syntax      : IDL> ftimes=filt_events(times,dur,count=count)
;
; Inputs      : TIMES = input times to filter (TAI format)
;
; Optional Inp: DUR = duration for each time (secs)
;               PREFLARE = minutes of preflare event time
;               to consider [def=0]
;
; Outputs     : FTIMES = times that overlap event start/stop times
;
; Keywords    : COUNT = # of overlapping times
;
; History     : Written 18 Jul 2001, D. Zarro, EITI/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

function filt_events,times,dur,count=count,err=err,preflare=preflare,$
                     verbose=verbose

;-- usual error checks

verbose=keyword_set(verbose)
err=''
count=0
ntimes=n_elements(times)
if ntimes eq 0 then return,-1
if (ntimes eq 1) or datatype(times) ne 'DOU' then return,times

if exist(dur) then tdur=dur else tdur=0
tmin=anytim2utc(min(times))
tmax=anytim2utc(max(times+tdur))

;-- find GOES flare start and end times

rd_gev,anytim(tmin,/int),anytim(tmax,/int),gev,status=status
if status ne 0 then begin
 message,'No GOES events during specified period',/cont
 return,-1
endif

decode_gev,gev,gstart,gend,gpeak,/tai,class=class
if is_number(window) then pref=window*60. else pref=0
gstart=gstart-pref

;-- loop thru each event and find overlapping times

nevents=n_elements(gev)
for i=0,nevents-1 do begin
 if verbose then message,'checking event at '+anytim2utc(gpeak[i],/vms),/cont
 g1=gstart[i] & g2=gend[i]
 ok=where (  ((times+tdur) ge g1) and (times le g2), count)
 if count gt 0 then begin
  if exist(ftimes) then ftimes=[temporary(ftimes),times[ok]] else $
   ftimes=times[ok]
 endif
endfor

if exist(ftimes) then return,get_uniq(ftimes,count=count)

message,'no overlapping times found',/cont
return,-1

end

