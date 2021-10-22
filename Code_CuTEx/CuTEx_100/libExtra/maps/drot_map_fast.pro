;+
; Project     : SOHO-CDS
;
; Name        : DROT_MAP_FAST
;
; Purpose     : fast version of DROT_MAP
;
; Category    : imaging
;
; Explanation : Just rotates central coordinates of map
;
; Syntax      : rmap=drot_map(map,duration,time=time)
;
; Examples    :
;
; Inputs      : MAP = map structure
;               DURATION = amount to rotate by [hours units]
;
; Opt. Inputs : None
;
; Outputs     : RMAP = map with rotated coordinates
;
; Opt. Outputs: None
;
; Keywords    : DAYS = duration units in days
;             : SECONDS = duration units in seconds
;               NO_RTIME = don't add RTIME property
;
;
; History     : Written 5 June 1998, D. Zarro, SAC/GSFC
;               Modified 22 Feb 2000, Zarro (SM&A/GSFC) - added roll correction
;
; Contact     : dzarro@solar.stanford.edu
;-

function drot_map_fast,map,duration,time=time,err=err,days=days,$
               seconds=seconds,no_rtime=no_rtime

on_error,1
err=''

;--check inputs

if not valid_map(map,err=err) then begin
 pr_syntax,'rmap=drot_map_fast(map,duration,[time=time])'
 if exist(map) then return,map else return,-1
endif

;-- get solar rotation duration

dtime=get_drot_dur(map,duration,time=time,days=days,seconds=seconds)
cur_time=get_map_time(map,/tai)
ntime=n_elements(dtime)
nmap=n_elements(map)
have_rtime=tag_exist(map,'rtime')
for i=0,nmap-1 do begin

;-- check if already solar rotated

 cdur=dtime(i < (ntime-1))
 dprint,'% duration (sec): ',cdur
 new_time=cur_time(i)+cdur
 already_rotated=(new_time eq cur_time(i))
 tmap=map(i)

 if (1-already_rotated) then begin

  if tag_exist(map(i),'soho') then soho=map(i).soho else soho=0

;-- only need solar radius once

  if not exist(radius) then begin
   pb=pb0r(cur_time(i),soho=soho,/arcsec,/retain)
   radius=float(pb(2))
  endif

;-- get center of map

  xc=get_map_prop(map(i),/xc)
  yc=get_map_prop(map(i),/yc)

;-- if not on disk then don't rotate

  on_disk=sqrt(xc^2+yc^2) lt radius
  if on_disk then begin
   rcor=rot_xy(xc,yc,tstart=cur_time(i),tend=new_time,soho=soho)
   rcor=reform(rcor)
   xr=rcor(0) & yr=rcor(1)
   still_on_disk=sqrt(xr^2+yr^2) le radius

;-- if still on disk then update map. Also correct for roll.

   if still_on_disk then begin
    xn=xr & yn=yr
    have_roll=tag_exist(map(i),'roll_angle')
    if have_roll then have_roll=(map(i).roll_angle mod 360.) ne 0.
    if have_roll then roll_xy,xr,yr,map(i).roll_angle,center=map(i).roll_center,xn,yn
    tmap=shift_map(map(i),xc=xn,yc=yn)
   endif
  endif
 endif
 if not have_rtime and (1-keyword_set(no_rtime)) then tmap=add_tag(tmap,anytim2utc(new_time,/vms),'rtime',index='time')
 rmap=merge_struct(rmap,tmap)
endfor

return,rmap

end
