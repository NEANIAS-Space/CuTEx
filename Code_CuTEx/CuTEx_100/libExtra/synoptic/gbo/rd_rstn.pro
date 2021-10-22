;+
; Project     : HESSI
;
; Name        : RD_RSTN
;
; Purpose     : read RSTN radio data
;
; Category    : synoptic gbo
;
; Syntax      : IDL> rd_rstn,files,times,data
;
; Inputs      : FILES = RSTN file names
;
; Outputs     : TIMES = time array
;               DATA  = data array (# TIMES x 8)
;               FREQ  = frequency string
;               TAI = return time in TAI, else UTC
;
; Keywords    : TSTART, TEND = time sub-interval to select
;
; History     : Written 8 April 2000, D. Zarro, SM&A/GSFC
;
; Contact     : dzarro@solar.stanford.edu
;-

pro rd_rstn,file,times,data,err=err,tstart=tstart,tend=tend,$
                 frequency=frequency,count=count,tai=tai

;-- usual error checks

err=''
count=0
delvarx,times,data,freq

if n_params(0) lt 2 then begin
 err='missing input'
 pr_syntax,'rd_rstn,files,times,data'
 return
endif

;-- read and concatanate files 

count=n_elements(file)
if count eq 0 then return
rfiles=get_uniq(file)
count=n_elements(rfiles)

for i=0,count-1 do begin
 err=''
 out=rd_ascii_c(rfiles[i],err=err,buff=44000l)
 if err eq '' then odata=append_arr(odata,out,/no_copy)
endfor

if not exist(odata) then begin
 err='Zero lightcurve data to plot'
 message,err,/cont
 return
endif

;-- decode times and frequency data 
;-- convert to byte array and count characters (bone-headed but fast)

odata=byte(temporary(odata))

year=fix(string(odata(4:7,*)))
month=fix(string(odata(8:9,*)))
day=fix(string(odata(10:11,*)))
hour=fix(string(odata(12:13,*)))
minute=fix(string(odata(14:15,*)))
sec=fix(string(odata(16:17,*)))
msec=intarr(n_elements(sec))
times=transpose([[temporary(hour)],[temporary(minute)],$
                [temporary(sec)],[temporary(msec)],[temporary(day)],$
                [temporary(month)],[temporary(year)]])

times=anytim2tai(temporary(times))

;-- speed up searches if TSTART/TEND specified

ntimes=n_elements(times)
ok=where_times(times,tstart=tstart,tend=tend,count=count)
if count eq 0 then begin
 err='no data during specified times'
 message,err,/cont
 return
endif

if count lt ntimes then begin
 times=times[ok]
 odata=odata[*,ok]
endif

if (1-keyword_set(tai)) then times=anytim2utc(temporary(times))
        
data=[[string(odata[19:23,*])],[string(odata[25:29,*])],$
      [string(odata[31:35,*])],[string(odata[37:41,*])],$
      [string(odata[43:47,*])],[string(odata[49:53,*])],$
      [string(odata[55:59,*])],[string(odata[61:65,*])]]

data=fix(strtrim(temporary(data),2))

frequency=string([245., 410., 610., 1415., 2695., 4995., 8800.,15400.],$
     format='(i5)')+' MHz'
                                                                    
return & end


