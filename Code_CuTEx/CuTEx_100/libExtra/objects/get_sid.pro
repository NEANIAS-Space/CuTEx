;+
; Project     : HESSI
;
; Name        : GET_SID
;
; Purpose     : Return a session ID (SID) for tracking users CGI forms
;
; Category    : HTML
;                   
; Inputs      : None
;
; Outputs     : SID = unique session ID string
;
; Keywords    : EXPIRED = if SID already exists in memory, and it is older
;               than EXPIRED minutes [def=10], then return it instead of creating
;               a new one.
;               RESET = set to clear out SID_POOL and release memory
;
; Common      : GET_SID,SID_POOL = saved array of SID structures
;                {sid, sid:sid, born:born, ptr:ptr} 
;                BORN = time when SID was created (TAI format)
;                PTR  = pointer in which FORM data relating to SID is stored 

; History     : 13-Sept-1999,  D.M. Zarro.  Written
;
; Contact     : DZARRO@SOLAR.STANFORD.EDU
;-

pro get_sid,sid,expired=expired,reset=reset,new=new,stc=stc,quiet=quiet
                
common get_sid,sid_pool

delvarx,sid
if keyword_set(reset) then begin
 if (datatype(sid_pool) eq 'STC') then free_pointer,sid_pool.ptr
 delvarx,sid_pool
 return
endif

if not exist(expired) then expired=10.

check_pool=datatype(sid_pool) eq 'STC' 

;-- look for first expired SID in SID_POOL

get_utc,utc,/vms
cur_time=anytim2tai(utc)

if check_pool then begin
 births=sid_pool.born
 age=cur_time-births
 old=where( age gt expired*60.,count)

;-- found one, rebirth it and return it

 if count gt 0 then begin
  oldest=min(sid_pool(old).born)
  chk=where(sid_pool.born eq oldest)
  sid=sid_pool(chk(0)).sid
  if (1-keyword_set(quiet)) then print,sid
  new=0
  sid_pool(chk(0)).born=cur_time
;  ptr_free,sid_pool(chk(0)).ptr
  stc=sid_pool(chk(0))
  return
 endif
endif

;-- create a new SID if SID_POOL is empty, or no old ones found

common random,seed
temp=anytim2ex(utc)
temp=strcompress(arr2str(temp(0:3),delim=''),/rem)
sid=temp+trim(string(nint(randomu(seed)*10000.)))

if (1-keyword_set(quiet)) then print,sid
new=1
new_sid={sid,sid:sid,born:cur_time,ptr:ptr_new(/all)}
stc=new_sid
sid_pool=merge_struct(sid_pool,new_sid)

return & end



