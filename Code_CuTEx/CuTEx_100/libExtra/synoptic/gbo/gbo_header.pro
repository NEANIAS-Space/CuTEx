
;-- GBO header include file

db_gbo
common gbo_com
restore_gbo

@html_tags

if datatype(gbo_records) ne 'STC' then begin
 err='No records found in GBO database'
 print_html,lun,err,ofile=ofile
 return
endif

if not exist(rec) then rec=-1
get_gbo,rec,result,count=count
if count eq 0 then begin
 err='Non-existent GBO record: '+num2str(rec)
 print_html,err,ofile=ofile
 return
endif

gbo_time,result,cstart,cend

tstart=anytim2utc(cstart,/vms,/trun)
tend=anytim2utc(cend,/vms,/trun)

