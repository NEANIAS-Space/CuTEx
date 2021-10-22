function get_goes_defsat,start_time,stop_time, $
   check_sswdb=check_sswdb, online=online, string_return=string_return
;+
;   Name: get_goes_defsat
;
;   Purpose: return time dependent GOES satellite number
;
;   Input Parameters:
;       start_time - time or start time of range
;       stop_time - optional stop time (if range)
;
;   Output:
;      function returns time dependent GOES satellite number ("default")
;
;
;   Keyword Parameters:
;      check_sswdb - (switch) - if set, verify dbase is online
;      online (output) - boolean - 1 if dbase is available 
;      string_return - (switch) - if set, output is string Sat#
;      
;   History: 
;      16-Apr-2003 - S.L.Freeland rd_gxd/plot_goes helper 
;
;-

retval=-1
stringit=keyword_set(string_return)

if n_elements(start_time) eq 0 then start_time=reltime(/now) ; default T=current UT

case n_params() of 
   2:       
   else: stop_time=start_time(0)
endcase
 
t0=anytim(start_time,/ecs)
t1=anytim(stop_time,/ecs)

def9t= '1-jul-1996'              ; GOES 7 off
def8t= '1-jul-1998'              ; GOES 9 defunct
def10t='8-apr-2003'              ; GOES 8 off

case 1 of 
   ssw_deltat(t1,ref=def10t) ge 0: retval=10
   ssw_deltat(t1,ref=def8t)  ge 0: retval=8
   ssw_deltat(t1,ref=def9t)  ge 0: retval=9
   else: retval=7
endcase
if stringit then retval=strtrim(retval,2)

return,retval
end

