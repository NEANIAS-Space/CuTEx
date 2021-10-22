;+
; Project     : HESSI
;                  
; Name        : CHECK_NETWORK
;               
; Purpose     : check if network checking enabled
;                             
; Category    : system utility sockets
;               
; Syntax      : IDL> help,check_network()
;
; Outputs     : 1/0 is SEARCH_NETWORK is defined to 1
;
; Keywords    : None
;
; History     : 14 April 2002, Zarro (L-3Com/GSFC)
;
; Contact     : dzarro@solar.stanford.edu
;-    

function check_network

return,chklog('SEARCH_NETWORK') eq '1'

end


