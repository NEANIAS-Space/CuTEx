;+                                                                                              
; Project     : VSO                                                                             
;                                                                                               
; Name        : VSO_SEARCH()                                                                    
;                                                                                               
; Purpose     : Send a search request to VSO                                                    
;                                                                                               
; Explanation : Sends a query to VSO, to obtain metadata records corresponding                  
;               to the records that match the query parameters.                                 
;                                                                                               
; Category    : Utility, Class2, VSO                                                            
;                                                                                               
; Syntax      : IDL> a = vso_search('2005-JAN-01', instrument='eit')                            
;                                                                                               
; Examples    : IDL> a = vso_search(date='2004-01-01', provider='sdac')                         
;               IDL> a = vso_search(date='2002-1-4 - 2002-1-4T07:05', inst='mdi')               
;               IDL> a = vso_search(date='2004/1/4T07:40-2004/1/4T07:45', inst='trace')         
;               IDL> a = vso_search(date='2004-1-1', extent='CORONA', /FLAT)                    
;               IDL> a = vso_search(date='2001-1-1', physobs='los_magnetic_field')              
;               IDL> a = vso_search(date='2004/1/1', inst='eit', /DEBUG)                        
;               IDL> a = vso_search('2004/1/1','2004/12/31', wave='171 Angstrom', inst='eit')   
;               IDL> a = vso_search('2004/6/1','2004/6/15', wave='284-305 Angstrom', inst='eit')
;               IDL> a = vso_search('2005-JAN-1', inst='eit', /FLAT, /URL)                      
;                                                                                               
;               IDL> print_struct, a                                                            
;               IDL> print_struct, a.time       ; if not called w/ /FLATTEN                     
;               IDL> sock_copy, a.url           ; if called w/ /URLS                            
;               IDL> vso_get, a                 ; attempt to download products                  
;		                                                                                             
;                                                                                               
; History     : Ver 0.1, 27-Oct-2005, J A Hourcle.  split this out from vso__define             
;               Ver 1,   08-Nov-2005, J A Hourcle.  Released                                    
;                        12-Nov-2005, Zarro (L-3Com/GSFC)                                       
;                         - added TSTART/TEND for compatability with SSW usage.                 
;                         - added _REF_EXTRA to communicate useful keywords                     
;                           such as error messages.                                             
;               Ver 1.1, 01-Dec-2005, Hourcle.  Updated documentation                           
;               Ver 1.2, 02-Dec-2005, Zarro. Added call to VSO_GET_C
;               Ver 1.3, 18-May-2006, Zarro. Removed call to VSO_GET_C
;                           because it confused the compiler
;                                                                                               
; Contact     : oneiros@grace.nascom.nasa.gov                                                   
;               http://virtualsolar.org/                                                        
;                                                                                               
; Input:                                                                                        
;   (note -- you must either specify DATE, START_DATE or TSTART)                                
; Optional Input:                                                                               
; (positional query parameters))                                                                
;   TSTART     : string ; the start date                                                        
;   TEND       : string ; the end date                                                          
; (named query parameters)                                                                      
;   DATE       : string ; (start date) - (end date)                                             
;   START_DATE : string ; the start date                                                        
;   END_DATE   : string ; the end date                                                          
;   WAVE       : string ; (min) - (max) (unit)                                                  
;   MIN_WAVE   : string ; minimum spectral range                                                
;   MAX_WAVE   ; string ; maximum spectral range                                                
;   UNIT_WAVE  ; string ; spectral range units (Angstrom, GHz, keV)                             
;   EXTENT     ; string ; VSO 'extent type' ... (FULLDISK, CORONA, LIMB, etc)                   
;   PHYSOBS    ; string ; VSO 'physical observable;                                             
;   PROVIDER   ; string ; VSO ID for the data provider (SDAC, NSO, SHA, MSU, etc)               
;   SOURCE     ; string ; spacecraft or observatory (SOHO, YOHKOH, BBSO, etc)                   
;   INSTRUMENT ; string ; instrument ID (EIT, SXI-0, SXT, etc)                                  
; (placeholders for the future)                                                                 
;   DETECTOR   ; string ; detector ID (not supported by all providers; use inst also)           
;   FILTER     ; string ; filter name (same problems as detector)                               
;                                                                                               
; (other flags)                                                                                 
;   URLS       ; flag ; attempt to get URLs, also                                               
;   QUIET      ; flag ; don't print informational messages                                      
;   DEBUG      ; flag ; print xml soap messages                                                 
;   FLATTEN    ; flag ; return vsoFlat Record (no sub-structures)                               
;                                                                                               
; Outputs:                                                                                      
;   a null pointer -> no matches were found                                                     
;   (or)                                                                                        
;   struct[n] : (vsoRecord or vsoFlatRecord) the metadata from the results                      
;                                                                                               
; Limitations : if using /URLS, you may wish to examine output.getinfo for messages             
;                                                                                               
; note -- resolution of date parameters: order of precidence                                    
;   DATE (named parameter)                                                                      
;   TSTART/TEND (positional parameters)                                                         
;   START_DATE/END_DATE (named parameters)                                                      
; it is possible to mix TSTART/END_DATE, or DATE(only a start)/END_DATE                         
; but it is not recommended, and may not be supported in the future.                            
;                                                                                               
; if no end date is specified, the system will use the start of the                             
; next day                                                                                      
;                                                                                               
;-                                                                                              
                                                                                                
function vso_search, tstart,tend, urls=urls, _ref_extra=extra                                   
                                                                                                
        contents=''                                                                             
        vso = obj_new('vso',_extra=extra)                                                       
        if not obj_valid(vso) then return,contents                                              
                                                                                                
;-- construct VSO query and send it                                                             
                                                                                                
        query = vso->buildquery(tstart,tend,_extra=extra )                                      
        records = vso->query(query, _extra=extra)                                               
                                                                                                
;-- check for results                                                                           
                                                                                                
        err_msg='no matching records found'                                                     
        if obj_valid(records) then begin                                                        
         if exist(records->contents()) then begin                                               
          contents=records->contents()                                                          
          if ( keyword_set(urls) ) then $                                                       
           contents=vso->getdata(contents, /merge, _extra=extra )                               
         endif else message,err_msg,/cont                                                       
        endif else message,err_msg,/cont                                                        
                                                                                                
;-- cleanup                                                                                     
                                                                                                
        obj_destroy,[records,vso]                                                               
                                                                                                
        return,contents                                                                         
                                                                                                
end                                                                                             
