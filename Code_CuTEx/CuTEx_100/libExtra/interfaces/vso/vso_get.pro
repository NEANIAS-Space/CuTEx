;+                                                                                                                                                                                               
; Project     : VSO                                                                                                                                                                              

; Name        : VSO_GET()                                                                                                                                                                        
                                                                                                                                                                                                 
; Purpose     : Send a GetData request to VSO                                                                                                                                                    
                                                                                                                                                                                                 
; Explanation : Attempts to download data products corresponding to the                                                                                                                          
;               metadata from a previous VSO search.  Also returns                                                                                                                               
;               metadata about the files to be downloaded                                                                                                                                        
                                                                                                                                                                                                 
; Category    : Utility, Class2, VSO                                                                                                                                                             
                                                                                                                                                                                                 
; Syntax      : IDL> a = vso_get( vso_search( date='2004.1.1', inst='eit' ) )                                                                                                                    
                                                                                                                                                                                                 
; Examples    : IDL> meta = vso_search( date='2004.6.5', inst='eit', /FLAT )                                                                                                                     
                                                                                                                                                                                                 
;               IDL> wanted = where( meta.wave_min eq 171.0 )                                                                                                                                    
                                                                                                                                                                                                 
;               IDL> results = vso_get( meta[wanted] )                                                                                                                                           
                                                                                                                                                                                                 
;               IDL> print_struct, results                                                                                                                                                       
                                                                                                                                                                                                 
; History     : Ver 0.1, 27-Oct-2005, J A Hourcle.  split this out from vso__define                                                                                                              
                                                                                                                                                                                                 
;               Ver 1,   08-Nov-2005, J A Hourcle.  Released                                                                                                                                     
                                                                                                                                                                                                 
;               Ver 1.1, 21-Nov-2005, Hourcle.  Added /DOWNLOAD flag                                                                                                                             
                                                                                                                                                                                                 
;               Ver 1.2, 22-Nov-2005, Hourcle.  Replaced /DOWNLOAD flag w/ /NODOWNLOAD                                                                                                           

;               Ver 1.3  18-May-2006, Zarro. Added /NOWAIT and removed VSO_GET
;                        procedure because it confused the compiler.
                                                                                                                                                                                                 
; Contact     : oneiros@grace.nascom.nasa.gov                                                                                                                                                    
                                                                                                                                                                                                 
;               http://virtualsolar.org/                                                                                                                                                         
; Inputs:                                                                                                                                                                                        
                                                                                                                                                                                                 
;   ARGS : Can be one of:                                                                                                                                                                        
                                                                                                                                                                                                 
;     struct[n] : vsoRecord (returned from vso::query())                                                                                                                                         
                                                                                                                                                                                                 
;     struct[n] : vsoFlatRecord (returned from vso::query(/FLAT))                                                                                                                                
                                                                                                                                                                                                 
;     struct[n] : datarequest (you probably don't want this)                                                                                                                                     
                                                                                                                                                                                                 
; Opt. Input:                                                                                                                                                                                    
                                                                                                                                                                                                 
;   METHODS : string[n] : acceptable transfer methods                                                                                                                                            
                                                                                                                                                                                                 
;   MERGE   : flag      : if input is vsoRecord or vsoFlatRecord,                                                                                                                                
                                                                                                                                                                                                 
;                          will insert URLs into the input structures                                                                                                                            
                                                                                                                                                                                                 
;   OUT_DIR : string    : directory to download files to                                                                                                                                         
                                                                                                                                                                                                 
;   FORCE   : flag      : don't prompt for seemingly excessive requests                                                                                                                          
                                                                                                                                                                                                 
;   QUIET   : flag      : work silently (implies /FORCE, as well)                                                                                                                                
                                                                                                                                                                                                 
;   ??      :           : any other input allowed by sock_copy                                                                                                                                   
                                                                                                                                                                                                 
;   NODOWNLOAD : flag   : don't attempt to download files                                                                                                                                        
                                                                                                                                                                                                 
;   URLS    : flag      : override METHODS to only use URL-type transfer methods.                                                                                                                
;                                                                                                                                                                                                
;   NOWAIT  : flag      : download in background without waiting                                                                                                                                 
                                                                                                                                                                                                 
; Output:                                                                                                                                                                                        
                                                                                                                                                                                                 
;   struct[n] : getDataRecord                                                                                                                                                                    
                                                                                                                                                                                                 
; See Also:                                                                                                                                                                                      
                                                                                                                                                                                                 
;   for more documentation, see vso__define.pro                                                                                                                                                  
                                                                                                                                                                                                 
;   see also vso_search.pro (w. /URLS flag), and sock_copy.pro                                                                                                                                   
                                                                                                                                                                                                 
;-                                                                                                                                                                                               
                                                                                                                                                                                                 
function vso_get, records, merge=merge, nodownload=nodownload, force=force, quiet=quiet,$                                                                                                        
                  nowait=nowait, _extra=extra                                                                                                                                                    
                                                                                                                                                                                                 
    vso = obj_new('vso')                                                                                                                                                                         
    results = vso->getdata( records, _extra=extra )                                                                                                                                              
    obj_destroy, vso                                                                                                                                                                             
    if keyword_set( nodownload ) then return, results                                                                                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                  
; if we made it this far, we need to download the files                                                                                                                                      
; TODO : benchmark the following two methods                                                                                                                                                 
                                                                                                                                                                                                 
    unique_urls = results[uniq(results[where (results.url ne '')].url)].url                                                                                                                      
                                                                                                                                                                                                 
    ; urls = results.urls                                                                                                                                                                        
    ; unique_urls = results[uniq(urls[where (urls ne '')])]                                                                                                                                      
                                                                                                                                                                                                 
    if not keyword_set(quiet) then begin                                                                                                                                                         
                                                                                                                                                                                                 
; check if any of the fileids had messages associated with them                                                                                                                              
; minor problem -- the 'merge' flag changes the returned structure                                                                                                                       
                                                                                                                                                                                                 
        info = keyword_set(merge) ? results.getinfo : results.info                                                                                                                               
                                                                                                                                                                                                 
; TODO : we might've had duplicated fileids in the structure, if it were                                                                                                                         
; from trace ... need to make sure we only print each uniq provider/fileid                                                                                                                       
; combo                                                                                                                                                                                          
                                                                                                                                                                                                 
        if ( not is_blank(info) ) then begin                                                                                                                                                     
            messages = where ( info ne '' )                                                                                                                                                      
            print, strtrim(n_elements(messages),2)+' file(s) had informational messages:'                                                                                                        
            print_struct, results[messages], ['provider', 'fileid', keyword_set(merge) ? 'getinfo' : 'info']                                                                                     
        endif                                                                                                                                                                                    
                                                                                                                                                                                                  
        message, 'This will download '+ strtrim(n_elements(unique_urls),2)+' file(s)', /cont                                                                                                     
                                                                                                                                                                                                 
; need to prompt to continue (maybe just if it's too large)                                                                                                                                      
; but I don't know if there's a generic way to prompt in solarsoft                                                                                                                               
; 50 is just an arbitrary number ... it's probably a factor of the                                                                                                                               
; network someone's on, and the size of the files they're asking for  -- Joe                                                                                                                     
                                                                                                                                                                                                 
        if not keyword_set( force ) and n_elements(unique_urls) gt 50 then begin                                                                                                                 
            userinput = ''                                                                                                                                                                       
            read, userinput, prompt='Do you wish to continue? [Yn] '                                                                                                                             
            if ( stregex( userinput, '^n', /bool,/fold ) ) then $                                                                                                                                
                return, results ; end now                                                                                                                                                        
        endif                                                                                                                                                                                    

    endif ; end of if  keyword_set(quiet)                                                                                                                                                        
                                                                                                                                                                                                 
;-- download files in the background if IDL >=6.3  

    err=''
    if keyword_set(nowait) then begin                                                                                                                                                              
     sock_copy,unique_urls,_extra=extra,/nowait,err=err,verbose=1-keyword_set(quiet)                                                                                                                                                   
     if is_blank(err) then return,results                                                                                                                                                                               
    endif                                                                                                                                                                                          
                                                                                                                                                                                                 
; yes, sock_copy can take a list of URLs ... but it does them serially, so                                                                                                                       
; there's no real benefit ... this way, we can give status messages                                                                                                                              
; I could probably merge this with the earlier if statement, but it just                                                                                                                         
; seems better compartmentalized like this, for some reason.  -Joe                                                                                                                               
                                                                                                                                                                                                 
    if keyword_set( quiet ) then begin                                                                                                                                                           
     sock_copy, unique_urls, _extra=extra,verbose=1-keyword_set(quiet)                                                                                                                                                        
    endif else begin                                                                                                                                                                             
                                                                                                                                                                                                 
; yes, I'm walking the array backwards                                                                                                                                                          
                                                                                                                                                                                                 
        i = n_elements( unique_urls )                                                                                                                                                            
        while ( i gt 0 ) do begin                                                                                                                                                                
	    print, strtrim(i,2)+' : '+unique_urls[--i]                                                                                                                                                  
            sock_copy, unique_urls[i], _extra=extra,verbose=1-keyword_set(quiet)                                                                                                                                              
        endwhile                                                                                                                                                                                 
        print, 'Downloading completed'                                                                                                                                                           
    endelse                                                                                                                                                                                      
                                                                                                                                                                                                 
    return, results                                                                                                                                                                              
                                                                                                                                                                                                 
end                                                                                                                                                                                              
                                                                                                                                                                                                 
