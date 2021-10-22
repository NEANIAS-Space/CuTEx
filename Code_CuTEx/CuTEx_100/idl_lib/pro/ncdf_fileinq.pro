;+
; NAME:
;       NCDF_FILEINQ
;
; PURPOSE:
;       Print a table of the contents of an ncdf file to the terminal
;       and/or save the structure of the file in a structure!
;
; CATEGORY:
;       Input/Output
;
; CALLING SEQUENCE:
;       ncdf_fileinq, file, result     
;
; INPUTS:
;      file:     the name of the file to be examined.
;      result:   a variable to take the structure containing the
;                result of the file inquiry.  The form of the
;                structure is given below.
;
; KEYWORD PARAMETERS:
;      QUIET:    Setting this keyword prevents printing any information
;                from the file. Error messsages still appear on the terminal
;      MAXNDIMS: the maximum number of dimensions to expect
;      MAXNATTS: the maximum number of attributes to expect, either
;                global or for any one particular variable
;      TEE:      The name of a file to wite information about the NCDF
;                file.  If quiet is set then nothing will be written.
;                If no filename is given then "tee.txt" is used.
;
; OUTPUTS:
;      Information about the netcdf file is printed on the terminal
;      depending upon the settings of the keyword arguments.
;      If the result argument is present a structure containing
;      information about the NCDF file is returned in that
;      argument. The previous contents of that argument, if any, are
;      lost.  The structure is not the same for each file but it does
;      follow a strict format and is self documenting.  
;
; RESTRICTIONS:
;      None are known.      
;
; EXAMPLE:
;      Here is an example of output from reading a typical ncdf file.
;      
;   IDL> help,r,/struct
;   ** Structure <3004701c>, 6 tags, length=2124, refs=1:
;      NGATTS          LONG                 2
;      NDIMS           LONG                 7
;      NVARS           LONG                35
;      GATTS           STRUCT    -> <Anonymous> Array[2]
;      DIMS            STRUCT    -> <Anonymous> Array[7]
;      VARS            STRUCT    -> <Anonymous> Array[35]
;
;   NGATTS, NDIMS, NVARS indicate how many of the corresponding
;   dtructures are present (nested) within the main structure.
;
;   The GATT struture array contains these tags, {NUMBER, NAME, VALUE}
;   and looks like:
;   IDL> help,r.gatts[0],/STRU
;   ** Structure <3007864c>, 3 tags, length=20, refs=3:
;      NUMBER          LONG                 0
;      NAME            STRING    'stamp'
;      VALUE           STRING    'd/m/y=30/12/2850, h:m:s=18: 0: 0'
;
;   The DIMS structure contains tags {NUMBER,NAME,SIZE} and looks like:
;   IDL> help,r.dims,/struc 
;   ** Structure <30078b1c>, 3 tags, length=16, refs=3:
;      NUMBER          LONG                 0
;      NAME            STRING    'jmt'
;      SIZE            LONG               102
;
;   The VARS structure has {NDIMS,NATTS,NUMBER,NAME,ATTS,DIMS} and
;   looks like:
;   IDL> help,r.vars,/struc
;   ** Structure <30078e1c>, 6 tags, length=56, refs=2:
;      NDIMS           LONG                 1
;      NATTS           LONG                 0
;      NUMBER          LONG                 0
;      NAME            STRING    'yt'
;      ATTS            STRUCT    -> <Anonymous> Array[1]
;      DIMS            STRUCT    -> <Anonymous> Array[1]
;
;  Each var structure may contain ATTS and DIMS array as well. There
;  will be NDIMS and NATTS of these subelements.
;
;
;  REPURCUSSIONS
;       This routine is not using any special memory tricks such as
;       pointers to heap variables.  As such it will waste memory,
;       especially if one variable has significantly more attributes
;       and/or dimensions than the others.  Even so it won't waste
;       much.
;
; MODIFICATION HISTORY:
; 	Written by: Edward C. Wiebe (ecwiebe@uvic.ca), 2000-05-10 
;                   (based on code by M Eby.)
;       Modified:   ECW, 2000-08-01 (Added QUIET keyword)
;       Modified:   ECW, 2000-08-02 (can now save results in an 
;                                    optional result argument.)
;       Modified:   ECW, 2000-08-02 (Added MAXNDIMS and MAXNATTS 
;                                   keywords and error checking.)
;       Modified:   ECW, 2001-04-11 (changed vartype to var_type)
;       Modified:   ECW, 2002-05-17 (changed print commands to messages
;                                    made quiet totally quiet) 
;       Modified:   ECW, 2002-08-09 (added the recdim token to the result) 
;-

pro ncdf_fileinq, file, result            $
                  , QUIET=quiet           $
                  , MAXNDIMS=maxndims     $
                  , MAXNATTS=maxnatts     $
                  , TEE=tee

; test for the quiet keyword 
  if (not Keyword_Set(quiet)) then quiet = 0 else quiet =1

; test for the result optional argument
  if (N_Params() eq 2) then save = 1 else save = 0

  if (not quiet) and (save) then Message,/INFO,'Saving data.'
  
  if (Keyword_Set(tee)) then begin
    if (Var_Type(tee) eq 7) then begin
      teefile = tee 
    endif else teefile = 'tee.txt'
    tee = 1
  endif else tee = 0

  if (save) then begin
    if (not Keyword_Set(maxndims)) then maxndims = 1
    if (not Keyword_Set(maxnatts)) then maxnatts = 1 
  endif

  cnt = 0
  ok  = 1
  while ok do begin
    cnt = cnt + 1
    if (cnt gt 1) then quiet = 1

    if (save) then begin
      nattcorr = maxnatts
      nattc    = 0
      ndimcorr = maxndims
      ndimc    = 0  
    endif

    atterr = 0
    dimerr = 0
    
    fid = NCDF_Open(file)

    if (not quiet and tee) then begin
      OpenW,tid,teefile,/GET_LUN
    endif
    
    if (not quiet) then Tee,tid,'FILE: '+file

    info = NCDF_Inquire(fid)
    
    if (not quiet) then begin
      Tee,tid, ''
      Tee,tid, 'GLOBAL'
      Tee,tid, '  ATTRIBUTES:' + StrCompress(info.ngatts)
    endif
    
    if (save) then begin
      att = {number:0L,name:'',value:''}
      if (info.ngatts gt 0) then begin
        gatts  = Replicate(att,info.ngatts)
      endif else begin
        gatts = att
      endelse
    endif
    
    for n=0,info.ngatts-1 do begin
      name = NCDF_AttName(fid, /GLOBAL, n)
      NCDF_AttGet, fid, name, value, /GLOBAL
      if (save) then begin
        gatts[n].number= n
        gatts[n].name  = name
        gatts[n].value = String(value)
      endif
      if (not quiet) then begin
        Tee,tid, '    NUMBER: NAME: VALUE: ' + StrCompress(n) + ': '    $
            + StrCompress(name) + ': ' + StrCompress(value)
      endif
    endfor
    
    if (save) then begin
      dim  = {number:0L,name:'',size:0L}
      dims = Replicate(dim,info.ndims)
    endif
    
    if (not quiet) then Tee,tid, '  DIMENSIONS:' + StrCompress(info.ndims)
    for n=0, info.ndims-1 do begin
      NCDF_DimInq, fid, n, name, size
      if (save) then begin
        dims[n].number = n
        dims[n].name   = name
        dims[n].size   = size
      endif
      if (not quiet) then begin
        Tee,tid, '    NUMBER: NAME: SIZE: ' + Strcompress(n) + ': '    $
            + StrCompress(name) + ': ' + StrCompress(size)
      endif
    endfor
    
    if (not quiet) then begin
      Tee,tid,''
      Tee,tid, 'VARIABLES: '+StrCompress(info.nvars-1)
    endif
    
    if (save) then begin
;     Define a variable structure using the maximum number of dimensions 
;     and the maximum number of attributes.  
      var = {ndims:0L,natts:0L,number:0L,name:''        $
             ,atts:Replicate(att,maxnatts),dims:Replicate(dim,maxndims)}
;     Make info.nvars of the var structures in an array
      vars = Replicate(var,info.nvars)
    endif
    
    for n=0, info.nvars-1 do begin
      r = NCDF_VarInq(fid, n)
      if (not quiet) then begin
        Tee,tid, '  NUMBER: NAME: ' + StrCompress(n) + ', ' + StrCompress(r.name)
        Tee,tid, '    ATTRIBUTES:' + StrCompress(r.natts)
      endif
      
      if (save) then begin
        vars[n].name   = String(r.name)
        vars[n].number = n      
        vars[n].natts  = r.natts
        vars[n].ndims  = r.ndims
      endif
      
      for m=0, r.natts-1 do begin
        name = NCDF_ATTNAME(fid, n, m)  
        NCDF_AttGet, fid, n, name, value
        if (not quiet) then begin
          Tee,tid, '      NUMBER: NAME: VALUE: ' + StrCompress(m) + ': ' $
              + StrCompress(name) + ': ' + StrJoin(value)
        endif
        if (save) then begin        
          if (m lt maxnatts) then begin
            vars[n].atts[m].number = m
            vars[n].atts[m].name   = name
;           It's possible for attributes to be arrays.  Byte arrays
;           are ok since we know they become strings but what about
;           the other types.
            if ((Var_Type(value) ne 1) and (N_Elements(value) gt 1)) then $
              value = StrJoin(value)
            vars[n].atts[m].value  = String(value)
          endif else begin
            atterr   = 1
            nattcorr = nattcorr+1
          endelse
        endif
      endfor
      
      if (save) then begin
        nattc = Max([nattc,nattcorr])
        nattcorr = maxnatts
      endif
      
      if (not quiet) then Tee,tid, '    DIMENSIONS:' + StrCompress(r.ndims)
      for m=0, r.ndims-1 do begin
        NCDF_DimInq, fid, r.dim[m], name, size
        if (not quiet) then begin
          Tee,tid, '      NUMBER: NAME: SIZE: ' + StrCompress(m) + ': ' $
              + Strcompress(name) + ': ' + StrCompress(size)
        endif
        if (save) then begin
          if (m lt maxndims) then begin
            vars[n].dims[m].number = m
            vars[n].dims[m].name   = name
            vars[n].dims[m].size   = String(size)
          endif else begin
            dimerr = 1
            ndimcorr = ndimcorr+1
          endelse
        endif
      endfor
      
      if (save) then begin
        ndimc = Max([ndimc,ndimcorr])
        ndimcorr = maxndims
      endif
    endfor
    
    NCDF_Close,fid
    if (tee) and (not quiet) then Free_Lun,tid
    
    if (save) then begin
      if (atterr) then begin
        if (not quiet) then begin
          Message,/INFO,'Not enough space in attribute array. ' + $
                  'Use MAXNATTS=' + StrCompress(nattc) + '.'  
        endif
        maxnatts = nattc
      endif
      
      if (dimerr) then begin
        if (not quiet) then begin
          Message,/INFO,'Not enough space in dimension array.  ' + $
                  'Use MAXNDIMS=' + StrCompress(ndimc) + '.'      
        endif
        maxndims = ndimc
      endif
    endif 
    
    if ((cnt eq 2) or ((not dimerr) and (not atterr))) then begin
      ok = 0 
      if (not quiet) then Message,/INFO,'NCDF_FILEINQ OK'
    endif else begin  
      Message,/INFO,'Repeating NCDF_FILEINQ with: ' +     $
              ' MAXNATTS=' + StrCompress(nattc) + ', ' +  $
              ' MAXNDIMS=' + StrCompress(ndimc) + '.' 
      quiet = 1
    endelse
    

  endwhile

  if (save) then begin
    result = {ngatts:  info.ngatts      $
              ,ndims:  info.ndims       $
              ,nvars:  info.nvars       $
              ,gatts:  gatts            $
              ,dims:   dims             $
              ,vars:   vars             $
              ,recdim: info.recdim      $
             }             
  endif

  Return
end


