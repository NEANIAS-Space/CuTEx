pro mdi_oldpath
;
;   Name: mdi_oldpath
;
;   Purpose: recover the (bizarre) mdi path including many ssw conflicts
;
;   Calling Sequence:
;      IDL> mdi_oldpath         ; interactive or in an idl startup
;
ssw_path,concat_dir('$SSW_MDI','idl_old')
return
end
