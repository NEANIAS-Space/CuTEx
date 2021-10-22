function extract_photo_oformat

	print, " Preparing IPAC table format for output files "


	field_len=15

	;col_name = ['N','group','ngroup','x','y', 'ra','dec','Wavelength','f_peak', 'fwhm_x','fwhm_y', 'pa','f_int','back','chi2_dof','rms', 'chi2',  'Status']
	;col_type = ['int','int','int', 'float','float',  'double', 'double','double','double','double','double','float','double','double','double','double','double', 'int']
	;col_unit = [' ', ' ', ' ', 'pixel', 'pixel' ,'degree' ,'degree' ,'micron' ,'MJy/sr' ,'arcsec' ,'arcsec' ,'degree ','Jy','MJy/sr',' ','MJy/sr',' ', ' ']

	col_name = [$
	'N','X','Y', $
	'RA','DEC', 'GLON', 'GLAT', $
	'WAVELENGTH','F_PEAK', $
	'FWHM_X','FWHM_Y',$
	'PA','F_INT','ERR_F_INT', $
	'BACK_ONPEAK','RMS', 'RMS_ON', 'RMS_OFF', 'RES2SUM', 'DOF',  'CHI2', 'CHI2OPP', $
	 'SIZE_FLAG', 'GROUP_FLAG', 'GAUSS_FLAG', 'CLUMP_FLAG', 'SHIFT_FIT', 'GUESS_FLAG','NCONTAM', 'STATUS',$
	 'FIT_FLAG', 'RMS_ON_FLAG', 'RMS_OFF_FLAG', $
	 'D2XDET', 'D2YDET', 'D2X45DET', 'D2Y45DET', 'D2XTHR', 'D2YTHR', 'D2X45THR',  'D2Y45THR',$
	 'D2XFITDET', 'D2YFITDET', 'D2X45FITDET',  'D2Y45FITDET', 'D2XFITBCKDET', 'D2YFITBCKDET', 'D2X45FITBCKDET', 'D2Y45FITBCKDET', $
	 'D2XFITPEAK', 'D2YFITPEAK', 'D2X45FITPEAK', 'D2Y45FITPEAK','D2XFITBCKPEAK', 'D2YFITBCKPEAK', 'D2X45FITBCKPEAK', 'D2Y45FITBCKPEAK']
	 
	col_type = [$
	'int', 'float','float',$
	'double','double','double','double',$
	'double','double',$
	'double','double',$
	'double','float','double',$
	'double','double','double','double','double', 'int', 'double', 'double', $
	'CHAR', 'LONG','int','int',  'FLOAT', 'int',  'int', 'float', 'float', 'int',  'int',$
	'double', 'double','double', 'double', 'double', 'double','double', 'double', $
	'double', 'double','double', 'double', 'double', 'double','double', 'double', $
	'double', 'double','double', 'double', 'double', 'double','double', 'double']
	
	col_unit = [' ',  'pixel', 'pixel' ,'degree' ,'degree' ,'degree' ,'degree' , 'micron' ,'MJy/sr' ,'arcsec' ,'arcsec' ,'degree ','Jy', 'Jy', 'MJy/sr', 'MJy/sr','MJy/sr', 'MJy/sr', ' ', ' ', ' ',$
	' ', ' ', ' ', ' ', ' ', 'pixel', ' ', ' ', ' ', ' ', ' ', ' ', $
	'MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2',$
	'MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2',$
	'MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2','MJy/sr/pixel^2']

	return,{str_extract_photo_oformat, c_name:col_name, c_type:col_type, c_unit:col_unit, c_len:field_len }



	
end
