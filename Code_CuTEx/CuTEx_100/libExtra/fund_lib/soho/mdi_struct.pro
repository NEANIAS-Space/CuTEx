;+
;
; MDI_STRUCT
;
; Return a structure containing a minimal subset of
; MDI FITS keywords
;
; Craig DeForest, 28-Apr-1997
; Sam Freeland, add some soi pointing info
; Zarro (EITI/GSFC), added REFTIME to support summary FITS files (28-Mar-01)
;-

function mdi_struct
return,{         SIMPLE:byte(0)	$
		,BITPIX:0       $
                ,BLANK:0        $
                ,BSCALE:0.0d0   $
                ,BUNIT:""       $
                ,BZERO:0.0d0    $
                ,NAXIS:0        $
                ,NAXIS1:0L      $
                ,NAXIS2:0L      $
                ,B0:0.0d0       $
                ,L0:0.0d0       $
                ,P_ANGLE:0.0d0  $
                ,TIME_OBS:""    $
                ,DATE_OBS:""    $
                ,REFTIME:""     $
                ,TIME:0L        $
                ,DAY:0          $ 
                ,OBS_MODE:""    $
                ,OBS_TYPE:""    $
                ,OBT_TIME:""    $
                ,R_SUN:0.0d0    $
                ,SOLAR_B0:0.0d0 $
                ,SOLAR_P0:0.0d0 $
                ,RADIUS:0.0     $
                ,SOLAR_R:0.0    $
                ,X0:0.0d0       $
                ,Y0:0.0d0       $
                ,DATE:""        $
                ,INSTRUME:""    $
                ,ORIGIN:""      $
		,OBJECT:""      $
                ,TELESCOP:""    $
                ,T_EARTH:""     $
                ,DPC:""         $
                ,DSNAME:""      $
                ,MISSVALS:0     $
                ,RUNTIME:""     $
                ,SQID:""        $
                ,T_REF:""       $
                ,CTYPE1:""      $
                ,CTYPE2:""      $
                ,CDELT1:0.0d0   $
                ,CDELT2:0.0d0   $
                ,CROTA:0.0d0    $
                ,CRPIX1:0.0d0   $
                ,CRPIX2:0.0d0   $
                ,XSCALE:0.0d0   $
                ,YSCALE:0.0d0   $
                ,CENTER_X:0.0d0 $
                ,CENTER_Y:0.0d0 $ 
		,COMMENT:['','','','','','','','','','']$
		,HISTORY:['','','','','','','','','','']$
        }
end
