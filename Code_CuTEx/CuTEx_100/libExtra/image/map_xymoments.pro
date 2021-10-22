;+
; PROJECT:
;	SSW
; NAME:
;	MAP_XYMOMENTS
;
; PURPOSE:
;	Computes the centroid and standard deviation
;	 along x and y (rows and columns) of a map distribution.
;
; CATEGORY:
;	Math, Util
;
; CALLING SEQUENCE:
;	map_xymoments, map, xaxis, yaxis, centroid, stdev

;
; CALLS:
;	none
;
; INPUTS:
;       Map- 2d array of map intensities
;		Xaxis - positions of pixels along first dimension of Map
;		Yaxis - positions of pixels along second dimension of Map
;		;
;
; OPTIONAL INPUTS:
;	none
;
; OUTPUTS:
;       Centroid - returns 2 element array of centroid along x and y
;			in the coordinates of xaxis and yaxis
;		Stdev   - returns 2 element array of standard deviation along
;			x and y in the coordinates of xaxis and yaxis.
;
; OPTIONAL OUTPUTS:
;	none
;
; KEYWORDS:
;	none
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;	Straighforward application of the definitions of mean and standard
;		deviations of the moments of a distribution
;
; MODIFICATION HISTORY:
;	Version 1, richard.schwartz@gsfc.nasa.gov,
;
;-


pro map_xymoments, map, xaxis, yaxis, centroid, stdev


xcentroid =  total( map ## transpose(xaxis )) / total( map)

ycentroid =  total( map # yaxis ) / total( map )


centroid = [xcentroid, ycentroid]

xstdev = sqrt( (total( map ## transpose(xaxis^2 )) / total( map) - xcentroid^2) > 0)

ystdev = sqrt( (total( map # yaxis^2 ) / total( map) - ycentroid^2) > 0)

stdev = [xstdev, ystdev]
end