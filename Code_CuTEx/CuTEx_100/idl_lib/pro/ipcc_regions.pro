;+
; NAME:
;	IPCC_REGIONS
;
; PURPOSE:
;	This function returns the coordinates of the boxes defining the 
;	requested geographical regions selected for use in the IPCC AR4 of WG1.
;
; CATEGORY:
;	Geographical
;
; CALLING SEQUENCE:
;	Result = ipcc_regions( Shortname )
;
; INPUTS:
;	Shortname:  A vector or scalar of strings containing the abbreviated 
;		three letter names of the regions to request.  Of length NREG.
;
; KEYWORD PARAMETERS:
;	LAND:  Returns a binary vector or scalar containing 1 if the region 
;		is specifically for land only, 0 otherwise.
;	NAME:  Returns a vector or scalar of strings containing the full names 
;		of the requested regions.  Of length NREG.
;	OCEAN:  Returns a binary vector or scalar containing 1 if the region 
;		is specifically for ocean/sea only, 0 otherwise.
;
; OUTPUTS:
;	Returns:  Returns an array of size 4*NREG containing the geographical 
;		boundaries of the boxes defining the requested regions.  The 
;		elements [*,I] contain the [ westernmost longitude, $
;		southernmost latitude, easternmost longitude, $
;		northernmost latitude ] boundaries for requested region I.
;	LAND, NAME, OCEAN
;
; USES:
;	-
;
; PROCEDURE:
;	This function selects from the region information provided by David 
;	Stainforth and David Sexton.
;
; EXAMPLE:
;	Enter:
;	  result = ipcc_regions( 'SSA', name=name )
;	This should return:
;	  result = [-75,-55,-40,-20]
;	  name = 'Southern South America'
;
; REFERENCES:
;	Giorgi, F.  2002.  Variability and trends of sub-continental scale 
;		surface climate in the twentieth century. Part I: observations.
;		Climate Dynamics, 18, 675-691.
;	Giorgi, F., and R. Francisco. 2000.  Uncertainties in regional climate 
;		change prediction: a regional analysis of ensemble simulations 
;		with the HADCM2 coupled AOGCM.  Climate Dynamics, 16, 169-182.
;	Ruosteenoja, K., T. R. Carter, K. Jylha, and H. Tuomenvirta.  2003.  
;		Future climate in world regions: an intercomparison of 
;		model-based projections for the new IPCC emissions scenarios.  
;		The Finnish Environment, 644, Finnish Environment Institute, 
;		83 pp.
;
; MODIFICATION HISTORY:
;	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2005-09-09
;	Modified:	DAS, 2006-01-16 (Converted from Giorgi regions to IPCC 
;			AR4 WG1 regions;  added global regions;  changed name 
;			from giorgi_regions.pro)
;	Modified:	DAS, 2006-04-26 (added NAT,SAT regions)
;	Modified:	DAS, 2006-08-02 (switched to revised IPCC AR4 WG1 
;			abbreviated names: SEU-SEM, AR3-ARC, and AN2-ANT)
;	Modified:	DAS, 2006-10-18	(added some region definitions used by 
;			climateprediction.net)
;	Modified:	DAS, 2007-01-04 (added hemispheric regions NHS and SHS)
;	Modified:	DAS, 2007-07-05 (added cpdn region CGI2)
;	Modified:	DAS, 2008-08-04 (duplicated NAT, SAT with NAC, SAC)
;	Modified:	DAS, 2009-09-30 (added SADC region)
;-

FUNCTION IPCC_REGIONS, $
	Shortname, $
	LAND=landopt, OCEAN=oceanopt, $
	NAME=name

;***********************************************************************
; Constants

; SHORTNAMELIST is a list of region abbreviations.
; BOUNDARYLIST is a list of the boundaries of the regions, in the same order 
; as in SHORTNAMELIST, in form [W,S,E,N]-ernmost boundaries.
; NAMELIST is a list of the names of the regions, in the same order as in 
; SHORTNAMELIST.
; LANDLIST is a list of whether the region is for land only, in the same order 
; as in SHORTNAMELIST.
; OCEANLIST is a list of whether the region is for ocean/sea only, in the same 
; order as in SHORTNAMELIST.

; Add the Arctic land region
shortnamelist = [ 'ARL' ]
boundarylist = [ -180, 67.5, 179.99, 90 ]
namelist = [ 'Arctic land' ]
landlist = [ 1 ]
oceanlist = [ 0 ]
; Add the Arctic Ocean region
shortnamelist = [ shortnamelist, 'ARO' ]
boundarylist = [ [ boundarylist ], [ -180, 67.5, 179.99, 90 ] ]
namelist = [ namelist, 'Arctic Ocean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Antarctic land region
shortnamelist = [ shortnamelist, 'ANL' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, -55 ] ]
namelist = [ namelist, 'Antarctic land' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Antarctic Ocean region
shortnamelist = [ shortnamelist, 'ANO' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, -55 ] ]
namelist = [ namelist, 'Antarctic Ocean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Alaska, NW Canada region
shortnamelist = [ shortnamelist, 'ALA' ]
boundarylist = [ [ boundarylist ], [ -170, 60, -103, 72 ] ]
namelist = [ namelist, 'Alaska, Northwestern Canada' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the E Canada, etc. region
shortnamelist = [ shortnamelist, 'CGI' ]
boundarylist = [ [ boundarylist ], [ -103, 50, -10, 85 ] ]
namelist = [ namelist, 'Eastern Canada, etc.' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Western North America region
shortnamelist = [ shortnamelist, 'WNA' ]
boundarylist = [ [ boundarylist ], [ -130, 30, -103, 60 ] ]
namelist = [ namelist, 'Western North America' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Central North America region
shortnamelist = [ shortnamelist, 'CNA' ]
boundarylist = [ [ boundarylist ], [ -103, 30, -85, 50 ] ]
namelist = [ namelist, 'Central North America' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Eastern North America region
shortnamelist = [ shortnamelist, 'ENA' ]
boundarylist = [ [ boundarylist ], [ -85, 25, -50, 50 ] ]
namelist = [ namelist, 'Eastern North America' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Central America region
shortnamelist = [ shortnamelist, 'CAM' ]
boundarylist = [ [ boundarylist ], [ -116, 10, -83, 30 ] ]
namelist = [ namelist, 'Central America' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Amazonia region
shortnamelist = [ shortnamelist, 'AMZ' ]
boundarylist = [ [ boundarylist ], [ -82, -20, -34, 12 ] ]
namelist = [ namelist, 'Amazonia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southern South America region
shortnamelist = [ shortnamelist, 'SSA' ]
boundarylist = [ [ boundarylist ], [ -76, -56, -40, -20 ] ]
namelist = [ namelist, 'Southern South America' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the North Europe region
shortnamelist = [ shortnamelist, 'NEU' ]
boundarylist = [ [ boundarylist ], [ -10, 48, 40, 75 ] ]
namelist = [ namelist, 'Northern Europe' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southern Europe, Northern Africa region
shortnamelist = [ shortnamelist, 'SEM' ]
boundarylist = [ [ boundarylist ], [ -10, 30, 40, 48 ] ]
namelist = [ namelist, 'Southern Europe, Northern Africa' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Sahara region
shortnamelist = [ shortnamelist, 'SAH' ]
boundarylist = [ [ boundarylist ], [ -20, 18, 65, 30 ] ]
namelist = [ namelist, 'Sahara' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Western Africa region
shortnamelist = [ shortnamelist, 'WAF' ]
boundarylist = [ [ boundarylist ], [ -20, -12, 22, 18 ] ]
namelist = [ namelist, 'Western Africa' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Eastern Africa region
shortnamelist = [ shortnamelist, 'EAF' ]
boundarylist = [ [ boundarylist ], [ 22, -12, 52, 18 ] ]
namelist = [ namelist, 'Eastern Africa' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southern Africa region
shortnamelist = [ shortnamelist, 'SAF' ]
boundarylist = [ [ boundarylist ], [ 10, -35, 52, -12 ] ]
namelist = [ namelist, 'Southern Africa' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Northern Asia region
shortnamelist = [ shortnamelist, 'NAS' ]
boundarylist = [ [ boundarylist ], [ 40, 50, 180, 70 ] ]
namelist = [ namelist, 'Northern Asia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Central Asia region
shortnamelist = [ shortnamelist, 'CAS' ]
boundarylist = [ [ boundarylist ], [ 40, 30, 75, 50 ] ]
namelist = [ namelist, 'Central Asia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Tibetan Plateau region
shortnamelist = [ shortnamelist, 'TIB' ]
boundarylist = [ [ boundarylist ], [ 75, 30, 100, 50 ] ]
namelist = [ namelist, 'Tibetan Plateau' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Eastern Asia region
shortnamelist = [ shortnamelist, 'EAS' ]
boundarylist = [ [ boundarylist ], [ 100, 20, 145, 50 ] ]
namelist = [ namelist, 'Eastern Asia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southern Asia region
shortnamelist = [ shortnamelist, 'SAS' ]
boundarylist = [ [ boundarylist ], [ 65, 5, 100, 30 ] ]
namelist = [ namelist, 'Southern Asia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southeast Asia region
shortnamelist = [ shortnamelist, 'SEA' ]
boundarylist = [ [ boundarylist ], [ 95, -11, 155, 20 ] ]
namelist = [ namelist, 'Southeast Asia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Northern Australia region
shortnamelist = [ shortnamelist, 'NAU' ]
boundarylist = [ [ boundarylist ], [ 110, -30, 155, -11 ] ]
namelist = [ namelist, 'Northern Australia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southern Australia region
shortnamelist = [ shortnamelist, 'SAU' ]
boundarylist = [ [ boundarylist ], [ 110, -45, 155, -30 ] ]
namelist = [ namelist, 'Southern Australia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Mediterranean region
shortnamelist = [ shortnamelist, 'MED' ]
boundarylist = [ [ boundarylist ], [ -5, 30, 35, 45 ] ]
namelist = [ namelist, 'Mediterranean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Caribbean region
shortnamelist = [ shortnamelist, 'CAR' ]
boundarylist = [ [ boundarylist ], [ -85, 10, -60, 25 ] ]
namelist = [ namelist, 'Caribbean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Tropical Northeast Atlantic region
shortnamelist = [ shortnamelist, 'TNE' ]
boundarylist = [ [ boundarylist ], [ -30, 0, -10, 40 ] ]
namelist = [ namelist, 'Tropical Northeast Atlantic Ocean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Northern Pacific region
shortnamelist = [ shortnamelist, 'NPA' ]
boundarylist = [ [ boundarylist ], [ 150, 0, -120, 40 ] ]
namelist = [ namelist, 'Northern Pacific' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Indian Ocean region
shortnamelist = [ shortnamelist, 'IND' ]
boundarylist = [ [ boundarylist ], [ 50, -35, 100, 17.5 ] ]
namelist = [ namelist, 'Indian Ocean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Southern Pacific region
shortnamelist = [ shortnamelist, 'SPA' ]
boundarylist = [ [ boundarylist ], [ 150, -55, -80, 0 ] ]
namelist = [ namelist, 'Southern Pacific' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Southwestern Australia region
shortnamelist = [ shortnamelist, 'SWA' ]
boundarylist = [ [ boundarylist ], [ 110, -38, 125, -28 ] ]
namelist = [ namelist, 'Southwestern Australia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Sahel region
shortnamelist = [ shortnamelist, 'SHL' ]
boundarylist = [ [ boundarylist ], [ -20, 10, 22, 18 ] ]
namelist = [ namelist, 'Sahel' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Guinea coast, etc. region
shortnamelist = [ shortnamelist, 'GUI' ]
boundarylist = [ [ boundarylist ], [ -20, 0, 10, 10 ] ]
namelist = [ namelist, 'Guinea coast, etc.' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Southern Andes region
shortnamelist = [ shortnamelist, 'SAN' ]
boundarylist = [ [ boundarylist ], [ -75, -45, -68, -30 ] ]
namelist = [ namelist, 'Southern Andes' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Arctic(2) land region
shortnamelist = [ shortnamelist, 'AR2' ]
boundarylist = [ [ boundarylist ], [ -180, 60, 179.99, 90 ] ]
namelist = [ namelist, 'Arctic(2) land' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Arctic(2) land and ocean region
shortnamelist = [ shortnamelist, 'ARC' ]
boundarylist = [ [ boundarylist ], [ -180, 60, 179.99, 90 ] ]
namelist = [ namelist, 'Arctic land and ocean' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add the Antarctic(2) land region
shortnamelist = [ shortnamelist, 'ANT' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, -60 ] ]
namelist = [ namelist, 'Antarctic land south of 60S' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add the Antarctic(2) land and ocean region
shortnamelist = [ shortnamelist, 'AN3' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, -60 ] ]
namelist = [ namelist, 'Antarctic(2) land and ocean' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]

; Some of my own regions.
; Add the Northern Atlantic region
shortnamelist = [ shortnamelist, 'NAT' ]
boundarylist = [ [ boundarylist ], [ -60, 0, -10, 60 ] ]
namelist = [ namelist, 'Northern Atlantic' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
shortnamelist = [ shortnamelist, 'NAC' ]
boundarylist = [ [ boundarylist ], [ -60, 0, -10, 60 ] ]
namelist = [ namelist, 'Northern Atlantic' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Add the Southern Atlantic region
shortnamelist = [ shortnamelist, 'SAT' ]
boundarylist = [ [ boundarylist ], [ -40, -40, 10, 0 ] ]
namelist = [ namelist, 'Southern Atlantic' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
shortnamelist = [ shortnamelist, 'SAC' ]
boundarylist = [ [ boundarylist ], [ -40, -40, 10, 0 ] ]
namelist = [ namelist, 'Southern Atlantic' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]

; Global and hemispheric regions.
; Specify global as a "region"
shortnamelist = [ shortnamelist, 'GLO' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, 90 ] ]
namelist = [ namelist, 'Global' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Specify global land as a "region"
shortnamelist = [ shortnamelist, 'LAN' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, 90 ] ]
namelist = [ namelist, 'Global land' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Specify global ocean as a "region"
shortnamelist = [ shortnamelist, 'OCE' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, 90 ] ]
namelist = [ namelist, 'Global ocean' ]
landlist = [ landlist, 0 ]
oceanlist = [ oceanlist, 1 ]
; Specify Northern Hemisphere as a "region"
shortnamelist = [ shortnamelist, 'NHS' ]
boundarylist = [ [ boundarylist ], [ -180, 0, 179.99, 90 ] ]
namelist = [ namelist, 'Northern Hemisphere' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Specify Southern Hemisphere as a "region"
shortnamelist = [ shortnamelist, 'SHS' ]
boundarylist = [ [ boundarylist ], [ -180, -90, 179.99, 0 ] ]
namelist = [ namelist, 'Southern Hemisphere' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]

; Some regions used by the climateprediction.net experiment.
; Add the E Canada, etc. #2 region (does not extend as far north as CGI)
shortnamelist = [ shortnamelist, 'CGI2' ]
boundarylist = [ [ boundarylist ], [ -103, 50, -10, 75 ] ]
namelist = [ namelist, 'Eastern Canada, etc.' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]
; Add Nino 3.4 region
shortnamelist = [ shortnamelist, 'N34' ]
boundarylist = [ [ boundarylist ], [ -170., -5, -120, 5 ] ]
namelist = [ namelist, 'Nino 3.4' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Aleutian Low region
shortnamelist = [ shortnamelist, 'ALL' ]
boundarylist = [ [ boundarylist ], [ 160., 40, -160, 60 ] ]
namelist = [ namelist, 'Aleutian Low' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add New Zealand region
shortnamelist = [ shortnamelist, 'NZ' ]
boundarylist = [ [ boundarylist ], [ 165., -47, 179, -34 ] ]
namelist = [ namelist, 'New Zealand' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - Highlands region
shortnamelist = [ shortnamelist, 'UKHL' ]
boundarylist = [ [ boundarylist ], [ -5.625, 56.25, -1.875, 58.75 ] ]
namelist = [ namelist, 'UK - Highlands' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - Northern Ireland region
shortnamelist = [ shortnamelist, 'UKNI' ]
boundarylist = [ [ boundarylist ], [ -9.375, 53.75, -5.625, 56.25 ] ]
namelist = [ namelist, 'UK - Northern Ireland' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - Borders region
shortnamelist = [ shortnamelist, 'UKNE' ]
boundarylist = [ [ boundarylist ], [ -5.625, 53.75, -1.875, 56.25 ] ]
namelist = [ namelist, 'UK - Borders' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Ireland region
shortnamelist = [ shortnamelist, 'EIRE' ]
boundarylist = [ [ boundarylist ], [ -9.375, 51.25, -5.625, 53.75 ] ]
namelist = [ namelist, 'Ireland' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - Wales & Midlands region
shortnamelist = [ shortnamelist, 'UKWM' ]
boundarylist = [ [ boundarylist ], [ -5.625, 51.25, -1.875, 53.75 ] ]
namelist = [ namelist, 'UK - Wales & Midlands' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - London & East Anglia region
shortnamelist = [ shortnamelist, 'UKLA' ]
boundarylist = [ [ boundarylist ], [ -1.875, 51.25, 1.875, 53.75 ] ]
namelist = [ namelist, 'UK - London & East Anglia' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - Cornwall region
shortnamelist = [ shortnamelist, 'UKCW' ]
boundarylist = [ [ boundarylist ], [ -5.625, 48.75, -1.875, 51.25 ] ]
namelist = [ namelist, 'UK - Cornwall' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add UK - Kent region
shortnamelist = [ shortnamelist, 'UKKT' ]
boundarylist = [ [ boundarylist ], [ -1.875, 48.75, 1.875, 51.25 ] ]
namelist = [ namelist, 'UK - Kent' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Cell 38N29W region
shortnamelist = [ shortnamelist, 'C38N29W' ]
boundarylist = [ [ boundarylist ], [ -31.875, 36.25, -28.125, 38.75 ] ]
namelist = [ namelist, 'Cell 38N29W' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Cell 64N22W region
shortnamelist = [ shortnamelist, 'C64N22W' ]
boundarylist = [ [ boundarylist ], [ -24.375, 63.75, -20.625, 66.25 ] ]
namelist = [ namelist, 'Cell 64N22W' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Cell 12S131E region
shortnamelist = [ shortnamelist, 'C12S131E' ]
boundarylist = [ [ boundarylist ], [ 129.375, -13.75, 133.125, -11.25 ] ]
namelist = [ namelist, 'Cell 12S131E' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Cell 18S150W region
shortnamelist = [ shortnamelist, 'C18S150W' ]
boundarylist = [ [ boundarylist ], [ -151.875, -18.75, -148.125, -16.25 ] ]
namelist = [ namelist, 'Cell 18S150W' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]
; Add Greenland region
shortnamelist = [ shortnamelist, 'GRNL' ]
boundarylist = [ [ boundarylist ], [ -105., 50, -10, 85 ] ]
namelist = [ namelist, 'Greenland' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 1 ]

; Some extra regions
; And Southern African Development Community region
shortnamelist = [ shortnamelist, 'SADC' ]
boundarylist = [ [ boundarylist ], [ 10., -36., 65., 7. ] ]
namelist = [ namelist, 'Southern African Development Community' ]
landlist = [ landlist, 1 ]
oceanlist = [ oceanlist, 0 ]

;***********************************************************************
; Select the Boundaries of the Requested Regions

; Initialise outputs
nreg = n_elements( shortname )
result = fltarr( 4, nreg )
name = strarr( nreg )
landopt = intarr( nreg )
oceanopt = intarr( nreg )

; Iterate through select regions
for i = 0, nreg - 1 do begin
  ; Find selected region
  id = where( strupcase( shortname[i] ) eq shortnamelist, nid )
  if nid ne 1 then stop
  id = id[0]
  ; Record information on selected region
  result[*,i] = boundarylist[*,id]
  name[i] = namelist[id]
  landopt[i] = landlist[id]
  oceanopt[i] = oceanlist[id]
endfor

;***********************************************************************
; The End

return, result
END
