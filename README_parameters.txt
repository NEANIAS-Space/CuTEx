# Parameter file for the Detection and Extraction of CuTEx
IMAGE = 'PARAM__INPUT_FILE'											###  Filename of the INPUT image in FITS format 
# DETECTION PARAMETERS	
THRESHOLD = PARAM__THRESHOLD										###  Threshold Level adopted to identify sources
#
SMOOTH = 0															###  Boolean Variable (0 -1) to apply a smoothing filter to the input image
NPIXMASK = PARAM__NPIXMASK # default: 4								###  Minimum number of pixels for a cluster to be significant
PSFPIX = PARAM__PSFPIX												###	 Number of pixels that sample the instrumental PSF on the input image
DERIVATE = 0														###  Boolean switch (0 -1) for Derivate computing method. 0) (Default) Refined 5-points derivate, 1) General 3-points Derivate; 
ALL_NEIGHBOURS = 0													###  Considers as nearest neighbouring pixels also the diagonal ones. Increases the running time if switched on
THRESH = 0															###	 (Default not set) Define the criteria to detect multiple sources in a large pixel clusters. Internal Threshold on curvature values over the pixels of a single cluster.
RANGE = 0															###  Maximum distance in pixels from the curvature peak adopted to determine the first minima. Used to estimate the size of the candidate sources. (Default internally set: 8)
SUPER_RESOLUTION = 0												###  (OBSOLETE) For clusters of sources. Checks curvature statistics to define how many sources there are on derivative maps in single directions instead of using the mean derivative.
ABSCURV = 0															###	 Boolean variable (0 - 1) for value of thresholding in curvature. 0) (Default) the threshold is in unit of standard deviation, 1) (not recommended) threshold is set in Absolute Curvature Values
LOCAL_THRESH = 1													###  Boolean variable (0 - 1) one methods to compute the comparison threshold level. If active (suggested) the standard deviation is computed locally on derivative maps as the median deviation form the median (MAD) in boxes of 61x61 pixels, and is applied to boxes of 31x31 over the entire map 
FACT_MASK = 0														###  Factor to assign the masked region ascribed to sources that is adopted for the fitting. (Default = 2) 
#
# EXTRACTION PARAMETERS
#
MARGIN = 0															###  Cutoff in pixels avoiding to extract sources detected too close to the image margin
MAX_DIST_FAC = 0													###  Length adopted to group sources together for simultaneous fitting (if adaptive_group = 0 it is in units of PSF, on the contrary it is in units of estimated source FWHMs)
PSFLIM = PARAM__PSFLIM # default: [0.5,2.0]							###  Defines the interval adopted for fitting the source size. Two-values array with lower and upper limiting variation with respect to the initial source size (i.e. [0.7,1.3] means +/- 30%  Guessed Size )
ADAPTIVE_GROUP = 0													###  (Default 0) Change units adopted to create group of sources. 0) (Default) Length is expressed in terms of PSF, 1) it is expressed in terms of Guessed Source Size
CLOSEST_NEIGH = 0													###  (Default 0) Boolean Variable to determine which measurements keep when multiple sources are fitted simultaneously. 0) (Default) keep the parameters for all the sources, 1) Cicle over each source and repeat the fitting procedure, and keep the output values only for current source (Very slow process)
ADAPTIVE_WINDOW = 0													###  (Default 0) Boolean Variable to change units in defining the size of the subframe adopted to fit the source. 0) The size of the window is determined in terms of PSF, 1) it is derived from the initial guessed size of the source. 
DMAX_FACTOR = 0														###  Size of the fitting window extracted to evaluate source fluxes (if adaptive_window is on it is in units of PSF, on the contrary it is in units of estimated source FWHMs)
SMOOTHING = 0														###  Width (in pixels) of boxcar smoothing window to be applied before the extractiones
POSBACK = 0															###  Forces the fitted background to be positive (Default on)
CORREL = 0															###  (Default on): Assume that that peak flux and size of fitted sources are correlated in error calculation
WEIGHT = 0															###  (Default off): Assign larger weights to source pixels with respect to the pixels assigned to the background.
NOFORCEFIT = 0														###  (Default off): Leave free the interval of parameters for sources for which it was not possible to determine a proper guessed size
BACKGFIT = 0														###  (Default 0): Boolean variable setting the polynomial shape of background adopted for fitting. 0) Simple plane with inclination (A*x + B*y + C), 1) Second order polynomial function with mixed terms (6 - variable function)
CENTERING = 0														###  (Default off): Boolean variable. If set on, it centers the subframe on the baricentre of the group of sources that is being fitted, on the contrary it centers the subframe on individual sources when multiple objects are fitted 
PSFPIX = PARAM__PSFPIX												###  Size of the PSF in pixels sampled on the data.
PEAK_SHIFT = 0														###  Interval in pixels to allow the maximum shift of source centre during the fit.
