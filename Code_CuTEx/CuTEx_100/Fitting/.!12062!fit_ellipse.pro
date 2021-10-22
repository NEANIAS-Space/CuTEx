;+
; NAME:
;       Fit_Ellipse
;
; PURPOSE:
;
;       This program fits an ellipse to an ROI given by a vector of ROI indices.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics, math.
;
; CALLING SEQUENCE:
;
;       ellipsePts = Fit_Ellipse(indices)
;
; OPTIONAL INPUTS:
;
;       indices - A 1D vector of pixel indices that describe the ROI. For example,
;            the indices may be returned as a result of the WHERE function.
;
; OUTPUTS:
;
;       ellipsePts - A 2-by-npoints array of the X and Y points that describe the
;            fitted ellipse. The points are in the device coodinate system.
;
; INPUT KEYWORDS:
;
;       NPOINTS - The number of points in the fitted ellipse. Set to 120 by default.
;
;       XSIZE - The X size of the window or array from which the ROI indices are taken.
;            Set to !D.X_Size by default.
;
;       YSIZE - The Y size of the window or array from which the ROI indices are taken.
;            Set to !D.Y_Size by default.
;
; OUTPUT KEYWORDS:
;
;       CENTER -- Set to a named variable that contains the X and Y location of the center
;            of the fitted ellipse in device coordinates.
;
;       ORIENTATION - Set to a named variable that contains the orientation of the major
;            axis of the fitted ellipse. The direction is calculated in degrees
;            counter-clockwise from the X axis.
;
;       AXES - A two element array that contains the length of the major and minor
;            axes of the fitted ellipse, respectively.
;
;       SEMIAXES - A two element array that contains the length of the semi-major and semi-minor
;            axes of the fitted ellipse, respectively.
;
;  EXAMPLE:
;
;       LoadCT, 0, /Silent
;       image = BytArr(400, 300)+125
;       image[125:175, 180:245] = 255B
;       indices = Where(image EQ 255)
;       Window, XSize=400, YSize=300
;       TV, image
;       PLOTS, Fit_Ellipse(indices, XSize=400, YSize=300), /Device, Color=FSC_Color('red')
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, April 2002. Based on algorithms provided by Craig Markwardt
;            and Wayne Landsman in his TVEllipse program.
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
