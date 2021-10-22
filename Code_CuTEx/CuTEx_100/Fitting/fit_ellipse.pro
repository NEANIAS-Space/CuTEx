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
; Copyright © 2002 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


FUNCTION Fit_Ellipse, indices, XSize=xsize, YSize=ysize, NPoints=npoints, $
   Axes=axes, SemiAxes=semiAxes, Orientation=orientation, Center=center

      ; The following method determines the "mass density" of the ROI and fits
      ; an ellipse to it. This is used to calculate the major and minor axes of
      ; the ellipse, as well as its orientation. The orientation is calculated in
      ; degrees counter-clockwise from the X axis.

IF N_Elements(xsize) EQ 0 THEN xsize = !D.X_Size
IF N_Elements(ysize) EQ 0 THEN ysize = !D.Y_Size
IF N_Elements(npoints) EQ 0 THEN npoints = 120

   ; Fake indices for testing purposes.

IF N_Elements(indices) EQ 0 THEN BEGIN
   xs = xsize / 4
   xf = xsize / 4 * 2
   ys = ysize / 4
   yf = ysize / 4 * 2
   array = BytArr(xsize, ysize)
   array[xs:xf, ys:yf] = 255B
   indices = Where(array EQ 255)
ENDIF

array = BytArr(xsize, ysize)
array[indices] = 255B
totalMass = Total(array)
xcm = Total( Total(array, 2) * Indgen(xsize) ) / totalMass
ycm = Total( Total(array, 1) * Indgen(ysize) ) / totalMass
center = [xcm, ycm]

   ; Obtain the position of every pixel in the image, with the origin
   ; at the center of mass of the ROI.

x = Findgen(xsize)
y = Findgen(ysize)
xx = (x # (y * 0 + 1)) - xcm
yy = ((x * 0 + 1) # y) - ycm
npts = N_Elements(indices)

   ; Calculate the mass distribution tensor.

i11 = Total(yy[indices]^2) / npts
i22 = Total(xx[indices]^2) / npts
i12 = -Total(xx[indices] * yy[indices]) / npts
tensor = [[ i11, i12],[i12,i22]]

   ; Find the eigenvalues and eigenvectors of the tensor.

evals = Eigenql(tensor, Eigenvectors=evecs)

   ; The semi-major and semi-minor axes of the ellipse are obtained from the eigenvalues.

semimajor = Sqrt(evals[0]) * 2.0
semiminor = Sqrt(evals[1]) * 2.0

   ; We want the actual axes lengths.

major = semimajor * 2.0
minor = semiminor * 2.0
semiAxes = [semimajor, semiminor]
axes = [major, minor]

   ; The orientation of the ellipse is obtained from the first eigenvector.

evec = evecs[*,0]

   ; Degrees counter-clockwise from the X axis.

orientation = ATAN(evec[1], evec[0]) * 180. / !Pi - 90.0

   ; Divide a circle into Npoints.

phi = 2 * !PI * (Findgen(npoints) / (npoints-1))

   ; Position angle in radians.

ang = orientation / !RADEG

   ; Sin and cos of angle.

cosang = Cos(ang)
sinang = Sin(ang)

   ; Parameterized equation of ellipse.

x =  semimajor * Cos(phi)
y =  semiminor * Sin(phi)

 ; Rotate to desired position angle.

xprime = xcm + (x * cosang) - (y * sinang)
yprime = ycm + (x * sinang) + (y * cosang)

   ; Extract the points to return.

pts = FltArr(2, N_Elements(xprime))
pts[0,*] = xprime
pts[1,*] = yprime

RETURN, pts
END