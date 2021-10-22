;+
; NAME:
;    INSIDE
;
; PURPOSE:
;
;    The purpose of this function is to indicate whether a specified
;    2D point is inside (returns a 1) a specified 2D polygon or outside
;    (returns a 0).
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;    Utility.
;
; CALLING SEQUENCE:
;
;    result = INSIDE(x, y, xpts, ypts)
;
; INPUTS:
;
;    x:        A scalar or vector of the x coordinates of the 2D point(s) to check.
;    y:        A scalar or vector of the y coordinates of the 2D point(s) to check.
;    xpts:     The x coordinates of the 2D polygon.
;    ypts:     The y coordinates of the 2D polygon.
;
; OUTPUTS:
;
;    result:  A scalar or vector set to 1 if the point is inside the polygon and to
;             0 if the point is outside the polygon.
;
; KEYWORDS:
;
;    INDEX:   An output keyword. If set to a named variable, will return the indices
;             of the X and Y points that are inside the polygon.
;
; ALGORITHM:
;
;    Based on discussions on the IDL newsgroup (comp.lang.idl-pvwave) and
;    discussed here:
;
;      http://www.dfanning.com/tips/point_in_polygon.html
;
;    Primarily the work of Bård Krane and William Connelly.
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 4 September 2003.
;    Vectorized the function in accord with William Connelly's suggestions 24 July 2005. DWF.
;-
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2003-2005 Fanning Software Consulting
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
FUNCTION Inside, x, y, xpts, ypts, INDEX=index

    On_Error, 1

    sx = Size(xpts)
    sy = Size(ypts)
    IF (sx[0] EQ 1) THEN NX=sx[1] ELSE Message, 'X coordinates of polygon not a vector.'
    IF (sy[0] EQ 1) THEN NY=sy[1] ELSE Message, 'Y coordinates of polygon not a vector.'
    IF (NX EQ NY) THEN N = NX ELSE Message, 'Incompatable vector dimensions.'

    ; Close the polygon.
    tmp_xpts = [xpts, xpts[0]]
    tmp_ypts = [ypts, ypts[0]]

    ; Set up counters.
    i = indgen(N)
    ip = indgen(N)+1

    nn = N_Elements(x)
    X1 = tmp_xpts(i)  # Replicate(1,nn) - Replicate(1,n) # Reform([x],nn)
    Y1 = tmp_ypts(i)  # Replicate(1,nn) - Replicate(1,n) # Reform([y],nn)
    X2 = tmp_xpts(ip) # Replicate(1,nn) - Replicate(1,n) # Reform([x],nn)
    Y2 = tmp_ypts(ip) # Replicate(1,nn) - Replicate(1,n) # Reform([y],nn)

    ; Calculate the dot and cross products of the point to neighboring points in the polygon.
    ; Calculate the tangent of the angle between the two nearest adjacent points. If the point
    ; is outside the polygon, then summing over all possible angles will give a small number,
    ; in this case less than an arbitrary 0.1.
    dp = X1*X2 + Y1*Y2 ; Dot-product
    cp = X1*Y2 - Y1*X2 ; Cross-product
    theta = Atan(cp,dp)

    ret = Replicate(0L, N_Elements(x))
    i = Where(Abs(Total(theta,1)) GT 0.01, count)
    IF (count GT 0) THEN ret(i)=1

    ; Make this a scalar value if there is only one value.
    IF (N_Elements(ret) EQ 1) THEN ret=ret[0]

    ; If the index keyword is set, then return indices.
    IF (Arg_Present(index)) THEN ret=(Indgen(/Long, N_Elements(x)))(Where(ret eq 1))

    RETURN, ret

END