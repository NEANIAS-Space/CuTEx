;+
;Name:
;	FIT_BACKGRND
;Call:
;       Background_rate=FIT_BACKGND( t_d, rate, erate, order, trange1, trange2, sigma=sigma, $
;	selected=selected, ltime=ltime)
;
;PURPOSE:
;	This function fits the count rate, RATE, to a polynomial of order, ORDER, over
;	the background intervals specified by the limits in TRANGE1 and TRANGE2.
;	Return the value of the fit over the time range, T_D.
;Calls:
;	POLY, POLYFITW, F_DIV, CHECKVAR
;Inputs:
;
;	t_d - time array, n  or (2 x n) where n is the number of time bins.
;		if t_d is dimensioned 2xn, then the first column is the
;		start times and the second column is the end times.
;		if t_d is dimensioned n, then intervals are uniform and
;		t_d is the center of each interval. t_d is monotonic.
;
;	rate - count rate vector, n elements.
;	erate - uncertainties on rate
;	ltime - livetime of each interval in rate, used for weighting
;
;	selected - indices of selected points in t_d and rate to use in fit
;	or
;	trange1 - 2 points on the range covered by t_d.  Should be prior
;		to the event (flare).
;	trange2 - 2 points after the event along t_d.
;
;	order - polynomial to fit over the ranges specified by
;		trange1 and trange2
;If it is
;Keyword SIGMA is the average standard deviation in the fit
;
;Default ORDER =1
;Uses POLY and POLY_FIT
;History:
;RAS, 92/1/27
;17-oct-93, modified to take time arrays with start and stop edges
;21-apr-94, fixed bug in use of order (degree of fit)
;	    also allows order 0, straight average
;30-aug-95, fixed sigma calculation
;23-aug-96, fixed basic sigma calculation
;09-sep-2004, Kim.  Fixed sigma again - previously er^2*lt, now (er*lt)^2
;09-Aug-2005, Kim.  Changed keyword_set(selected) to exist(selected)
;11-Jan-2006, Kim.  Fixed bug in computing xedges (changed + to - for last point)
;-


function fit_backgrnd, t_d, rate, erate, order, trange1, trange2, sigma=sigma, $
	selected=selected, ltime=ltime

checkvar, order, 1

order_t = (size(t_d))(0) ;what are the dimensions of t_d
if order_t eq 1 then begin ; transform to edges
	ntd = n_elements( t_d) -1
	xedges = [1.5* t_d(0)-.5*t_d(1), ( t_d(1:*)+t_d) / 2.,  $
	       1.5*t_d(ntd) - 0.5*t_d(ntd-1) ]
endif else xedges = [ (t_d(0,*))(*), t_d(1, n_elements(t_d(1,*))-1)]

if not exist(selected) then begin
;*******************
;TRANSFORM THE TIME RANGES INTO INDEX RANGES
	t1 = trange1(sort(trange1))
	t2 = trange2(sort(trange2))

	n1s = ((where( xedges ge t1(0), nx))(0) -1)>0
	n1e = ((where( xedges ge t1(1), nx))(0) -1)>0
	nrange1 = indgen((n1e-n1s)>1) + n1s

	n2s = ((where( xedges ge t2(0), nx))(0) -1)>0
	n2e = ((where( xedges ge t2(1), nx))(0) -1)>0
	nrange2 = indgen((n2e-n2s)>1) + n2s

	r = [nrange1, nrange2]
;*******************
endif else r = selected

xm = .5* (xedges + xedges(1:*))

weight = fcheck( ltime, xm*0.0+1.)
if order eq 0 then begin
	back  = total(rate(r)*ltime(r))/total(ltime(r)) + xm*0.
	yband = sqrt( total((back(0)-rate(r))^2)/n_elements(r) ) + xm*0
	endif else begin
	coeff= polyfitw(xm(r),rate(r),weight(r), order > 1, yfit, yband)
        back = poly( xm, coeff)
	endelse

;sigma =   f_div(sqrt(total( erate(r)*ltime(r))),total(ltime(r))) ;old bad calculation
sigma =   f_div( sqrt(total( (erate(r)*ltime(r))^2 ) ), total(ltime(r)) )

;
;	guard against degenerate ybands emerging from polyfitw when there
;	are no degrees of freedom.
;
if n_elements(r) gt ((order>1)+1) then sigma = sigma + avg(yband)

return, back

end

