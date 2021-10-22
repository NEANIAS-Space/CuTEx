;+
; Name: mark_intervals
;
; Purpose: Mark events on a time plot
;
; Calling sequence:
;	mark_intervals, stimes, etimes, label=label, n_bar=n_bar
;
; Method:
;	For each start/end interval a thick colored bar is drawn at the top of the plot
;	for the duration of the interval.  If start/end is -1, then the bar extends for
;	the entire x range.  The label is placed to the right or left of the line depending
;	on where the line is on the plot.  n_bar is both an input and output keyword.
;	It keeps track of how many bars are already drawn to decide how far down from
;	the top (or bottom) of the plot to draw the next bar.
;
; Input arguments:
;	stimes,etimes - start, end times of intervals.  If start[i]=-1 then the condition
;		applies to the entire plot interval
;
; Input keyword arguments:
;	label - very short (like 1 or 2 letters) string to identify bar (like N for night)
;	color - color of bar
;	charsize - character size for label
;	n_bar - number of bars drawn on plot so far (also an output parameter after being
;		incremented)
;	longline - if set, draw lines the full y width of plot in addition to drawing bars
;	bottom - if set, draw bars at bottom of plot (top is default)
;
; Output keyword arguments:
;	n_bar - (also input).  n_bar is incremented and returned for potential use in
;		another call to mark_intervals.  Total number of bars drawn on plot so far.
;
; Modifications:
; Written: 4-Mar-2002, Kim Tolbert.
;   15-May-2002 - Kim, for log plots, check that ycrange is OK (for GOES plots, where !y.crange is
;       not returned as log of limits, but the limits themselves)
;	27-May-2002 - Kim, show label only on first interval when multiple intervals on same level
;	20-Sep-2002 - Kim. Save !p.nsum, set to 1, and restore original value on exit.
;	19-Jan-2005 - Kim. Thick=2.6 isn't thick when drawing to the Z buffer in 6.1 (??), so
;		make it thicker in that case
;
;-
pro mark_intervals, stimes, etimes, label=label, color=color, $
	charsize=charsize, n_bar=n_bar, longline=longline, bottom=bottom

checkvar, label, ''
checkvar, n_bar, 0

save_nsum = !p.nsum
!p.nsum=1

ycrange = crange('y')  ; take care of y log
if !y.type then begin
	ratio = abs(f_div(ycrange[1],ycrange[0]))
	if ratio gt 0. and ratio lt 2. then ycrange=!y.crange   ;  !!!! temporary
endif

pos = convert_coord (!x.crange, ycrange, /data, /to_device)

ydiff = (n_bar+2) * !d.y_ch_size * .7
ydev = keyword_set(bottom) ? pos[1,0] + ydiff : pos[1,1] - ydiff
pos = convert_coord (0., ydev, /device, /to_data)
yd = pos[1]

utbase = getutbase()

st = anytim(stimes)
et = anytim(etimes)

;force label to have same number of elements as st
if n_elements(label) eq 1 then label = replicate(label,n_elements(st))

last_label = ''
for i=0,n_elements(st)-1 do begin
	;if start and end are both -1 then draw line for entire x width
	if st[i] eq -1 then begin
		left = 0
		lo = !x.crange[0]
		hi = !x.crange[1]
	endif else begin
		; otherwise only draw line if it overlaps plot x interval
		st[i] = st[i] - utbase
		et[i] = et[i] - utbase
		if (st[i] lt !x.crange[1] and et[i] gt !x.crange[0]) then begin
			left = st[i] le !x.crange[0] ? 0 : 1
			lo = st[i] > !x.crange[0]
			hi = et[i] < !x.crange[1]
		endif
	endelse
	; if either condition above was true, lo should be defined, otherwise don't draw
	if exist(lo) then begin

		; using polyfillv to shade the interval doesn't work well because the colors
		; underneath change color.  Leave this here for now in case want to try again.
		;if keyword_set(polyfill) then begin
		;	y = !y.crange
		;	device, get_graphics=gr_sav
		;	device, set_graphics=7
		;	patt=bytarr(10,10) + color
		;	polyfill, [lo,hi,hi,lo], [y[0],y[0], y[1],y[1]],color=color, /data
		;	device, set_graphics=gr_sav
		;endif else begin
		;	label_line, lo, hi, yd, label[i], left=left, color=color, thick=2.5, charsize=charsize
		;endelse

		if keyword_set(longline) then begin
			oplot, [lo,lo], ycrange, linestyle=0, color=color
			oplot, [hi,hi], ycrange, linestyle=2, color=color
		endif
		; use label provided if we haven't yet shown that label for these intervals (and if
		; user didn't provide different labels for each interval)
		lab = label[i] eq last_label ? '' : label[i]
		if lab ne '' then last_label = lab
		thick=2.6
		if !d.name eq 'Z' and since_version('6.1') then thick=3.2
		label_line, lo, hi, yd, lab, left=left, color=color, thick=thick, charsize=charsize
	endif
endfor

; only increment n_bar if we drew at least one interval on this call (lo will be defined)
if exist(lo) then n_bar=n_bar+1

!p.nsum=save_nsum

end
