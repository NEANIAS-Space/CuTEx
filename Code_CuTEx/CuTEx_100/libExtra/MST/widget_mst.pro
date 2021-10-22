pro mstw_event, ev

widget_control, ev.id, get_uvalue=eval
common wdrawblock, num_win, orig_image, x_im_size, y_im_size
	
case eval OF
	
	'GEN' : BEGIN
			check = file_search('coord.dat',count=count)
			if count gt 0 then file_delete, 'coord.dat'
			nwin = !d.window
			print, 'Generating values' 
			x = nearint(randomu(seed, 30)*100)
			y = nearint(randomu(seed, 30)*100)
			wset, num_win
			plot, x, y, psym=8, /isotropic, xs=1, ys=1, xr=[-1,101], yr=[-1,101]
			wset, nwin
			coord = [[x],[y]]
			save, coord, filename='coord.dat'	
		END

		
	'MST' : BEGIN
			check = file_search('coord.dat',count=count)
			if count eq 0 then begin
				print, "No data were sampled, please first create dataset'
			endif else begin
				restore, 'coord.dat'
				print, 'Computing Minimum Spanning Tree'
				nwin = !d.window	
				x = coord(*,0)
				y = coord(*,1)		
				dm = gen_dm(x, y)
				mstree = mst(dm)
				wset, num_win
				@colors
				for i = 0, n_elements(mstree(0,*))-1 do oplot, [x(mstree(0,i)),x(mstree(1,i))], [y(mstree(0,i)),y(mstree(1,i))], color=red
				oplot, x,y, PSYM=8
				wset, nwin
			endelse
			if count gt 1 then file_delete, 'coord.dat'
		END
		
	'DONE': widget_control, ev.top, /DESTROY
		
endcase

end
	
pro mstw
	common wdrawblock, num_win, orig_image, x_im_size, y_im_size
	
	nwin = !d.window
	base = WIDGET_BASE(TITLE='Minimum Spanning Tree application', /column)
	widget_control, /managed, base
	draw = widget_draw (base, /button_events, /frame, uvalue='draw_win',retain=2, XSIZE=400, ySIZE=400 )
	button1 = widget_button(base, uvalue='GEN', value='Values Generator')
	button2 = widget_button(base, uvalue='MST', value='MST Calculator')
	button3 = widget_button(base, uvalue='DONE', value = 'Done')
	widget_control, base, /realize 
	widget_control, draw, get_value=num_win
	wset, num_win
	
	a = findgen(20) * (!pi*2/19.)
	usersym, sin(a), cos(a), /FILL
	
	if (n_elements(xw) gt 1) and (n_elements(yw) gt 1) then plot, x, y, psym=8 else plot, [0,100], [0,100], /nodata, /isotropic
	wset, nwin
	x_im_size = 400
	y_im_size = 400
	
	xmanager, 'mstw', base, group_leader=group, /no_block
	
end	
  	
