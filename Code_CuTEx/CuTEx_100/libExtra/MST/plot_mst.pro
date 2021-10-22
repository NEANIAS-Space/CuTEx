pro plot_mst, x, y

	out = gen_dm(x,y)
	mstree = mst(out)
	
	plot, x,y, psym=2
	for i = 0, n_elements(mstree(0,*))-1 do oplot, [x(mstree(0,i)),x(mstree(1,i))], [y(mstree(0,i)),y(mstree(1,i))]
	branches = out(mstree(0,*), mstree(1,*))
	
end
