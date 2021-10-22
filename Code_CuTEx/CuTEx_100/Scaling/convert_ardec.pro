function convert_ardec, ra, dec
		
		ras=strarr(n_elements(ra))
		decs=strarr(n_elements(ra))


		for ij=0L,n_elements(ra)-1 do begin

  			ra1=fix(ra(ij)/15.)
  			ra2=fix((ra(ij)/15.-ra1)*60.)
  			ra3=(((ra(ij)/15.-ra1)*60.)-ra2)*60.
			if (dec(ij) ge 0.) then segno='+' else segno='-'
  			dec1=fix((dec(ij)))
  			dec2=fix(abs((dec(ij)-dec1)*60.))
  			dec3=abs(((abs(dec(ij)-dec1)*60.)-dec2)*60.)
  			r3=strmid(string(ra3),7,5)
  			d3=strmid(string(dec3),7,5)
  			
			
			if (strmid(r3,0,1) eq '.' ) then r3='00'+strmid(r3,0,3)
  			if (strmid(d3,0,1) eq '.' ) then d3='00'+strmid(d3,0,3)
	  		if (strmid(r3,1,1) eq '.' ) then r3='0'+strmid(r3,0,4)
			if (strmid(d3,1,1) eq '.' ) then d3='0'+strmid(d3,0,4)
			
  			r2=strmid(string(ra2),6,2)
  			d2=strmid(string(dec2),6,2)
  			if (strmid(r2,0,1) eq ' ') then r2='0'+strmid(r2,1,1) else r2=r2
  			if (strmid(d2,0,1) eq ' ') then d2='0'+strmid(d2,1,1) else d2=d2
  			decs(ij)=strmid(string(dec1),5,3)+':'+d2+':'+d3
  			ras(ij)=strmid(string(ra1),5,3)+':'+r2+':'+r3
  
		endfor

return, {ras:ras, decs:decs}

end
