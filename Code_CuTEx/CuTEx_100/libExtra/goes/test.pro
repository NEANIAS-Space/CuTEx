o = obj_new('goes')

;o -> read, '19-sep-2005 03:00', '19-sep-2005 04:00'
;help,o->getdata()

o->plotman, '19-sep-2005 03:00', '19-sep-2005 04:00'
;or

d=o -> getdata(tstart='19-sep-2005 03:00', tend='19-sep-2005 05:00')
help,d
o->plot

window,/free
d = o->getdata(tend='19-sep-2005 06:00', /temperature)
help,d
o->plot,/temp

o->plot,/temp
window,/free
o->plot,/emis

;choices are ['Coronal', 'Photospheric', 'Meyer'] or 0,1,2
o->plot,/tem, abund='photospheric'

;ta = tarray
;yc=yclean
;em=emis
;te=tempr
;ch0=ch0_bad
;ch1=ch1_bad
