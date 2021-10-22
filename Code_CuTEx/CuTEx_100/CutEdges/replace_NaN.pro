pro replace_NaN, ima

im = readfits(ima, h)
indx = where(im eq 0)
wheretomulti, im, indx, colu, row
im(colu,row) = !Values.F_NAN
writefits, strmid(ima, 0, strpos(ima, '.'))+'_nan.fits', im, h

end
