Kim Tolbert
20-Dec-2005

do_fitsfiles, gfits_w, goes_3hour were the routines that write the SDAC GOES FITS files until ~September 2005.  At that time, NOAA started sending ASCII 3-hour files instead of binary 3-hour files.  Amy Skowronek modified the routines to accomodate this change, so now the routines used are do_fitsfiles_ascii, gfits_w_ascii, goes_3hour_ascii.