
;-- Pipeline program to create EIT/LASCO overlay tiles
;   Written, 20-Feb-2007, Zarro (ADNET)
;-- Example: eit_lasco_tiles,'1-jan-07','31-jan-07'

pro eit_lasco_tiles,tstart,tend,_extra=extra,top_dir=top_dir

;-- Verify output directory for full-size original images

if is_blank(top_dir) then top_dir='~/Sites/images'
if ~write_dir(top_dir,/verb) then return

;-- Find matching EIT and LASCO C2/C3 files

struct=eit_lasco_files(tstart,tend,_extra=extra,count=count)
if count eq 0 then return

;-- Create full-size overlay images for each matching set

out_dir=concat_dir(top_dir,'original') 
mk_dir,out_dir
nsize=4096  
eit_lasco_over,struct,nsize=nsize,_extra=extra,out_dir=out_dir,files=files
if is_blank(files) then return

;-- Output directory for tiles

tile_dir=concat_dir(top_dir,'tiles')
mk_dir,tile_dir

;-- Start making tiles

do_tiles,files,tile_dir,_extra=extra

;-- Create tile index files

index_tiles,tile_dir,_extra=extra
 
return & end



