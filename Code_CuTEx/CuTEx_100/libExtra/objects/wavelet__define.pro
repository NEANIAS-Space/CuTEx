;+
; Project     : HESSI
;
; Name        : WAVELET__DEFINE
;
; Purpose     : Define a WAVELET class
;               This class inherits FITS properties and methods, and
;               has an additional property called WAVELET which is a
;               LINKED_LIST object in which wavelet processed images
;               are stored.
;
; Category    : imaging objects
;
; Syntax      : IDL> new=obj_new('wavelet',type)
;
; Example     : IDL> eit=obj_new('wavelet','eit')  ;-- create EIT wavelet
;               IDL> eit->read,filenames           ;-- read in EIT files
;               IDL> eit->process                  ;-- wavelet process 
;               IDL> eit->plot,wavelet=0           ;-- plot first wavelet  
;               IDL> eit->plot,wavelets=1          ;-- plot second wavelet
;               IDL> wavelets=eit->get_wavelets()  ;-- get wavelet data arrays
;                                             
; History     : Written 5 May 2007, Zarro (ADNET)
; 
; Contact     : dzarro@solar.stanford.edu
;-
function wavelet::init,type,_ref_extra=extra
               
;-- create a LINKED_LIST object to store wavelet processed images

if is_blank(type) then type='map'
error=0
catch,error
if error ne 0 then begin
 catch,/cancel
 message,'Invalid object class - '+type,/cont
 return,0
endif

self.type=call_function('obj_new',type)
self.wavelet=obj_new('linked_list')
return,1      
end
                                                                                               
;----------------------------------------------------------------------------
;-- clean method

pro wavelet::cleanup

obj_destroy,self.wavelet
obj_destroy,self.type

return
end

;---------------------------------------------------------------------------
;-- read method

pro wavelet::read,files,_ref_extra=extra

self.type->read,files,_extra=extra

return & end

;-----------------------------------------------------------------------------
;-- Wavelet processing method 

pro wavelet::process,_ref_extra=extra

if ~self->have_data(count) then return

;-- Extract each image array and wavelet process.
;   WAVELET_DATA will have N extra dimensions depending upon number of
;   wavelet scales.
;   Store results in a LINKED_LIST for fast and easy retrieval.

obj_destroy,self.wavelet
self.wavelet=obj_new('linked_list') 
for i=0,count-1 do begin
 map=self.type->get(i,/map)

; wavelet_process,map.data,wavelet_data,_extra=extra

;-- following is just test data. Comment out and replace by call to wavelet_process

 wavelet_data=reproduce(map.data,3)

 if exist(wavelet_data) then self.wavelet->add,wavelet_data,i

endfor

return & end

;---------------------------------------------------------------------------
;-- check that data has been read

function wavelet::have_data,count

count=0
count=self.type->get(/count)
if count eq 0 then begin
 message,'No image data has been read yet',/cont
 return,0b
endif

return,1b
end

;-------------------------------------------------------------------------
;-- get wavelet processed arrays

function wavelet::get_wavelets,i,nwavelets=nwavelets

nwavelets=0
if ~self->have_data(count) then return,0
if ~is_number(i) then i=0
i= i < (count-1)

wavelet_data=(self.wavelet->get_value(i))
ndim=size(wavelet_data,/n_dim)
dimensions=size(wavelet_data,/dimensions)
if ndim eq 2 then nwavelets=1
if ndim eq 3 then nwavelets=dimensions[2]
if nwavelets eq 0  then begin
 message,'No wavelet images processed yet',/cont
 return,0
endif

return,wavelet_data
end

;--------------------------------------------------------------------------
;-- plot wavelet processed images

pro wavelet::plot,i,original=original,wavelet_index=wavelet_index,_ref_extra=extra

;-- set /original to plot original image
;-- set wavelet_index = index of wavelet image to plot [def = 0, or first]

if ~self->have_data(count) then return

;-- plot original image if requested

if keyword_set(original) then begin
 self.type->plot,_extra=extra
 return
endif

;-- plot selected wavelet processed image

wavelet_data=self->get_wavelets(i,nwavelets=nwavelets)
if nwavelets eq 0 then begin
 message,'Plotting original image',/cont
 self.type->plot,_extra=extra
 return
endif
if ~is_number(wavelet_index) then wavelet_index=0
wavelet_index=abs(wavelet_index)
if wavelet_index ge nwavelets then begin
 message,'Input index exceeds number of wavelet scales - '+trim(nwavelets),/cont 
 return
endif

self.type->plot,data=wavelet_data[*,*,wavelet_index],/no_copy,_extra=extra

return & end

;-----------------------------------------------------------------------------
;-- WAVELET class definition

pro wavelet__define

void={wavelet,wavelet:obj_new(),type:obj_new()}

return & end
