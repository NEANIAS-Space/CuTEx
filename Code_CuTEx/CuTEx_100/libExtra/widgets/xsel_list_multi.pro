;+
; Name: xsel_list_multi
;
; Purpose: Function to select one or more item(s) from a list widget.  User can use
;   shift and control keys to select multiple items from the list.
;
; Calling sequence:  list = xsel_list_multi (items)
;
; Input arguments:
;   items - string array of items for selection
;
; Keywords:
;   initial - index into items of initial selection
;   index - if set, return index of selection(s), otherwise return string
;   title - title of widget
;   label - label above list widget
;   group - widget id of calling widget
;   cancel - set to 1 if user pressed cancel button
;
; Output:
;   Function returns item selected (as index if index keyword set).
;
; Written:  Kim Tolbert, 18-Jul-2002
; Modifications:
;   1-Aug-2002, Kim.  Added xoffset, yoffset keywords
;-
;===========================================================================


pro xsel_list_multi_event, event

widget_control, event.top, get_uvalue=state

widget_control, event.id, get_uvalue=uvalue

exit = 0
case uvalue of
	'select': begin
		ind = widget_selected(event.id, /index)
		*state.ptr = ind
		end

	'cancel': begin
		*state.ptr = state.initial
		*state.ptr_cancel = 1
		exit = 1
		end

	'accept': exit = 1

endcase


if exit then widget_control, event.top, /destroy else $
	widget_control, event.top, set_uvalue=state

end

;------------

function xsel_list_multi, items, $
	initial=initial, $
	index=index, $
	title=title, $
	label=label, $
	group=group, $
	xoffset=xoffset, $
	yoffset=yoffset, $
	cancel=cancel

if n_elements(items) eq 0 then begin
	message,'Syntax:  list = xsel_list_multi(items)', /cont
	return, -1
endif

if size(items,/tname) ne 'STRING' then begin
	message, 'Items must be of type STRING.', /cont
	return, -1
endif

checkvar, initial, 0
checkvar, index, 0
checkvar, title, 'Selection Widget'
checkvar, label, 'Select from List: '

initial = initial > 0 < (n_elements(items)-1)

w_base = widget_base (group=group, $
					/column, $
					title=title, $
					/frame, space=10, modal = exist(group), xoffset=xoffset, yoffset=yoffset)

tmp = widget_label (w_base, value=label, /align_center)
tmp = widget_label (w_base, value='(Use shift and control keys to select multiple items.)')

; On windows, need to define x size of widgets
if os_family() eq 'Windows' then xsize=10+max(strlen(trim(items)))
w_list = widget_list (w_base,  $
					/multiple, $
					/align_center, $
					ysize=n_elements(items) < 20, $
					xsize=xsize, $
					value='  ' + trim(items) + '  ', $
					uvalue='select')
widget_control, w_list, set_list_select=initial

w_buttons = widget_base (w_base, /row, space=30, /align_center)
tmp = widget_button (w_buttons, value='Cancel', uvalue='cancel')
tmp = widget_button (w_buttons, value='Accept', uvalue='accept')

; pointer to store output value in so we can get it after widget is destroyed

state = {w_base: w_base, $
	items: items, $
	initial: initial, $
	ptr: ptr_new(initial), $
	ptr_cancel: ptr_new(0) }

if xalive(group) then begin
	widget_offset, group, xoffset, yoffset, newbase=w_base
	widget_control, w_base, xoffset=xoffset, yoffset=yoffset
endif

widget_control, w_base, /realize

widget_control, w_base, set_uvalue=state

xmanager, 'xsel_list_multi', w_base;, /no_block

item_sel = *state.ptr
cancel = *state.ptr_cancel
ptr_free, state.ptr, state.ptr_cancel

if index then return, item_sel else return, items[item_sel]

end
