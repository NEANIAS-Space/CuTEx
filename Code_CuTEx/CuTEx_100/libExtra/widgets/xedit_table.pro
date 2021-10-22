;+
; Project     : HESSI
;
; Name        : xedit_table
;
; Purpose     : Allows you to edit an array of values using a table widget.
;
; Category    : widgets
;
; Explanation : Builds a base for a widget_table, and adds accept and cancel buttons.
;
; Syntax      : vals = xedit_table (array [, group=group, title=title, _extra=_extra])
;
; Inputs      : array = 1 or 2-D array of values (strings or numbers) to edit
;
; Outputs     : vals - modified array
;
; Keywords    : group   = widget id of parent widget (if any)
;               title   = string to put in title of widget
;               _extra  = any keywords to pass into IDL widget_table routine
;
; Restrictions:
;              Changes to values only take effect when user presses return or clicks
;              in another cell of table.  I gave up trying to make it work without that
;              requirement.  There are lots of way to generate events when
;              the user does stuff, but doing a widget_control,id,get_value=v doesn't
;              get the changed values until one of those two things happen.
;
; Example     :
;               vals = xedit_table (findgen(8,3), $
;                      column_labels=['a','b','c','d','e','f','g','h'], $
;                      row_labels=['A','B','C'], $
;                      title='XEDIT_TABLE Example')
;
; Written     : 4-May-2005, Kim Tolbert
;
; Modifications:
;-

pro xedit_table_event, event

widget_control, event.top, get_uvalue=state
widget_control, event.id, get_uvalue=uvalue
exit=0
case uvalue of
	'accept': begin
		exit=1
		widget_control, state.w_table, get_value=vals
		*state.vals = vals
		end
	'cancel': begin
		exit=1
		*state.vals = state.orig
		end
	else:
	endcase
if exit then widget_control,event.top, /destroy else $
	widget_control, event.top, set_uvalue=state
end

;-----

function xedit_table, array, group=group, title=title, _extra=_extra

w_base = widget_base (group=group, title=title, /column, modal=exist(group))
w_table = widget_table (w_base, value=array, uvalue='table',  /edit, _extra=_extra)
tmp= widget_label (w_base, $
	value='Note: Changes to a cell are registered only when you click another cell or press Enter')
w_buttons = widget_base (w_base, /row, space=10, /align_center)
tmp = widget_button (w_buttons, value='Accept', uvalue='accept')
tmp = widget_button (w_buttons, value='Cancel', uvalue='cancel')

state = {w_table: w_table, vals: ptr_new(array), orig: array}
widget_control, w_base, set_uvalue = state

if xalive(group) then begin
	widget_offset, group, xoffset, yoffset, newbase=w_base
	widget_control, w_base, xoffset=xoffset, yoffset=yoffset
endif
widget_control, w_base, /realize
xmanager, 'xedit_table', w_base

return, *state.vals
end