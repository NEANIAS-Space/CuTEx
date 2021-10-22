;+
; NAME:
;      CW_AXIS_SELECT
;
; PURPOSE:
;      This compound widget provides an interface for selecting points
;      along multiple axes.  It was written to be used as an aid for
;      the analysis of gridded data.
;
; CATEGORY:
;	Compound widgets
;
; CALLING SEQUENCE:
;	widget = CW_Axis_Select(parent,numaxes, axes, b_labels, s_labels)
;
; INPUTS:
;       PARENT   - The ID of the parent widget.
;       NUMAXES  - The number of axes
;       AXES     - A pointer array where each element points to an
;                  array of values corresponding to points along an
;                  axis the pointee arrays can be of any type.
;       B_LABELS - Labels corresponding to the names of the axes.
;                  These should be short to make the overall widget as
;                  compact as possible.
;       S_LABELS - Labels to be used on each slider.  There is one
;                  slider per axis.  Labels could include a name and
;                  units. (ex. 'Longitude (deg.):')
;
; KEYWORD PARAMETERS:
;	UVALUE - Supplies the user value for the widget.
;       UNAME - Supplies the user name for the widget.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE_EFFECTS:
;  This widget generates event structures with the following
;  definitions:
;  
;  Tag_Names(event,/STRUCTURE) will return 'AXIS_SELECT_SLIDER'
;  event = { ID:0L, TOP:0L, HANDLER:0L, CUR_AXIS:0, SLIDER_VALUE:0 }
;
;  Tag_Names(event,/STRUCTURE) will return 'AXIS_SELECT_BUTTON'
;  event = { ID:0L, TOP:0L, HANDLER:0L, CUR_AXIS:0, SELECT:0 }
;
; COMMON BLOCKS:
;	None.
;
; PROCEDURE:
;       You can set or get the value of this compound widget using
;       calls to Widget_Control.
;
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
;  value is always a structure of the form:  { axis:1, index:1 }
;  where axis is an integer (0 to num_axes-1) and index is the slider position.
;
; MODIFICATION HISTORY:
;     Written by:   Edward C. Wiebe, 2002-09-18
;
;-

pro AS_Set_Value, id, value
  compile_opt hidden

  On_Error, 2

; Retrieve the state.
  stash = Widget_Info(id, /CHILD)
  Widget_Control, stash, GET_UVALUE=state, /NO_COPY

; Set the value here.  You may only set the currently selected axis
; and the current slider index. 

; value is a struct with the form { axis:1, index:1 }
; set the button, slider and slider label
  Widget_Control, state.ornt_rb, SET_VALUE=value.axis
  Widget_Control, state.ornt_sl[value.axis], SET_VALUE=value.index
  text = state.s_labels[value.axis]  $
    + StrTrim( (*(state.axes[value.axis]))[value.index] , 2) 
  Widget_Control, state.ornt_sll[value.axis], SET_VALUE=text

; remap the bases (if necessary) to show the selected one
  if (state.cur_axis ne value.axis) then begin
    Widget_Control, state.ornt_mode_bases[state.cur_axis], MAP = 0  
    Widget_Control, state.ornt_mode_bases[value.axis], MAP = 1
  endif

; save the axis information, the slider's value is stored by the
; slider widget itself
  state.cur_axis = value.axis 
 
; Restore the state.
  Widget_Control, stash, SET_UVALUE=state, /NO_COPY
  Return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function AS_Get_Value, id
  compile_opt hidden

; Return to caller.
  On_Error, 2

; Retrieve the structure from the child that contains the sub ids.
  stash = Widget_Info(id, /CHILD)
  Widget_Control, stash, GET_UVALUE=state, /NO_COPY

; return a struct that looks like { axis:1, index:1 }
  Widget_Control, state.ornt_rb, GET_VALUE=axis
  Widget_Control, state.ornt_sl[axis], GET_VALUE=index
 
  result = { axis:axis, index:index }

; Restore the state.
  Widget_Control, stash, SET_UVALUE=state, /NO_COPY

; Return the value here.
  Return, result
end

;-----------------------------------------------------------------------------

function AS_Base_Event, ev
  compile_opt hidden

;  Message,/INFO,'BEGINS' ;@@
  parent=ev.handler

; Retrieve the structure from the child that contains the sub ids.
  stash = Widget_info(parent, /CHILD)
  Widget_Control, stash, GET_UVALUE=state, /NO_COPY

  evtype = Tag_Names(ev,/STRUCTURE)

; assume we return an event (ret eq 1)
  ret = 1
  case evtype of
    'WIDGET_SLIDER':begin
      indx = Where(ev.id eq state.ornt_sl)
      if (ev.drag eq 1) then begin
;        The slider is being dragged. Update the label with the axis value. 
        ret = 0
      endif else begin
        retv = {AXIS_SELECT_SLIDER, ID:parent, TOP:ev.top, handler:0L      $
                , cur_axis:state.cur_axis, slider_value:ev.value }
      endelse
      value = state.s_labels[indx]                                    $
        + StrTrim( (*(state.axes[indx]))[ev.value-1] , 2)
      Widget_Control, state.ornt_sll[indx], SET_VALUE=value
    end
    else:begin
      case ev.id of
        state.ornt_rb: begin 
          Widget_Control, state.ornt_mode_bases[ev.value], MAP = ev.select
          state.cur_axis = ev.value
          ret = ev.select 
          retv = {AXIS_SELECT_BUTTON, ID:parent, TOP:ev.top, HANDLER:0L $
                  , cur_axis:state.cur_axis, select:ev.select }
        end
        else: Message,/INFO,'AXIS SELECT BASE EVENT: No matching case.' 
      endcase
    end
  endcase  

  if (ret eq 0) then retv = 0

; Restore the state structure
  Widget_Control, stash, SET_UVALUE=state, /NO_COPY

  Return, retv
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro AS_Destroy, id

; destroy the pointer array that is part of the state variable stored
; in the first widget of cw_axis_select.  The first widget is the widget_base
; called "one" and must be created with the KILL_NOTIFY keyword
; listing this routine.
  Widget_Control, id, GET_UVALUE=state, /NO_COPY
  Ptr_Free,state.axes
 
  Return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function CW_Axis_Select, parent, numaxes, axes, b_labels, s_labels  $
                    , UVALUE = uval, UNAME = uname

  if (N_Params() eq 0) then Message, 'Must specify a parent for CW_Axis_Select'
  On_Error, 2                   ;return to caller

; Defaults for keywords
  if not (Keyword_Set(uval))  then uval = 0
  if not (Keyword_Set(uname))  then uname = 'CW_AXIS_SELECT'

  base = Widget_Base(parent, UVALUE = uval, UNAME = uname   $
                            , /FRAME, /COLUMN                      $
                            , EVENT_FUNC = "as_base_event"      $
                            , FUNC_GET_VALUE = "as_get_value"  $
                            , PRO_SET_VALUE = "as_set_value")

; orientation base first 
; "one" is the first widget in this compound widget and will store the
; state variable.  Since one element of the state variable is a
; pointer array we need a kill_notify procedure for cleanup.
  one = Widget_Base(base, KILL_NOTIFY='as_destroy')
  two = Widget_Base(base)

; orientation radio buttons
  if (N_Elements(numaxes) eq 0) then numaxes = 3

  if (N_Elements(axes) eq 0) then begin
    axes = PtrArr(numaxes)    
    for n=0,numaxes-1 do axes[n] = Ptr_New(IndGen(100))
  endif

  if (N_Elements(b_labels) eq 0) then begin
    b_labels = StrArr(numaxes)
    for n=0,numaxes-1 do b_labels[n] = StrTrim(n,2)
  endif

  if (N_Elements(s_labels) eq 0) then begin
    s_labels = StrArr(numaxes)
    for n=0,numaxes-1 do s_labels[n] = 'label '+StrTrim(n,2) +': '
  endif

  n_axes = IntArr(numaxes)
  for n=0,numaxes-1 do begin
    s = Size(*(axes[n]))
    n_axes[n] = s[1]
  endfor
  cur_axis = 0
  ornt_rb = CW_BGroup(one, b_labels, /RETURN_INDEX, /ROW 	$
                      , /EXCLUSIVE, SET_VALUE = cur_axis)

  ornt_mode_bases = LonArr(3)
  ornt_sl         = LonArr(3)
  ornt_sll        = LonArr(3)
  
  for n = 0,numaxes-1 do begin
    tmp = Widget_Base(two, /COLUMN)
    ornt_sl[n]  = Widget_Slider(tmp,/DRAG, XSIZE=150         $
                                , MINIMUM=1,MAXIMUM=n_axes[n])
    ornt_sll[n] = Widget_Label(tmp, VALUE = s_labels[n]        $
                               + StrTrim( (*axes[n])[0]  ,2)  $ 
                               ,/DYNAMIC_RESIZE                $
                               ,/ALIGN_LEFT)
    Widget_Control, tmp, MAP = 0
    ornt_mode_bases[n] = tmp
  endfor

  Widget_Control, ornt_mode_bases[cur_axis], MAP = 1

  state = { ornt_rb:ornt_rb, ornt_sl:ornt_sl, ornt_sll:ornt_sll  $
            , s_labels:s_labels, ornt_mode_bases:ornt_mode_bases $
            , numaxes:numaxes, axes:axes, cur_axis:cur_axis }

; Save out the initial state structure into the first childs UVALUE.
  Widget_Control, Widget_Info(base, /CHILD), SET_UVALUE=state, /NO_COPY

; Return the base ID of the compound widget. 
  Return, base
end
