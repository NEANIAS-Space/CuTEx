;+
; NAME:
;      CW_BGROUP2
;
; PURPOSE:
;      Provides a radio-like button group which allows you to select
;      any two buttons.
;
; CATEGORY:
;	Compound widgets
;
; CALLING SEQUENCE:
;	widget = CW_BGROUP2(parent,names)
;
; INPUTS:
;       parnet - The ID of the parent widget.
;       names  - The names to give to the buttons.
;
; KEYWORD PARAMETERS:
;       EVENT_FUNC - The name of an optional user-supplied event function
;           for buttons. This function is called with the return
;           value structure whenever a button is pressed, and
;           follows the conventions for user-written event
;           functions.
;       GREY_BUTTONS - a vector indicating which buttons should be
;                      greyed out.  At the moment there is no way to
;                      sensitize them again.  (0,1) = (off,on)
;	UVALUE - Supplies the user value for the widget.
;       UNAME - Supplies the user name for the widget.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;   This widget generates event structures with the following
;   definition:
;
;       event = { ID:0L, TOP:0L, HANDLER:0L, SELECT_RANK:0, 
;                 SELECT:0, VALUE:IntArr(2), COUNT:0}
;    The SELECT field is passed through from the button event.  VALUE
;    is an array containing the rank number of the currently selected
;    buttons.  COUNT contains the number of selected buttons (0,1,2).
;
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; MODIFICATION HISTORY:
;       Written By:    Edward C Wiebe, 2002-08-23
;             
;-


pro BGroup2_SetV, id, value

  compile_opt hidden
  On_Error, 2
; Change the value for setv to an array with length eqaul the number
; of buttons or less.  Allow only the first two buttons encountered as
; we move along value to be set
;
; value = {index:intarr(2),state:intarr(2)}
;         index = rank of button in the widget (0...n)
;         state = (0,1) = (off,on)

; get the current state
  stash = Widget_Info(id, /CHILD)
  Widget_Control, stash, GET_UVALUE=state, /NO_COPY

; Set the value here.
  if (Var_Type(value) ne 8) then begin
    Message,'Value must be a structure.'
  endif

  for n=0,1 do begin
     Widget_Control,state.id[value.index[n]],SET_BUTTON=value.state[n]
  endfor

; Restore the state.
  Widget_Control, stash, SET_UVALUE=state, /NO_COPY

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function BGroup2_GetV, id

  compile_opt hidden
  On_Error, 2

; Retrieve the structure from the child that contains the sub ids.
  stash = Widget_Info(id, /CHILD)
  Widget_Control, stash, GET_UVALUE=state, /NO_COPY

; Get the value here
  ret = state.state

; Restore the state.
  Widget_Control, stash, SET_UVALUE=state, /NO_COPY

; Return the value here.
  Return,ret
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function BGroup2_Event, ev

  compile_opt hidden
  parent=ev.handler
; Retrieve the structure from the child that contains the sub ids.
  stash = Widget_Info(parent, /CHILD)
  Widget_Control, stash, GET_UVALUE=state, /NO_COPY

  curr = (Where(ev.id eq state.id))[0]
  if (ev.select) then begin

    state.nselect = state.nselect + 1       

    if (state.nselect gt 2) then begin
      Widget_Control, state.b2, SET_BUTTON = 0      
      state.nselect = state.nselect - 1
    endif
    if (state.state[0] ne -1) then begin
      state.b2 = state.b1
      state.state[1] = state.state[0]
    endif 
    state.b1 = ev.id
    state.state[0] = curr

;  endelse
  endif else begin
    if (state.nselect eq 2) and (ev.id eq state.b1) then begin
      state.b1 = state.b2
    endif
    indx = Where(curr eq state.state)
    state.state[indx] = -1
    state.nselect = state.nselect - 1
  endelse

  count = state.nselect

; get the event_function
  event_func = state.event_func

; Restore the state structure
  Widget_Control, stash, SET_UVALUE=state, /NO_COPY

  result = { ID:parent, TOP:ev.top, HANDLER:0L, SELECT_RANK:curr     $
             , SELECT:ev.select, COUNT:count}

  if (event_func ne '') then begin
    Return, Call_Function(event_func, result) 
  endif else Return, result

  Return, result
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Function CW_BGroup2, parent, names                   $
                     , GREY_BUTTONS = grey_buttons   $
                     , UVALUE = uval                 $
                     , UNAME = uname                 $                   
                     , EVENT_FUNC = event_func  

  if (N_PARAMS() eq 0) then MESSAGE, 'Must specify a parent for Cw_Tmpl'
  on_error, 2                   ;return to caller

; Defaults for keywords
  if (not Keyword_Set(uval))  then uval = 0
  if (not Keyword_Set(uname))  then uname = 'CW_BGROUP2_UNAME'

  mainbase = Widget_Base(parent                      $
                         , UVALUE = uval                      $
                         , UNAME = uname                      $
                         , EVENT_FUNC = "bgroup2_event"       $
                         , FUNC_GET_VALUE = "bgroup2_getv"    $
                         , PRO_SET_VALUE = "bgroup2_setv"     $
                         , /NONEXCLUSIVE                      $
                         , /FRAME )

  if (N_Elements(names) eq 0) then names = ['A','B','C','D']   
  if (N_Elements(names) lt 2) then Message,'names array must contain two or more elements.'

  nb = N_Elements(names)

  id    = LonArr(nb)  
  state = IntArr(2)-1

  for n=0,nb-1 do begin
    id[n] = Widget_Button(mainbase,VALUE=names[n])
  endfor 

  if (Keyword_Set(grey_buttons)) then begin
    for n=0,nb-1 do begin
      Widget_Control,id[n],SENSITIVE=grey_buttons[n]
    endfor
  endif


  if (N_Elements(event_func) le 0) then event_func = ''

  state = {nb:nb, id:id, nselect:0, b1:0L, b2:0L, state:state, event_func:event_func}

; Save out the initial state structure into the first childs UVALUE.
  Widget_Control, Widget_Info(mainbase, /CHILD), SET_UVALUE=state, /NO_COPY

  Return, mainbase
end





