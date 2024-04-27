;+
; NAME: 
;   ph_baseobj
;
; PURPOSE:
;   Basic object implementing Copy and ShowError methods
;   mainly intended to be inherited
;
; CLASS:
;  class = { ph_BaseObj,$
;    _name : '',      $ ; object's name (defaults to ClassName_ObjN)
;    _debug: 0b,      $ ; debug flag (>1 will include call stack in ShowError)
;    _destroy:0b,     $ ; change behavior if included into a ph_Container
;    wParent:0L,      $ ; parent widget ID for dialog_message (in ShowError)
;    oRef : obj_new() $ ; a reference to any object (no cleanup)
;  }
;
;  set wParent to -1 to show errors using 'print'
;  _destroy = 0 will force a ph_BaseObj object to be removed from a container 
;               object before container is destroyed
;
; METHODS:
;   obj->SelfDestroy       mainly for garbage collecion
;   obj->Content           show content of properties
;   new = obj->copy()      a copy of the object (using save/restore)
;   obj->ShowError, msg    shows an error dialog
;   name = obj->AutoName() return a string ClassName_ObjN
;                          obj->SetProperty, name=obj->AutoName()
;   obj->GetProperty
;   obj->SetProperty
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com> (ivz)
;
;   2008-04-25 ivz Original
;-

pro ph_BaseObj::SelfDestroy
  compile_opt hidden
  obj_destroy, self
end

function ph_BaseObj::copy, TmpDir=tmpdir
  compile_opt hidden
  on_error, 2
  cd, current=cwd
  if n_elements(tmpdir) eq 1 then $
    if file_test(tmpdir, /directory) then cd, tmpdir
  copy = self
  save, copy, filename='idl_obj_copy.tmp'
  restore, 'idl_obj_copy.tmp', restored_obj=copy
  file_delete, 'idl_obj_copy.tmp'
  cd, cwd
  return, copy[0]
end ; copy() 

pro ph_BaseObj::ShowError, msg
  compile_opt hidden

  if (n_elements(msg) eq 0) then $
    help, /last_message, output=msg

  if (self._debug gt 0) then begin
    ;; add obj name and systime in front of the message
    caldat, systime(/julian), m, d, y, hh, mm, ss
    time = string(y,m,d,hh,mm,ss,format='(%"%4d-%02d-%02d %02d:%02d:%02d")')
    tlt = '['+self._name+']' + ' ' + time
    msg = [tlt, msg]
    ; add call stack
    if (self._debug gt 1) then begin
      help, calls=calls
      ;; skip errors from $main$ level
      if (strmatch(calls[0], '$MAIN$') eq 0) then $
        msg = [msg, "Call stack:", calls]
    endif
  endif

  if self.wParent eq -1 then begin
    for l=0L, n_elements(msg)-1 do print, msg[l]
    return
  endif

  if widget_info(self.wParent, /valid_id) then $
    ok = dialog_message(msg, /error, dialog_parent=self.wParent) $
  else $
    ok = dialog_message(msg, /error)
end 

function ph_BaseObj::AutoName
;{{{
  compile_opt hidden
  help, self, output=s
  s = strsplit(s, '<(', /Extract)
  name = obj_class(self) + '_' + strmid(s[1], 10)
  return, name
end

; private method (shortcut for getproperty, name=n)
function ph_BaseObj::_name
  compile_opt hidden
  return, self._name
end

pro ph_BaseObj::GetProperty, name=name, debug=debug, $
  oRef=oRef, destroy=destroy

  compile_opt hidden
  name = self._name
  debug = self._debug
  destroy = self._destroy
  oRef = self.oRef
end

pro ph_BaseObj::SetProperty, name=name, debug=debug, $ 
  wParent=wParent, oRef=oRef, destroy=destroy

  compile_opt hidden
  if n_elements(name) gt 0 then self._name=name
  if n_elements(debug) gt 0 then self._debug=debug
  if n_elements(destroy) gt 0 then self._destroy=keyword_set(destroy)
  if n_elements(oRef) gt 0 then $
    if obj_valid(oRef) then self.oRef = oRef
end

function ph_BaseObj::Init, name=name, debug=debug, destroy=destroy
  compile_opt hidden
  if n_elements(name) gt 0 then self._name=name $
  else begin
    ; constuct the name from class_name and unique obj number
    ;= <ObjHeapVar??(class_name)>
    ;help, self, output=s
    ;s = strsplit(s, '<(', /Extract)
    ;self._name = obj_class(self) + '_' + strmid(s[1], 10)
    self->SetProperty, name=self->AutoName()
  endelse
  if n_elements(debug) gt 0 then self._debug=debug
  ; for ShowError to use dialog_message() without parent widget
  self.wParent = 0L
  ; by default we want object destroyed together with parent container
  self._destroy = (n_elements(destroy) eq 0) ? 1 : destroy
  return, 1
end

pro ph_BaseObj::cleanup
  compile_opt hidden
  ;nothing to clean
end

pro ph_BaseObj__define
  compile_opt hidden
  class = { ph_BaseObj,$
    _name    : '',   $ ; object's name
    _debug   : 0b,   $ ; debug flag
    _destroy : 0b,   $ ; change behavior if included into a ph_Container
    wParent  : 0L,   $ ; parent widget ID for dialog_message (in ShowError)
    oRef : obj_new() $ ; a reference to any object (no cleanup)
  }
end

