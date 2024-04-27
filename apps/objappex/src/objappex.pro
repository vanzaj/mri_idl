;+
; NAME:
;   OBJAPPEX
; 
; PURPOSE:
;   This is an example of an application coded as an object. 
;   it doesn't do anything useful apart from showing how to handle 
;   widget events via two possible mechanisms.
;   Main advantage of this approach is easy access to application's
;   internal variables.
;
; NOTES:
;   SELF is stored in TLB's uvalue i.e. can be accessed from any event
;   GUI widgets should have a unique UNAME 
;   or UVALUE={obj:obj_ref, method:'method'} (obj_ref can be SELF)
;   if UVALUE is defined then we try to: call_method, uv.method, uv.obj, ev
;   otherwise we try default EvtHandler, ev, uname
;
; FUTHER INSPIRATION:  
;   http://www.dfanning.com/documents/programs.html#CATALYST%20LIBRARY
;   http://michaelgalloy.com/2006/06/14/object-widgets.html
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com>
;
;   2007-12-26 ivz Original
;   2010-02-01 ivz Major rewrite (cleanup and rearragnement)
;-
; Copyright (C) 2007,2010 Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

;;;
;;; GUI layout
;;;
function ObjAppEx::GUI
  compile_opt idl2,hidden

  ; gui is already running
  if self.running then return, 1

  catch, error
  if error ne 0 then begin
    self->error
    self.running=0
    return, 0
  endif

  ; display size
  winX = 450
  winY = 450

  tlb = widget_base(title=self.title, /column, mbar=tlm, $
        tlb_size_events=0, /base_align_left, xpad=0, ypad=0)
  self.tlb = tlb

  ; menu
  menu = widget_button(tlm, value='File')
  wdg = widget_button(menu, value='Open...', uname='file/open')
  wdg = widget_button(menu, value='Save...', uname='file/save')
  wdg = widget_button(menu, value='Exit', uname='file/exit', /separator)

  ; main layout base
  base = widget_base(tlb, /row, xpad=0, ypad=0, uname='base/main')

  ; graphics area
  subb = widget_base(base, /column, xpad=0, ypad=0, uname='base/graphics')
  dsp = widget_draw(subb, xsize=winX, ysize=winY, uname='graphics/main',$
    uvalue={obj:self, method:'mainDraw'})

  ; some control buttons 
  subb = widget_base(base, /column, xpad=0, ypad=0, uname='base/controls')
  btn = widget_button(subb, value=' hiya ', uname='controls/hiya', $
    uvalue={obj:self, method:'hiBtn'})

  ; status bar
  self.sbar = widget_label(tlb, /align_left, /dynamic_resize, $
    /sunken_frame)

  widget_control, tlb, /realize
  geo = widget_info(base, /geometry)
  widget_control, self.sbar, xsize=geo.xsize

  widget_control, self.tlb, set_uvalue=self
  xmanager, 'ObjAppEx', self.tlb, /no_block, $
    event_handler='ObjAppEx_events', cleanup='ObjAppEx_kill'

  self->debug, "Done with GUI"
  return, 1
end

;;;
;;; Events
;;;
pro ObjAppEx_kill, tlb
;; application exit
  widget_control, tlb, get_uvalue=self
  obj_destroy, self
end

pro ObjAppEx_events, ev
;; wrapper for events
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  widget_control, ev.top, get_uvalue=self
  widget_control, ev.id, get_uvalue=handle
  uname = widget_info(ev.id, /uname)
  self->debug, 'got events from '+uname
  ;; use generic method if uvalue of the widget is undefined
  if size(handle,/tname) eq 'UNDEFINED' then begin
    self->EventHandler, ev, uname
    return
  endif
  call_method, handle.method, handle.obj, ev
end

pro ObjAppEx::eventHandler, ev, uname
;; default handler
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  case uname of
    'file/open': begin
      file = dialog_pickfile(dialog_parent=self.tlb)
      if file eq '' then return
      ;self->LoadData, fls
      return
    end
    'file/save': begin
      filt = ['*.png','*.jpg', '*.bmp', '*.tif']
      file = dialog_pickfile(dialog_parent=self.tlb, /write)
      if file eq '' then return
      return
    end
    'file/exit': begin
      widget_control, self.tlb, /destroy
      return
    end
    else : begin
      self->debug, 'unknow uname in EventHandler: '+uname
      return
    end
  endcase
end

pro ObjAppEx::mainDraw, ev, uname
;; draw widget
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
end 

pro ObjAppEx::hiBtn, ev, uname
;; put 'Hello world' semi-randomly on display
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  wID = self->getWidgetID('graphics/main')
  geo = widget_info(wID, /geometry)
  xloc = ( randomu(seed)*0.7 + 0.15)*geo.xsize
  yloc = ( randomu(seed)*0.7 + 0.15)*geo.ysize
  self->setStatus, string([xloc,yloc], format='(%"(%d,%d)")')
  widget_control, wID, get_value=windowID
  wset, windowID
  erase
  xyouts, xloc, yloc, "Hello world", align=0.5, charsize=1.25, /device 
end

;;;
;;; private
;;;
pro ObjAppEx::error
;; local error reporting
  compile_opt hidden
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  ok = dialog_message(err,/error,dialog_parent=self.tlb)
  catch,/cancel
end

pro ObjAppEx::setStatus, text
;; set status bar
  compile_opt hidden
  widget_control, self.sbar, set_value=text
end

function ObjAppEx::getWidgetID, uname
;; return widget ID by uname
  compile_opt hidden
  if n_elements(uname) eq 0 then return, -1L
  return, widget_info(self.tlb, find_by_uname=uname)
end

pro ObjAppEx::debug, text
  compile_opt hidden
  if n_elements(text) eq 0 then text='hi there...'
  if self.debug then print, 'DBG: ',text
end

pro ObjAppEx::set, title=title, debug=debug
  if n_elements(title) gt 0 then self.title = title
  self.debug = keyword_set(debug)
end

;;;
;;; life-cycle
;;;
function ObjAppEx::init, title=title
  compile_opt idl2,hidden

  on_error, 2
  self.version = '0.5'

  if n_elements(title) eq 0 then title='ObjApp (v. '+ self.version + ')'
  self.title = title

  ok = self->gui()

  if ~ok then $
    message,  'Failed to start GUI'
  self.running = 1b
  return, 1
end

pro ObjAppEx::cleanup
  compile_opt idl2,hidden
end

pro ObjAppEx__define
  compile_opt idl2,hidden

  obj = { ObjAppEx    ,$
    version: ''       ,$ ; prog version
    title:   ''       ,$ ; GUI title
    tlb:     0        ,$
    sbar:    0        ,$ ; status bar id (widget_label)
    running: 0b       ,$ ; running flag
    debug:   0b        $ ; debub flag
  }
end

;;;
;;; Main (starts the application)
;;;
pro ObjAppEx, app=app
  app = obj_new('ObjAppEx')
  app->set, /debug
  app->setStatus, 'Ready...'
end

