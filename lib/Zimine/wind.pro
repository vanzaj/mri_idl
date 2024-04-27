;+
; NAME:
;   WIND
;
; PURPOSE:
;   "Clickable" replacement of WINDOW procedure
;
; USAGE: 
;   wind [, wsize , hsize, title=title, parent=parent_id ]
;   if called without parameters, 256 x 256 window is created
;   if called with one parameter, wsize x wsize window is created
;   if called with two parameters, wsize x hsize window is created
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com>
;   
;   2001-08-01 ivz Original (adapted from smart_w.pro by P. Romashkin)
;-
; Copyright (C) 2001, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 
;vim: ts=2 sw=2 sts=2 tw=80
;-

pro wind_event, ev
  widget_control, ev.top, get_uvalue=draw
  widget_control, draw, get_value=win_id
  wset, win_id
  if tag_names(ev, /structure_name) eq 'WIDGET_BASE' then $
    widget_control, draw, draw_xsize=ev.x, draw_ysize=ev.y
end

pro wind, wsize, hsize, parent=parent, title=title, get_tlb=tlb

  on_error, 2 ; return to caller

  if n_elements(wsize) eq 0 then wsize=256 
  if n_elements(hsize) eq 0 then hsize=wsize
  if n_elements(title) eq 0 then title='IDL'

  ; minimum window size
  wsize = wsize > 50 & hsize = hsize > 50

  if n_elements(parent) ne 0 then begin
    if widget_info(long(parent), /valid_id) then $
      tlb = widget_base(group_leader=parent, title=title, $
            /kbrd_focus, /tlb_size_event) $
    else message, "invalid parent id"
  endif else $
      tlb = widget_base(title=title, /kbrd_focus, /tlb_size_event)

  draw = widget_draw(tlb, xsize=wsize, ysize=hsize, retain=2)
  widget_control, tlb, /realize
  widget_control, draw, get_value=win_id
  wset, win_id
  widget_control, tlb, set_uvalue=draw
  if title eq 'IDL' then $
      widget_control, tlb, tlb_set_title='IDL: '+strtrim(win_id,2)
  xmanager, 'wind', tlb, /no_block
end

