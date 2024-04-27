;+
; NAME:
;   STRUCTTABLE
;
; PURPOSE:  
;   Simple GUI app to show a structure in widget_table
;   only for basic viewing of  fields' values
;
;   no recursive viewing for substructures
;   pointers and objects are only marked as such 
; 
; USAGE:
;   structtable, str_var
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2008-01-27 ivz original  
;-
; Copyright (C) 2008, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


pro StructTable_event,ev
  if tag_names(ev, /struct) eq 'WIDGET_BUTTON' then begin
    widget_control, ev.top,/destroy
    return
  endif
  ; resize event
  widget_control, ev.top, get_uvalue=info
  widget_control, info.tbl, scr_xsize=ev.x, scr_ysize=ev.y-info.btn_ysize, $
      column_widths=ev.x/2
end


pro StructTable, struct, title=title, group_leader=group_leader, $
    xsize=xsize, ysize=ysize, alignment=align

  if size(struct,/type) NE 8 then $
    message, "input structure required"

  struct_name = tag_names(struct, /structure_name)
  if n_elements(title) eq 0 then  title=struct_name
  if title eq '' then title='Anonymous'

  group_leader = (n_elements(group_leader) eq 1) ? group_leader : -1L

  if n_elements(xsize) eq 0 then xsize=300
  if n_elements(ysize) eq 0 then ysize=500
  scr_res = get_screen_size()
  xsize = xsize < scr_res[0]
  ysize = ysize < scr_res[1]

  if n_elements(align) eq 0 then align=0

  tags = tag_names(struct)
  srt = sort(tags)
  n_t = n_tags(struct)
  tbl_val = strarr(2, n_t)
  for i=0L, n_t-1 do begin
    tbl_val[0,i] = tags[srt[i]]
    val = struct.(srt[i])
    dtype = size(val, /tname)
    case dtype of
      'POINTER': tbl_val[1,i] = 'Pointer'
      'OBJREF' : tbl_val[1,i] = 'Object ref.'
      'STRUCT' : tbl_val[1,i] = 'Structure'
      else : begin
        nv = n_elements(val)
        if nv gt 1 then $
          tbl_val[1,i] = strjoin(strtrim(val[0:2<(nv-1)],2),', ') $
        else tbl_val[1,i] = strtrim(val,2) 
      endelse
    endcase
  endfor

  if widget_info(group_leader,/valid_id) then $
    base = widget_base(/column, title=title, group_leader=group_leader,$
          /base_align_center, /modal) $
  else base = widget_base(/column, title=title, /base_align_center)

  widget_control, base, tlb_size_events=1

  ; change align for each cell
  if n_elements(align) ge 2 then align=rebin(align[[0,1]], 2, n_t)
  tbl = widget_table(base, value=tbl_val, alignment=align, /no_headers, $
        column_width=xsize/2, /scroll, scr_xsize=xsize, scr_ysize=ysize)

  void = widget_button(base,value=' Close ')
  widget_control,base, default_button=void

  geo = widget_info(base, /geometry)
  btn_ysize = geo.scr_ysize - ysize  ; Close btn Ysize

  widget_control,base,/realize

  info = {tbl:tbl, btn_ysize:btn_ysize}
  widget_control, base,set_uvalue=info
  xmanager, 'structtable',base
end

