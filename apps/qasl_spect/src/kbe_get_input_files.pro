;+
; NAME:
;   kbe_get_input_files
;
; PURPOSE:
;   Dialog to get file names of ASL/SPECT/3D MR data
;
; USAGE: 
;   files = kbe_get_input_files(data_dir=default, group_leader=gid, $ 
;                              canceled=canceled)
;
;   files are returned as structure 
;   {asl_cbf:'', asl_raw:'', spect_cbf:'', mr_brain:''}
;
; NOTES:
;   self-contained modal dialog widget app w/o internal error checking
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;
;   2008-03-25 ivz Original
;-

; all events in the same pro
; button events only
pro kbe_get_input_files_events, ev
  widget_control, ev.top, get_uvalue=pin
  uname = widget_info(ev.id, /uname)

  case uname of 
    'accept' : begin
      (*pin).canceled = 0
      widget_control, ev.top, /destroy
      return
    end
    'cancel' : begin
      (*pin).canceled = 1
      widget_control, ev.top, /destroy
      return
    end
    'browse_spect_cbf' : begin
      f = dialog_pickfile(title='Select SPECT cbf', filter='*.IMA', $
          path=(*pin).data_dir, get_path=data_dir, dialog_parent=ev.top)
      if f eq '' then return
      wdg = widget_info(ev.top, find_by_uname='file_spect_cbf')
      widget_control, wdg, set_value=f
      (*pin).files.spect_cbf = f
      (*pin).data_dir = data_dir
    end ; browse_spect_asl
    'browse_spect_reg' : begin
      f = dialog_pickfile(title='Select SPECT (reg)', filter='*_epi.hdr', $
          path=(*pin).data_dir, get_path=data_dir, dialog_parent=ev.top)
      if f eq '' then return
      wdg = widget_info(ev.top, find_by_uname='file_spect_reg')
      widget_control, wdg, set_value=f
      (*pin).files.spect_cbf_reg = f
      (*pin).data_dir = data_dir
    end ; browse_spect_asl
    'browse_asl_cbf' : begin
      f = dialog_pickfile(title='Select qASL cbf', filter='*qasl*.hdr', $
          path=(*pin).data_dir, get_path=data_dir, dialog_parent=ev.top)
      if f eq '' then return
      wdg = widget_info(ev.top, find_by_uname='file_asl_cbf')
      widget_control, wdg, set_value=f
      (*pin).files.asl_cbf = f
      (*pin).data_dir = data_dir
    end ; browse_asl_cbf
    'browse_asl_raw' : begin
      f = dialog_pickfile(title='Select qASL raw', filter='*.PAR', $
          path=(*pin).data_dir, get_path=data_dir, dialog_parent=ev.top)
      if f eq '' then return
      wdg = widget_info(ev.top, find_by_uname='file_asl_raw')
      widget_control, wdg, set_value=f
      (*pin).files.asl_raw = f
      (*pin).data_dir = data_dir
    end ; browse_asl_raw
    'browse_mr_brain' : begin
      f = dialog_pickfile(title='Select MR3D (brain)', filter='*.hdr', $
          path=(*pin).data_dir, get_path=data_dir, dialog_parent=ev.top)
      if f eq '' then return
      wdg = widget_info(ev.top, find_by_uname='file_mr_brain')
      widget_control, wdg, set_value=f
      (*pin).files.mr_brain = f
      (*pin).data_dir = data_dir
    end ; browse_mr_brain
    'browse_stack_coords' : begin 
      f = dialog_pickfile(title='Select stacks coordinates', filter='*_coords.txt', $
          path=(*pin).data_dir, get_path=data_dir, dialog_parent=ev.top)
      if f eq '' then return
      wdg = widget_info(ev.top, find_by_uname='file_stack_coords')
      widget_control, wdg, set_value=f
      (*pin).files.stack_coord = f
    end ; browse_stack_coords
  endcase
end

  ; Main
function kbe_get_input_files, data_dir=data_dir, group_leader=gid, $
      canceled=canceled

  if strlowcase(!version.os_family) eq 'unix' then $
    default_dir = getenv('HOME') $
  else default_dir = 'C:\'

  if n_elements(data_dir) eq 0 then data_dir=default_dir

  title = 'Select Input Data...'

  if n_elements(gid) gt 0 then $
    tlb = widget_base(group_leader=gid, title=title, /column, $
                      /base_align_center, /floating) $
  else tlb = widget_base(title=title, /column, /base_align_center)

  ; widget_text xsize (file name)
  text_size = 40

  rb = widget_base(tlb, /row, /align_right)
  wid = widget_label(rb, value='3D (BET):')
  wid = widget_text(rb, value='', xsize=text_size, uname='file_mr_brain')
  wid = widget_button(rb, value='Browse', uname='browse_mr_brain',sensitive=0)
  
  rb = widget_base(tlb, /row, /align_right)
  wid = widget_label(rb, value='ASL (CBF):')
  wid = widget_text(rb, value='', xsize=text_size, uname='file_asl_cbf')
  wid = widget_button(rb, value='Browse', uname='browse_asl_cbf')

;  rb = widget_base(tlb, /row, /align_right)
;  wid = widget_label(rb, value='ASL (Raw):')
;  wid = widget_text(rb, value='', xsize=text_size, uname='file_asl_raw')
;  wid = widget_button(rb, value='Browse', uname='browse_asl_raw')

  rb = widget_base(tlb, /row, /align_right)
  wid = widget_label(rb, value='SPECT (CBF):')
  wid = widget_text(rb, value='', xsize=text_size, uname='file_spect_cbf')
  wid = widget_button(rb, value='Browse', $
    uname='browse_spect_cbf', sensitive=0)

  rb = widget_base(tlb, /row, /align_right)
  wid = widget_label(rb, value='SPECT (registered):')
  wid = widget_text(rb, value='', xsize=text_size, uname='file_spect_reg')
  wid = widget_button(rb, value='Browse', uname='browse_spect_reg')

  rb = widget_base(tlb, /row, /align_right)
  wid = widget_label(rb, value='Stacks Coords:')
  wid = widget_text(rb, value='', xsize=text_size, uname='file_stack_coords')
  wid = widget_button(rb, value='Browse', $
    uname='browse_stack_coords', sensitive=0)

  rb = widget_base(tlb, space=20, column=2, /grid_layout, tab_mode=1)
  wid = widget_button(rb, value='Accept', uname='accept')
  wid = widget_button(rb, value='Cancel', uname='cancel')

  widget_control, tlb, /realize

  cd, current=orig_dir

  files = {asl_raw:'', asl_cbf:'', $
      spect_cbf:'', spect_cbf_reg:'', mr_brain:'', $
      stack_coord:''}

  info = { data_dir:data_dir, orig_dir:orig_dir, $
      files:files, canceled:0b }

  pin = ptr_new(info,/no_copy)
  widget_control, tlb, set_uvalue=pin

  ; blocking widget
  xmanager, 'kbe_get_input_files', tlb, event_handler='kbe_get_input_files_events'

  canceled = (*pin).canceled
  new_files = (*pin).files
  data_dir = (*pin).data_dir

  ptr_free, pin
  cd, orig_dir

  if canceled then return, files

  return, new_files
end
