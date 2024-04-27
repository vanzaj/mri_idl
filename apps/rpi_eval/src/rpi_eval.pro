;+
; NAME:
;   RPI_EVAL
;
; PURPOSE:  
;   GUI app for evaluation of 'rotated control' subtraction of RPI data
;   (for ISMRM 2010 abstract)
;
; NOTES:
;   data organization:
;     <study_dir>/<subj_prefix>XX/rpi_ph??.png
;     <study_dir>/<subj_prefix>XX/t2w.png
;
; HISTORY:
;   Authors: Ivan Zimine <ivan.zimine@philips.com> (ivz)
;
;   2009-09-18 ivz Original
;-


function rpi_eval_get_subj_files, study_dir, subj
;;; return specific subject files as structure
  catch, error
  if error ne 0 then begin
    catch, /cancel
    ok = error_message()
    return,0
  endif
  path = filepath(subj, root=study_dir)
  t2w = filepath('t2w.png', root=study_dir, subdir=subj)
  cd, path, current=here
  rpi = file_search('rpi*.png')
  rpi = filepath(rpi, root=path)
  cd, here
  return, {t2w:t2w,rpi:rpi}
end

function rpi_eval_get_subjects, dir, prefix=prefix
;;; get a list of file names in <dir> starting with <prefix>
  catch, error
  if error ne 0 then begin
    catch, /cancel
    ok = error_message()
    return,''
  endif
  if ~file_test(dir, /directory) then return, ''
  if n_elements(prefix) eq 0 then prefix='subj'
  fs = file_search(dir, prefix+'*', count=nf)
  if nf eq 0 then return, ''
  return, file_basename(fs)
end

pro rpi_eval_change_study_dir, ev
;;; change study root directory
  catch, error
  if error ne 0 then begin
    catch, /cancel
    ok = error_message()
    return
  endif
  widget_control, ev.top, get_uvalue=pi
  new_dir = dialog_pickfile(dialog_parent=ev.top, /directory, $
    path=(*pi).study_dir)
  if new_dir eq '' then return
  (*pi).study_dir = new_dir
  subj_list = rpi_eval_get_subjects(new_dir, prefix=(*pi).subj_prefix)
  if subj_list[0] eq '' then begin
    msg = ["Failed to get subjects list."]
    ok = dialog_message(msg, /error)
    widget_control, (*pi).wid.bLoad, sensitive=0
    return
  endif
  if ptr_valid((*pi).psubj_list) then *((*pi).psubj_list) = subj_list $
  else (*pi).psubj_list = ptr_new(subj_list)
  ; update list widget with a few additional spaces
  widget_control, (*pi).wid.lSubj, set_value=subj_list+'     '
  widget_control, (*pi).wid.bLoad, sensitive=1
end

pro rpi_eval_select, ev
  widget_control, ev.top, get_uvalue=pi
  (*pi).subj_idx = ev.index
  ;if ptr_valid((*pi).psubj_list) then $
  ;  print, (*((*pi).psubj_list))[ev.index]
end

pro rpi_eval_load, ev
;;; read images
  catch, error
  if error ne 0 then begin
    catch, /cancel
    ok = error_message()
    return
  endif
  widget_control, ev.top, get_uvalue=pi

  id = (*pi).subj_idx
  if id eq -1 then begin
    m = 'No subject selected'
    ok = dialog_message(m, dialog_parent=ev.top,/error)
    return
  endif
  widget_control, ev.top,/hour

  subj = (*((*pi).psubj_list))[id]
  fls = rpi_eval_get_subj_files((*pi).study_dir, subj)

  print, fls.rpi
  print, fls.t2w

  if file_test(fls.t2w,/read) then begin
    im = read_png(fls.t2w)
    if ptr_valid( (*pi).pt2w ) then ptr_free, (*pi).pt2w
    (*pi).pt2w = ptr_new(im,/no_copy)
    widget_control, (*pi).wid.bT2W, sensitive=1
  endif else begin
    widget_control, (*pi).wid.bT2W, sensitive=0
    if ptr_valid( (*pi).pt2w ) then ptr_free, (*pi).pt2w
  endelse

  if file_test(fls.rpi[0],/read) then begin
    nrpi = n_elements(fls.rpi)
    im = read_png(fls.rpi[0])
    s =size(im)
    rpi = bytarr(3, s[2], s[3], nrpi)
    rpi[*,*,*,0] = im
    for i=1, nrpi-1 do rpi[*,*,*,i] = read_png(fls.rpi[i])
    if ptr_valid( (*pi).prpi ) then ptr_free, (*pi).prpi
    (*pi).prpi = ptr_new(rpi,/no_copy)
    widget_control, (*pi).wid.bRPI, sensitive=1
  endif else begin
    widget_control, (*pi).wid.bRPI, sensitive=0
    if ptr_valid( (*pi).prpi ) then ptr_free, (*pi).prpi
  endelse

    widget_control, ev.top,/hour
end

pro rpi_eval_showt2w, ev
  widget_control, ev.top, get_uvalue=pi
  subj = (*(*pi).psubj_list)[(*pi).subj_idx]
  ;title = "Subject " + strtrim( (*pi).pat_id, 2 )
  title = subj + ': ' + 'T2'
  if ptr_valid( (*pi).pt2w ) then begin
    sz=size( *(*pi).pt2w, /dimen )
    wind, sz[1], sz[2], title=title, parent=ev.top
    tv, *(*pi).pt2w, /true
  endif else $
    ok = dialog_message("T2W data not available", dialog_parent=ev.top, /info)
end

pro rpi_eval_showrpi, ev
  widget_control, ev.top, get_uvalue=pi
  subj = (*(*pi).psubj_list)[(*pi).subj_idx]
  ;title = "Subject " + strtrim( (*pi).pat_id, 2 )
  title = subj + ': ' + 'RPI'
  if ptr_valid( (*pi).prpi ) then slide_trueimg, *(*pi).prpi, title=title, group_leader=ev.top $
      else  ok = dialog_message("RPI data not available", dialog_parent=ev.top, /info)
end

; exit
pro rpi_eval_exit, ev
  widget_control, ev.top, /destroy
end
pro rpi_eval_cleanup, tlb
  widget_control, tlb, get_uvalue=pi
  ptr_free, [(*pi).psubj_list, (*pi).pt2w,(*pi).prpi]
  ptr_free, pi
end
pro rpi_eval_dummy, ev
; empty handler to keep xmanager happy
end


; Main
pro rpi_eval

  study_dir = programrootdir()
  ;study_dir = '/export/data/asl/ksh/rpi_asl_ismrm10/rpi_maps'
  subj_prefix = 'subj' ; 

  ; widgets
  tlb = widget_base(/colum, title="RPI study")
  btn = widget_button(tlb, value="Change study directory", $
    event_pro="rpi_eval_change_study_dir")
  cnt = widget_base(tlb, /row) ; container
  jnk = ['Select study directory', '']
  lSubj = widget_list(cnt, value=jnk, ysize=15, event_pro="rpi_eval_select")
  ctrl = widget_base(cnt,/column)
  bLoad = widget_button(ctrl, value="Load Data", event_pro="rpi_eval_load")
  bRPI = widget_button(ctrl, value="Show RPI", event_pro="rpi_eval_showrpi")
  bT2W = widget_button(ctrl, value="Show T2W", event_pro="rpi_eval_showt2w")
  btn = widget_button(ctrl, value="Exit", event_pro="rpi_eval_exit")

  widget_control, tlb, /realize

  widget_control, bLoad, sensitive=0
  widget_control, bT2W, sensitive=0
  widget_control, bRPI, sensitive=0

; buttons IDs
wid = { lSubj:lSubj, bLoad:bLoad, bT2W : bT2W, bRPI:bRPI}

info = {pat_id:-1, $ ; patient ID
        study_dir : study_dir,  $ ; study root dir
        subj_prefix : subj_prefix,  $ ; study root dir
        subj_idx: -1   ,$
        psubj_list: ptr_new() ,$ ; widget_list value
        pt2w: ptr_new(), $ ; T2W data
        prpi: ptr_new(), $ ; RPI data
        wid : wid        $
       }

widget_control, tlb, set_uvalue=ptr_new(info,/no_copy)

xmanager, "rpi_eval", tlb, event_handler="rpi_eval_dummy",/no_block, $
    cleanup="rpi_eval_cleanup"
end
