;+
; NAME:
;   KBE_QASL_SPECT 
;
; PURPOSE:  
;   GUI app for simultaneous display of CBF maps obtained by QUASAR and SPECT
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com> (ivz)
;
;   2008-03-28 ivz Original (derived from objappex)
;   2008-04-24 ivz Display with voldisp object
;-
; Copyright (C) 2007, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 


; application exit
pro kbe_qasl_kill, tlb
;{{{
  widget_control, tlb, get_uvalue=self
  obj_destroy, self
end
;}}}

; wrapper for events
pro kbe_qasl_events, ev
;{{{
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  widget_control, ev.top, get_uvalue=self
  widget_control, ev.id, get_uvalue=handle
  uname = widget_info(ev.id, /uname)
  ;; use generic method if uvalue of the widget is undefined
  if size(handle,/tname) eq 'UNDEFINED' then begin
    self->EventHandler, ev, uname
    return
  endif
  call_method, handle.method, handle.obj, ev, uname
end
;}}}

; local error reporting
pro kbe_qasl::error
;{{{
  compile_opt hidden
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  ok = dialog_message(err,/error,dialog_parent=self.tlb)
  catch,/cancel
end
;}}}

; for debugging
pro kbe_qasl::info, text, print=print
;{{{
compile_opt hidden
  tlt = 'KBE_qASL info'
  if keyword_set(print) then begin
    print, tlt + ': ' + text
    return
  endif
  ok = dialog_message(text, title=tlt, /info, dialog_parent=self.tlb)
end
;}}}

; set status bar
pro kbe_qasl::set_sbar, text
;{{{
  compile_opt hidden
  widget_control, self.sbar, set_value=text
end
;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; default handler
pro kbe_qasl::EventHandler, ev, uname
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  case uname of
    'file/exit': begin
      self->saveConfig
      widget_control, self.tlb, /destroy
      return
    end
    'file/new': begin
      cd, current=here
      if ~file_test(self.dir_data, /directory) then path=here $
      else path = self.dir_data

      fls = kbe_get_input_files(group_leader=self.tlb, data_dir=path, $
            canceled=canceled)
      if canceled then return
      cd, path
      self.dir_data = path
      self->appReset
      self->LoadData, fls
      ;self->LoadTestData
      return
    end
    'file/save/left_dsp': begin
      filt = ['*.png','*.jpg', '*.bmp', '*.tif']
      file = dialog_pickfile(dialog_parent=self.tlb, /write, $
          filter=filt, path=self.dir_data, get_path=path)
      if file eq '' then return
      self.dsp1->ScreenDump, file
      return
    end
    'file/save/right_dsp': begin
      filt = ['*.png','*.jpg', '*.bmp', '*.tif']
      file = dialog_pickfile(dialog_parent=self.tlb, /write, $
          filter=filt, path=self.dir_data, get_path=path)
      if file eq '' then return
      self.dsp2->ScreenDump, file
      return
    end
    'file/save/reg_spect': begin
      stack=self.stacks->get(name="SPECT_CBF_REG")
      if ~obj_valid(stack) then return
      fl = dialog_pickfile(dialog_parent=self.tlb, /write, $
          filter='*.hdr', path=self.dir_data, get_path=path)
      if fl eq '' then return ; canceled
      fl = file_basename(fl, '.hdr')
      fl = file_basename(fl, '.img')
      fl = filepath(fl+'.hdr', root=path)
      writeanz, stack->data(), fl, voxel=stack->voxel()
      return
    end
    'file/restore': begin
      self->info, "coming soon"
    end
    'drop/left_stack': begin
      widget_control, ev.id, get_value=stack_names
      self->Change_Stack, self.dsp1, stack_names[ev.index]
      return
    end
    'drop/right_stack': begin
      widget_control, ev.id, get_value=stack_names
      self->Change_Stack, self.dsp2, stack_names[ev.index]
      return
    end
    else : begin
      self->info, 'unknow uname: '+uname, /print
      return
    end
  endcase
  msg = 'unknow event: ' + tag_names(ev, /structure)
  self->info,  msg, /print
end ; EventHandler }}}

pro kbe_qasl::ToolsImgReg, ev, uname
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    if obj_valid(air) then obj_destroy, air
    return
  endif
  case uname of
    'reg/air/spect_3d' : begin
      self->logStatus
      spect = self.stacks->get(name='SPECT_CBF')
      if ~obj_valid(spect) then message, "SPECT data is not available"
      anat = self.stacks->get(name='MR_BRAIN')
      if ~obj_valid(anat) then message, "MR_BRAIN data is not available"
      ;bin_air = self.dir_prog + path_sep() + 'bin_air'
      ;air = obj_new('ph_ImgReg', air_dir=bin_air, tmp_dir=self.dir_tmp)
      ;if ~obj_valid(air) then return 
      self.air -> SetAirParameters, model=6, costf=1, convth=0.0001, $
          t1=10, t2=7, b1='3.0 3.0 3.0', p1='256', rmode=1
      widget_control, /hourglass
      self.air -> Align, ref_stack=anat, tst_stack=spect, status=status
      if strlen(status[0]) GT 0 then message, status[0]
      res = self.air->reslice(status=status)
      if strlen(status[0]) GT 0 then message, status[0]
      if obj_valid(res) then begin
        ; remove an old stack with the same name
        try = self.stacks->get( name=res->_name() )
        if obj_valid(try) then begin
          self.stacks->remove, try
          obj_destroy, try
        endif
        self.stacks->add, res
        self->UpdateDropStacks, right=res->_name()
        ;obj_destroy, air
      endif
      return
    end
    'reg/man/spect_asl' : begin
      ; put registered SPECT into ASL space
      asl = self.stacks->get(name='ASL_CBF')
      if ~obj_valid(asl) then message, "ASL data is not available"
;      fova = asl->fov()
;      dima = asl->dim()
;      voxa = asl->voxel()
      offca = self.offc_asl
      spect = self.stacks->get(name='SPECT_CBF_REG')
      if ~obj_valid(spect) then message, "SPECT_REG data is not available"
;      fovb = spect->fov()
;      dimb = spect->dim()
;      voxb = spect->voxel()
      offcb = self.offc_mr3d
      ; crop SPECT
      offc_diff = offcb - offca
;      crop = (fovb - fova) / voxb
;      crop_left = round(crop/2 + offc_diff/voxb)
;      crop_right = round(crop/2 - offc_diff/voxb)
;      end_idx = dimb - crop_right - 1
;      new = ( spect->data() )[crop_left[0]:end_idx[0], $ 
;                   crop_left[1]:end_idx[1], $
;                   crop_left[2]:end_idx[2] ]
;      new = congrid(new, dima[0],dima[1],dima[2], /center, /interp)
;
;      obj = obj_new('slice_stack', new, voxel=voxa, name='SPECT_CBF_REG_ASL', $
;                  win_level=0.5, win_width=1.0)
;      
;      self.stacks->add, obj
      self.air->SetProperty, stack_ref=asl, stack_tst=spect
      res = self.air -> ManualAir(trans=-offc_diff, new_name='SPECT_ASL_REG', status=status)
      if strlen(status[0]) GT 0 then message, status[0]
      if obj_valid(res) then begin
        ; remove an old stack with the same name
        try = self.stacks->get( name=res->_name() )
        if obj_valid(try) then begin
          self.stacks->remove, try
          obj_destroy, try
        endif
        ; update CBF range
        res->GetProperty, imin=imin, imax=imax
        win_width = (self.cbf_max - self.cbf_min) / (imax-imin)
        win_level = ((self.cbf_max - self.cbf_min)/2 - imin) / (imax-imin)
        res->SetProperty, win_width=win_width, win_level=win_level
        self.stacks->add, res
        self->UpdateDropStacks, right=res->_name()
      endif
      return
    end
    else : begin
      self->info, 'unknow uname: '+uname, /print
      return
    endelse
  endcase
end ; }}}

pro kbe_qasl::ToolsROIcopyLR, ev, uname
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  rois = self.dsp1->GetROIs(count=count)
  if count gt 0 then self.dsp2->ResetROIs, self.dsp2->CopyROIs(rois) $
  else self->info, 'No ROIs available'
end ;}}}

pro kbe_qasl::ToolsROIstats, ev, uname
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  text = ''
  dsp = self.dsp1
  ROIs = dsp->GetROIs(count=nROIs)
  if nROIs gt 0 then begin
    stack = dsp->GetRef(/stack)
    slice = stack->slice(dsp->cur_sl(), view_ori=dsp->view_ori())
    dims = size(slice,/dimen)
    line='ROI statistics for '+ stack->_name() + ', ' + $
      'slice=' + strtrim(dsp->cur_sl(),1) + ', ' + dsp->view_ori() 
    text = [text, line]
    line = string(' ', 'mean', 'stdev', 'npix', $
        format='(%"%6s %8s %8s %6s")')
    text = [text, line]
    for i=0L, nROIs-1 do begin
      ROIs[i]->GetProperty, name=roi_name
      msk = ROIs[i]->computemask(dimensions=dims, mask_rule=2)
      IMAGE_STATISTICS, slice, mask=msk, count=roi_count, $
          mean=roi_mean, stddev=roi_std
      line = string(roi_name, roi_mean, roi_std, roi_count, $
        format='(%"%s: %8.2f %8.2f %6d")')
      text = [text, line]
    endfor
  endif else text = [text, 'No ROIs defined in '+dsp->name()]
  text = [text, '']
  dsp = self.dsp2
  ROIs = dsp->GetROIs(count=nROIs)
  if nROIs gt 0 then begin
    stack = dsp->GetRef(/stack)
    slice = stack->slice(dsp->cur_sl(), view_ori=dsp->view_ori())
    dims = size(slice,/dimen)
    line='ROI statistics for '+ stack->_name() + ', ' + $
      'slice=' + strtrim(dsp->cur_sl(),1) + ', ' + dsp->view_ori() 
    text = [text, line]
    line = string(' ', 'mean', 'stdev', 'npix', $
        format='(%"%6s %8s %8s %6s")')
    text = [text, line]
    for i=0L, nROIs-1 do begin
      ROIs[i]->GetProperty, name=roi_name
      msk = ROIs[i]->computemask(dimensions=dims, mask_rule=2)
      IMAGE_STATISTICS, slice, mask=msk, count=roi_count, $
          mean=roi_mean, stddev=roi_std
      line = string(roi_name, roi_mean, roi_std, roi_count, $
        format='(%"%s: %8.2f %8.2f %6d")')
      text = [text, line]
    endfor
  endif else text = [text, 'No ROIs defined in '+dsp->name()]
  xdisplayfile, 'junk.txt', text=text[1:*], done='Done', $
    title='qASL_SPECT ROI statistics', group=self.tlb,/block
end ;}}}

pro kbe_qasl::Change_Stack, disp, stack_name
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  if ~obj_valid_isa(disp, 'voldisp') then $
    message, '_invalid display object_'
  ;; get current stack
  stack = disp->GetRef(/stack)
  ;; do nothing if currrent stack has the same name
  if StrMatch(stack->_name(), stack_name) then return

  new_stack = self.stacks->Get(name=stack_name)
  if ~obj_valid(new_stack) then $
    message, 'Failed to change stack named: '+stack_name

  disp->change_stack, new_stack
end; }}}

pro kbe_qasl::LoadData, fst
;{{{
; fls_struct is {stack_name:file_name}
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  if size(fst,/tname) NE 'STRUCT' then return
  tags = tag_names(fst)
  for i=0L, n_elements(tags)-1 do begin
    name = tags[i]
    fn = fst.(i)
    if fn eq '' then continue
    ext = strmid(fn, strpos(fn,'.', /reverse_search)+1,3)
    win_width = 1.0
    win_level = 0.5
    case strlowcase(ext) of
      'par' : begin
        ;print, name, ' parrec'
      end
      'hdr' : begin
        im = readanz(fn, hdr=hdr)
        if n_elements(im) eq 1 then $
          message, 'Failed to read '+fn
        vox = hdr.dime.pixdim[1:3]
        ;if strmatch(name, 'ASL_CBF') then begin
          ;; ivz CBF maps from EasyMRI are reversed
          ;im=reverse(im, 2) < 300
          ;; wrong voxel info
          ;if vox[0] gt 10 then vox[0:1] = 240./64
        ;endif
        ; reset default width/level for ASL and REG_SPECT CBF maps
        if (stregex(name, 'CBF') gt -1) then begin
          imin = min(im, max=imax)
          win_width = (self.cbf_max - self.cbf_min) / (imax-imin)
          win_level = ((self.cbf_max - self.cbf_min)/2 - imin) / (imax-imin)
        endif
        ;; ivz hardcoded rebin 3D if inplane resolution is < 0.5mm 
;        if strmatch(name, 'MR_BRAIN') then begin
;          if (vox[0] lt 0.5) and (vox[1] lt 0.5) then begin
;            ds = size(im,/dimen)
;            im=rebin(im, ds/2)
;            vox = vox*2
;          endif
;        endif
        obj = obj_new('slice_stack', im, voxel=vox, name=name, $
          win_level=win_level, win_width=win_width)
        self.stacks -> add, obj
      end
      'ima' : begin ; SPECT
        im = read_spect_dcm(fn, voxel=vox)
        if n_elements(im) eq 1 then $
          message, 'Failed to read '+fn
        imin = min(im, max=imax)
        ; SPECT is multiplied by 100
        if imax gt 1000 then begin
          im = float(im)/100
          imin = float(imin)/100
          imax = float(imax)/100
        endif
        ; reset default width/level
        win_width = (self.cbf_max - self.cbf_min) / (imax-imin)
        win_level = ((self.cbf_max - self.cbf_min)/2 - imin) / (imax-imin)
        obj = obj_new('slice_stack', im, voxel=vox, name=name, $
          win_level=win_level, win_width=win_width)
        self.stacks -> add, obj
      end
      'txt' : begin ; ASL & 3D stack coordinates (offset and angulation)
        ; ugly hack to get offecenter coords into the program
        buf = ''
        openr, lun, fn, /get_lun  
        while ~eof(lun) do begin
          readf, lun, buf
          if strlen(buf) eq 0 then continue
          try = strsplit(buf,':', /extract)
          if n_elements(try) NE 3 then continue
          if strmatch(try[0], 'ASL') AND (stregex(try[1],'Off Centre') gt -1) then begin
            self.offc_asl = strsplit(strtrim(try[2],2), ' ',/extract)
            self.offc_asl = self.offc_asl[[2,0,1]] ; ap,fh,rl -> rl,ap,fh
          endif
          if strmatch(try[0], '3D') AND (stregex(try[1],'Off Centre') gt -1) then begin
            self.offc_mr3d = strsplit(strtrim(try[2],2), ' ',/extract)
            self.offc_mr3D = self.offc_mr3d[[2,0,1]] ; ap,fh,rl -> rl,ap,fh
          endif
        endwhile
        ;print, 'asl:', self.offc_asl
        ;print, '3D:', self.offc_mr3d
        free_lun, lun
      end
      else : message, 'Inknown file type: ' + ext
    endcase
  endfor
  if fst.asl_cbf ne '' then begin
    title=self.title+ ': ' + file_basename(fst.asl_cbf)
    widget_control, self.tlb, tlb_set_title=title
  endif
  self->updateDropStacks, left='ASL_CBF', right='SPECT_CBF_REG'
end; }}}

pro kbe_qasl::LoadTestData
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  ;path = 'C:\data\ASL_RPI\kbe'
  path = 'H:\ivz\imgs'
  fasl = path+'\080204_asl_spect\asl1_CBF_5.hdr'
  im = readanz(fasl, hdr=h)
  im = reverse(im,2)
;  ds = size(im,/dimen)
;  for i=0,ds[2]-1 do im[*,*,i]  = (i+1)*10
;  for i=0, 3 do im[*,(i+1)*(ds[1]-2)/4,*]=0
;  for i=0, 7 do im[(i)*(ds[0]-1)/8, *, *]=0

  stack = obj_new('slice_stack', im, voxel=[3.75,3.75,8], name='qASL_CBF')
  self.dsp1->change_stack, stack
  self.stacks->add, stack
  fspect = path+'\080204_asl_spect\reg_test\080204_spect.hdr'
  im = float(readanz(fspect))/100
  stack = obj_new('slice_stack', im, voxel=[4.8,4.8,4.8], name='SPECT_CBF')
  self.dsp2->change_stack, stack 
  self.stacks->add, stack
  stack_names = self.stacks->GetNames(/noempty)
  if n_elements(stack_names) gt 0 then begin
    wdg = widget_info(self.tlb, find_by_uname='drop/left_stack')
    widget_control, wdg, set_value=stack_names, set_droplist_select=0, $
                    sensitive=1
    wdg = widget_info(self.tlb, find_by_uname='drop/right_stack')
    widget_control, wdg, set_value=stack_names, set_droplist_select=0, $
                    sensitive=1
  endif
end; }}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro kbe_qasl::UpdateDropStacks, left=left, right=right
;{{{
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->error
    return
  endif
  ; drop widgets IDs
  dropL = widget_info(self.tlb, find_by_uname='drop/left_stack')
  dropR = widget_info(self.tlb, find_by_uname='drop/right_stack')

  stack_names = self.stacks->GetNames(/noempty)

  if n_elements(stack_names) EQ 0 then begin
    widget_control, dropL, set_value='', sensitive=0
    widget_control, dropR, set_value='', sensitive=0
    return
  endif

  widget_control, dropL, set_value=stack_names, sensitive=1
  widget_control, dropR, set_value=stack_names, sensitive=1
                
  if n_elements(left) NE 0 then begin
    pos = where(strmatch(stack_names, left), cn)
    if cn GT 0 then begin
      left_stack = self.stacks->get(name=left)
      self.dsp1 -> change_stack, left_stack
      widget_control, dropL, set_droplist_select=pos
    endif else begin
      self.dsp1 -> change_stack, self.stacks->get(name=stack_names[0])
      widget_control, dropL, set_droplist_select=0
    endelse
  endif

  if n_elements(right) NE 0 then begin
    pos = where(strmatch(stack_names, right), cn)
    if cn GT 0 then begin
      right_stack = self.stacks->get(name=right)
      self.dsp2 -> change_stack, right_stack
      widget_control, dropR, set_droplist_select=pos
    endif else begin
      self.dsp2 -> change_stack, self.stacks->get(name=stack_names[0])
      widget_control, dropR, set_droplist_select=0
    endelse
  endif
end
;}}}

pro kbe_qasl::appReset
;{{{
  compile_opt hidden
  empty_stack = self.trash->get(name='EMPTY_STACK')
  if ~obj_valid(empty_stack) then begin
    empty_stack = obj_new('slice_stack', bytarr(32,32,1), name='EMPTY_STACK')
    self.trash->add, empty_stack
  endif
  self.dsp1->change_stack, empty_stack
  self.dsp1->resetROIs
  self.dsp1 -> gui_deactivate, /all
  self.dsp2->change_stack, empty_stack
  self.dsp2->resetROIs
  self.dsp2 -> gui_deactivate, /all
  stack_obj = self.stacks->get(/all, count=count)
  if count gt 0 then obj_destroy, stack_obj
;  obj_destroy, self.stacks
;  self.stacks = obj_new('ph_Container')
  self->UpdateDropStacks
;  wdg = widget_info(self.tlb, find_by_uname='drop/left_stack')
;  widget_control, wdg, sensitive=0, set_value=''
;  wdg = widget_info(self.tlb, find_by_uname='drop/right_stack')
;  widget_control, wdg, sensitive=0, set_value=''
  self.offc_asl *= 0
  self.offc_mr3d *= 0
  self->set_sbar, 'Reset done. Ready...'
end
;}}}

function kbe_qasl::getref, dsp1=dsp1, dsp2=dsp2, stacks=stacks
;{{{
  compile_opt hidden
  ref = obj_new()
  if keyword_set(dsp1) then ref=self.dsp1
  if keyword_set(dsp2) then ref=self.dsp2
  if keyword_set(stacks) then ref=self.stacks
  return, ref
end ; getref() }}}

pro kbe_qasl::loadConfig
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  conf_file = filepath('config.txt', sub=['etc'], root=self.dir_prog)
  if ~file_test(conf_file, /read) then $
    message, "config.txt not found"

  openr, lun, conf_file, /get_lun
  line=''
  while ~eof(lun) do begin
    readf, lun, line
    ; skip comments
    if strmid(line, 0,1) eq '#' then continue
    try = strsplit(line, '=', /extract)
    ; skip lines that are not "par = value"
    if n_elements(try) eq 1 then continue
    cpar = strtrim(strlowcase(try[0]), 2)
    cval = strtrim(try[1],2)
    if cpar eq 'data_dir' then $
      if file_test(cval, /directory) then self.dir_data = cval
    if cpar eq 'tmp_dir' then $
      if file_test(cval, /directory) then self.dir_tmp = cval
    if cpar eq 'cbf_min' then $
      self.cbf_min = float(cval)
    if cpar eq 'cbf_max' then $
      self.cbf_max = float(cval)
  endwhile
  free_lun, lun
end ;}}}

pro kbe_qasl::saveConfig
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  conf_file = filepath('config.txt', sub=['etc'], root=self.dir_prog)
  ;if ~file_test(conf_file, /read) then return
  openw, lun, conf_file, /get_lun
  printf, lun, 'tmp_dir = ' + self.dir_tmp
  printf, lun, 'data_dir = ' + self.dir_data
  printf, lun, 'cbf_min = ' + string(self.cbf_min, format='(F5.1)')
  printf, lun, 'cbf_max = ' + string(self.cbf_max, format='(F5.1)')
  free_lun, lun
end ; }}}

pro kbe_qasl::LogStatus
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  log_file = filepath('qasl_spect.log', sub=['etc'], root=self.dir_prog)
  openw, lun, log_file, /get_lun
  if obj_valid(self.air) then begin
    self.air->GetProperty, dir_air=dir_air, dir_tmp=dir_tmp
    printf, lun, 'data_air = ' + dir_air
    printf, lun, 'data_tmp = ' + dir_tmp
  endif else printf, lun, "Invalid AIR (ImgReg) object"
  stacks = self.stacks->getnames()
  printf, lun, "Avalable data: " + strjoin(stacks, ' ')
  free_lun, lun
end ; }}}

; GUI layout
function kbe_qasl::GUI
;{{{
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

;{{{ widgets
  self.title = 'qASL-SPECT'+' (ver. '+self.version+')'
  tlb = widget_base(title=self.title, /column, mbar=tlm, $
        tlb_size_events=0, /base_align_left, xpad=0, ypad=0)
  self.tlb = tlb
  ; menu
  menu = widget_button(tlm, value='File')
  wdg = widget_button(menu, value='New', $
        uname='file/new')
  subm = widget_button(menu, value='Save', /menu)
  wdg = widget_button(subm, value='Left display', $
        uname='file/save/left_dsp')
  wdg = widget_button(subm, value='Right display', $
        uname='file/save/right_dsp')
  wdg = widget_button(subm, value='Registered SPECT', /separator, $
        uname='file/save/reg_spect')
  ;wdg = widget_button(menu, value='Restore', uname='file/restore')
  wdg = widget_button(menu, value='Exit', /separator, $ 
        uname='file/exit')

  menu = widget_button(tlm, value='Tools')
  subm = widget_button(menu, value='Register', /menu)
  wdg = widget_button(subm, value='SPECT to 3D', $
        uname='reg/air/spect_3d', sensitive=0, $
        uvalue={obj:self, method:'ToolsImgReg'})
  wdg = widget_button(subm, value='SPECT to ASL', $
        uname='reg/man/spect_asl', sensitive=0, $
        uvalue={obj:self, method:'ToolsImgReg'})
  ;wdg = widget_button(subm, value='3D to ASL', uname='reg/air/3d_asl')
  subm = widget_button(menu, value='ROIs', /menu)
  wdg = widget_button(subm, value='Copy to Right', $
        uname='roi/copy_l_r', $
        uvalue={obj:self, method:'ToolsROIcopyLR'})
  wdg = widget_button(subm, value='ROI statistics', $
        uname='roi/stats', $
        uvalue={obj:self, method:'ToolsROIstats'})
  ;subm = widget_button(menu, value='Create mask', /menu)
  ;wdg = widget_button(subm, value='Brain (3D)', uname='mask/brain_3d')
  ;wdg = widget_button(subm, value='Brain (EPI)', uname='mask/brain_epi')
  ;wdg = widget_button(subm, value='GM (EPI)', uname='mask/gm_epi')
  ;wdg = widget_button(subm, value='WM (EPI)', uname='mask/gm_epi')

  ; main layout base
  base = widget_base(tlb, /row, xpad=0, ypad=0, uname='base/main')

  ; 1st display
  subb = widget_base(base, /column, xpad=0, ypad=0, $
        uname='base/left_dsp')
  stack_names=['ASL_CBF', 'reg_SPECT_CBF']
  wdg = widget_droplist(subb, title='Image', value=stack_names, $
          uname='drop/left_stack', sensitive=0)

  dsp = obj_new('voldisp', subb, xsize=winX, ysize=winY, name='LEFT DISP')
  if ~obj_valid(dsp) then $
    message, 'failed to init 1st voldisp'
  self.dsp1 = dsp
  self.trash->add, self.dsp1

  ; 2d display
  subb = widget_base(base, /column, xpad=0, ypad=0, $
        uname='base/right_dsp')
  stack_names=['ASL_CBF', 'reg_SPECT_CBF']
  wdg = widget_droplist(subb, title='Image', value=stack_names, $
        uname='drop/right_stack', sensitive=0)

  dsp = obj_new('voldisp', subb, xsize=winX, ysize=winY, name='RIGHT DISP')
  if ~obj_valid(dsp) then message, 'failed to init 2d voldisp'
  self.dsp2 = dsp
  self.trash->add, self.dsp2

  ; status bar
  self.sbar = widget_label(tlb, value='Ready', /dynamic_resize, /sunken_frame)

  widget_control, tlb, /realize
  geo = widget_info(base, /geometry)
  widget_control, self.sbar, xsize=geo.xsize
  ;}}}

  self.dsp1->setproperty, statusbar=self
  self.dsp2->setproperty, statusbar=self

  widget_control, self.tlb, set_uvalue=self
  xmanager, 'kbe_qasl', self.tlb, /no_block, $
    event_handler='kbe_qasl_events', cleanup='kbe_qasl_kill'
  return, 1
end ; GUI }}}

function kbe_qasl::init
;{{{
  compile_opt idl2,hidden
  catch, error
  if error NE 0 then begin
    self->error
    obj_destroy, self.trash
    obj_destroy, self.stacks
    obj_destroy, self.air
    return, 0
  endif

  self.version = '0.4.6'

  ; init containers
  self.stacks = obj_new('ph_Container')
  if ~obj_valid(self.stacks) then return, 0
  self.trash = obj_new('ph_Container')
  if ~obj_valid(self.trash) then return, 0

  self.dir_prog = programrootdir()
  self->LoadConfig

  if file_test(self.dir_tmp,/directory) eq 0 then $
    message, 'check tmp_dir in config.txt'

  ; hasn't been set in config.txt
  if self.cbf_max eq 0.0 then $
    self.cbf_max = 100.0

  ;air_bin = self.dir_prog + 'bin_air'
  ff = file_search(self.dir_prog, 'alignlinear*', count=nf)
  if nf gt 0 then begin
    air_bin = file_dirname(ff[0])
    self.air = obj_new('ph_ImgReg', air_dir=air_bin, tmp_dir=self.dir_tmp)
    if ~obj_valid(self.air) then $
      message, 'Failed to init ph_ImgReg'
  endif

  ok = self->gui()
  if ~ok then $
    message,  'Failed to start GUI'
  self.running = 1b
  return, 1
  end
;}}}

pro kbe_qasl::cleanup
;{{{
  compile_opt idl2,hidden
  obj_destroy, self.stacks
  obj_destroy, self.trash
  obj_destroy, self.air
end ;}}}

pro kbe_qasl__define
  compile_opt idl2,hidden

  obj = { KBE_QASL    ,$
    version: ''       ,$ ; prog version
    title:   ''       ,$ ; GUI title
    tlb:     0        ,$
    sbar:    0        ,$ ; status bar id (widget_label)
    running: 0b       ,$ ; running flag
    dsp1:    obj_new(),$ ;
    dsp2:    obj_new(),$ ;
    stacks:  obj_new(),$ ; ph_Container
    trash:   obj_new(),$ ; ph_Container
    air:     obj_new(),$ ; ph_ImgReg (AIR calc)
    cbf_min: 0.0 ,$ ; min CBF for display (config)
    cbf_max: 0.0 ,$ ; max CBF for display (config)
    offc_asl: [0.0,0.0,0.0], $
    offc_mr3d: [0.0,0.0,0.0], $
    dir_prog:''       ,$ ; program directory
    dir_data:''       ,$ ; data directory
    dir_tmp: ''        $ ; temporary directory
  }
end

pro kbe_qasl_spect, app=app
  app = obj_new('kbe_qasl')
end
