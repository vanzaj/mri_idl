;+
; CLASS:
;   voldisp
;
;   object display to show 3D stack of contiguous slices
;
; NOTES:
;   very basic color table managment using hardcoded indicies into colors1.tbl
;   via submenu of the context popup associated to window_draw
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com> 
;   2008-03-27 ivz: Original
;   2008-04-23 ivz: all tools implemented as popup
;   2008-05-29 ivz: added ROI staff
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro voldisp::info, msg, print=print, logfile=logfile
;{{{
  compile_opt hidden
  text = self->name() + ': ' + msg
  if keyword_set(print) then begin
      print, text
      return
  endif
  if keyword_set(logfile) then begin
    if strupcase(!version.os_family) eq 'WINDOWS' then $
      logfname = 'c:\temp\voldisp.log' $
      else logfname = '/tmp/voldisp.log'
    openw, lun, logfname, /append, /get_lun
    printf, lun, text
    free_lun, lun
    return
  endif
  if obj_valid(self.statusbar) then self.statusbar->set_sbar, text $
  else ok = dialog_message(text, title=tlt, /info, dialog_parent=self.wTOP)
end
;}}}

pro voldisp::error
;{{{
  compile_opt hidden
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  ok = dialog_message(err,/error,dialog_parent=self.wTOP)
  catch,/cancel
end
;}}}

pro voldisp::change_OverIm
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
      self->error
      return
  endif

  if not ptr_valid(self.mask) then begin
    self.grOverIm->SetProperty, data=bytarr(32,32), hide=1
    self.grWin -> Draw, self.grScene
    return
  endif

  ds = size(*self.mask, /dimen)
  dat = bytarr(4, ds[0], ds[1])
  msk = (*self.mask) * 255
  dat[0,*,*] = msk
  dat[1,*,*] = msk
  dat[2,*,*] = msk
  dat[3,*,*] = msk ; alpha
  self.grOverIm->SetProperty, data=dat
  self.grWin -> Draw, self.grScene
end ;}}}

pro voldisp::change_backIm, notransformation=notransformation
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
      self->error
      return
  endif

  widget_control, self.wDraw, get_value=grWin

  sl = self->cur_sl()
  dat = self.stack->slice(sl, view_ori=self->view_ori(), /bytscl)
  self.grBackIm->SetProperty, data=dat

  ; no need to update transformation model
  ; slider, window/level...
  if keyword_set(notransformation) then begin
      grWin -> Draw, self.grScene
      return
  endif

  ; change of orientation
  fov = self.stack->fov()
  case self->view_ori() of
    'SAG' : fov_xy = fov[[1,2]]
    'COR' : fov_xy = fov[[0,2]]
    'TRA' : fov_xy = fov[[0,1]]
  endcase

  self.grImod -> Reset
  sc = fov_xy/max(fov_xy)*self.zoom
  self.grImod -> scale, sc[0], sc[1], 1
  self.grImod -> translate, 0.5*(1-sc[0]), 0.5*(1-sc[1]), 1

  grWin -> Draw, self.grScene
end ; change_backIm }}}

pro voldisp::change_stack, stack
;{{{
  compile_opt idl2, hidden
  if obj_valid(stack) then begin
    self.stack = stack
    ;slide_max = stack->dim(self.orient)-1
    slide_max = stack->dim(self->view_ori())-1
    if slide_max gt 1 then self->gui_activate, /slider $
    else self->gui_deactivate, /slider
    widget_control, self.wSlide, Set_Slider_Max=slide_max
    ; set to middle slice along all directions
    self.cur_sl = stack->dim()/2
    widget_control, self.wSlide, Set_Value=self->cur_sl()
    self->change_backIm
    self->gui_activate, /button_evt, /keyboard_evt
  endif
  if ~obj_valid(self.stack) then $
    self->gui_deactivate, /button_evt, /keyboard_evt
end ; change_stack }}}

function voldisp::cur_sl
  compile_opt hidden
  ret = self.cur_sl[self.ori_sel]
  return, ret
end

function voldisp::view_ori
  compile_opt hidden
  return, self.ori_list[self.ori_sel]
end

pro voldisp::LoadBackCT, index
  compile_opt hidden
  catch, error
  if error ne 0 then begin
      self->error
      return
  endif

  widget_control, self.wDraw, get_value=grWin
  self.grBackCT->LoadCT, index
  grWin -> Draw, self.grScene
end


;;;;;;;;;;;;;;;;;;;; ROI staff 
function voldisp::AutoROI
; make 2 circle ROIs in upper left and right side
  compile_opt hidden
  dims = self->dims(/data, /float)
  xp = dims[0] / 4 & yp = dims[1] / 4
  npts = 27
  ang = findgen(npts) * (2 * !pi / (npts-1))
  xp = cos(ang) * dims[0] / 15 + dims[0] * 0.3
  yp = sin(ang) * dims[1] / 15 + dims[1] * 0.6 
  roi1 = obj_new('IDLgrROI', xp, yp, NAME='ROI_1', $
    color=[255,0,0], thick=2)
  xp = cos(ang) * dims[0] / 15 + dims[0] * 0.7 
  yp = sin(ang) * dims[1] / 15 + dims[1] * 0.6 
  roi2 = obj_new('IDLgrROI', xp,yp, NAME='ROI_2', $
    color=[255,0,0], thick=2)

  return, [roi1, roi2]
end

pro voldisp::ResetROIs, new
  compile_opt hidden

  old = self->GetROIs(count=nOld)

  if self.debug then begin
    self->info, 'reseting rois...', /print
    self->info, '  nb of old rois:' + strtrim(total(obj_valid(old)),2),/print
    self->info, '  nb of new rois:' + strtrim(total(obj_valid(new)),2),/print
  endif

  if nOld gt 0 then begin
    self.rois->remove, /all
    obj_destroy, old
    ptr_free, self.mask
  endif

  ; do nothing if no input
  if n_elements(new) eq 0 then begin
    self->change_OverIm
    return
  endif

  if obj_valid(new[0]) eq 0 then begin
    self->info, 'invalid roi'
    self->change_OverIm
    return
  endif

  ; update ROI mask data
  dim = self->dims(/data)
  nr = n_elements(new)
  if (nr gt 1) then begin
    msk = new[0]->computemask(dimensions=dim, mask_rule=0)
    for i=1L, nr-1 do $
      msk = new[i]->computemask(mask_in=msk, mask_rule=0)
  endif else $
    msk = new->computemask(dimensions=dim, mask_rule=0)

  msk = msk gt 0
  if ptr_valid(self.mask) then *(self.mask)=msk $;ptr_free, self.mask
  else self.mask = ptr_new(msk,/no_copy)

  self.rois -> add, new

  self.grOverIm->SetProperty, hide=0
  self->change_OverIm
end

function voldisp::CopyROIs, rois, count=count
  compile_opt hidden
  null = obj_new()
  n_rois = n_elements(rois) 
  if n_rois eq 0 then return, null
  tmp = obj_new('IDL_Container')
  for i=0L, n_rois-1 do begin
    roi = rois[i]
    if obj_valid_isa(roi, 'IDLgrROI') eq 0 then continue
    roi->GetProperty, data=pts, name=name, $
      thick=thick, style=style, color=color
    new = OBJ_NEW('IDLgrROI', pts, name=name, $
      color=color, thick=thick, style=style)
    tmp->add, new
  endfor
  copy = tmp->get(/all, count=count)
  tmp->remove, /all
  obj_destroy, tmp
  return, copy
end

function voldisp::GetROIs, count=count
  compile_opt hidden
  ROIs = self.rois -> get(/all, count=count)
  if count gt 0 then return, ROIs 
  return, obj_new()
end

;;;;;;;;;;;;;;;;;;;; GUI events 

; default draw events
pro voldisp::draw_main, ev, uname
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  case ev.type of
    0: begin ; btn press {{{
      self.click.press = ev.press
      self.click.x = ev.x
      self.click.y = ev.y
      buttons = ['','left','middle','left+mid','right', 'left+right']
      btn=buttons[ev.press]
      case btn of
        'left': begin
          ;; on dbl click, expose and exit
          if ev.clicks eq 2 then begin
            self->_redraw
            return
          endif
          hit = self.grWin->Select(self.grIView,[ev.x,ev.y])
          if ~obj_valid_isa(hit[0],'IDLgrModel') then return
          xyz = self->click_coords()
          if xyz[0] eq -1 then return
          val = self.stack->value(xyz[0], xyz[1], xyz[2])
          txt = string(xyz, val, format='(%"%4d %4d %4d : %7.2f")')
          self->info, self.stack->_name() + ': '+ txt
        end
        'middle': begin
          ; window/level
          ;; on dbl click, reset L/W to 0.5/1
          if ev.clicks eq 2 then begin
            self.stack->SetProperty, win_level=0.5, win_width=1.0
            self -> change_backIm, /notransform
            return
          endif
        end
        'right': begin
          widget_displaycontextmenu, ev.id, ev.x, ev.y, self.wCM
        end
      endcase
    end ; }}}
    1: begin ; btn release {{{
      self->gui_deactivate, /motion_evt
      self.click.press = 0b
      self.click.x = -1
      self.click.y = -1
    end ;}}}
    2: begin ; pointer motion {{{
      dx = double(ev.x - self.click.x)
      dy = double(ev.y - self.click.y)
      if (dx eq 0) and (dy eq 0) then return
      self.stack->GetProperty, win_level=wL, win_width=wW, imin=imin, imax=imax
      wL += dy/2/self.win_size[1]
      wW += dx/2/self.win_size[0]
      self.stack->SetProperty, win_level=wL, win_width=wW
      range = imin + (imax-imin)*(wL + [-1,1]*wW/2)
      ;print, imin, imax, range
      self.grCTbar->SetProperty, range=range
      self-> change_backIm, /notransform
    end ;}}}
    4: begin ; expose {{{
      self->_redraw
      return
    end ;}}}
    5: begin ; ascii key ;{{{
      if ev.release then return
      char = string(ev.ch)
      ; change to SAG
      if char eq 's' then begin
        event = { WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:1L } 
        self->popup_events, event, 'ori_sag'
        return
      endif
      ; change to COR
      if char eq 'c' then begin
        event = { WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:1L } 
        self->popup_events, event, 'ori_cor'
        return
      endif
      ; change to TRA
      if char eq 't' then begin
        event = { WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:1L } 
        self->popup_events, event, 'ori_tra'
        return
      endif
    end ;}}}
    6: begin ; non ascii key {{{
      if ev.release then return
      ; left arrow -> slider
      if ev.key eq 5 then begin 
        view_ori = self.ori_list[self.ori_sel]
        slide_max = self.stack->dim(view_ori)-1
        cur_sl = self.cur_sl[self.ori_sel]
        cur_sl = (cur_sl eq 0) ? slide_max : cur_sl-1
        event={WIDGET_SLIDER, ID:0L, TOP:0L, HANDLER:0L, VALUE:cur_sl, DRAG:0}
        self.cur_sl[self.ori_sel] = cur_sl
        widget_control, self.wSlide, set_value=cur_sl
        widget_control, self.wSlide, send_event=event
        return
      endif
      ; right arrow -> slider
      if ev.key eq 6 then begin 
        view_ori = self.ori_list[self.ori_sel]
        slide_max = self.stack->dim(view_ori)-1
        cur_sl = self.cur_sl[self.ori_sel]
        cur_sl = (cur_sl eq slide_max) ? 0 : cur_sl+1
        event={WIDGET_SLIDER, ID:0L, TOP:0L, HANDLER:0L, VALUE:cur_sl, DRAG:0}
        self.cur_sl[self.ori_sel] = cur_sl
        widget_control, self.wSlide, set_value=cur_sl
        widget_control, self.wSlide, send_event=event
        return
      endif
      ; pageup -> zoom_in
      if ev.key eq 9 then begin 
        self->setproperty, zoom=self.zoom+0.2
        self->change_BackIm
        return
      endif
      ; pageup -> zoom_out
      if ev.key eq 10 then begin 
        self->setproperty, zoom=self.zoom-0.2
        self->change_BackIm
        return
      endif
    end ;}}}
  endcase
end

; slice slider
pro voldisp::slider_events, ev, uname
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  case self->view_ori() of
    'SAG' : self.cur_sl[0] = ev.value
    'COR' : self.cur_sl[1] = ev.value
    'TRA' : self.cur_sl[2] = ev.value
  endcase
  self-> change_backIm, /notransform
end

; context popup
pro voldisp::popup_events, ev, uname
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  if uname eq '' then $
    uname = widget_info(ev.id, /uname)

  case uname of
    'ori_sag' : begin ;{{{
      view_ori = self.ori_list[self.ori_sel]
      if view_ori eq 'SAG' then return ; same orientation
      self.ori_sel = where(self.ori_list eq 'SAG')
      view_ori = self.ori_list[self.ori_sel]
      slide_max = self.stack->dim(view_ori)-1
      widget_control, self.wSlide, Set_Slider_Max=slide_max
      widget_control, self.wSlide, Set_Value=(self->cur_sl() < slide_max)
      self->change_backIm
      return
    end ;}}}
    'ori_cor' : begin ;{{{
      view_ori = self.ori_list[self.ori_sel]
      if view_ori eq 'COR' then return ; same orientation
      self.ori_sel = where(self.ori_list eq 'COR')
      view_ori = self.ori_list[self.ori_sel]
      slide_max = self.stack->dim(view_ori)-1
      widget_control, self.wSlide, Set_Slider_Max=slide_max
      widget_control, self.wSlide, Set_Value=(self->cur_sl() < slide_max)
      self->change_backIm
      return
    end ;}}}
    'ori_tra' : begin ;{{{
      view_ori = self.ori_list[self.ori_sel]
      if view_ori eq 'TRA' then return ; same orientation
      self.ori_sel = where(self.ori_list eq 'TRA')
      view_ori = self.ori_list[self.ori_sel]
      slide_max = self.stack->dim(view_ori)-1
      widget_control, self.wSlide, Set_Slider_Max=slide_max
      widget_control, self.wSlide, Set_Value=(self->cur_sl() < slide_max)
      self->change_backIm
      return
    end ;}}}
    'colors_load' : begin ;{{{
      widget_control, ev.id, get_uvalue=uval
      if uval.index ge 0 then begin
        self.grBackCT->LoadCT, uval.index
      endif else begin ; load color table from lut file (MRIcro format)
        print, programrootdir()
        f = dialog_pickfile(filter='*.lut')
        if f[0] eq '' then return
        openr, lun, f[0], /get_lun
        fs = fstat(lun)
        buf = bytarr(fs.size)
        readu, lun, buf
        free_lun, lun
        nel = n_elements(buf)
        if nel mod 256 ne 0 then $
          message, lut_file + ' does not seem to be regular RGB lut with 256 entries'
        buf = reform(buf, 256, nel/256)
        self.grBackCT->Setproperty, red_values=buf[*,0], green_values=buf[*,1], blue_values=buf[*,2]
      endelse
      self->_redraw
    end ;}}}
    'color_bar' : begin
      self.stack->GetProperty, imin=imin, imax=imax, win_level=wL, win_width=wW
      range = imin + (imax-imin)*(wL + [-1,1]*wW/2)
      ;print, self.stack->_name()+': ', imin, imax, wl, ww, range
      self.grCTbar->IDLgrModel::GetProperty, hide=hide
      self.grCTbar->SetProperty, Range=range, hide=([1,0])[hide]
      self->_redraw
    end
    'roi_auto' : begin
      roi = self->AutoROI()
      self->ResetROIs, roi
    end 
    'roi_edit' : begin ;{{{
      ; get real scale factor orig_xy_pix * scale = disp_xy_pix
      winXY = self->dims(/float)
      imgXY = self->dims(/data, /float)
      scale = winXY / imgXY * self.zoom
      ; current slice
      sl = self->cur_sl()
      slice = self.stack->slice(sl, view_ori=self->view_ori(), /bytscl)
      oROIs = self->getROIs(count=nr)
      ;print, 'before edit', total(obj_valid(oROIs))
      ph_xroi, slice, scale=scale, $
        regions_in=oROIs, regions_out=nROIs, $
        group=self.wTOP, /modal, /block
      new_rois = self->copyROIs(nROIs)
      obj_destroy, nROIs
      self->ResetROIs, new_rois 
    end ;}}}
    'roi_hide' : begin
      if self.rois->count() eq 0 then return
      self.grOverIm->SetProperty, hide=1
      self->_redraw
    end
    'roi_show' : begin
      if self.rois->count() eq 0 then return
      self.grOverIm->SetProperty, hide=0
      self->_redraw
    end
    else : self->info, $
      'Unexpected event in ' + self->name() + ': ' + uname, /print
  endcase
end ; popup_events 

; routine to save current display
pro voldisp::ScreenDump, file
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  if n_elements(file) eq 0 then $
    message, "No output filename specified"
  self.grWin->GetProperty, image_data=img
  ext = strupcase(strmid(file, strlen(file)-3, 3))
  case ext of
    'PNG': write_png, file, img
    'JPG': write_jpeg, file, img, true=1
    'TIF': write_tiff, file, reverse(img,3), compression=1
    'BMP': write_bmp, file, img, /rgb
    else : message, "Unkown file extension:" + ext
  endcase
end

;;;;;;;;;;;;;;;;;;;; misc methods

function voldisp::dims, data=data, float=float
  compile_opt hidden
  mf = keyword_set(float) ? 1.0 : 1L
  if keyword_set(data) then begin
    sl = self->cur_sl()
    dat = self.stack->slice(sl, view_ori=self->view_ori())
    dims = size(dat, /dimen)
  endif else begin
    self.grWin->GetProperty, dimensions=dims
    dims = long(dims)
  endelse
  return, dims * mf
end

function voldisp::click_coords
  compile_opt hidden
  click_xy = [self.click.x, self.click.y]
  r = self.grWin->PickData(self.grIView, self.grBackIm, click_xy, xyz)
  ;if r eq 0 then return, -1
  view_ori = self->view_ori()
  dims = self.stack->dim()
  x=-1 & y=-1 & z=-1
  case view_ori of
    'SAG' : begin
      y = round(xyz[0] * (dims[1]-1) )
      z = round(xyz[1] * (dims[2]-1) )
      x = self->cur_sl()
    end
    'COR' : begin
      x = round(xyz[0] * (dims[0]-1) )
      z = round(xyz[1] * (dims[2]-1) )
      y = self->cur_sl()
    end
    'TRA' : begin
      x = round(xyz[0] * (dims[0]-1) )
      y = round(xyz[1] * (dims[1]-1) )
      z = self->cur_sl()
    end
  endcase
  return, [x,y,z]
end

pro voldisp::redraw_zoom
  compile_opt hidden
  self.grIMod -> getproperty, transform=tr
  dx = tr[3,0]/2 & dy = tr[3,1]/2
  sx = tr[3,0] & sy = tr[3,1]
  self.grIMod -> translate, -dx/2, -dy/2, 0 
  self.grIMod -> scale, self.zoom, self.zoom, 1
  self.grIMod -> translate, dx*self.zoom, dy*self.zoom, 0 
  widget_control, self.wDraw, get_value=grWin
  grWin -> Draw, self.grScene
end

pro voldisp::_redraw
  compile_opt hidden
  if ~obj_valid(self.grWin) then begin
    widget_control, self.wDraw, get_value=grWin
    self.grWin = grWin
  endif
  self.grWin -> Draw, self.grScene
end

pro voldisp::gui_activate, slider=slider, $
  button_evt=button_evt, motion_evt=motion_evt, keyboard_evt=keyboard_evt

  compile_opt hidden
  if keyword_set(slider) then $
    widget_control, self.wSlide, sensitive=1
  if keyword_set(button_evt) then $
    widget_control, self.wDraw, draw_button_events=1
  if keyword_set(motion_evt) then $
    widget_control, self.wDraw, draw_motion_events=1
  if keyword_set(keyboard_evt) then $
    widget_control, self.wDraw, draw_keyboard_events=2
end

pro voldisp::gui_deactivate, all=all, slider=slider, $
  button_evt=button_evt, motion_evt=motion_evt, keyboard_evt=keyboard_evt

  compile_opt hidden
  all = keyword_set(all)

  if (keyword_set(slider) or all) then $
    widget_control, self.wSlide, sensitive=0
  if (keyword_set(button_evt) or all) then $
    widget_control, self.wDraw, draw_button_events=0
  if (keyword_set(motion_evt) or all) then $
    widget_control, self.wDraw, draw_motion_events=0
  if (keyword_set(keyboard_evt) or all) then $
    widget_control, self.wDraw, draw_keyboard_events=0
end

function voldisp::getref, grIView=grIView, grbackim=grBackIm, $
  grImod=grImod, stack=stack

  compile_opt hidden
  ref = obj_new()
  if keyword_set(grIView) then ref=self.grIView
  if keyword_set(grBackIm) then ref=self.grBackIm
  if keyword_set(grImod) then ref=self.grImod
  if keyword_set(stack) then ref=self.stack
  return, ref
end

function voldisp::name, class=class, stack=stack

  compile_opt hidden
  if keyword_set(stack) then return, self.stack->_name()
  if self.name ne '' then return, self.name
  return, obj_class(self)
end

pro voldisp::SetpRoperty, name=name, zoom=zoom, debug=debug, statusbar=sbar

  compile_opt hidden
  if n_elements(name) gt 0 then self.name=string(name[0])
  if n_elements(zoom) gt 0 then self.zoom=zoom
  if n_elements(debug) gt 0 then self.debug = debug GT 0
  if n_elements(sbar) gt 0 then $
    if obj_valid(sbar) then self.statusbar = sbar
end

function voldisp::init, parent, xsize=xs, ysize=ys, name=name

  compile_opt idl2, hidden
  on_error, 2

  if n_elements(parent) eq 0 then return, 0
  if widget_info(parent, /valid_id) eq 0 then return, 0

  if n_elements(xs) eq 0 then xs=300
  if n_elements(ys) eq 0 then ys=300

  self.ori_list = ['SAG', 'COR', 'TRA']
  self.ori_sel = 2
  self.interp = 1
  self.zoom = 1.0
  self.win_size = [xs,ys]

  ; GUI {{{
  top = widget_base(parent, /column, uname='voldisp', /frame)
  self.wTOP = top

  bb = widget_base(top, /row, xpad=0, ypad=0)
  wdg = widget_draw(bb, xsize=xs, ysize=ys, graphics_level=2, $
      /expose_events, uvalue={obj:self, method:'draw_main'} )
  self.wDraw = wdg

  wdg = widget_slider(top, value=0, min=0, max=10, xsize=xs, $
      uvalue={obj:self, method:'slider_events'}, sensitive=0 )
  self.wSlide = wdg

  ; context menu (right-click popup)
  cmb = widget_base(self.wDraw, /context_menu)
  self.wCM = cmb
  mb = widget_button(cmb, value='Orientation', /menu)
    wdg = widget_button(mb, value='TRA', uname='ori_tra', $
      uvalue={obj:self, method:'popup_events'})
    wdg = widget_button(mb, value='SAG', uname='ori_sag', $
      uvalue={obj:self, method:'popup_events'})
    wdg = widget_button(mb, value='COR', uname='ori_cor', $
      uvalue={obj:self, method:'popup_events'})
  mb = widget_button(cmb, value='ROI', /menu)
    wdg = widget_button(mb, value='Auto', uname='roi_auto', $
      uvalue={obj:self, method:'popup_events'})
    wdg = widget_button(mb, value='Edit', uname='roi_edit', $
      uvalue={obj:self, method:'popup_events'})
    wdg = widget_button(mb, value='Hide', uname='roi_hide', $
      /separator, uvalue={obj:self, method:'popup_events'})
    wdg = widget_button(mb, value='Show', uname='roi_show', $
      uvalue={obj:self, method:'popup_events'})
  mb = widget_button(cmb, value='Colors', /menu)
    smb = widget_button(mb, value='Load', /menu)
      wdg = widget_button(smb, value='Black & White', uname='colors_load', $
        uvalue={obj:self, method:'popup_events', index:0})
      wdg = widget_button(smb, value='Temperature', uname='colors_load', $
        uvalue={obj:self, method:'popup_events', index:3})
      wdg = widget_button(smb, value='BGRY', uname='colors_load', $
        uvalue={obj:self, method:'popup_events', index:4})
      wdg = widget_button(smb, value='Rainbow', uname='colors_load', $
        uvalue={obj:self, method:'popup_events', index:13})
      wdg = widget_button(smb, value='LUT file', uname='colors_load', $
        uvalue={obj:self, method:'popup_events', index:-1})
    wdg = widget_button(mb, value='color bar', uname='color_bar', $
      uvalue={obj:self, method:'popup_events'})

  widget_control, self.wTOP, set_uvalue=self
  ;}}}

  ; grObjects {{{
  self.grScene = obj_new('IDLgrScene', color=[100,100,100])
  self.grLView = obj_new('IDLgrView', color=[200,127,127], hide=1, $
      viewplane=[0,0,1,1], location=[0,0], dimension=[1,1], units=3,$
      transparent=1)
  self.grScene->add, self.grLView
  self.grLmod = obj_new('IDLgrModel')
  self.grLview->add, self.grLmod

  self.grIView = obj_new('IDLgrView', color=[200,127,127], hide=0, $
      viewplane=[0,0,1,1], location=[0,0], dimension=[1,1], units=3)
  self.grScene->add, self.grIView
  self.grImod = obj_new('IDLgrModel', select_target=1)
  self.grIview->add, self.grImod

  ; fake data
  void = bytarr(64,64)
  self.grBackIm = obj_new('IDLgrImage', void, location=[0,0], dimension=[1,1])
  self.grBackIm->SetProperty, interpolate=1
  self.grImod->add, self.grBackIm

  self.grBackCT = obj_new('IDLgrPalette')
  self.grBackCT->LoadCT, 13
  self.grBackIm->SetProperty, palette=self.grBackCT

  self.grCTbar = obj_new('vcolorbar', palette=self.grBackCT, $
    name='colorbar')
  self.grCTbar->SetProperty, hide=1
  self.grIview->add, self.grCTbar

  ; overlay image
  void = bytarr(4, 64,64)
  self.grOverIm = obj_new('IDLgrImage', void, location=[0,0], dimension=[1,1], $
      blend_function=[3,4])
  self.grOverIm->SetProperty, hide=1
  self.grImod->add, self.grOverIm

  ; empty container to hold ROIs
  self.rois = obj_new('IDL_Container')

  ;}}}

  return, 1
end

pro voldisp::cleanup

  compile_opt hidden
  obj_destroy, self.rois
  if obj_valid(self.aroi) then obj_destroy, self.roi
  obj_destroy, self.grScene
  obj_destroy, self.grBackCT
  if ptr_valid(self.mask) then ptr_free, self.mask
  if ptr_valid(self.event) then ptr_free, self.event
end

pro voldisp__define
compile_opt idl2, hidden
; mouse button click info
click = { VOLDISP_CLICK, $
  press:0b, x:0L, y:0L }

obj = { VOLDISP,       $
  name    : '',        $ ; user provided object name
  wTOP    : 0,         $
  wDraw   : 0,         $
  wSlide  : 0,         $
  wCM     : 0,         $ ; context menu base
  grWin   : obj_new(), $ ; reference to window object
  grScene : obj_new(), $
  grIview : obj_new(), $ ; image view
  grImod  : obj_new(), $
  grLview : obj_new(), $ ; annotation layer view
  grLmod  : obj_new(), $
  grBackIm: obj_new(), $ ; background image (indexed)
  grBackCT: obj_new(), $ ; color table for backgr image
  grOverIm: obj_new(), $ ; overlay image    (true color)
  grCTbar : obj_new(), $ ; color bar object
  statusbar: obj_new(),$ ; reference to an object acting as status bar
  win_size: intarr(2) ,$ ; display window size (pixels)
  interp  : 0,         $ ; interpolate flag
  zoom    : 0.0,       $ ; current zoom
  cur_sl  : intarr(3), $ ; current slice (index for SAG,COR,TRA)
  ori_list: strarr(3), $ ; TRA/SAG/COR
  ori_sel : 0L,        $ ; current orientation selection
  stack   : obj_new(), $ ; 3D stack reference (slice_stack object)
  overlay : obj_new(), $ ; 3D stack reference (slice_stack object)
  rois    : obj_new(), $ ; container holding ROIs
  aroi    : obj_new(), $ ; reference to a current ROI
  mask    : ptr_new(), $ ; mask data displayed in grOverIm
  click   : click,     $ ; saves last mouse btn press event
  event   : ptr_new(), $ ; generic event holder
  debug   : 0b         $
}
end


; Testing {{{

;pro voldisp_test_events, ev
;widget_control, ev.id, get_uvalue=handler
;uname = widget_info(ev.id, /uname)
;if size(handler,/tname) eq 'STRUCT' then begin
;    call_method, handler.method, handler.obj, ev, uname=uname
;    return
;endif
;
;type=tag_names(ev, /structure_name)
;case type of
;'WIDGET_BUTTON':begin
;    widget_control, ev.id, get_value=name
;    if strupcase(name) eq 'EXIT' then begin
;        widget_control, ev.top, get_uvalue=dsp
;        obj_destroy, dsp
;        widget_control, ev.top,/destroy
;    endif
;end
;else : print, type
;endcase
;end
;
;pro voldisp_test
;    tlb = widget_base(title='voldisp_test', /column)
;    dsp = obj_new('voldisp', tlb)
;    wb = widget_button(tlb, value='exit')
;    widget_control, tlb, /realize
;    widget_control, tlb, set_uvalue=dsp
;    xmanager, 'voldisp_test', tlb, /no_block, event_handler='voldisp_test_events'
;end
;}}}
