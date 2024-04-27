;+
; NAME:
;     rpiview
;
; PURPOSE:
;     GUI for rpicalc
;
; MODIFICATION HISTORY:
; Author: Ivan Zimine <ivan.zimine@philips.com>
; 2007-12-09 ivz Original 
; 2008-01-10 ivz Minor bug fix with Expose event after dbl click
; 2008-01-17 ivz Added standard warning message
; 2008-01-21 ivz Changed drop lists to radio buttons
; 2008-01-22 ivz Added screenshot, zooming, 2x2 grid view for rpi
; 2008-01-28 ivz Added info button
; 2008-01-29 ivz Replaced pixmap method by direct map creation for 2x2 view
; 2008-02-14 ivz switched 2x2 to 3x3 grid view
;-

; local copy of Fanning's normalize function
function rpiview_normalize, range, position=position
;{{{
  on_error, 2
  if n_params() eq 0 then message, 'please pass range vector as argument.'

  if (n_elements(position) eq 0) then position = [0.0, 1.0] else $
    position=float(position)
  range = float(range)

  scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

  return, scale
end ;---------------------------------------------- }}}

; local error reporting
pro rpiview_errormsg
;{{{
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  ok = dialog_message(err,/error,title='rpiview')
  catch,/cancel
end ;---------------------------------------------- }}}

; load par/rec data
pro rpiview_load, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif
  ; multiple files with different territories must be selected in correct order
  ; LICA,RICA,POST or LICA-POST,RICA-POST
  file = dialog_pickfile(filter=['*.PAR','*.par'],/read, $
          get_path=path, dialog_parent=ev.top, /multiple)
  ; /multiple makes pickfile return an array
  if file[0] eq '' then return

  widget_control, ev.top, get_uvalue=pin
  widget_control, (*pin).gui.slideID, sensitive=0
  cd, path
  (*pin).calc->SetProperty, datapath=path, datafile=file

  ; get rid of the "image" of file dialog in the draw widget
  (*pin).ogr.Win->Draw, (*pin).ogr.Scene

  widget_control, /hourglass
  (*pin).calc->LoadData

  if (*pin).calc->status(/data) eq 0 then $
    message, 'Failed to read data'

  widget_control, (*pin).gui.slideID, sensitive=1

  ; default settings
  (*pin).gui.lb_drop ->SetSelection, 'normal'
  (*pin).gui.ct_drop ->SetSelection, 'individual'
  (*pin).calc -> SetProperty, label_type='normal', control_type='individual'

  pars = (*pin).calc->GetRPIpars(xres=xres, yres=yres)

  rawminmax = (*pin).calc->GetMinMax(/data)
  (*pin).var.rawmin = rawminmax[0]
  (*pin).var.rawmax = rawminmax[1]

  ; switch to "raw data"
  wid = widget_info(ev.top, find_by_uname='view/raw')
  widget_control, wid, /set_button
  (*pin).gui.min_field->Set_Value, (*pin).var.rawmin
  (*pin).gui.max_field->Set_Value, (*pin).var.rawmax
  (*pin).gui.im_view = 'view/raw'

  wid = widget_info(ev.top, find_by_uname='slide/slice')
  widget_control, wid, /set_button

  ; reset grid view
  grid_id = widget_info(ev.top, find_by_uname='view/grid')
  widget_control, grid_id, set_button=0
  widget_control, grid_id, sensitive=0
  (*pin).gui.grid_view=0b

  ; enable NAV field
  maxnav = (*pin).calc->MaxNAV()
  (*pin).gui.nav_field->SetProperty, value=maxnav, nonsensitive=0

  widget_control, (*pin).gui.slideID, set_slider_max=pars.n_slices-1
  widget_control, (*pin).gui.slideID, set_value=0
  (*pin).var.slice=0
  (*pin).var.phase=0
  (*pin).var.dyn=0

  RPIview_minmax_lbl, ev.top
  RPIview_Redraw, ev.top
end ;--------------------------------------------}}}

; basic data info
pro rpiview_datainfo, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif
  widget_control, ev.top, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then begin
    msg="Data has not been loaded"
    ok = dialog_message(msg, dialog_parent=ev.top, /info)
    return
  endif

  pars = (*pin).calc->getRPIpars()
  (*pin).calc -> GetProperty, datapath=path, datafile=files

  info = create_struct('path', path, 'files', files, pars)

  structtable, info, title='Data Info', group_leader=ev.top, xsize=300
end ;--------------------------------------------}}}

pro rpiview_subtract, ev
; {{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif
  widget_control, ev.top, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then $
    message, "No available data"

  lb_type = (*pin).gui.lb_drop->GetSelection()
  ct_type = (*pin).gui.ct_drop->GetSelection()

  (*pin).calc->SetProperty, label_type=lb_type, control_type=ct_type

  nav = (*pin).gui.nav_field->Get_Value()

  widget_control, /hourglass
  (*pin).calc->Subtract, nav=nav, status=status
  if status ne 'ok' then message, 'Subtraction failed'

  rpiminmax = (*pin).calc->GetMinMax(/rpi)
  if n_elements(rpiminmax) gt 1 then begin
    (*pin).var.rpimin = rpiminmax[0] > (0.0)
    (*pin).var.rpimax = rpiminmax[1] < (15.0)
  endif

  ; simulate view/rpi click
  wid = widget_info(ev.top, find_by_uname='view/rpi')
  if widget_info(wid,/valid_id) then begin
    widget_control, wid, sensitive=1
    widget_control, wid, /set_button
    event = {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:1}
    widget_control, wid, send_event=event
  endif
end ;--------------------------------------------}}}

; simple screen save
pro rpiview_save, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif

  f = dialog_pickfile(dialog_parent=ev.top, /write, $
                    filter=['*.png', '*.jpg', '*.tif', '*.bmp'])
  if f eq '' then return ; canceled

  widget_control, ev.top, Get_UValue=pin
  ; make sure data is displayed
  rpiview_redraw, ev.top
  (*pin).ogr.win->GetProperty, image_data=img

  ; save based on file extension
  ext = strupcase(strmid(f, strlen(f)-3, 3))
  case ext of
    'PNG': write_png, f, img
    'JPG': write_jpeg, f, img, true=1
    'TIF': write_tiff, f, reverse(img,3), compression=1
    'BMP': write_bmp, f, img, /rgb
    else : message, "Please use png, jpg or tif file extension"
  endcase
end ;--------------------------------------------}}}

; zoom in/out
pro rpiview_zoom, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif
  widget_control, ev.top, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then return

  zoom_btn = widget_info(ev.id, /uname)

  if zoom_btn eq 'zoom/in' then (*pin).var.scale += 0.1 $
  else (*pin).var.scale -= 0.1

  rpiview_redraw, ev.top
end ;--------------------------------------------}}}

; raw data or rpi view or grid view
pro rpiview_switch_image, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif
  widget_control, ev.top, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then return

  im_view = widget_info(ev.id, /uname)
  if im_view eq 'interp' then begin
    (*pin).ogr.Im->SetProperty, Interpolate=ev.select
    (*pin).ogr.Win->Draw, (*pin).ogr.Scene
    return
  endif

  if im_view eq 'view/grid' then begin
    (*pin).gui.grid_view = ev.select
    rpiview_redraw, ev.top
    return
  endif

  ; "auto" event that unsets previous button
  if ev.select eq 0 then return

  dyn_id = widget_info(ev.top, find_by_uname='slide/dyn')
  grid_id = widget_info(ev.top, find_by_uname='view/grid')

  if im_view EQ  'view/rpi' then begin
    (*pin).gui.min_field->Set_Value, (*pin).var.rpimin
    (*pin).gui.max_field->Set_Value, (*pin).var.rpimax
    widget_control, dyn_id, sensitive=0
    widget_control, grid_id, sensitive=1
  endif else begin
    (*pin).gui.min_field->Set_Value, (*pin).var.rawmin
    (*pin).gui.max_field->Set_Value, (*pin).var.rawmax
    widget_control, dyn_id, sensitive=1
    widget_control, grid_id, sensitive=0
    widget_control, grid_id, set_button=0
    (*pin).gui.grid_view=0b
  endelse

  (*pin).gui.im_view = im_view
  rpiview_minmax_lbl, ev.top
  rpiview_redraw, ev.top
end ;--------------------------------------------}}}

pro rpiview_switch_slider, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif
  widget_control, ev.top, get_uvalue=pin

  if (*pin).calc->status(/data) eq 0 then return
  pars = (*pin).calc->GetRPIpars()

  ; ignore "auto" event that unsets previous button
  if ev.select eq 0 then return

  im_slide = widget_info(ev.id, /uname)

  case im_slide of
    'slide/slice' : begin
      widget_control, (*pin).gui.slideID, set_slider_max=pars.n_slices-1
      widget_control, (*pin).gui.slideID, set_value=(*pin).var.slice
    end
    'slide/phase' : begin
      widget_control, (*pin).gui.slideID, set_slider_max=pars.n_phases-1
      widget_control, (*pin).gui.slideID, set_value=(*pin).var.phase
    end
    'slide/dyn' : begin
      widget_control, (*pin).gui.slideID, set_slider_max=pars.n_dyns-1
      widget_control, (*pin).gui.slideID, set_value=(*pin).var.dyn
    end
    else : message, 'unknown slider: ' + im_slide
  endcase
  (*pin).gui.im_slide = im_slide
  RPIview_Redraw, ev.top
END ;--------------------------------------------}}}

pro rpiview_slider, ev
;{{{
  catch, error
  if error ne 0 then begin
    rpiview_errormsg
    return
  endif

  widget_control, ev.top, get_uvalue=pin
  case (*pin).gui.im_slide of
    'slide/slice': (*pin).var.slice = ev.value
    'slide/phase': (*pin).var.phase = ev.value
    'slide/dyn': (*pin).var.dyn = ev.value
    else : message, 'unknown slider: ' + (*pin).gui.im_slide
  endcase

  rpiview_redraw, ev.top
end ;--------------------------------------------}}}

pro rpiview_nav_field, ev
;{{{
  widget_control, ev.top, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then return

  maxnav = (*pin).calc -> MaxNAV()

  if *ev.value gt maxnav then begin
    msg = "NAV should be equal or less than " + strtrim(maxnav,2)
    ok = dialog_message(msg, dialog_parent=ev.top, /error)
    (*pin).gui.nav_field -> Set_Value, maxnav
    return
  endif
end ;--------------------------------------------}}}

; min & max fields
pro rpiview_minmax_field, ev
;{{{
  widget_control, ev.top, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then return

  field = widget_info(ev.id, /uname)
  im_view = (*pin).gui.im_view

  case field of
    'field/min': begin
      if strmatch(im_view, 'view/rpi', /fold) then $
        (*pin).var.rpimin=*ev.value $
      else (*pin).var.rawmin=*ev.value
    end
    'field/max': begin
      if strmatch(im_view, 'view/rpi', /fold) then $
        (*pin).var.rpimax=*ev.value $
      else (*pin).var.rawmax=*ev.value
    end
    else : help, field
  endcase
  rpiview_redraw, ev.top
end ;--------------------------------------------}}}

; labels showing real data min/max
pro rpiview_minmax_lbl, tlb
;{{{
  widget_control, tlb, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then return
  im_view = (*pin).gui.im_view

  if im_view eq 'view/rpi' then minmax = (*pin).calc->GetMinMax(/rpi) $
  else minmax = (*pin).calc->GetMinMax(/data)

  minval = string(minmax[0], format='("(",F8.1," )")')
  wid = widget_info(tlb, find_by_uname='data/min')
  widget_control, wid, set_value=minval
  maxval = string(minmax[1], format='("(",F8.1," )")')
  wid = widget_info(tlb, find_by_uname='data/max')
  widget_control, wid, set_value=maxval
end ;--------------------------------------------}}}

; default Draw events
pro rpiview_draw_main, ev
;{{{
  catch, error
  if error ne 0 then begin
      rpiview_errormsg
      return
  endif

  widget_control, ev.top, get_uvalue=pin
  ; process events only when inside the view
  ;hit=(*pin).ogr.Win->Select((*pin).ogr.View, [ev.x,ev.y], dimensions=[1,1])
  ;if not obj_valid(hit) then return

  eventNames = ['DOWN','UP','MOTION','SCROLL','EXPOSE','KEY', 'MOD']

  if strmatch(eventNames[ev.type], 'EXPOSE') then begin
    rpiview_redraw, ev.top
    return
  endif

  return

  btnNames = ['NONE','LEFT','MIDDLE','RIGHT']


  case eventNames[ev.type] of
    'DOWN' : begin
      if ev.clicks eq 2 then begin
        rpiview_redraw, ev.top
        return
      endif
      hit = (*pin).ogr.Win->PickData( (*pin).ogr.View, $
                      (*pin).ogr.Im, [ev.x, ev.y], xyz)
      if hit ne -1 then begin
        (*pin).dsp.xyz = xyz
        rpiview_plotsig, ev.top
      endif
    end ; Mouse DOWN
    'UP' : begin
        ;print, 'press:', ev.press, 'release:', ev.release
    end ; Mouse UP
    'MOTION' : begin
      hit = (*pin).ogr.Win->PickData((*pin).ogr.View, $
                      (*pin).ogr.Im, [ev.x, ev.y], xyz)
      if hit ne -1 then begin
        x=floor(xyz[0]) & y=floor(xyz[1])
        if ptr_valid((*pin).im) then $
          val = (*(*pin).im)[x,y] $
        else val = 'unknown'
        str = string(x,y, format='(%"(%d,%d): ")')
        str += strtrim(val,2)
        Widget_Control, (*pin).wID.sbar, set_value=str
      endif
    end ; MOTION
    'KEY' : begin
      if ev.press then begin
      key = strlowcase(string(ev.ch))
      if key eq '+' then begin
        sc = ((*pin).var.scale+0.2) < 5
        loc = (1.0 - sc)/2
        (*pin).ogr.View->SetProperty, Location=[loc,loc], dimension=[sc,sc]
        (*pin).var.scale=sc
        rpiview_redraw, ev.top
      endif
      if key eq '-' then begin
        sc = ((*pin).var.scale-0.2) > 0.1
        loc = (1.0 - sc)/2
        (*pin).ogr.View->SetProperty, Location=[loc,loc], dimension=[sc,sc]
        (*pin).var.scale=sc
        rpiview_redraw, ev.top
      endif
      endif
    end ; KEY
    'MOD' : 
    else : ; do noting
  endcase
end ;--------------------------------------------}}}

; screen update
pro rpiview_redraw, tlb
;{{{
  widget_control, tlb, get_uvalue=pin
  if (*pin).calc->status(/data) eq 0 then begin
    (*pin).ogr.Win->Draw, (*pin).ogr.Scene
    return
  endif

  sl = (*pin).var.slice
  ph = (*pin).var.phase
  dy = (*pin).var.dyn

  ; 2x2 grid view for RPI
  if (*pin).gui.grid_view eq 1 then begin
    pars = (*pin).calc -> GetRPIPars(xres=xres, yres=yres, $
        n_slices=n_slices, n_phases=n_phases)

    mmin=(*pin).var.rpimin
    mmax=(*pin).var.rpimax

    disp = bytarr(xres*3, yres*3, 3)

    ; show 9 slices of current phase
    if (*pin).gui.im_slide eq 'slide/slice' then begin
      first = long(sl)
      last = long((first+8) < (n_slices-1))
      for i=first, last do begin
        map = (*pin).calc->RPImap(slice=i, phase=ph)
        x=((i-first) mod 3)*xres
        y=((i-first) / 3)* yres
        disp[x:x+xres-1,y:y+yres-1,*] = bytscl(map, min=mmin, max=mmax)
      endfor
      status = 'slices: ' + strtrim(first,2) + ' - ' + strtrim(last,2)
      status += ' phase: ' + strtrim(ph,2) + ' '
    endif else begin ; show 9 phases of current slice
      first = long(ph)
      last = long((first+8) < (n_phases-1))
      for i=first, last do begin
        map = (*pin).calc->RPImap(slice=sl, phase=i)
        x=((i-first) mod 3)*xres
        y=((i-first) / 3)* yres
        disp[x:x+xres-1,y:y+yres-1,*] = bytscl(map, min=mmin, max=mmax)
      endfor
      status = 'slice: ' + strtrim(sl,2) + ' '
      status += ' phases: ' + strtrim(first,2) + ' - ' + strtrim(last,2)
    endelse
    (*pin).ogr.Im -> SetProperty, data=disp
  endif

  case (*pin).gui.im_view of
    'view/raw' : begin
      map = (*pin).calc->rawdata(sl,ph,dy)
      mmin=(*pin).var.rawmin
      mmax=(*pin).var.rawmax
      status = 'slice: ' + strtrim(sl,2)
      status += ' phase: ' + strtrim(ph,2)
      status += ' dynamic: ' + strtrim(dy,2)
      (*pin).ogr.Im -> SetProperty, Data=bytscl(map, min=mmin, max=mmax)
    end
    'view/rpi' : begin
      if (*pin).gui.grid_view eq 1 then break
      map = (*pin).calc->RPImap(slice=sl, phase=ph)
      mmin=(*pin).var.rpimin
      mmax=(*pin).var.rpimax
      status = 'slice: ' + strtrim(sl,2)
      status += ' phase: ' + strtrim(ph,2)
      (*pin).ogr.Im -> SetProperty, Data=bytscl(map, min=mmin, max=mmax)
    end
    else :
  endcase

  ; center the image
  scale = (*pin).var.scale
  (*pin).ogr.Win -> GetProperty, dimensions=w_dim
  wxs = double(w_dim[0]) & wys = double(w_dim[1])

  if wxs gt wys then begin
    sc=wys/wxs
    loc=[ (1-sc*scale)/2, (1.0-scale)/2]
    dim = [sc, 1.0]*scale
  endif else begin
    sc=wxs/wys
    loc=[ (1.0-scale)/2, (1-sc*scale)/2]
    dim = [1.0, sc]*scale
  endelse

  (*pin).ogr.view -> SetProperty, location=loc, dimension=dim
  (*pin).ogr.Win -> Draw, (*pin).ogr.Scene
  widget_control, (*pin).gui.sbarid, set_value=status
end ;--------------------------------------------}}}

pro rpiview_tlb_resize, ev
;{{{
  widget_control, ev.top, get_uvalue=pin

  ev_name = tag_names(ev, /structure_name)
  if strmatch(ev_name, 'fsc_droplist_event',/fold) then begin
    lb_type = (*pin).gui.lb_drop->GetSelection()
    ct_type = (*pin).gui.ct_drop->GetSelection()
    (*pin).calc->SetProperty, label_type=lb_type, control_type=ct_type
    (*pin).gui.nav_field -> SetProperty, value=(*pin).calc->maxnav()
    return
  endif

  dsp_xsize = ev.x - (*pin).gui.cb_size[0]
  dsp_ysize = ev.y - (*pin).gui.cb_size[1]
  (*pin).ogr.Win->SetProperty, Dimension=[dsp_xsize, dsp_ysize]
  widget_control, (*pin).gui.sbarID, xsize=dsp_xsize-1
  rpiview_redraw, ev.top
end ;--------------------------------------------}}}

pro rpiview_quit, ev
;{{{
  widget_control, ev.top, /destroy
end ;--------------------------------------------}}}

pro rpiview_cleanup, tlb
;{{{
  widget_control, tlb, get_uvalue=pin
  if ptr_valid(pin) eq 1 then begin
    cd, (*pin).var.orig_path
    obj_destroy, (*pin).calc
    obj_destroy, (*pin).ogr.container
    ptr_free, (*pin).im
    ptr_free, pin
  endif
end ;--------------------------------------------}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                      Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rpiview, dsp_xsize=xsize, dsp_ysize=ysize, title=title, $
    group_leader=group, calc=calc

  catch, error
  if error ne 0 then begin
    ;catch, /cancel
    ;print, !error_state.msg
    rpiview_errormsg
    if obj_valid(ocont) then obj_destroy, ocont
    return
  endif

  version='0.9.2'
  app_path = programrootdir()

  pp='C:\DBIEX_data'
  if file_test(pp,/directory) then cd, pp

  image = bytarr(128,128,3)
  imgSize = size(image, /dimensions)

  ; grObjects {{{
  ; container for easy clean up.
  oCont = obj_new('IDL_Container')

  oIm = obj_new('IDLgrImage', image, interleave=2, $
      dimensions=imgSize[0:1],order=1)
  oIm->GetProperty, Xrange=xr, Yrange=yr
  oIm->SetProperty, Xcoord_Conv=RPIview_Normalize(xr), $
      Ycoord_Conv=RPIview_Normalize(yr)

  oMod = obj_new('IDLgrModel')
  oMod->Add, oIm

  oView = obj_new('IDLgrView', color=[127,127,127], viewplane=[0,0,1,1],  $
      location=[0,0], dimensions=[1,1], units=3)

  oView->Add, oMod

  oScene = obj_new('IDLgrScene', Color=[127,127,127])
  oScene->Add, oView
  oCont->Add, oScene
  ;}}}

  ; default image display size
  if n_elements(dsp_xsize) eq 0 then dsp_xsize = 400
  if n_elements(dsp_ysize) eq 0 then dsp_ysize = 400
  if n_elements(title) eq 0 then title = 'RPIview'
  title += ' (ver. ' + version + ')'


  tlb = widget_base(/column, title=title, TLB_Size_Events=1, $
      /base_align_left) 

  ; tools bar
  tb = widget_base(tlb, /row, xpad=1, ypad=1)
  bmpfile = filepath('open.bmp', root_dir=app_path, subdir=['bitmaps'])
  btn = widget_button(tb, value=bmpfile, /bitmap, $
      tooltip='Load Data', Event_Pro='RPIview_Load')
  bmpfile = filepath('itl.bmp', root_dir=app_path, subdir=['bitmaps'])
  btn = widget_button(tb, value=bmpfile, /bitmap, $
      tooltip='Data Info', Event_Pro='RPIview_DataInfo')
  bmpfile = filepath('camera.bmp', root_dir=app_path, subdir=['bitmaps'])
  btn = widget_button(tb, value=bmpfile, /bitmap, $
      tooltip='Screen Shot', Event_Pro='RPIview_Save')
  bmpfile = filepath('zoom_in.bmp', root_dir=app_path, subdir=['bitmaps'])
  btn = widget_button(tb, value=bmpfile, /bitmap, uname='zoom/in', $
      tooltip='Zoom In', Event_Pro='RPIview_Zoom')
  bmpfile = filepath('zoom_out.bmp', root_dir=app_path, subdir=['bitmaps'])
  btn = widget_button(tb, value=bmpfile, /bitmap, uname='zoom/out', $
      tooltip='Zoom Out', Event_Pro='RPIview_Zoom')
  nexb = widget_base(tb, /nonexclusive, xpad=0, ypad=0)
  bmpfile = filepath('grid2x2.bmp', root_dir=app_path, subdir=['bitmaps'])
  btn = widget_button(nexb, value=bmpfile, /bitmap, uname='view/grid', $
      tooltip='3x3 view', Event_Pro='RPIview_switch_image', sensitive=0)

  ; container base
  contb = widget_base(tlb, /row, /base_align_left)

  ; display window
  db = widget_base(contb, /column, xpad=0, ypad=0)
  drawID = Widget_Draw(db, XSize=dsp_xsize, YSize=dsp_ysize, Graphics_Level=2, $
     /Button_Events, /Motion_Events, Keyboard_Events=2, /Expose_Events,$
     Event_Pro='RPIview_Draw_Main', Retain=0)
  sbarID = Widget_Label(db, Value='', /dynamic_resize, /align_left,$
      xsize=dsp_xsize-2, /sunken_frame)

  ; controls
  cb = widget_base(contb, /column, /tab_mode, xpad=0, ypad=0)

  ; label/control types
  lcbase = widget_base(cb, /column, /base_align_right, /frame, xpad=0)
  lb_types=['normal', 'dual']
  ; fsc_droplist return an object
  lb_drop = fsc_droplist(lcbase, Title='Label', Value=lb_types, Index=0, $
      uname='drop/label', /align_right)
  ct_types=['individual', 'rotating']
  ct_drop = fsc_droplist(lcbase, Title='Control', Value=ct_types, Index=0, $
      uname='drop/control', /align_right)
  jnk = widget_base(lcbase, /row, /base_align_center)
  ; nb of averages (pairs per territory to average)
  navID = fsc_field(jnk, title='NAV:', Value=20, Xsize=4, /positive, /cr_only, $
      object=nav_field, name='field/nav', event_pro='RPIview_NAV_Field',$
      /nonsensitive)
  ;widget_control, navID, map=0
  btn = widget_button(jnk, Value='Subtract', /align_center, $
      Event_Pro='RPIView_Subtract')
  ; for cleanup
  oCont -> add, lb_drop
  oCont -> add, ct_drop

  ; raw data or RPI
  jnk = widget_base(cb, /column, /frame)
  lbl = widget_label(jnk, value='View', /align_left)
  exb = widget_base(jnk, /exclusive, /row)
  btn = widget_button(exb, value='Raw Data', uname='view/raw', $
      Event_Pro='RPIview_switch_image')
  widget_control, btn, /set_button
  btn = widget_button(exb, value='RPI', uname='view/rpi', $
      Event_Pro='RPIview_switch_image', sensitive=0)
  ; interpolate display image
  nexb = widget_base(jnk, /nonexclusive)
  btn = widget_button(nexb, value='interpolate', uname='interp', $
      Event_Pro='RPIview_switch_image')

  ; slider
  jnk = widget_base(cb, /column, /frame)
  exb = widget_base(jnk, /exclusive, row=2)
  btn = widget_button(exb, value='slice', uname='slide/slice', $
      Event_Pro='RPIview_switch_slider')
  widget_control, btn, /set_button
  btn = widget_button(exb, value='phase', uname='slide/phase', $
      Event_Pro='RPIview_switch_slider')
  btn = widget_button(exb, value='dynamic', uname='slide/dyn', $
      Event_Pro='RPIview_switch_slider')

  slideID = Widget_Slider(jnk, Value=0, Min=0, Max=2, $
      sensitive=0, Event_Pro='RPIview_Slider')

  ; min/max for bytscl (display)
  jnk = widget_base(cb, /column, /base_align_left, /frame)
  ; !!! fsc_field uses name instead of uname
  jj = widget_base(jnk,/row)
  minID = fsc_field(jj, title='Min:', Value=0.0, Xsize=6, /cr_only, $
      object=min_field, name='field/min', event_pro='RPIview_MinMax_Field')
  ; to show real min after changes
  minlbl = widget_label(jj, value='',/dynamic_resize, uname='data/min')
  jj = widget_base(jnk,/row)
  maxID = fsc_field(jj, title='Max:', Value=0.0, Xsize=6, /cr_only, $
      object=max_field, name='field/max', event_pro='RPIview_MinMax_Field')
  maxlbl = widget_label(jj, value='',/dynamic_resize, uname='data/max')

  ; exit button
  jnk = widget_base(cb, /column, xpad=0, ypad=10, /align_center)
  quiter = widget_button(jnk, Value='  Exit  ', $
      Event_Pro='RPIview_Quit')

  ; update xsizes
  geo = widget_info(cb, /geometry)
  widget_control, lcbase, xsize=geo.xsize
  widget_control, slideID, xsize=geo.xsize

  ; center application on the monitor
  mon_size = get_screen_size() ; monitor screen size
  app_size = widget_info(tlb, /geometry) ; application size
  xoff = 0 > ((mon_size[0] - app_size.scr_xsize)/2)
  yoff = 0 > ((mon_size[1] - app_size.scr_ysize)/2)
  widget_control, tlb, xoffset=xoff, yoffset=yoff

  ; size of control panel(+ padding) for TLB resizing
  cb_size = [app_size.scr_xsize,app_size.scr_ysize] - [dsp_xsize, dsp_ysize]

  ; Realize the widgets. Get the object window.
  Widget_Control, tlb, /Realize
  Widget_Control, drawID, Get_Value=oWin
  oCont->Add, oWin

  ; warning message
  warn_txt = ["CAUTION - Investigation Device.",$
              "Limited by Federal Law to investigational use only."]
  ok = dialog_message(warn_txt, dialog_parent=tlb, /information)

  ; info structure for program information.
  oGr = { im : oIm, model: oMod, $
      view: oView, scene: oScene, $
      container: oCont, win: oWin}

  gui = { lb_drop:lb_drop, ct_drop:ct_drop, $
      im_view:'view/raw', im_slide:'slide/slice', $
      nav_field: nav_field, $
      min_field: min_field, max_field:max_field, $
      slideID:slideID, sbarID:sbarID, $
      cb_size:cb_size, grid_view:0b }

  cd, current=orig_path
  var = { scale: 1.0, slice: 0, phase:0, dyn:0, $
      rpimin:0.0, rpimax:0.0, rawmin:0.0, rawmax:0.0, $
      orig_path:orig_path }

  dsp = {left_down : 0b, right_down : 0, key:'', x0:0, y0:0, xyz:fltarr(3)}

  calc = obj_new('rpicalc')

  info = { $
    calc : calc,$
    im : ptr_new(), $ ; current image
    ogr  : oGr, $ ; graphic objects
    var  : var, $ ; variables
    gui  : gui, $ ; some widget IDs
    dsp  : dsp  $ ; display variables
 }

  widget_control, tlb, set_uvalue=ptr_new(info, /no_copy)

  xmanager, 'rpiview', tlb, /no_block, cleanup='rpiview_cleanup', $
      event_handler="rpiview_tlb_resize", group_leader=group

end

