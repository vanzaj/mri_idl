;+
; NAME:
;   qasltime
; 
; PURPOSE:
;   GUI app for exploring QUASAR ASL data
;   mainly for obtaining time to peak from deltaM (crush/non-crush) data
; 
; CALLING SEQUENCE:  
;   qasltime, app=app
;
;   app is the reference to running application object
;   useful for debugging purposes with app->set, debug=1
;   or getting full access to data and qaslcalc object
;   calc = app->get('calc')
;   p = calc->getData('deltaMcr')
;
; NOTES:
;   This GUI is written as an object application
;   'self' is stored in TLB's uvalue i.e. can be accessed from any event
;   GUI widgets (buttons, lists et al) should have a specific uname 
;   or uvalue={obj:obj_ref, method:'method'} (obj_ref can be 'self')
;   if uvalue is defined then we try to do 'call_method, uv.method,uv.obj,ev'
;   otherwise we try default EvtHandler, ev, uname
;
;   image/plot displays are implemented using direct graphics
;   (current assumption: device, decomposed=0)
;
; TODO:
;   Need to add color handling both for image and plot
;
;---------------
; MODIFICATION HISTORY:
;
; Author: Ivan Zimine <ivan.zimine@philips.com> (ivz)
;
; Nov 2009    ivz   derived from objappex.pro
; Feb 2010    ivz   data load moved completely to qaslcalc__define
; Mar 2010    ivz   imageType switch and display 
;---------------
; Copyright (C) 2007 Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

pro qaslTime::dialog, msg, error=error, warning=warning
;; shortcut for dialog_message()
;{{{
  compile_opt idl2,hidden
  error = keyword_set(error)
  warning = keyword_set(warning)
  ; dialog_message() shows a warning only if error or info are unused!
  if warning then begin
    ok = dialog_message(msg, dialog_parent=self.gui.tlb)
    return
  endif
  ; info=0 == /error
  ok = dialog_message(msg, dialog_parent=self.gui.tlb, info=~error)
end;}}}

pro qaslTime::error
;; local error reporting
;{{{
  compile_opt hidden
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  catch,/cancel
  self->dialog, err, /error
end ;}}}

;;;
;;; GUI layout 
;;;
function qaslTime::GUI
;{{{
  compile_opt idl2,hidden
  ; gui is already running
  if self.running then return, 1

  catch, error
  if error ne 0 then begin
    self->error
    self.running=0
    return, 0b
  endif

  tlb = widget_base(title=self.title, /column, mbar=tlm,  uname='base/tlb',$
        tlb_size_events=1, /base_align_left, xpad=0, ypad=0)
  self.gui.tlb = tlb
  ; File menu
  menu = widget_button(tlm, value='File')
  wdg = widget_button(menu, value='Open', uname='file/open')
  wdg = widget_button(menu, value='Save', uname='file/save', sensitive=0)
  wdg = widget_button(menu, value='Exit', uname='file/exit', /separator)
  ; Tools menu
  menu = widget_button(tlm, value='Tools')
  wdg = widget_button(menu, value='Gauss filter', uname='tool/gauss', sensitive=0)
  wdg = widget_button(menu, value='Make TTP map', uname='tool/TTPmap', sensitive=0)

  ; main layout base
  mbase = widget_base(tlb, /row, xpad=0, ypad=0, uname='base/main')

  ; left & right subbases
  lbase = widget_base(mbase, /column, xpad=0, ypad=0, uname='base/left')
  rbase = widget_base(mbase, /column, xpad=0, ypad=0, uname='base/right')

  ; image display
  subb = widget_base(lbase, /row, xpad=0, ypad=0, uname='base/left')
  dXs = self.gui.drawXsize
  wdg = widget_draw(subb, xsize=dXs, ysize=dXs, $
        /expose_events, uname='draw/image', $
        uvalue={obj:self,method:'drawImage'})
  
  subsb = widget_base(subb, /column, xpad=0, ypad=0, uname='base/ctrls', $
        /base_align_left, frame=0)
  wdg = widget_droplist(subsb, value=self.gui.imgType, $
        sensitive=0, dynamic_resize=1, uname='drop/imgType', $
        uvalue={obj:self,method:'dropImgType'})
  ; sliders for slice, phase and dynamics
  slBase = widget_base(subsb, /row, xpad=0, ypad=0)
  jnk = widget_base(slBase, /column, xpad=0, ypad=0)
  wdg = widget_label(jnk, value='slice')
  wdg = widget_slider(jnk, /vertical, uname='slider/slice', $
        ysize=dXs*0.6, maximum=1, sensitive=0)
  jnk = widget_base(slBase, /column, xpad=0, ypad=0)
  wdg = widget_label(jnk, value='phase')
  wdg = widget_slider(jnk, /vertical, uname='slider/phase', $
        ysize=dXs*0.6, maximum=1, sensitive=0)
  jnk = widget_base(slBase, /column, xpad=0, ypad=0)
  wdg = widget_label(jnk, value='dynamic')
  wdg = widget_slider(jnk, /vertical, uname='slider/dyn', $
        ysize=dXs*0.6, maximum=1, sensitive=0)
  ; TODO level/width or min/max for display image bytscale
  
  ; plot display (ysize = 75% of xsize)
  subb = widget_base(lbase, /row, xpad=0, ypad=0)
  wdg = widget_draw(subb, xsize=dXs, ysize=dXs*0.75, $
        uname='draw/plot')
  ; time-course fit button (jnk base to avoid button ysize match plot ysize)    
  jnk = widget_base(subb, /row, xpad=0, ypad=0)
  wdg = widget_button(jnk, value = 'Fit TC', uname='button/fitTC', $
        uvalue={obj:self,method:'fitTimeCourse'}, sensitive=0)

  ; status bar
  sbar = widget_label(tlb, /align_left, /dynamic_resize, $
    /sunken_frame, uname='label/statusbar')
  ;-------- end of widgets def

  ; create widgets hierarchy
  widget_control, tlb, /realize
  geo = widget_info(mbase, /geometry)
  widget_control, sbar, xsize=geo.xsize
  ; save the difference between TLB xsize and img display xsize
  self.gui.ctlXsize = geo.xsize - dXs
  
  ; calc maximum draw X size
  scr = get_screen_size()
  tlmGeo = widget_info(tlm, /geometry)
  sbarGeo = widget_info(tlm, /geometry)
  extraYsize =  tlmGeo.ySize + sbarGeo.ySize + 5
  self.gui.drawMaxSize = fix(float(scr[1] - extraYsize)/1.75)

  widget_control, self.gui.tlb, set_uvalue=self
  xmanager, 'qaslTime', self.gui.tlb, /no_block, $
    event_handler='qaslTime_events', cleanup='qaslTime_kill'
  return, 1b
end ; }}}

function qasltime::getWidgetSize, uname
;; return widget size by uname
;; return [xsize,ysize]
;{{{
  wid = self->GetWidgetID(uname)
  if widget_info(wid, /valid_id) then begin
    geo = widget_info(wid, /geometry)
    return, fix([geo.xsize, geo.ysize])
  endif
  return, 0
end;}}}

function qasltime::getWidgetID, uname
;; return widget ID by uname
;{{{
  compile_opt hidden
  if n_elements(uname) eq 0 then return, -1L
  return, widget_info(self.gui.tlb, find_by_uname=uname)
end;}}}

pro qaslTime::setStatus, text
;; update status bar
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  if n_elements(text) eq 0 then text='...'
  wdg = self->getWidgetID('label/statusbar')
  widget_control, wdg, set_value=text
end ;}}}

;;;
;;; events
;;;
pro qaslTime_kill, tlb
;; application exit
;{{{
  widget_control, tlb, get_uvalue=self
  self->debug, 'application exit'
  obj_destroy, self
end ;}}}

pro qaslTime_events, ev
;; main wrapper for events
;{{{
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  widget_control, ev.top, get_uvalue=self
  widget_control, ev.id, get_uvalue=handle
  uname = widget_info(ev.id, /uname)
  self->debug, 'got events from '+uname
  ; dedicated method
  if size(handle,/tname) eq 'STRUCT' then $
    call_method, handle.method, handle.obj, ev $
  else $ ; default handler
    self->EventHandler, ev, uname
end ;}}}

pro qaslTime::eventHandler, ev, uname
;; default handler
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    self->setStatus, 'check results...'
    return
  endif

  case uname of
    'base/tlb': self->resizeTLB, ev
    'file/open': begin
      widget_control, self->getWidgetID('drop/imgType'), sensitive=0
      widget_control, /hourglass
      self->setStatus, 'Reading data...'
      dataPath=self.hash->get('dataPath')
      cd, current=thisDir
      if file_test(dataPath, /directory) then cd, dataPath
      ret = self.calc->LoadData()
      ;TODO reset 'dataPath'
      cd, thisDir
      ; cancelled or read error
      if ~ret then return
      ; get PAR file info
      par = self.calc->get('info')
      ; average and subract
      self->setStatus, 'Mean volume...'
      ret = self.calc->redoMaskVol()
      ret = self.calc->redoMeanVol()
      self->setStatus, 'Averaging...'
      ret = self.calc->redoSimpleAverage()
      if ret eq 0 then message, 'Avergaring failed'
      self->setStatus, 'Subracting...'
      ret = self.calc->redoSubtraction()
      if ret eq 0 then message, 'Subtraction failed'
      ; reset sliders max values
      widget_control, self->getWidgetID('slider/slice'), $
          set_slider_max=(par->get('nb_slices')-1)
      widget_control, self->getWidgetID('slider/phase'), $
          set_slider_max=(par->get('nb_phases')-1)
      widget_control, self->getWidgetID('slider/dyn'), $
          set_slider_max=(par->get('nb_dynamics')/2-1)
      ; reactivate image type selection
      wid=self->getWidgetID('drop/imgType')
      widget_control, wid, sensitive=1
      self.gui.imgType = 'meanVol'
      self->updateImgTypes
      self->changeImgType
      self->redraw
      self->setStatus, 'Ready...'
      ; enable mouse press events
      widget_control, self->getWidgetID('draw/image'), $
          draw_button_events=1
      ; activate Tools buttons  
      widget_control, self->getWidgetID('tool/gauss'), /sensitive  
      widget_control, self->getWidgetID('tool/TTPmap'), /sensitive  
    end
    'file/save': begin
      info = self.calc->get('info')
      dataFile = info->get('data_file')
      dataDir = file_dirname(dataFile)
      file = dialog_pickfile(dialog_parent=self.gui.tlb, filt='*.hdr',$
        path=dataDir, title='Select file to write TTP map')
      if file eq '' then return
      ; save TTP as analyze
      p = self.calc->getData('TTPmap')
      ; convert to integer with max bound
      ttp = round(*p)
      vx = max(info->get('pixel_spacing_x'))
      vy = max(info->get('pixel_spacing_y'))
      vz = max(info->get('slice_th'))+max(info->get('slice_gap'))
      writeanz, ttp, file, voxel=[vx,vy,vz]
    end
    'file/exit': begin
      self->debug, 'app destroy'	    
      widget_control, self.gui.tlb, /destroy
    end
    'tool/gauss' : begin
      widget_control, /hourglass
      self->setStatus, 'Gaussian smoothing...'
      ret = self.calc->GaussSmooth()
      if ret ne 1 then message, 'GaussSmooth() failed'
      ; redo average and subract
      self->setStatus, 'Mean volume...'
      ret = self.calc->redoMaskVol()
      ret = self.calc->redoMeanVol()
      self->setStatus, 'Averaging...'
      ret = self.calc->redoSimpleAverage()
      if ret eq 0 then message, 'Avergaring failed'
      self->setStatus, 'Subracting...'
      ret = self.calc->redoSubtraction()
      self.gui.imgType = 'meanVol'
      self->updateImgTypes
      self->changeImgType
      self->redraw
      self->setStatus, 'Ready...'
    end
    'tool/TTPmap' : begin
      widget_control, /hourglass
      self->setStatus, 'gammavar fitting...'
      ret = self.calc->redoFitTTP()
      if ret ne 1 then message, 'redoFitTTP() failed'
      self.gui.imgType = 'TTPmap'
      self->updateImgTypes
      self->changeImgType
      self->redraw
      self->setStatus, 'Ready...'
      widget_control, self->getWidgetID('file/save'), /sensitive
    end
    'slider/slice': begin
      self.gui.slice = ev.value
      self->redraw
    end
    'slider/phase': begin
      self.gui.phase = ev.value
      self->redraw
    end
    'slider/dyn': begin
      self.gui.dyn = ev.value
      self->redraw
    end
    else : begin
      self->debug, 'unknow uname in EventHandler: '+uname
    end
  endcase
end ; }}}

pro qaslTime::resizeTLB, ev
;; app resizing
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  ; keep dsp xsize btw 256 and maxSize
  newx = 256 > fix(ev.x - self.gui.ctlXsize) < self.gui.drawMaxSize
  self.gui.drawXsize = newx
  wdg = self->getWidgetID('draw/image')
  widget_control, wdg, draw_xsize=newx, draw_ysize=newx
  wdg = self->getWidgetID('draw/plot')
  widget_control, wdg, draw_xsize=newx, draw_ysize=newx*0.75
  ;wdg = self->getWidgetID('drop/scroll_type')
  ;widget_control, wdg, scr_ysize=fix(newx*0.6)
  wdg = self->getWidgetID('label/statusbar')
  widget_control, wdg, scr_xsize=(ev.x > (256+self.gui.ctlXsize))
  self->redraw
end; }}}

pro qaslTime::dropImgType, ev
;; events from drop/imgType
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  widget_control, ev.id, get_value=imgTypes
  dp = self.calc->getData(imgTypes[ev.index])
  if size(dp,/tname) ne 'POINTER' then $
    message, 'unknown image type:' + imgTypes[ev.index]

  self.gui.imgType = imgTypes[ev.index]
  self->changeImgType
  self->redraw
  widget_control, self->getWidgetID('button/fitTC'), sensitive=0
end ;}}}

pro qaslTime::drawImage, ev
;;  mouse events from draw/image 
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  ; ignore mouse wheel events
  if ev.press gt 4 then return

  ; event types
  evType = (['press','release', 'mousemove', 'viewport',$
            'expose','key_ascii', 'key_modifier'])[ev.type]
  mouseBtn = (['xxx', 'left','middle','xxx','right'])[ev.press]

  case evType of
    'press' : begin
      case mouseBtn of
        'left' : begin 
          self.gui.mouseX = ev.x
          self.gui.mouseY = ev.y
          val = self->getValueUnderMouse(statusString=str)
          self->setStatus, str
          ; activate mouse motion events
          widget_control, ev.id, draw_motion_events=1
        end ; left btn press  
        else : ; pass 
      endcase
    end
    'release' : begin
      ; de-activate mouse motion events
      widget_control, ev.id, draw_motion_events=0
      xy = self->convertDevToData(ev.x, ev.y)
      crd = [xy, self.gui.slice]
      self->debug, string(crd, format='(%"crd: [%d,%d,%d]")')
      ; get the time-course [n_phases,3] (time, signal, error)
      dat = self.calc->getTimeCourse(crd, 'deltaMcr')
      if n_elements(dat) gt 1 then begin
        p = self.hash->get('Xarr')
        *p = dat[*,0]
        p = self.hash->get('Yarr')
        *p = dat[*,1]
        p = self.hash->get('Earr')
        *p = dat[*,2]
        self->replot
        widget_control, self->getWidgetID('button/fitTC'), sensitive=1
      endif else begin
        widget_control, self->getWidgetID('button/fitTC'), sensitive=0
      endelse
    end
    'mousemove' : begin
      self.gui.mouseX = ev.x
      self.gui.mouseY = ev.y
      val = self->getValueUnderMouse(statusString=str)
      self->setStatus, str
    end
    'expose' : begin
      ;TODO do we ever get here?
      self->debug, 'inside expose'
    end
    else : return
  endcase
end ;}}}

pro qaslTime::changeImgType
;; reset imgSize, imgRange, and sliders sensitivity
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  dp = self.calc->getData(self.gui.imgType)
  sz = size(*dp, /dimension)
  nDims = n_elements(sz)

  self.gui.imgSize = [sz[0], sz[1]]
  dataMin = min(*dp, max=dataMax)
  self.gui.imgRange = [dataMin, dataMax]

  ; [de]activate sliders
  sensArr = [0b,0b,0b] ; array for sliders sensitivity setting
  wSlice = self->getWidgetID('slider/slice')
  wPhase = self->getWidgetID('slider/phase')
  wDyn = self->getWidgetID('slider/dyn')

  sensArr[0] = (nDims ge 3) ? 1b : 0b 
  sensArr[1] = (nDims ge 4) ? 1b : 0b 
  sensArr[2] = (nDims ge 5) ? 1b : 0b 

  widget_control, wSlice, sensitive=sensArr[0]
  widget_control, wPhase, sensitive=sensArr[1]
  widget_control, wDyn, sensitive=sensArr[2]
  self->setStatus, 'Ready...'
end; }}}

pro qaslTime::fitTimeCourse, ev
;; fit current Yarr with gammavar function
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  pX = self.hash->get('Xarr')
  pY = self.hash->get('Yarr')
  pE = self.hash->get('Earr')
  
  npts = n_elements(*pY)
  if npts eq 1 then return
  if total(*pY) eq 0 then return

  tc = *pY
  m1 = mean(tc[0:npts/2])
  m2 = mean(tc[npts/2+1:npts-1])
  if m1 le m2 then begin
    self->debug, 'can not fit'
    return
  endif

  pf0 = [max(tc), 15., 100.]
  pf = mpfitfun('gammavar', *pX, tc, *pE, pf0, /quiet)
  self->debug, string(pf, format='(%"fit: %5.1f %6.2f %6.2f")')
  self->debug, string(pf[1]*pf[2], format='(%"ttp: %7.2f")')
  xx = rebin(*pX, npts*4)
  tc_fit = gammavar(xx, pf)
  widget_control, self->getWidgetID('draw/plot'), get_value=winID
  wset, winID
  ploterr, *pX, *pY, *pE
  oplot, xx, tc_fit, thick=2
end; }}}

pro qaslTime::redraw
;; update image display
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  dp = self.calc->getData(self.gui.imgType)
  case size(*dp, /n_dimen) of
    3 : im = (*dp)[*,*,self.gui.slice]
    4 : im = (*dp)[*,*,self.gui.slice,self.gui.phase]
    6 : im = (*dp)[*,*,self.gui.slice,0,self.gui.dyn,self.gui.phase]
    else : message, "unknown data size"
  endcase
  dXs = self.gui.drawXSize
  im = congrid(reform(im,/over),dXs,dXs, /center, /interp)
  widget_control, self->getWidgetID('draw/image'), get_value=winID
  wset, winID
  tv, bytscl(im, min=self.gui.imgRange[0], max=self.gui.imgRange[1])
  ; clear plot display
  widget_control, self->getWidgetID('draw/plot'), get_value=winID
  wset, winID
  erase
end; }}}

pro qaslTime::replot
;; update plot display
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  pX = self.hash->get('Xarr')
  pY = self.hash->get('Yarr')
  pE = self.hash->get('Earr')
  widget_control, self->getWidgetID('draw/plot'), get_value=winID
  wset, winID
;  plot, *pX, *pY, xtitle='time (ms)'
  ploterr, *pX, *pY, *pE/2
end; }}}

;;;
;;; internal utility methods
;;;

function qasltime::getValueUnderMouse, statusString=statusString
;; get data value under mouse cursor
;; statusString is for statusBar; C-formated as (%d,%d): %f
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, -9999
  endif
  xy = self->convertDevToData(self.gui.mouseX, self.gui.mouseY)
  dp = self.calc->getData(self.gui.imgType)
  case size(*dp, /n_dimen) of
    3 : val = (*dp)[xy[0],xy[1],self.gui.slice]
    4 : val = (*dp)[xy[0],xy[1],self.gui.slice,self.gui.phase]
    6 : val = (*dp)[xy[0],xy[1],self.gui.slice,0,self.gui.dyn,self.gui.phase]
    else : message, "unknown data size"
  endcase
  if arg_present(statusString) then $
    statusString = string(xy,self.gui.slice,val, $
                          format='(%"(%d,%d,%d): %f")')
  return, val
end; }}}

function qasltime::convertDevToData, x, y
;; convert device to data coordinates
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, -1
  endif
  scX = double(self.gui.imgSize[0])/self.gui.drawXsize
  scY = double(self.gui.imgSize[1])/self.gui.drawXsize
  return, floor([x*scX, y*scY])
end; }}}

pro qasltime::updateImgTypes
;; update the value of drop/imgType using calc.data keys
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  data = self.calc->get('data')
  tags = data->keys()
  ; remove unnecessary data from drop list
  ignore = ['TIarr', $ 
            'meanNonCrushCtl', 'meanNonCrushCtl_stderr', $
            'meanNonCrushLbl', 'meanNonCrushLbl_stderr', $
            'meanCrushCtl_stderr', 'meanCrushLbl_stderr', $
            'deltaMcr_stderr', 'deltaMncr_stderr'] 
  for j = 0, n_elements(tags)-1 do begin
    match=strmatch(ignore, tags[j], /fold_case)
    if max(match) eq 1 then tags[j]='ignore'
  endfor
  wh = where(tags ne 'ignore', cn)
  if cn gt 0 then tags = tags[wh]
  ; sort alphabetically
  tags = tags[sort(tags)]
  wdg = self->getWidgetID('drop/imgType')
  widget_control, wdg, set_value=tags
  ; reset previously selected imgType
  wh = where(tags eq self.gui.imgType, cn)
  if cn gt 0 then widget_control, wdg, set_droplist_select=wh $
  else begin ; or change to first item
    self.gui.imgType = tags[0]
    widget_control, wdg, set_droplist_select=0
    self->redraw
  endelse
end; }}}

;;;
;;; internal private methods
;;;
pro qaslTime::debug, text
;; debugging
;{{{
  compile_opt hidden
  if n_elements(text) eq 0 then text='hi there...'
  if self.debug then print, 'DBG: ',text
  ;ok = dialog_message(text, title=tlt, /info, dialog_parent=self.gui.tlb)
end ;}}}

function qaslTime::get, property
;; return object's properties
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return,0
  endif
  case strlowcase(property) of
    'version' : return, self.version
    'title'   : return, self.title
    'tlb'     : return, self.gui.tlb
    'running' : return, self.running
    'debug'   : return, self.debug
    'calc'    : return, self.calc
    'hash'    : return, self.hash
    else : message, "Unknown object property: ", property
  endcase
end;}}}

pro qaslTime::set, title=title, debug=debug
;; set object's properties
;{{{
  compile_opt hidden
  if n_elements(title) gt 0 then begin
    self.title = title
    if self.running then $
      widget_control, self.gui.tlb, base_set_title=title
  endif
  if n_elements(debug) gt 0 then self.debug = keyword_set(debug)
end ;}}}

;;;
;;; life-cycle
;;;
function qaslTime::init, title=title
;{{{
  compile_opt idl2,hidden

  on_error, 2
  self.version = '0.2'

  if n_elements(title) eq 0 then title='qASL-timing'
  self.title = title + ' (ver.'+self.version+')'

  self.calc = obj_new('qaslcalc')
  if ~obj_valid(self.calc) then $
    message, 'Failed to initalize <qaslcalc>'

  ; use this obj error handler for calc object
  self.calc->setErrObj, self, 'error'

  self.hash = obj_new('hashtable', null_value=-9999L, /no_duplicates)
  ; application and default data path
  appdir = programrootdir()
  self.hash->add, 'programPath', appdir
  ; TODO improve default data path setting
  dataPath = file_dirname(appdir)
  self.hash->add, 'dataPath', dataPath
  ; pointers for time course display and fitting
  self.hash->add, 'Xarr', ptr_new(/allocate)
  self.hash->add, 'Yarr', ptr_new(/allocate)
  self.hash->add, 'Earr', ptr_new(/allocate)
  
  self.gui.drawXsize = 350
  self.gui.imgType = 'unknown'

  ok = self->gui()
  if ~ok then begin
    obj_destroy, self.calc
    obj_destroy, self.hash
    message,  'Failed to start GUI'
  endif
  self.running = 1b

  return, 1b
end; }}}

pro qaslTime::cleanup
;{{{
  compile_opt idl2,hidden
  self->debug, 'app obj cleanup'
  if obj_valid_isa(self.hash, 'hashtable') then begin
    keys = self.hash->keys(count=nk)
    for j=0,nk-1 do begin
      k = keys[j]
      v = self.hash->get(k)
      if size(v,/type) eq 10 then ptr_free, v
      if size(v,/type) eq 11 then obj_destroy, v
    endfor
    obj_destroy, self.hash
  endif
  obj_destroy, self.calc
end; }}}

pro qaslTime__define
;{{{
  compile_opt idl2,hidden
  
  gui = { qaslTimeGUI, $ ; internal variables
    drawMaxSize: 0,$ ; maximum allowed draw window X size
    drawXsize: 0,  $ ; draw widget X size	  
    ctlXsize:  0,  $ ; controls base X size (needed for app resizing)
    imgType: '', $ ; current image type
    imgSize: [0,0], $ ; image [xsize, ysize]
    imgRange: [0.0, 0.0], $ ; image [min, max]
    slice:  0, $ ; current slice
    phase:  0, $ ; current phase
    dyn:    0, $ ; current dynamic
    mouseX: 0, $ ; mouse click X-coord (device) 
    mouseY: 0, $ ; mouse click Y-coord (device)
    tlb:    0L $
  }

  obj = { qaslTime    ,$
    version: ''       ,$ ; prog version
    title:   ''       ,$ ; GUI title
    running: 0b       ,$ ; running flag
    debug:   0b       ,$ ; debub flag
    gui:     gui      ,$ ; gui internal variables
    calc:    obj_new(),$ ; calc object
    hash:    obj_new() $ ; hashtable object for storing 'randomly' useful data 
  }
end; }}}

;;;
;;; Main (starts the application)
;;;
pro qaslTime, app=app
  app = obj_new('qaslTime', title='qASL-TTP')
  app->set, debug=1
  ; PHILIPS standard warning
  msg = ["CAUTION - Investigation Device",$
         "Limited by Federal Law to investigational use only"]
  app->dialog, msg, /warning
  app->setStatus, 'Ready...'
end

