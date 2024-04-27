;+
; T1rho mapping tool
;
; NOTES:
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com> 
;   2008-09-17 ivz Original
;   2008-10-01 ivz LoadData method
;   2008-10-04 ivz basic GUI (File menu, dsp object)
;   2008-10-06 ivz drop list for different spin-lock experiments
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; reporting via text (default), logfile or dialog
pro t1rho::info, text, log=log, gui=gui
;{{{
  compile_opt hidden
  if keyword_set(log) then begin
    openw, flun, self.config.FileLog, /get_lun,/append
    for i=0L, n_elements(text)-1 do printf, flun, text[i]
    free_lun, flun
    return
  endif
  if keyword_set(gui) then begin
    if widget_info(self.gui.tlb, /valid_id) then $
      ok = dialog_message(text,/error,dialog_parent=self.gui.tlb) $
    else $ 
      ok = dialog_message(text, /error) 
  endif
  ; default text output
  for i=0L, n_elements(text)-1 do print, text[i]
end ;}}}

;;; error reporting via dialog or logfile
pro t1rho::error, log=log
;{{{ 
  compile_opt hidden
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  if widget_info(self.gui.tlb, /valid_id) then $
    ok = dialog_message(err,/error,dialog_parent=self.gui.tlb) $
  else $ 
    ok = dialog_message(err, /error) 
  if n_elements(log) then $
    self->info, err, /log
  catch,/cancel
end ;}}}

;;;;;;;;;;;;;;;;;;;;;;;;; data processing

pro t1rho::LoadData, DataDir=DataDir 
;{{{
; load data into self.data
; DataDir points to directory with subdirs containing dicom data for
; individual spin-lock time aquisitions ex. SL01, SL10, ...
; spin-lock times are extracted from subdirectory names!!!
; read array order is [nx, ny, n_slice, n_spin_lock]
; 2d dimension (y) is flipped at the end (for correct display with !order=0)
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    if obj_valid(dcm) then obj_destroy, dcm
    return
  endif

  if n_elements(DataDir) gt 0 then $
    self->SetConfig, DataDir=DataDir

  if self.config.debug GT 0 then $
    self->info, "DataDir: " + self.config.DataDir

  cd, self.config.DataDir

  ; get dicom sub directories
  sDirs = file_search(self.config.FilterDir, /Test_Directory, count=nDirs)
  if nDirs eq 0 then $
    message, 'No directories matching <'+self.config.FilterDir + '>'
  ; make sure sub dirs are sorted (alphabetically)
  sDirs = sDirs[sort(sDirs)]
  self.sdirs = strjoin(sDirs,';')
  ; spin-lock time array
  tsl = fltarr(nDirs)

  if self.config.debug gt 0 then begin
    self->info, 'subs: '+strjoin(sdirs, ';')
  endif

  ; get all file names
  fls = ['']
  for i=0, nDirs-1 do begin
    ff = file_search(sDirs[i], self.config.FilterDcm, count=nf)
    if nf eq 0 then $
      message, 'No files matching <'+ self.config.FilterDcm + '> in ' + sDirs[i] 
    fls = [fls, ff]
    tsl[i] = float(stregex(sDirs[i], '[0-9][0-9]', /extract))
  endfor
  ; there should be same nb of files in each subdir
  ; make fls 2D array with rows holding files for one subdir
  fls = reform(fls[1:*], nf, nDirs)

  ; read first file
  dcm = obj_new('IDLffDicom')
  r = dcm->read(fls[0,0])
  if r[0] eq 0 then message, 'Failed to read ' + fls[0,0]

  voxel = replicate(1e-7,3)

  ;; PixelSpacing
  val = dcm->getvalue('0028'x,'0030'x,/no_copy)
  if size(val, /Tname) eq 'POINTER' then $
    voxel[0:1] = float(strsplit(*val[0], '\',/extract))

  ;; SliceThickness
  val = dcm->getvalue('0018'x,'0050'x,/no_copy)
  if size(val, /Tname) eq 'POINTER' then $
    voxel[2] = float(*val[0])
  
  ;; get image dimensions and type
  val = dcm->getvalue('7FE0'x,'0010'x, /no_copy)
  n_tot = n_elements(val)
  xy_dims = size(*val[0], /dimen)
  im_type = size(*val[0], /type)

  data = make_array(dimension=[xy_dims, nf, nDirs],type=im_type)

  for n=0L, nDirs-1 do begin
    for i=0L, nf-1 do begin
      r = dcm->read(fls[i,n])
      if r[0] eq 0 then message, 'Failed to read ' + fls[i,n]
      val = dcm->getvalue('7FE0'x,'0010'x, /no_copy)
      data[*,*,i,n] = *val[0]
    endfor
  endfor
  obj_destroy, dcm

  ; data loaded
  self->free_all_pointers
  self.dims = size(data, /dimen)
  ; vertical flip
  data = reverse(data, 2, /over)
  self.data = ptr_new(data,/no_copy)
  self.time_vec = ptr_new(tsl, /no_copy)
  self.voxs = voxel
end ;}}}

;;; compute binary mask where fitting will be done
pro t1rho::NewMask
;{{{
; mask = data > noise
; semi-arbitrary noise estimation as
; 5% of the max of slightly smoothed 1st spin-lock time

  compile_opt hidden

  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  if self->status(/data) eq 0 then $
    message, 'No data'

  noise_thresh = 0.05 * max(g_smooth(float( (*self.data)[*,*,*,0] ), 3, 5))
  msk = (*self.data)[*,*,*,0] gt round(noise_thresh)

  ; remove specles
  mm = mm_kern(3, /k3d)
  msk = dilate(erode(msk, mm), mm)

  if ptr_valid(self.mask) eq 0 then begin
    ptr_free, self.mask
    self.mask = ptr_new(msk,/no_copy)
  endif else *self.mask = msk
end ;}}}

pro t1rho::fit_in_mask
;{{{
  compile_opt idl2,hidden

  catch, error
  if error ne 0 then begin
    self->error
    if obj_valid(progress) then obj_destroy, progress
    return
  endif
  if self->status(/data) eq 0 then $
    message, 'No data'
  ;if self->status(/mask) eq 0 then self->NewMask

  t1r = (*self.mask) * 0.0 

  ; "shortcuts" to image size
  sx = self.dims[0] & sy = self.dims[1] & sz = self.dims[2]
  sxy = long(sx)*sy

  ; progress bar
  if self->status(/gui) then begin
    progress = obj_new('progressbar', Text="Fitting in progress...", $
          group_leader=self.gui.tlb, /fast_loop)
    progress -> start
  endif

  idx = where( *self.mask eq 1, cn)
  for i=0L, cn-1 do begin
    ; use progress bar to break calculation
    if obj_valid(progress) then begin
      if progress -> checkcancel() then begin
        progress -> destroy
        return
      endif
    endif
    ; compute 3D indicies from 1D
    zi = idx[i] / sxy
    yi = (idx[i] mod sxy) / sx
    xi = (idx[i] mod sxy) mod sx
    sig = double(reform((*self.data)[xi,yi,zi,*]))
    r = linfit(*self.time_vec, alog(sig), chisq=chi2 )
    ; only save if slope is < -0.001 
    if r[1] lt -1e-3 then begin
      t1r[xi,yi,zi] = -1/r[1]
    endif

    ; update progress bar
    if obj_valid(progress) then begin
      percent = 100.0 * (i+1) / cn
      percent = float(round(percent*10))/10
      if percent mod 1 eq 0.0 then $
        progress->update, percent, $
          text='Fitting: ' + strtrim(fix(percent),2)+'%'
    endif
  endfor

  if obj_valid(progress) then progress->destroy

  ; save map as analyze
  f_ana = filepath('t1r_map.hdr', root_dir=self.config.DataDir)
  writeanz, t1r, f_ana, voxel=self.voxs, /over
  if ptr_valid(self.t1r_map) then ptr_free, self.t1r_map
  self.t1r_map = ptr_new(t1r,/no_copy)
end ;}}}

;;;;;;;;;;;;;;;;;;;;;;;;; GUI
;function t1rho::RawImage, slice=slice, frame=frame
;;{{{
;  compile_opt hidden
;  catch, error
;  if error ne 0 then begin
;    self->error
;    return, -1
;  endif
;
;  if self->status(/data) eq 0 then $
;    message, "No data"
;  dims = self->dims()
;
;  ; middle slice
;  if n_elements(slice) eq 0 then slice=dims[2]/2
;  if n_elements(frame) eq 0 then frame=0
;
;  slice = 0 > slice < (dims[2] - 1)
;  frame = 0 > frame < (dims[3] - 1)
;  im = (*self.data)[*,*,slice,frame]
;  return, im
;end ;}}}

;;; set status bar
pro t1rho::set_sbar, text
;{{{
  compile_opt hidden
  widget_control, self.gui.sbar, set_value=text
end
;}}}

pro t1rho::EventHandler, ev, uname
;{{{
  compile_opt idl2, hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif
  case uname of
    'file/exit': begin
      widget_control, self.gui.tlb, /destroy
    end
    'file/load_data': begin ;{{{
      path = dialog_pickfile(filter=self.config.FilterDir, $
        dialog_parent=self.gui.tlb,/directory)
      if path eq '' then return
      self->SetConfig, DataDir=path
      self->LoadData
      if self->status(/data) eq 0 then return
      ; update display image stack
      stack = self.gui.trash->get(name='img_TSL')
      if obj_valid_isa(stack, 'slice_stack') then begin
        self.gui.trash->remove, stack
        obj_destroy, stack
      endif
      stack = obj_new('slice_stack', (*self.data)[*,*,*,0], voxel=self.voxs,$
        name='img_TSL')
      self.gui.trash->add, stack
      self.gui.dsp->LoadBackCT, 0
      self.gui.dsp->change_stack, stack
      ; activate buttons
      self.gui.dsp->gui_activate, /keyboard_evt
      wid = widget_info(self.gui.tlb, find_by_uname='tools/fit')
      widget_control, wid, sensitive=1
      ; image drop list set to dicom subdir names
      drop = widget_info(self.gui.tlb, find_by_uname='drop/img_tsl')
      vals = strsplit(self.sdirs, ';',/extract)
      widget_control, drop, set_value=vals
      widget_control, drop, sensitive=1
      widget_control, drop, sensitive=1
    end ;}}}
    'tools/fit': begin
      self->NewMask
      self->fit_in_mask
      wid = widget_info(self.gui.tlb, find_by_uname='drop/maps')
      widget_control, wid, sensitive=1
      stack = self.gui.trash->get(name='mask')
      if obj_valid_isa(stack, 'slice_stack') then begin
        self.gui.trash->remove, stack
        obj_destroy, stack
      endif
      stack = obj_new('slice_stack', (*self.mask), voxel=self.voxs,$
        name='mask')
      if obj_valid_isa(stack, 'slice_stack') then begin
        self.gui.trash->remove, stack
        obj_destroy, stack
      endif
      t1r_min = min(*self.t1r_map, max=t1r_max)
      ww = 100./( t1r_max-t1r_min)
      wl = 50./( t1r_max-t1r_min)
      stack = obj_new('slice_stack', (*self.t1r_map), voxel=self.voxs,$
        name='T1rho', win_level=wl, win_width=ww )
      self.gui.trash->add, stack
      self.gui.dsp->change_stack, stack
    end
    'drop/img_tsl': begin
      ;if self.gui.dsp->name(/stack) NE 'img_TSL' then begin
      ;endif
      stack = self.gui.trash->get(name='img_TSL')
      ; bad hack ...
      p = stack->data(/pointer)
      *p = (*self.data)[*,*,*,ev.index]
      self.gui.dsp->change_stack, stack
      self.gui.dsp->change_backIm, /notransform
    end
    'drop/maps': begin
      widget_control, ev.id, get_value=val_arr
      map_name = val_arr[ev.index]
      stack = self.gui.trash->get(name=map_name)
      if ~obj_valid(stack) then return
      self.gui.dsp->change_stack, stack
    end
  endcase
end ;}}}

pro t1rho::GUI
;{{{
  compile_opt idl2, hidden
  catch, error
  if error ne 0 then begin
    self->error
    return
  endif

  if Xregistered(self.config.ProgName, /noshow) NE 0 then $
    Message, self.config.ProgName + ' GUI is already running'
    
  ; image display size (for voldisp object)
  winX = 400
  winY = 400

  self.gui.title = self.config.progName + $
      ' (v.' + self.config.version + ')'

  tlb = widget_base(title=self.gui.title, /column, $
    mbar=tlm, tlb_size_events=0, /base_align_left, xpad=0, ypad=0)
  self.gui.tlb = tlb
  menu = widget_button(tlm, value='File')
  wdg = widget_button(menu, value='Load Data', $
        uname='file/load_data')
  wdg = widget_button(menu, value='Exit', $
        uname='file/exit', /separator)
  menu = widget_button(tlm, value='Tools')
  wdg = widget_button(menu, value='Fit all', $
        uname='tools/fit', sensitive=0)

  ; main layout base
  base = widget_base(tlb, /row, xpad=0, ypad=0, uname='base/main')
  subb = widget_base(base, /column, xpad=0, ypad=0, $
        uname='base/controls', /base_align_right)
  bb = widget_base(subb, /column, /frame)
  sl_names=['SL01','SL10']
  wdg = widget_droplist(bb, title='Image', value=sl_names, $
          uname='drop/img_tsl', sensitive=0)
  map_names=['mask','T1rho']
  wdg = widget_droplist(bb, title='  Map', value=map_names, $
          uname='drop/maps', sensitive=0)

  dsp = obj_new('voldisp', base, xsize=winX, ysize=winY, name='DSP')
  if ~obj_valid(dsp) then $
    message, 'failed to init voldisp'

  self.gui.dsp = dsp

  if ~obj_valid(self.gui.trash) then $
    self.gui.trash = obj_new('ph_Container')
  self.gui.trash->add, self.gui.dsp
  
  ; status bar
  self.gui.sbar = widget_label(tlb, value='Ready', /dynamic_resize, $
    /sunken_frame)

  widget_control, self.gui.tlb, /realize
  geo = widget_info(self.gui.tlb, /geometry)
  widget_control, self.gui.sbar, xsize=geo.xsize
  ; make status bar available for display obj
  self.gui.dsp->SetProperty, statusbar=self
  widget_control, self.gui.tlb, set_uvalue=self

  Xmanager, self.config.ProgName, self.gui.tlb, /no_block, $
    event_handler='T1rho_jp_event', cleanup='t1rho_jp_kill'
end ;}}}

;;;;;;;;;;;;;;;;;;;;;;;;; misc methods
function t1rho::dims
;{{{
  return, self.dims
end ;}}}

function t1rho::GetPtr, data=data, mask=mask, time=time, $
  T1r_map = T1r_map
;{{{
    on_error, 2
    null = ptr_new()
    if self->status(/data) eq 0 then return, null
    if keyword_set(data) then return, self.data
    if keyword_set(mask) then return, self.mask
    if keyword_set(time) then return, self.time_vec
    if keyword_set(T1r_map) then return, self.t1r_map
    ; else
    return, null
end ;}}}

function t1rho::status, data=data, mask=mask, fitted=fitted, gui=gui
;{{{
  compile_opt hidden
    dat = ptr_valid(self.data)
    if keyword_set(data) then return, dat
    if keyword_set(mask) then return, dat * ptr_valid(self.mask)
    if keyword_set(fitted) then return, ptr_valid(self.t1r_map)
    if keyword_set(gui) then return, widget_info(self.gui.tlb, /valid_id)
    ; else
    return, dat
end ;}}}

pro t1rho::SetConfig, DataDir=DataDir, FilterDcm=FilterDcm, $
  FilterDir=FilterDir, debug=debug 
;{{{
  compile_opt hidden
  if n_elements(FilterDir) gt 0 then self.config.FilterDir=FilterDir
  if n_elements(FilterDcm) gt 0 then self.config.FilterDcm=FilterDcm
  if n_elements(DataDir) gt 0 then $
    if file_test(DataDir, /directory) then self.config.DataDir=DataDir
  if n_elements(debug) then self.config.debug=debug
end ;}}}

pro t1rho::SetProperty, fit_init=pini
;{{{
  compile_opt hidden
  self.pini = pini
end;}}}

function t1rho::init, DataDir=DataDir, FilterDcm=FilterDcm, FilterDir=FilterDir 
;{{{
  on_error, 2

  self.config.version = '0.2'

  self.config.ProgName = 'T1rho'
  self.config.ProgDir = programrootdir()
  self.config.DataDir = ''
  self.config.FilterDcm = 'IM*'
  self.config.FilterDir = 'SL*'
  self.config.LogFile = filepath('logfile.txt', root_dir=self.config.ProgDir)
  openw, flun, self.config.LogFile, /get_lun
  caldat, systime(/julian), mm, dd, yy, hh, mm, ss
  DateTime = string(yy,mm,dd,hh,mm,ss,$
    format='(%"%04d-%02d-%02d at %02d:%02d:%02d")')
  printf, flun, 'Log started: '+ DateTime
  printf, flun, ''
  free_lun, flun

  self->SetConfig, DataDir=DataDir, FilterDcm=FilterDcm, FilterDir=FilterDir 
  return, 1
end ; }}}

pro t1rho::free_all_pointers
;;{{{
  class = {t1rho}
  for i=0, n_tags(class)-1 do begin
    if size(self.(i), /tname) NE 'POINTER' then continue
    if ptr_valid(self.(i)) then ptr_free, self.(i)
  endfor
end ; }}}

pro t1rho::cleanup
;{{{
  compile_opt hidden
  self->free_all_pointers
  obj_destroy, self.gui.trash
end ;}}}

;;; application object
pro t1rho__define
;{{{
  compile_opt idl2, hidden
  config = { t1rhoCONFIG, $
    ProgName: '',  $
    LogFile : '',  $ ; error logging
    ProgDir: '',   $ 
    DataDir: '',   $ ; path to dicom folders
    FilterDcm: '', $ ; dicom files filter
    FilterDir: '', $ ; dicom dir filter
    version : '',  $
    debug : 0b     $ ; debugging level
  }

  gui = { T1rhoGUI, $
    title : '',       $ ; GUI window title 
    tlb : 0, $ ; top level base ID
    sbar: 0, $ ; status bar ID (widget label)
    dsp : obj_new(),  $ ; display object
    trash : obj_new() $ ; cleanup object 
  }

  class = { t1rho,$
    config : config,  $ ; config structure
    sdirs: '',        $ ; subdirs separated by ';'
    data : ptr_new(), $ ; pointer to raw data
    dims : intarr(4),  $ ; image dimensions
    voxs : fltarr(3),  $ ; voxel size
    info : ptr_new(), $ ; pointer to data info struct
    time_vec : ptr_new(), $ ; spin lock times in increasing order [ms]
    mask : ptr_new(), $ ; mask
    t1r_map : ptr_new(), $ ; T1rho map
    fit_err : ptr_new(), $ ; estimated errors on fitted parameters
    fit_sigma : ptr_new(), $ ; sqrt(total((y-yfit)^2)/dof)
    fit_bad : ptr_new(), $ ; possible outliers
    gui : gui $ ; gui structure
 }
end ;}}}

;;; wrapper for events
pro t1rho_jp_event, ev
;{{{
  catch, error
  if error ne 0 then begin
    if obj_valid(self) eq 0 then begin
      print, !error_state.msg 
      catch, /cancel
    endif else self->error 
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
end ;}}}

;;; wrapper for exit
pro t1rho_jp_kill, tlb
;{{{
  widget_control, tlb, get_uvalue=self
  obj_destroy, self
end ;}}}

;;; main
pro t1rho_jp, app=app
  if obj_valid_isa(app,'t1rho') then obj_destroy, app
  app = obj_new('t1rho')
  ;dataDir="C:\data\T1rho\kuma"
  dataDir = "/export/data/pride_jp/t1rho/kuma/'"
  if ~file_test(dataDir, /directory) then dataDir=""
  app->SetConfig, DataDir=dataDir, FilterDir='SL*'
  app->SetConfig, debug=1
  app->GUI
end

