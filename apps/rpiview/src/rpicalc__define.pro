;
; Notes:
; Possible data
;   3 separate files with individual territories
;   2 separate files with dual territories
;   (data in multiple files must have same nb of L-C pairs)
;   1 file with mixed/interleaved territories
;      lb_type=normal, ct_type=individual   L1C1 L2C2 L3C3 ...
;      lb_type=dual,   ct_type=individual   L1C1 L2C2 ...
;      lb_type=normal, ct_type=rotating     L1L2L3 C1 L1L2L3 C2 L1L2L3 C3 ...
;      lb_type=dual,   ct_type=rotating     L1L2 C1 L1L2 C2  ...
;
;      lb_type=hadamard, ct_type=common     not implemented
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;   2007-11-14 ivz Original
;   2007-12-06 ivz Fixed LblIndex()
;   2008-01-10 ivz Forgot to calcualte rpi min/max for 'dual' in Subtraction 
;   2008-01-25 ivz Fixed "wrong" colors (LICA - green, RICA - red) 
;   2008-11-06 ivz Added fake_rot control type (simulation of norm-rot from norm-indiv)


;+++++
; Method (P): obj -> LoadData
;-----
;{{{
pro rpicalc::LoadData, get=get
  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    return
  endif
  ; empty self.datafile
  if (self.datafile[0] eq '') or keyword_set(get) then begin
    fls = dialog_pickfile(filter=['*.PAR','*.par'], path=self.datapath, $
        get_path=newpath, /multiple, title='Select RPI files...')
    if fls[0] eq '' then return ; canceled
    nf = n_elements(fls)
    if nf gt 3 then message, 'Too many files selected (max=3)'
    self.datafile = strarr(3)
    self.datafile[0] = file_basename(fls)
    self.datapath = newpath
  endif

  fls = self.datafile
  f = filepath(fls[0], root=self.datapath)
  data = parrec(f, info=inf)
  self->report, "loaded " + f
  sz = size(data)
  n_img = sz[sz[0]]

  if inf.version ge 4.2 then $
    if inf.nb_asl_lbl eq 2 then self.pars.csk = 1 else self.pars.csk=0

  self.pars.patient = inf.pat_name
  self.pars.protocol = inf.protocol
  self.pars.date = inf.exam_date + ' / ' + inf.exam_time
  self.pars.technique = inf.technique
  ; acquisition parameters
  self.pars.dTI = inf.tr ; TR in the par file is time btw LL readouts
  self.pars.fov = max(inf.fov)
  self.pars.slice_thick = max(inf.slice_thickness)
  self.pars.slice_gap = max(inf.slice_gap)
  self.pars.fa = max(inf.image_flip_angle)
  ; resolution
  rec = inf.recon_res
  n_sl = inf.nb_slices
  n_ph = inf.nb_phases
  n_dyn = inf.nb_dynamics
  ;n_dyn = sz[sz[0]] ; dynamics including phases

  ; reform to 3D array
  ;data = reform(data, [rec, n_sl*n_dyn], /overwrite)

  ; reorder CSK data
  if self.pars.csk then begin
    data = reform(data, rec[0], rec[1], 2, n_sl, n_dyn, n_ph,/over)
    data = transpose(data, [0,1,3,2,4,5])
    data = reform(data, rec[0], rec[1], n_img, /over)
  endif


  if fls[1] NE '' then begin
    f = filepath(fls[1], root=self.datapath)
    data1 = parrec(f, info=inf)
    self->report, "loaded " + f
    sz = size(data1)
    n_img = sz[sz[0]]
    ;data1 = reform(data1, [inf.recon_res, inf.nb_slices*n_dyn1], /over)
    if self.pars.csk then begin
      data1 = reform(data1, rec[0], rec[1], 2, n_sl, n_dyn, n_ph,/over)
      data1 = transpose(data1, [0,1,3,2,4,5])
      data1 = reform(data1, rec[0], rec[1], n_img, /over)
    endif
    data = [ [[temporary(data)]], [[temporary(data1)]] ]
  endif

  if fls[2] NE '' then begin
    f = filepath(fls[2], root=self.datapath)
    data1 = parrec(f, info=inf)
    self->report, "loaded " + f
    sz = size(data1)
    n_dyn1 = sz[sz[0]]
    ;data1 = reform(data1, [inf.recon_res, inf.nb_slices*n_dyn1], /over)
    if self.pars.csk then begin
      data1 = reform(data1, rec[0], rec[1], 2, n_sl, n_dyn, n_ph,/over)
      data1 = transpose(data1, [0,1,3,2,4,5])
      data1 = reform(data1, rec[0], rec[1], n_img, /over)
    endif
    data = [ [[temporary(data)]], [[temporary(data1)]] ]
  endif

  ; reform to [x,y, slice, dynamic, phase]
  sz = size(data)
  n_im = sz[sz[0]]
  n_dyn = n_im / (n_sl*n_ph)
  data = reform(data, [rec, n_sl, n_dyn, n_ph], /over)
  *self.data = temporary(data)
  ;help, *self.data

  rawmin = min(*self.data, max=rawmax)
  if ptr_valid(self.rawminmax) then ptr_free, self.rawminmax
  self.rawminmax = ptr_new([rawmin, rawmax])

  ; "clear" lica/rica/post
  *self.lica = 0
  *self.rica = 0
  *self.post = 0
  *self.mask = 0

  self.pars.xres = rec[0] & self.pars.yres = rec[1]
  self.pars.n_slices = n_sl
  self.pars.n_phases = n_ph
  self.pars.n_dyns = n_dyn
end ; LoadData }}}

;+++++
; Method (F): msk = obj -> BrainMask(/redo, noise=noise)
;-----
;{{{
function rpicalc::BrainMask, redo=redo, noise=noise
    catch, error
    if error ne 0 then begin
        self->errmsg, /log
        return, 0
    endif
    if n_elements(*self.data) eq 1 then message, 'Data not available'
    if n_elements(*self.mask) gt 1 and ~keyword_set(redo) then $
        return, *self.mask

    epi = avrg( (*self.data)[*,*,*,*, self.pars.n_phases-1], 4 )
    *self.mask = mrimask(epi, noise=noise, /fill)
    return, *self.mask
end ; BrainMask() }}}

;+++++
; Method (P): obj -> Subtract
;   perform subtraction and averaging over dynamics
;-----
;{{{
pro rpicalc::Subtract, NAV=nav, status=status
  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    status='error'
    return
  endif

  status=''

  if n_elements(*self.data) eq 1 then message, 'Nothing to subtract'

  if n_elements(nav) eq 0 then nav=self->maxnav()

  li = self -> LblIndex('lica', 'label', ln)
  ci = self -> LblIndex('lica', 'control', cn)
  if ln EQ 0 or cn EQ 0 then message, 'Failed to get LICA image indices'
  li = li[0:nav-1]
  ci = ci[0:nav-1]
  *self.lica = avrg( (*self.data)[*,*,*,ci,*] - $
                     (*self.data)[*,*,*,li,*], 4 )
  if self.pars.csk then *self.lica *= -1
  self->report, "LICA label inds: " + strjoin(strtrim(li,2),', ')
  self->report, "LICA contr inds: " + strjoin(strtrim(ci,2),', ')

  li = self -> LblIndex('rica', 'label', ln)
  ci = self -> LblIndex('rica', 'control', cn)
  if (ln EQ 0) OR (cn EQ 0) then message, 'Failed to get RICA image indices'
  li = li[0:nav-1]
  ci = ci[0:nav-1]
  *self.rica = avrg( (*self.data)[*,*,*,ci,*] - $
                     (*self.data)[*,*,*,li,*], 4 )
  if self.pars.csk then *self.rica *= -1
  self->report, "RICA label inds: " + strjoin(strtrim(li,2),', ')
  self->report, "RICA contr inds: " + strjoin(strtrim(ci,2),', ')

  ; dual-vessel labeling recon
  if strmatch(self.lb_type, 'dual',/fold) then begin
    *self.post = 0.5 * ( *self.lica + *self.rica - $
                         abs(*self.lica - *self.rica) )
    *self.lica -= *self.post
    *self.rica -= *self.post
    status='ok'
    self->report, "Reconstructed POST from Dual-Vessel labeling"
  endif else begin
    li = self -> LblIndex('post', 'label', ln)
    ci = self -> LblIndex('post', 'control', cn)
    if ln EQ 0 or cn EQ 0 then message, 'Failed to get POST image indices'
    li = li[0:nav-1]
    ci = ci[0:nav-1]
    *self.post = avrg( (*self.data)[*,*,*,ci,*] - $
                       (*self.data)[*,*,*,li,*], 4 )
    if self.pars.csk then *self.rica *= -1
    self->report, "POST label inds: " + strjoin(strtrim(li,2),', ')
    self->report, "POST contr inds: " + strjoin(strtrim(ci,2),', ')
  endelse               

  mil = min(*self.lica, max=mal)
  mir = min(*self.rica, max=mar)
  mip = min(*self.post, max=map)

  if ptr_valid(self.rpiminmax) then ptr_free, self.rpiminmax
  self.rpiminmax = ptr_new([min([mil,mir,mip]), max([mal,mar,map])])
  status='ok'
end ; Subtract }}}

;+++++
; Method (F): m0 = obj -> GetM0(dyn_idx)
;   get M_0 from specified dynamics (can be mixed label and control)
;-----
;{{{
function rpicalc::GetM0, dyn_idx, status=status
    catch, error
    if error ne 0 then begin
        self->errmsg, /log
        status='error'
        return, 0
    endif

    status=''
    max_dyns = self.pars.n_dyns

    if n_elements(*self.data) eq 1 then message, 'No data'
    if n_elements(dyn_idx) eq 0 then dyn_idx = indgen(max_dyns)
    if max(dyn_idx) gt max_dyns then message, 'Dynamic index higher than max'

    m0 = avrg( (*self.data)[*,*,*,dyn_idx,self.pars.n_phases-1], 4 )
    self->report, "dyn idx for M0: " + strjoin(strtrim(dyn_idx,2),', ')

    status='ok'
    return, m0
end ; GetM0() }}}

;+++++
; Method (F) : idx = obj -> LblIndex(territory, experiment, count=count)
; Generate image index for different territories/experiments
;
; territory = lica|rica|post
; for dual labeling use 'lica' for 'lica-post' territory, same for rica-post
; experiment = label|control
;-----
;{{{
function rpicalc::LblIndex, territory, experiment, count
  compile_opt idl2
  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    return, -1L
  endif
  idx = -1L
  count = 0L

  if n_elements(territory) eq 0 then territory = 'empty'
  if n_elements(experiment) eq 0 then type = 'empty'

  if total(strmatch(['lica','rica','post'], territory, /fold)) eq 0 then $
    message, 'unknown territory: '+ territory
  if total(strmatch(['label','control'], experiment, /fold)) eq 0 then $
    message, 'unknown label type: '+ experiment

  if strmatch(self.lb_type, 'hadamard') then $
    message, 'RPI with hadamard labeling is not supported...'

  if strmatch(self.ct_type, 'common') then $
    message, 'RPI with common control is not supported...'

  n_dyn = self.pars.n_dyns

  nf = self -> n_files()

  ; normal or dual from multiple files
  if nf gt 1 then begin
    ; nb of images for one territory
    n_im = (nf eq 2) ? n_dyn/2 : n_dyn/3
    idx = lindgen(n_im/2)*2 ; LICA label
    if strmatch(experiment, 'control',/fold) then idx += 1
    if strmatch(territory, 'rica') then idx += n_im
    if strmatch(territory, 'post') then begin
      if nf eq 2 then message, 'No POST for dual labeling'
      idx += 2*n_im
    endif
    count=n_elements(idx)
    return, idx
  endif

  ; single file
  if strmatch(self.lb_type, 'normal',/fold) then begin
    case strlowcase(self.ct_type) of
    'individual': begin
      if (n_dyn mod 6) NE 0 then $
          message, 'Nb of dynamics should be divisible by 6'
      all = lindgen(6, n_dyn/6)
      if strmatch(territory, 'lica') then idx = reform(all[0,*])
      if strmatch(territory, 'rica') then idx = reform(all[2,*])
      if strmatch(territory, 'post') then idx = reform(all[4,*])
      if strmatch(experiment, 'control') then idx += 1
    end
    'rotating': begin
      if (n_dyn mod 4) NE 0 then $
          message, 'Nb of dynamics should be divisible by 4'
      all = lindgen(4, n_dyn/4)
      if strmatch(territory, 'lica') then idx = reform(all[0,*])
      if strmatch(territory, 'rica') then idx = reform(all[1,*])
      if strmatch(territory, 'post') then idx = reform(all[2,*])
      if strmatch(experiment, 'control') then idx = reform(all[3,*])
    end
    'fake_rot': begin ; simulation of rotating from norm-indiv data
      if (n_dyn mod 6) NE 0 then $
          message, 'Nb of dynamics should be divisible by 6'
      all = lindgen(6, n_dyn/6)
      if strmatch(territory, 'lica') then idx = reform(all[0,*])
      if strmatch(territory, 'rica') then idx = reform(all[2,*])
      if strmatch(territory, 'post') then idx = reform(all[4,*])
      if strmatch(experiment, 'control') then begin
        ci = indgen(n_dyn/6)*3 + indgen(n_dyn/6) mod 3
        idx = all[[1,3,5],*]
        idx = idx[ci]
      endif
    end
    endcase
    count=n_elements(idx)
    return, idx
  endif

  if strmatch(self.lb_type, 'dual',/fold) then begin
    case strlowcase(self.ct_type) of
    'individual': begin
      if (n_dyn mod 4) NE 0 then $
        message, 'Nb of dynamics should be divisible by 4'
      all = lindgen(4, n_dyn/4)
      if strmatch(territory, 'lica') then idx = reform(all[0,*])
      if strmatch(territory, 'rica') then idx = reform(all[2,*])
      if strmatch(experiment, 'control') then idx += 1
    end
    'rotating': begin
      if (n_dyn mod 3) NE 0 then $
        message, 'Nb of dynamics should be divisible by 3'
      all = lindgen(3, n_dyn/3)
      if strmatch(territory, 'lica') then idx = reform(all[0,*])
      if strmatch(territory, 'rica') then idx = reform(all[1,*])
      if strmatch(experiment, 'control') then idx = reform(all[2,*])
    end
    endcase
    count=n_elements(idx)
    return, idx
  endif

  if n_elements(idx) eq 0 then message, 'something went wrong'
  return, -1L
end ; LblIndex() }}}

;+++++
; Method (F): map = obj -> RPImap(slice, phase, crop=crop)
;-----
;{{{
function rpicalc::RPImap, slice=slice, phase=phase, crop=crop, $
    scale=scale, interp=interp, contour=contour, flipY=flipY
    catch, error
    if error ne 0 then begin
        self->errmsg, /log
        return, 0
    endif

    if (n_elements(*self.lica) eq 1) OR $
       (n_elements(*self.rica) eq 1) OR $
       (n_elements(*self.post) eq 1) then message, 'Territories not available'

    if n_elements(slice) eq 0 then slice = self.pars.n_slices/2
    if n_elements(phase) eq 0 then phase = 0

    slice = slice < (self.pars.n_slices-1)
    phase = phase < (self.pars.n_phases-1)

    map = [ [[(*self.rica)[*,*,slice, phase] ]], $
            [[(*self.lica)[*,*,slice, phase] ]], $
            [[(*self.post)[*,*,slice, phase] ]] ]
    
;    mask= self->brainmask(noise=15,/redo)
;    kk = bytarr(3,3)+1
;    cont = dilate(mask[*,*,slice], kk) - mask[*,*,slice]  

    if n_elements(crop) eq 4 then begin
        map = arrex(map, [crop[0:1],0], [crop[2:3],2])
;        cont = arrex(cont, [crop[0:1],0], [crop[2:3],2])
    endif

    if n_elements(scale) eq 1 then begin
        sz = size(map, /dimension)
        nx = sz[0]*scale
        ny = sz[1]*scale
        map = congrid(map, nx, ny, 3, interp=keyword_set(interp))
;        cont = congrid(cont, nx, ny, 3)
    endif

;    if keyword_set(contour) then begin
;        map[*,*,0] *= (1-cont) + cont * 255
;        map[*,*,1] *= (1-cont) + cont * 255
;        map[*,*,2] *= (1-cont) + cont * 255
;    endif
    if keyword_set(flipY) then map=reverse(map, 2)
    return, map
end ; RPImap() }}}

;+++++
; Method (F): im = obj -> rawdata(slice, phase, dynamic)
;-----
;{{{
function rpicalc::rawdata, slice, phase, dyn
  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    return, 0
  endif

  if self->status(/data) eq 0 then $
    message, 'Data not loaded'

  n_sl = self.pars.n_slices
  n_ph = self.pars.n_phases
  n_dy = self.pars.n_dyns
  if n_elements(slice) eq 0 then slice=n_sl/2
  if n_elements(phase) eq 0 then phase=0
  if n_elements(dyn) eq 0 then dyn=0

  slice = 0 > slice < (n_sl-1)
  phase = 0 > phase < (n_ph-1)
  dyn = 0 > dyn < (n_dy-1)

  im = (*self.data)[*,*,slice,dyn,phase]
  return, [ [[im]], [[im]], [[im]] ]
end ;}}}

;================
; Get/Set methods
;================

;+++++
; Method (F): ok = obj->status(/data)
; returns 1 if data is present
;-----
;{{{
function rpicalc::status, data=data
  ok = 0
  if keyword_set(data) then ok=n_elements(*self.data) gt 1
  return, ok
end ;}}}

;+++++
; Method (F): nav = obj->maxnav()
; max nb of averages
;-----
;{{{
function rpicalc::maxnav
  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    return, 0
  endif

  if self->status(/data) eq 0 then return, 0
  switch strlowcase(self.ct_type) of
    'fake_rot' : 
    'individual' : begin 
      if strmatch(self.lb_type, 'dual',/fold) then denum = 4 $
      else denum = 6
      break
    end
    'rotating' : 
    'common' : begin 
      if strmatch(self.lb_type, 'dual',/fold) then denum = 3 $
      else denum = 4
      break
    end
    else : message, 'unknown control type ' + self.ct_type
  endswitch
  
  return, self.pars.n_dyns / denum
end ;}}}

;+++++
; Method (F): minmax = obj->GetMinMax(/data, /rpi)
;-----
;{{{
function rpicalc::GetMinMax, data=data, rpi=rpi
  if self->status(/data) eq 0 then return, 0
  if keyword_set(data) then $
    if ptr_valid(self.rawminmax) then return, *self.rawminmax
  if keyword_set(rpi) then $
    if ptr_valid(self.rpiminmax) then return, *self.rpiminmax
  ; else
  return, 0
end ;}}}

;+++++
; Method (F): nf = obj->n_files()
; returns nb of data files
;-----
;{{{
function rpicalc::n_files
  return, 3 - total(strmatch(self.datafile, ''))
end ;}}}

;+++++
; Method (P): obj -> GetProperty, datapath=datapath...
;-----
;{{{
pro rpicalc::GetProperty, datapath=datapath, datafile=datafile, $
    label_type=lb_type, control_type=ct_type

  on_error, 2
  nf = self->n_files()
  datapath = self.datapath
  datafile = self.datafile[0:nf-1]
  lb_type = self.lb_type
  ct_type = self.ct_type
end ; GetProperty }}}

;+++++
; Method (P): obj -> SetProperty, datapath=datapath...
;-----
;{{{
pro rpicalc::SetProperty, datapath=datapath, datafile=datafile, $
    label_type=lb_type, control_type=ct_type

  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    return
  endif

  if n_elements(datapath) gt 0 then $
    if file_test(datapath,/dir) eq 1 then self.datapath = datapath $
    else message, 'Bad directory: '+datapath

  nf = n_elements(datafile)
  if nf gt 0 then begin
    ; override datapath if filename contains path
    ; multiple files should be in the same directory
    dir = file_dirname(datafile[0])
    if dir[0] NE '.' then self.datapath = dir[0]
    for i=0, (nf<3)-1 do $
      if file_test(datafile[i],/read) eq 1 then $
        self.datafile[i] = file_basename(datafile[i]) $
      else message, 'Inaccessible file: '+datafile[i]
  endif

  ; valid label types
  label_types = ['normal', 'dual', 'hadamard']
  if n_elements(lb_type) gt 0 then begin
    si = strmatch(label_types, lb_type, /fold)
    if total(si) gt 0 then self.lb_type = lb_type $
    else message, 'Bad Label type: '+ lb_type
  endif

  ; valid control types
  control_types = ['individual', 'rotating', 'common', 'fake_rot']
  if n_elements(ct_type) gt 0 then begin
    si = strmatch(control_types, ct_type, /fold)
    if total(si) gt 0 then self.ct_type = ct_type $
    else message, 'Bad Control type: '+ ct_type
  endif
end ; SetProperty }}}

;+++++
; Method (F): pars = obj -> GetRPIpars(xres=xres...)
;    returns parameters structure
;    individual fields via keywords
;-----
;{{{
function rpicalc::GetRPIpars, xres=xres, yres=yres, n_slices=n_slices, $
    n_phases=n_phases, n_dyns=n_dyns, fov=fov, slice_thick=slice_thick, $
    slice_gap=slice_gap, FA=FA, TR=tr, TI1=TI1, dTI=dTI

  on_error, 2
  xres = self.pars.xres
  yres = self.pars.yres
  n_slices = self.pars.n_slices
  n_phases = self.pars.n_phases
  n_dyns = self.pars.n_dyns
  fov = self.pars.fov
  slice_thick = self.pars.slice_thick
  slice_gap = self.pars.slice_gap
  FA = self.pars.FA
  TR = self.pars.TR
  TI1 = self.pars.TI1
  dTI = self.pars.dTI
  return, self.pars
end ; GetRPIpars() }}}

;+++++
; Method (P): obj -> SetRPIpars, fov=fov ...
;-----
;{{{
pro rpicalc::SetRPIpars, FOV=fov, slice_thick=slick_thick, $
    slice_gap=slice_gap, FA=FA, TR=tr, TI1=ti1, dTI=dTI, n_dyn=n_dyn

  catch, error
  if error ne 0 then begin
    self->errmsg, /log
    return
  endif
  if n_elements(fov) gt 0 then self.pars.fov = fov[0]
  if n_elements(slice_thick) gt 0 then self.pars.slice_thick = slice_thick
  if n_elements(slice_gap) gt 0 then self.pars.slice_gap = slice_gap
  if n_elements(FA) gt 0 then self.pars.FA = FA
  if n_elements(TR) gt 0 then self.pars.TR = TR
  if n_elements(TI1) gt 0 then self.pars.TI1 = TI1
  if n_elements(dTI) gt 0 then self.pars.dTI = dTI
  ; for testing
  if n_elements(n_dyn) gt 0 then self.pars.n_dyns = n_dyn
end ; SetRPIpars }}}

;+++++
; Method (F): p = obj -> _getptr(/data, /lica, /rica, /post, /mask)
;    returns a pointer to data
;    danger of incorrect data manipulation
;    use only for debugging
;-----
;{{{
function rpicalc::_getptr, data=data, $
    lica=lica, rica=rica, post=post, mask=mask

  compile_opt hidden
  if keyword_set(data) then return, self.data
  if keyword_set(lica) then return, self.lica
  if keyword_set(rica) then return, self.rica
  if keyword_set(post) then return, self.post
  if keyword_set(mask) then return, self.mask
  ; else
  return, ptr_new()
end ; getptr() }}}

;+++++
; Constructor: obj = obj_new('rpicalc', datapath=path)
;-----
;{{{
function rpicalc::init, datapath=datapath
  on_error, 2
  cd, current=current_dir
  if n_elements(datapath) gt 0 then $
    if file_test(datapath,/dir) eq 1 then self.datapath=datapath $
  else begin
    self.datapath = current_dir
  endelse

  self.calcpath = programrootdir()
  logfile = filepath('rpicalc_log.txt', root=self.calcpath)
  ok = self->ivzerror::init(title='RPIcalc', logfile=logfile )
  self->clearlog
  self.data = ptr_new(0)
  self.lica = ptr_new(0)
  self.rica = ptr_new(0)
  self.post = ptr_new(0)
  self.mask = ptr_new(0)
  return, 1
end ; init() }}}

pro rpicalc::cleanup
  ptr_free, [self.data, self.lica, self.rica, self.post, self.mask]
  ptr_free, [self.rpiminmax, self.rawminmax]
end

pro rpicalc__define
  compile_opt idl2
  pars = { RPIPARS, $ ; acquisition parameters
    CSK:0b, $ ; patch or CSK data
    patient: '', protocol: '', date:'', $
    technique: '', $
    xres:0, yres:0, n_slices:0, $
    n_phases:0, n_dyns:0, FA:0.0, $
    FOV:0.0, slice_thick:0.0, slice_gap:0.0, $
    TR:0.0, TI1:0.0, dTI:0.0 $
  }

  class = { RPICALC, inherits IVZerror, $
    calcpath : '', $        ; installation dir
    datapath : '', $        ; data path
    datafile : strarr(3), $ ; max 3 files for separate lica/rica/post
    lb_type  : '', $        ; normal/dual/hadamard
    ct_type  : '', $        ; individual/rotating/common/none
    pars     : pars,      $
    data     : ptr_new(), $ ; raw data (separate files stuck together)
    lica     : ptr_new(), $ ; subtracted & averaged
    rica     : ptr_new(), $ ;         "
    post     : ptr_new(), $ ;         "
    rpiminmax: ptr_new(), $ ; rpi min/max (all territories)
    rawminmax: ptr_new(), $ ; raw data min/max
    mask     : ptr_new()  $ ; brain mask
  }
end
