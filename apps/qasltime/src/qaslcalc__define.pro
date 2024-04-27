;+
; CLASS:
;   gaslcalc
; 
; PURPOSE:
;   Processing of QUASAR ASL data
;
; NOTES:
;   self.info is a hash object returned by parrec(file, info=info, /hash_info)
;   self.data is a hash object holding pointers to data and results
;   for external data access:
;     data = calc->get('data')
;     print, data->keys()
;     p = data->get('QUASAR')
;     p = calc->getData('QUASAR') ; access w/o 'getting' data first
;
;   Typical QUASAR data acquired for Model-free CBF quantification has the
;   form: 64x64 matrix, 7 slices, 2 labeling conditions, 42 dynamics, 
;   13 look-locker phases i.e. can be read into [64,64,7,2,42,13] INTARR.
;   2d labeleling condition is "control" 
;   42 dynamics are (Cr, Cr, Cr, Cr, NCr, NCr, LowFA)x6
;   Cr means data aquired with bipolar crusher and NCr is for non-crushed
;   LowFa is low flip angle data (11deg) used for estimation of B1 which is
;   needed for automatic M0a calculation
;
;   !!! In ASL Specialist and CSK nb_dymanics corresponds to 
;       nb of label-control pairs 
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;   2009-10-26 ivz original
;   2009-11-03 ivz changed error handling and separated info and data
;   2009-11-04 ivz added redoMeanVol(), redoMaskVol(), data->add,'TIarr'
;   2010-03-06 ivz added getData()
;

function qaslcalc::loadData, file, get=get
;; load data from new par/rec
;; data must be acquired with QUASAR patch by ETP (test-retest)
;; return 1 on success, 0 on failure
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0b
  endif

  cd, current=data_path

  ; destroy previous hash objects
  if obj_valid(self.data) then begin
    self->hashPtrFree, self.data
    obj_destroy, self.data
  endif
  if obj_valid(self.info) then begin
    ; try to get previous data path
    data_file = self.info->get('data_file')
    if file_test(data_file) then $
      data_path = file_dirname(data_file)
    obj_destroy, self.info
  endif

  if (n_elements(file) eq 0) or keyword_set(get) then begin
    fls = dialog_pickfile(filter=['*.PAR','*.par'], $
      path=data_path, title='Select QASL file')
    if fls[0] eq '' then return, 0 ; canceled
    file = fls[0]
  endif

  data = parrec(file, info=info, /flipY, /dispvalue, $
    hash_null_value=self.hash_null, /hash_info)

  if n_elements(data) eq 1 then $
    message, "Failed to read "+file

  ; make sure the data is from the QUASAR patch (R2.1.3 - R2.5.3 by ETP)
  tech = info->get('technique')
  if strpos(tech, 'PASL-QA-M') eq - 1 then begin
    obj_destroy, info
    message, "Data has not been acquired with the QUASAR patch."
  endif

  info->add, 'data_file', file

  xyres = info->get('recon_res')
  n_sl = info->get('nb_slices')
  n_dy = info->get('nb_dynamics') ; both lbl and ctl
  n_ph = info->get('nb_phases')

  ; get acquisition trigger time
  trig_time = info->get('trigger_time')
  trig_time = reform(trig_time, n_sl, n_dy, n_ph)
  ; inversion time for each slice and phase
  ti_arr = reform(trig_time[*,0,*])
  ; ti_arr[3,*] would be inversion time for 4th sl (+ 33ms needed for WET)

  self.info = info
  ; data order is [x,y,z,asl-lbl,dyn,phase]
  data = reform(data, [xyres, n_sl, 2, n_dy/2, n_ph], /overwrite)
  self.data = obj_new('hashtable', null_value=self.hash_null, /no_duplicates)
  self.data->add, 'QUASAR', ptr_new(data, /no_copy)
  self.data->add, 'TIarr', ptr_new(ti_arr,/no_copy)

  return, 1b
end ; LoadData }}}

function qaslcalc::redoMeanVol, phase=phase, crushed=crushed
;; make an average EPI volume of a given phase
;; use non-crushed data by default or crushed via keyword
;; return 1 on success, 0 on failure
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0b
  endif

  if ~obj_valid(self.info) then $
    message, "info object not available"
  if ~obj_valid(self.data) then $
    message, "data object not available"

  ; get the data pointer
  qp = self.data->get('QUASAR')
  iCr = self->getImgIndex(lowfa=ilow, non_crushed=iNCr)
  n_ph = self.info->get('nb_phases')
  ; use last phase as default
  if n_elements(phase) eq 0 then phase = n_ph-1
  phase = 0 > phase < n_ph-1

  if keyword_set(crushed) then $
    ave = avrg(reform((*qp)[*,*,*,1, iCr, phase]), 4) $
  else ave = avrg(reform((*qp)[*,*,*,1, iNCr, phase]), 4)

  dataTag = 'meanVol'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = ave $
  else self.data->add, dataTag, ptr_new(ave, /no_copy)
  return, 1b
end ; }}}

function qaslcalc::redoMaskVol, noise=noise
;; make a binary mask using meanVol
;; noise * max(smoothed_epi) is the threshold for mask creation
;;   default = 0.15, bound btw 0.01 and 0.5
;; return 1 on success, 0 on failure
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0b
  endif

  if ~obj_valid(self.info) then $
    message, "info object not available"
  if ~obj_valid(self.data) then $
    message, "data object not available"

  if n_elements(noise) eq 0 then noise = 0.15 ; 15% of max(smoothed_average)
  noise = 0.01 > noise < 0.5 ; bound from 1% to 50%

  ; get the mean EPI volume
  dataTag = 'meanVol'
  ap = self.data->get(dataTag)
  if ~ptr_valid(ap) then begin
    ret = self->redoMeanVol()
    ap = self.data->get(dataTag)
  endif

  vol = smooth(*ap, 3)
  amax = max(vol)
  msk = vol gt amax*noise
  mm_kernel = [[0,1,0],[1,1,1],[0,1,0]]
  msk = erode(msk, mm_kernel)

  dataTag = 'maskVol'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = msk $
  else self.data->add, dataTag, ptr_new(msk, /no_copy)

  return, 1b
end ; }}}

function qaslcalc::GaussSmooth
;; Gaussian smoothing of the whole QUASAR dataset
;; Directly modifies data loaded into memory !!! 
;; return 1 on success, 0 on failure
;{{{
  compile_opt idl2,hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0b
  endif

  if ~obj_valid(self.info) then $
    message, "info object not available"
  if ~obj_valid(self.data) then $
    message, "data object not available"

  dataTag = 'QUASAR' 
  qp = self.data->get(dataTag)

  xyres = self.info->get('recon_res')
  n_sl = self.info->get('nb_slices')
  n_dy = self.info->get('nb_dynamics') ; includes lbl and ctr
  n_ph = self.info->get('nb_phases')
  ; total nb of 2D images
  nimgs = n_sl * n_dy * n_ph 

  ; reform to 3D array
  *qp = reform(*qp, [xyres,nimgs],/over)

  kern = [ [1.0, 2.0, 1.0], $
           [2.0, 4.0, 2.0], $
           [1.0, 2.0, 1.0] ]
  scale = total(kern)

  for j=0, nimgs-1 do $
    (*qp)[*,*,j] = convol( (*qp)[*,*,j], kern, scale, /edge_truncate )

  ; reform back to 6D array
  *qp = reform(*qp, [xyres,n_sl,2,n_dy/2,n_ph],/over)

  return, 1b
end ;}}}

function qaslcalc::getImgIndex, lowFA=lowFA, non_crushed=non_crushed
;; return index of crushed pairs
;; low FA and non_crushed via keywords
;; QUASAR data has a cycle of 7 (L,C) pairs
;; TODO: add better description of quasar data structure
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, -1L
  endif
  n_dyn = self.info->get('nb_dynamics')
  n_pairs = n_dyn/2
  idx = indgen(7, n_pairs/7)
  lowfa = (idx[6,*])[*]
  non_crushed = (idx[[2,5],*])[*]
  crushed = (idx[[0,1,3,4],*])[*]
  return, crushed
end ;}}}

function qaslcalc::redoSimpleAverage
;; perform data averaging over dynamics
;; separately for labels, controls, crushed, non-crushed, lowFA
;; save results into self.data with following tags:
;;   meanCrushLbl, meanCrushCtl, meanNonCrushLbl, meanNonCrushCtl, 
;;   meanLowFaLbl, meanLowFaCtl
;;   and tag+'_stderr'
;; return 1 on success, 0 on failure
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0b
  endif

  if ~obj_valid(self.info) then $
    message, "info object not available"
  if ~obj_valid(self.data) then $
    message, "data object not available"

  ; get the data pointer
  qp = self.data->get('QUASAR')
  iCr = self->getImgIndex(lowfa=ilFA, non_crushed=iNCr)

  ; save results into existing pointers or create new ones
  ; crushed label
  ave = reform( avrg((*qp)[*,*,*,0,icr,*], 5, variance=var, /double) )
  stderr = sqrt(reform(var))
  dataTag = 'meanCrushLbl'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = ave $
  else self.data->add, dataTag, ptr_new(ave, /no_copy)
  ap = self.data->get(dataTag+'_stderr')
  if ptr_valid(ap) then *ap = stderr $
  else self.data->add, dataTag+'_stderr', ptr_new(stderr, /no_copy)
  ; crushed control
  ave = reform(avrg((*qp)[*,*,*,1,icr,*], 5, variance=var, /double) )
  stderr = sqrt(reform(var))
  dataTag = 'meanCrushCtl'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = ave $
  else self.data->add, dataTag, ptr_new(ave, /no_copy)
  ap = self.data->get(dataTag+'_stderr')
  if ptr_valid(ap) then *ap = stderr $
  else self.data->add, dataTag+'_stderr', ptr_new(stderr, /no_copy)
  ; non-crushed label
  ave = reform(avrg((*qp)[*,*,*,0,incr,*], 5, variance=var, /double) )
  stderr = sqrt(reform(var))
  dataTag = 'meanNonCrushLbl'
  if ptr_valid(ap) then *ap = ave $
  else self.data->add, dataTag, ptr_new(ave, /no_copy)
  ap = self.data->get(dataTag+'_stderr')
  if ptr_valid(ap) then *ap = stderr $
  else self.data->add, dataTag+'_stderr', ptr_new(stderr, /no_copy)
  ; non-crushed control
  ave = reform(avrg((*qp)[*,*,*,1,incr,*], 5, variance=var, /double) )
  stderr = sqrt(reform(var))
  dataTag = 'meanNonCrushCtl'
  if ptr_valid(ap) then *ap = ave $
  else self.data->add, dataTag, ptr_new(ave, /no_copy)
  ap = self.data->get(dataTag+'_stderr')
  if ptr_valid(ap) then *ap = stderr $
  else self.data->add, dataTag+'_stderr', ptr_new(stderr, /no_copy)
;lowFA is currently not used
;  ; lowFA label
;  ave = reform(avrg((*qp)[*,*,*,0,ilFA,*], 5, variance=var, /double) )
;  stderr = sqrt(reform(var))
;  dataTag = 'meanLowFaLbl'
;  if ptr_valid(ap) then *ap = ave $
;  else self.data->add, dataTag, ptr_new(ave, /no_copy)
;  ap = self.data->get(dataTag+'_stderr')
;  if ptr_valid(ap) then *ap = stderr $
;  else self.data->add, dataTag+'_stderr', ptr_new(stderr, /no_copy)
;  ; lowFA control
;  ave = reform(avrg((*qp)[*,*,*,1,ilFA,*], 5, variance=var, /double) )
;  stderr = sqrt(reform(var))
;  dataTag = 'meanLowFaCtl'
;  if ptr_valid(ap) then *ap = ave $
;  else self.data->add, dataTag, ptr_new(ave, /no_copy)
;  ap = self.data->get(dataTag+'_stderr')
;  if ptr_valid(ap) then *ap = stderr $
;  else self.data->add, dataTag+'_stderr', ptr_new(stderr, /no_copy)

  return, 1b ; no errors
end; }}}

function qaslcalc::redoSubtraction
;; compute deltaM both for crushed and non-crushed data
;; save results into self.data with following tags:
;;   deltaMcr, deltaMncr
;;   and tag+'_stderr' 
;; return 1 on success, 0 on failure
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0b
  endif

  if ~obj_valid(self.info) then $
    message, "info object not available"
  if ~obj_valid(self.data) then $
    message, "data object not available"

  ; crushed deltaM
  pLbl = self.data->get('meanCrushLbl')
  pCtl = self.data->get('meanCrushCtl')
  dM = *pCtl - *pLbl
  dataTag = 'deltaMcr'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = dM $
  else self.data->add, dataTag, ptr_new(dM, /no_copy)
  ; compute std errors
  pLbl = self.data->get('meanCrushLbl_stderr')
  pCtl = self.data->get('meanCrushCtl_stderr')
  err = sqrt( (*pLbl)^2 + (*pCtl)^2 )
  ap = self.data->get(dataTag+'_stderr')
  if ptr_valid(ap) then *ap = err $
  else self.data->add, dataTag+'_stderr', ptr_new(err, /no_copy)

  ; non-crushed deltaM
  pLbl = self.data->get('meanNonCrushLbl')
  pCtl = self.data->get('meanNonCrushCtl')
  dM = *pCtl - *pLbl
  dataTag = 'deltaMncr'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = dM $
  else self.data->add, dataTag, ptr_new(dM, /no_copy)
  ; compute std errors
  pLbl = self.data->get('meanNonCrushLbl_stderr')
  pCtl = self.data->get('meanNonCrushCtl_stderr')
  err = sqrt( (*pLbl)^2 + (*pCtl)^2 )
  ap = self.data->get(dataTag+'_stderr')
  if ptr_valid(ap) then *ap = err $
  else self.data->add, dataTag+'_stderr', ptr_new(err, /no_copy)
  return, 1b ; no errors
end; }}}

function qaslcalc::getTimeCourse, crd, dataTag
;; get time-course at (x,y,z) voxel pointed by dataTag
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0
  endif
  if n_elements(crd) ne 3 then $
    message, "First parameter must be 3-el array"

  x = crd[0] & y = crd[1] & z = crd[2]
  ; signal
  p= self.data->get(dataTag)
  sig = (*p)[x,y,z,*,*,*,*] ; extra '*' are ignored if needed
  nPts = n_elements(sig)
  sig = 1.0d * reform(sig, nPts)
  ; signal error (if available)
  p= self.data->get(dataTag+'_stderr')
  if ptr_valid(p) then $
    err = reform( (*p)[x,y,z,*] ) $
  else err = sig*0  
  ; time
  pTI = self.data->get('TIarr')
  time = 1.0d * reform( (*pTI)[z,*] )
  ; convert time to image index for QUASAR
  if n_elements(time) ne nPts then time = dindgen(nPts)
  return, [[time],[sig],[err]]
end;}}}

function qaslcalc::redoFitTTP 
;; fit time-to-peak for all voxels in brain mask using gammavar function
;; ttp = p[1]*p[2]
;; save into self.data as 'TTPmap'
;; toto: improve handling of "bad" time-courses
;{{{
  compile_opt idl2,hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0
  endif
  p = self->getData('maskVol')
  msk = *p
  ; results volume
  ttp = msk * 0.0d
  nPh = self.info->get('nb_phases')
  ;max_ttp = max(self.info->get('trigger_time'))
  max_ttp = 4000.0
  wh = where(msk eq 1, cn)
  for j=0, cn-1 do begin
    crd = array_indices(msk, wh[j])
    x = crd[0] & y = crd[1] & z = crd[2]
    dat = self->getTimeCourse(crd, 'deltaMcr')
    time = dat[*,0]
    dM = dat[*,1]
    dMerr = dat[*,2]
    ;todo can this be improved?
    m1 = mean( dM[0:nPh/2] ) ; 1st half signal mean
    m2 = mean( dM[nPh/2+1:nPh-1] ) ; 2d half sig mean
    if m1 le m2 then begin
      ttp[x,y,z] = max_ttp ; no fitting
      continue
    endif
    ; todo put bounds on fit parameters
    pf0 = [max(dM), 15.0d, 100.0d]
    pf = mpfitfun('gammavar', time, dM, dMerr, pf0, /quiet)
    ttp[x,y,z] = pf[1]*pf[2]
  endfor
  ttp = ttp < max_ttp ; restrict to max
  dataTag = 'TTPmap'
  ap = self.data->get(dataTag)
  if ptr_valid(ap) then *ap = ttp $
  else self.data->add, dataTag, ptr_new(ttp, /no_copy)
  return, 1b
end; }}}

;;;;
;;;; access, life cycle and object definition methods
;;;;
function qaslcalc::getData, dataTag
;; get data pointer
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return, 0
  endif

  if ~obj_valid(self.data) then $
    message, "data object not available"
  
  if n_elements(dataTag) eq 0 then $
    message, "dataTag parameter is required"

  ; check if dataTag is in keys list
  tags = self.data->keys()
  wh = where(strpos(tags, dataTag) eq 0, cn)
  if cn eq 0 then $ 
    message, 'unknown dataTag: <'+dataTag+'>' 

  return, self.data->get(dataTag)
end;}}}

function qaslcalc::get, property
;; return object's properties
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->error
    return,0
  endif
  case strlowcase(property) of
    'name' : return, self.name
    'info' : return, self.info
    'data' : return, self.data
    'extra' : return, self.extra
    else : message, "Unknown object property: ", property
  endcase
end;}}}

pro qaslcalc::setErrObj, errObj, errMethod
;; setup error handling via external object
;{{{
  compile_opt hidden
  if obj_valid(errobj) then begin
    self.errObject = errobj
    self.errMethod = errMethod
  endif
end ; error }}}

pro qaslcalc::error
;; local error reporting
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->_error
    return
  endif
  if obj_valid(self.errObject) then $
    call_method, self.errMethod, self.errObject $
  else self->_error
end ; error }}}

pro qaslcalc::_error
;; internal error reporting
;{{{
  compile_opt hidden
  help, /last_message, output=err
  if n_elements(err) eq 0 then err = !error_state.msg
  ;ok = dialog_message(err,/error)
  for i=0, n_elements(err)-1 do print, err[i]
  catch,/cancel
end ; error }}}

function qaslcalc::badHashKey, value
;; helper function to check return value of hash->get(key)
;{{{
  compile_opt hidden
  type = size(value,/type)
  if type eq 0 then return, 1 ; undefined
  if type eq 7 then return, 0 ; string (i.e. must come from valid key)
  if type eq 10 then return, 0 ; pointer (   "   )
  return, value[0] eq self.hash_null
end;}}}

pro qaslcalc::hashPtrFree, hashobj
;; remove pointers from a hashtable object
;{{{
  compile_opt hidden
  if obj_valid(hashobj) then begin
    keys = hashobj->keys()
    for i=0, n_elements(keys)-1 do begin
      p = hashobj->get(keys[i])
      ; assuming no pointer-to-pointer and ptr_arr
      if size(p, /type) eq 10 then ptr_free, p
    endfor
  endif
end ; hashPtrFree }}}

pro qaslcalc::cleanup
;{{{
  compile_opt hidden
  ; errObj should not be destroyed by this object
  self->HashPtrFree, self.data
  obj_destroy, self.data
  obj_destroy, self.info
  if ptr_valid(self.extra) then ptr_free, self.extra
end ; cleanup }}}

function qaslcalc::init, name=name, hash_null=hash_null
;{{{
  on_error, 2
  if n_elements(name) gt 0 then self.name = 'qASLcalc'
  if n_elements(hash_null) eq 0 then hash_null=-9999L
  self.hash_null = hash_null
  return, 1
end ; init() }}}

pro qaslcalc__define
  compile_opt idl2, hidden
  class = { QASLCALC, $
    name     : "",         $ ; object name
    errObject: obj_new(),  $ ; error handling object
    errMethod: "",         $ ; error handling method
    hash_null: 0L,         $ ; hash null value
    info     : obj_new(),  $ ; hashtable contains PAR header
    data     : obj_new(),  $ ; hash with pointers to data & results
    extra    : ptr_new()   $ ; utility pointer
  }
end

