; fitting functions
;{{{
function fatcalc_fit_t2s, t, p
  compile_opt hidden
  return, p[0]*exp(-t/p[1])
end

; p = [scale, ff, t2s, b0_field]
; b0_field is a fixed parameter
function fatcalc_fit_wf3par, t, p
  compile_opt hidden
  wfs_rad = 3.35e-6 * 2.675e8 * p[3] * 1e-3; [rad/ms]
  e_t2s = exp(-t/p[2]) 
  return, p[0]*sqrt( (1-p[1])^2 + p[1]^2 + $
          2*(1-p[1])*p[1]*cos(wfs_rad*t) )*e_t2s
end

; p = [scale, ff, t2sw, t2sf, b0_field]
; b0_field is a fixed parameter
function fatcalc_fit_wf4par, t, p
  compile_opt hidden
  wfs_rad = 3.35e-6 * 2.675e8 * p[4] * 1e-3; [rad/ms]
  e_t2sw = exp(-t/p[2]) 
  e_t2sf = exp(-t/p[3]) 
  return, p[0]*sqrt( ((1-p[1])*e_t2sw)^2 + (p[1]*e_t2sf)^2 + $
          2*(1-p[1])*p[1]*e_t2sw*e_t2sf*cos(wfs_rad*t))
end
;}}}

;;;;;;;;;;;;;;;;;;;;;;;; time-series
function fatcalc::fit_water_t2s, x, y,z, yfit=yfit
;{{{
  compile_opt hidden
  on_error, 2
  tc = self->timecourse(x,y,z)
  npts = n_elements(tc)
  ieven = indgen(npts/2)*2
  iodd = ieven + 1
  tc = tc[iodd]
  tt = (*self.time_vec)[iodd]
  p0 = [max(tc), 10]
  p_fit = mpfitfun('fatcalc_fit_t2s', tt, tc,1,p0, /quiet)
  yfit = fatcalc_fit_t2s(*self.time_vec, p_fit)
  return, p_fit
end ;}}}

function fatcalc::fit_pixel_wf, x, y, z, yfit=yfit, chi2=chi2, $
    np=np, use_real=use_real, max_echo=max_echo, fit_info=fit_info
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(np) eq 0 then np = 3
  use_real = keyword_set(use_real)
  if n_elements(max_echo) eq 0 then max_echo = self.dim[3]
  max_echo = 3 > max_echo < self.dim[3]

  pini = self.pini
  pinf = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, np+1)
  ;if np eq 4 then pinf = [pinf, pinf[2]]
  pinf[1].limited = [1,1]
  pinf[1].limits = [0.0,1.0]
  pinf[2].limited = [1,1]
  pinf[2].limits = [1e-3,2e2]
  pinf[1:2].value = pini[0:1,0]
  if np eq 4 then pinf[3].value = pini[2,0]
  pinf[np].value = self->get_b0_field()
  pinf[np].fixed = 1
    
  tt = (*self.time_vec)[0:max_echo-1]
  npts = n_elements(tc)

  s_cpx = reform((*self.cmpx)[x,y,z,0:max_echo-1])
  s_re = real_part(s_cpx)
  s_mag = double(abs(s_cpx))

  wght = (1.0)/(s_mag)

  pinf[0].value = max(s_mag)
  if (use_real eq 1) then begin
    if s_re[0] le 0 then begin ; fat
      pinf[1].value = pini[0,1]
      pinf[2].value = pini[1,1]
      if np eq 4 then begin
        pinf[2].value = pini[1,1]
        pinf[3].value = pini[2,1]
      endif
    endif else begin
      pinf[1].value = pini[0,0]
      pinf[2].value = pini[1,0]
      if np eq 4 then begin
        pinf[2].value = pini[1,0]
        pinf[3].value = pini[2,0]
      endif
    endelse
  endif

  if np eq 3 then begin
    p_fit = mpfitfun('fatcalc_fit_wf3par', tt, s_mag, 1,parinfo=pinf, $
        weight=wght, status=status, errmsg=mpfmsg, bestnorm=bn, $
        perror=perr, /quiet)
  endif else begin
    p_fit = mpfitfun('fatcalc_fit_wf4par', tt, s_mag, 1,parinfo=pinf, $
        weight=wght, status=status, errmsg=mpfmsg, bestnorm=bn, $
        perror=perr, /quiet)
  endelse

  tab = string('    ') & cr = string(13b)
  fit_info = strarr(4)
  fit_info[0] = 'crd: ' + string(x, y, z, format='(I4, I4, I4)')
  fit_info[1] = 'np_fit: ' + strtrim(np,2) + tab
  fit_info[1] += 'realimg: ' + strtrim(use_real,2) + tab
  fit_info[1] += 'nb_echoes: ' + strtrim(max_echo,2)
  fit_info[2] = 'initial: '+ tab
  if np eq 3 then fit_info[2] += string(pinf[0:2].value, $
      format='(E0.2, F8.2, F8.2)' ) + cr $
  else fit_info[2] += string(pinf[0:3].value, $
      format='(E0.2, F8.2, F8.2, F8.2)' ) + cr 
  fit_info[3] = 'fitted: '+ tab
  if np eq 3 then fit_info[3] += string(p_fit[0:2], $
      format='(E0.2, F8.2, F8.2)' ) $
  else fit_info[3] += string(p_fit[0:3], $
      format='(E0.2, F8.2, F8.2, F8.2)' )
  ;print, fit_info
  return, p_fit
end ;}}}

function fatcalc::timecourse, x,y,z, real=real, imag=imag
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(x) eq 0 then x=0
  if n_elements(y) eq 0 then y=0
  if n_elements(z) eq 0 then z=0
  x = 0 > x < (self.dim[0]-1)
  y = 0 > y < (self.dim[1]-1)
  z = 0 > z < (self.dim[2]-1)
  if keyword_set(real) then $
    tc = reform(real_part((*self.cmpx)[x,y,z,*])) $
  else if keyword_set(imag) then $
    tc = reform(imaginary((*self.cmpx)[x,y,z,*])) $
  else tc = reform((*self.data)[x,y,z,*])
  return, tc
end ;}}}

;;;;;;;;;;;;;;;;;;;;;;;;; images/maps
pro fatcalc::fit_in_mask, correct_dB=correct_dB, errmsg=errmsg, npar=npar, $
    use_real=use_real, max_echo=max_echo
;{{{
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    catch, /cancel
    help, /last_message, output=errmsg
    if obj_valid(progress) then progress -> destroy
    return
  endif

  if self->status(/data) eq 0 then begin
    errmsg = 'data not available'
    return
  endif
    
  if n_elements(npar) eq 0 then npar=4
  if (npar lt 3) or (npar gt 4) then $
    message, "Npar = "+strtrim(npar,2) + " (must be 3 or 4)"

  np=npar
  np0 = np-1 ; for array subscription
    
  use_real = keyword_set(use_real)

  if n_elements(max_echo) eq 0 then max_echo = self.dim[3]
  max_echo = 3 > max_echo < self.dim[3]

  ; body mask
  if ptr_valid(self.mask) eq 0 then msk = self->newmask(echo=1, noise=0.05) $
  else msk = *self.mask

  wn = where(msk gt 0, cn)
  if cn eq 0 then begin
    errmsg = 'empty mask'
    return
  endif

  ; make sure single slice is still represented as 3D array 
  if self.dim[2] eq 1 then msk = reform(msk, self.dim[0:2])

  ; convert 1D to 3D indices
  xyz = array_indices(msk, wn)

  ; results arrays
  ;ims = size(msk,/dimen)
  ;if np eq 3 then dimen = [ims,3] else dimen = [ims,4]

  fit_res = make_array(/float, dimension=[self.dim[0:2], 4])
  fit_err = fit_res
  fit_sigma = make_array(/float, dimension=self.dim[0:2])
  fit_bad = msk*0b

  b0 = self->get_b0_field()
  pini = self.pini
  ; fit parameters bounds
  if np eq 3 then begin
    pinf = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, 4)
    pinf[*].value = [0.0, pini[0:1,0],b0] ; initial values
    pinf[1].limited = [1,1] ; fat fraction
    pinf[1].limits = [0.0,1.0]
    pinf[2].limited = [1,1] ; water t2s [ms]
    pinf[2].limits = [1e-3,2e2]
    pinf[3].fixed = 1
  endif else begin
    pinf = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, 5)
    pinf[*].value = [0.0, pini[*,0], b0 ] ; initial values
    pinf[1].limited = [1,1] ; fat fraction
    pinf[1].limits = [0.0,1.0]
    pinf[2].limited = [1,1] ; water t2s [ms]
    pinf[2].limits = [1e-3,2e2]
    pinf[3].limited = [1,1] ; fat t2s [ms]
    pinf[3].limits = [1e-3,2e2]
    pinf[4].fixed = 1
  endelse

  tt = (*self.time_vec)[0:max_echo-1]
  dof = n_elements(tt) - 4

  progress = obj_new('progressbar', Text="Fitting in progress...", $
        group_leader=self.guitlb, /fast_loop)

  progress -> start
    
  for i=0L,cn-1 do begin
    if progress -> checkcancel() then begin
      progress -> destroy
      errmsg="Fitting cancelled..."
      ptr_free, self.fit_res, self.fit_err, self.fit_sigma, self.fit_bad
      return
    endif
    x = xyz[0,i] & y = xyz[1,i] & z=xyz[2,i]
    s_cpx = reform((*self.cmpx)[x,y,z,0:max_echo-1])
    s_re = real_part(s_cpx)
    s_mag = double(abs(s_cpx))
    ;s_m_max = max(s_mag, min=s_m_min)
    wght = (1.0)/(s_mag)
    pinf[0].value = max(s_mag)
    if (use_real eq 1) then begin
      if s_re[0] le 0 then begin ; fat
        pinf[1].value = pini[0,1]
        pinf[2].value = pini[1,1]
        if np eq 4 then begin
          pinf[2].value = pini[1,1]
          pinf[3].value = pini[2,1]
        endif
      endif else begin
        pinf[1].value = pini[0,0]
        pinf[2].value = pini[1,0]
        if np eq 4 then begin
          pinf[2].value = pini[1,0]
          pinf[3].value = pini[2,0]
        endif
      endelse
    endif

; old pinf {{{
;            if (s_mag[0] gt s_mag[1]) then begin ; pure fat
;                pinf[1].value = 1.0
;                pinf[1].fixed = 1
;                pinf[2].value = 0.0 ; water t2s
;                pinf[2].fixed = 1
;            endif else begin
;                pinf[1].value = 0.75 ; more fat than water
;                pinf[1].fixed = 0
;                pinf[2].value = 30.0 ; water t2s
;                pinf[2].fixed = 0
;            endelse
;        endif else begin ; water
;            if (s_mag[0] gt s_mag[1]) then begin ; pure water
;                pinf[1].value = 0.0
;                pinf[1].fixed = 1
;                pinf[3].value = 0.0 ; fat t2s
;                pinf[3].fixed = 1
;            endif else begin
;                pinf[1].value = 0.05 ; more water than fat
;                pinf[1].fixed = 0
;                pinf[3].value = 20.0 ; fat t2s
;                pinf[3].fixed = 0
;            endelse
;        endelse
;        endif
;}}}

    if np eq 3 then begin
      ;if i eq 0 then print, " 3 par fit"
      p_fit = mpfitfun('fatcalc_fit_wf3par', tt, s_mag, 1,parinfo=pinf, $
          weight=wght, status=status, errmsg=mpfmsg, bestnorm=bn, $
          perror=perr, /quiet)
    endif else begin
      ;if i eq 0 then print, " 4 par fit"
      p_fit = mpfitfun('fatcalc_fit_wf4par', tt, s_mag, 1,parinfo=pinf, $
          weight=wght, status=status, errmsg=mpfmsg, bestnorm=bn, $
          perror=perr, /quiet)
    endelse
    if status LE 0 then begin
      fit_bad[x,y,z] = 1
      errmsg = 'mpfitfun: ' + mpfmsg
    endif else begin
      fit_res[x,y,z,0:np0] = p_fit[0:np0]
      fit_err[x,y,z,0:np0] = perr[0:np0]
      fit_sigma[x,y,z] = sqrt(bn/dof)
    endelse

    percent = 100.0 * (i+1) / cn
    percent = float(round(percent*10))/10

    if percent mod 1 eq 0.0 then $
      progress -> update, percent, $
          text='Fitting: ' + strtrim(fix(percent),2)+'%'
  endfor

  progress->destroy

  ; save results
  if ptr_valid(self.fit_res) then begin
    ptr_free, self.fit_res, self.fit_err, self.fit_sigma, self.fit_bad
  endif
  self.fit_res = ptr_new(fit_res, /no_copy)
  self.fit_err = ptr_new(fit_err, /no_copy)
  self.fit_sigma = ptr_new(fit_sigma, /no_copy)
  self.fit_bad = ptr_new(fit_bad, /no_copy)
end ;}}}

pro fatcalc::dixon3pt
;{{{
  compile_opt hidden
  on_error, 2
  if ptr_valid(self.dB_map) eq 0 then $
      self->deltaB, echo1=1, echo2=3 

  dB = *self.dB_map

  m1 = self->rawimage(echo=1)
  m3 = self->rawimage(echo=3)
  m2 = self->rawimage(echo=2, /complex)
  te2 = (*self.time_vec)[2] * 1e-3
  gamma_rad = 42.5729 * 1e6 * 2 * !pi ; [rad/s/T]
  phase = gamma_rad * dB * te2 + (*self.ph0_map)
  m_op = m2 * complex(cos(phase),sin(phase))
  m_in = 0.5 * (m1+m3)
  ptr_free, self.w_map, self.f_map
  self.w_map = ptr_new(0.5 * abs(m_in + m_op))
  self.f_map = ptr_new(0.5 * abs(m_in - m_op))
end ;}}}

; cpxrecon(/han) for all images
; replacing self.data and self.cpx
pro fatcalc::rec_all
;{{{
  compile_opt hidden
  on_error, 2
  if self.sess.reconstructed eq 1 then return
  han = hanning(self.dim[0], self.dim[1], alpha=0.5)
  han = shift(han, self.dim[0]/2, self.dim[1]/2)
  for ec=0, self.dim[3]-1 do begin
    for sl=0, self.dim[2]-1 do begin
      cpx = (*self.cmpx)[*,*,sl,ec]
      ks = fft(cpx,/inverse) * han
      cpx = fft(ks)
      (*self.cmpx)[*,*,sl,ec] = cpx
      (*self.data)[*,*,sl,ec] = abs(cpx)
    endfor
  endfor
  self.sess.reconstructed = 1b
  ptr_free, self.mask
  ;ptr_free, self.dB_map
  self->deltaB
;    gamma_rad = 2*!pi*self.sess.gammabar * 1e6
;    for i=0, self.dim[3]-1 do begin
;        cpx = (*self.cmpx)[*,*,i]
;        te = (*self.time_vec)[i] * 1e-3
;        phase = gamma_rad * te * (*self.dB_map) 
;        cpx *= complex(cos(phase),sin(phase))
;        (*self.data)[*,*,i] = abs(cpx)
;    endfor
end;}}}

; compute deltaB from 2 in-phase images
pro fatcalc::deltaB, echo1 = echo1, echo2=echo2
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(echo1) eq 0 then echo1 = 1
  if n_elements(echo2) eq 0 then echo2 = 3

  gamma_rad = 2*!pi*self.sess.gammabar * 1e6 ; [rad/s/T]

  te1 = (*self.time_vec)[echo1] * 1e-3 ; [s]
  te2 = (*self.time_vec)[echo2] * 1e-3 ; [s]
  
  ph1 = self->rawphase(echo=echo1)
  ph2 = self->rawphase(echo=echo2)
  
  deltaB = 1.0/(gamma_rad * (te1-te2)) * (ph2-ph1) ; [T]
  phi0 = -ph1 + gamma_rad * te1 * deltaB ; [rad]

  if ptr_valid(self.dB_map) then ptr_free, self.dB_map
  self.dB_map = ptr_new(deltaB, /no_copy)
  if ptr_valid(self.ph0_map) then ptr_free, self.ph0_map
  self.ph0_map = ptr_new(phi0, /no_copy)
end ;}}}

function fatcalc::newmask, echo=echo, noise=noise, fill=fill
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(echo) eq 0 then echo=1
  im = (*self.data)[*,*,*,echo]

  ; default "noise" level = 5% of cumulative image intensity
  if n_elements(noise) eq 0 then noise = 0.05
  
  ; just an effective way to get image subscripts 
  ; for linear division image range into 100 bins
  h = histogram(im, location=bins, nbins=100, reverse_ind=r)
  eh = h*0.0
  for i=0,99 do eh[i] = (r[i] ne r[i+1]) ? total(im[r[r[i]:r[i+1]-1]]) : 0.0 
  cum = total(eh, /cumulative)
  ii = value_locate(cum, cum[99]*noise)
  thres = bins[ii]

  msk = im ge thres
  ptr_free, self.mask
  self.mask = ptr_new(msk,/no_copy)
  return, *self.mask
end ;}}}

function fatcalc::cpxrecon, slice=slice, echo=echo, phase=phase, hanfilt=hanfilt
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(slice) eq 0 then slice=0
  if n_elements(echo) eq 0 then echo=0
  cim = (*self.cmpx)[*,*,slice,echo]
  k_space = shift(fft(cim, /inverse), self.dim[0]/2, self.dim[1]/2)
  if keyword_set(hanfilt) then begin
    han = hanning(self.dim[0], self.dim[1], alpha=0.5, /double)
    k_space *= han
  endif
  cpx = fft(k_space)
  phase=atan(cpx,/phase)
  return, abs(cpx)
end ;}}}

;;;;;;;;;;;;;;;;;;;;;;;;; general
function fatcalc::rawphase, echo=echo
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(echo) eq 0 then echo=0
  cim = (*self.cmpx)[*,*,*,echo]
  return, atan(cim,/phase)
end ;}}}

function fatcalc::rawimage, slice=slice, echo=echo, $
    complex=cpx, real=real, imag=imag
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(echo) eq 0 then echo=0
  if n_elements(slice) eq 0 then slice=0
  max_slice = fix(max((*self.info).slice_number))-1
  max_echoe = fix(max((*self.info).echo_number))-1
  echo = 0 > echo < max_echoe
  slice = 0 > slice < max_slice
  cpx = keyword_set(cpx) || keyword_set(real) || keyword_set(imag)
  if cpx eq 1 then begin
    im = (*self.cmpx)[*,*,slice,echo]
    if keyword_set(real) then im=real_part(im)
    if keyword_set(imag) then im=imaginary(im)
  endif else im = (*self.data)[*,*,slice,echo]
  return, im
end ;}}}

; returns a slice or 3D volume of any map
; note: 1st slice should be 1 not 0 
; (else keyword_set(slice) doesn't work) <-- BAD practice (use n_elements)
function fatcalc::getmap, mask=mask, dB=dB, AW=AW, AF=AF, $
    T2SW=t2sw, T2SF=t2sf, chi2=chi2, bad_fit=bad_fit, slice=slice
;{{{
  compile_opt hidden
  on_error,2
  if keyword_set(mask) and ptr_valid(self.mask) then $
      ret = *self.mask
  if keyword_set(dB) and ptr_valid(self.dB_map) then $
      ret = *self.dB_map
  if keyword_set(AW) and ptr_valid(self.fit_res) then $
      ret = 1.0 - (*self.fit_res)[*,*,*,1]
  if keyword_set(AF) and ptr_valid(self.fit_res) then $
      ret = (*self.fit_res)[*,*,*,1]
  if keyword_set(t2sw) and ptr_valid(self.fit_res) then $
      ret = (*self.fit_res)[*,*,*,2]
  if keyword_set(t2sf) and ptr_valid(self.fit_res) then begin
      ret = (*self.fit_res)[*,*,*,3]
      if max(ret) eq 0 then return, 0
  endif
  if keyword_set(chi2) and ptr_valid(self.fit_res) then $
      ret = *self.fit_sigma
  if keyword_set(bad_fit) and ptr_valid(self.fit_bad) then $
      ret = *self.fit_bad

  if n_elements(ret) eq 0 then return, 0

  if keyword_set(slice) then $
    return, ret[*,*,slice-1] * (*self.mask)[*,*,slice-1] $
  else return, ret * (*self.mask)
end ;}}}

function fatcalc::getdim
;{{{
  compile_opt hidden
  return, self.dim
end ;}}}

function fatcalc::getinfo, tag, echo=echo
;{{{
  compile_opt hidden
  on_error, 2
  tag = strupcase(tag)
  wi = where( tag_names(*self.info) eq tag, cn)
  if cn gt 0 then begin
    vals = ((*self.info).(wi[0]))
    ; most general info tags have single value
    if n_elements(vals) eq 1 then return, vals
    im_type = 0 ; only magnitude
    type_idx = where((*self.info).image_type_mr eq im_type)
    all = vals[type_idx]
    if n_elements(echo) gt 0 then $
      return, all[echo<(n_elements(all)-1)] $
    else return, all
  endif else return, 'unknown'
end ;}}}

function fatcalc::ptrref, data=data, cpx=cpx, info=info, mask=mask, $
    time=time, fit_res=fit_res
;{{{
  compile_opt hidden
  on_error, 2
  null = ptr_new()
  if self->status(/data) eq 0 then return, null
  if keyword_set(data) then return, self.data
  if keyword_set(cpx) then return, self.cmpx
  if keyword_set(info) then return, self.info
  if keyword_set(mask) then return, self.mask
  if keyword_set(time) then return, self.time_vec
  if keyword_set(fit_res) then return, self.fit_res
  ; else
  return, null
end ;}}}

function fatcalc::status, data=data, cpx=cpx, mask=mask, dB=dB, fitted=fitted
;{{{
  compile_opt hidden
  dat = ptr_valid(self.data)
  if keyword_set(data) then return, dat
  if keyword_set(cpx) then return, dat * ptr_valid(self.cmpx)
  if keyword_set(mask) then return, dat * ptr_valid(self.mask)
  if keyword_set(dB) then return, dat * ptr_valid(self.dB_map)
  if keyword_set(fitted) then return, ptr_valid(self.fit_res)
  ; else
  return, dat
end ;}}}

function fatcalc::get_initial_fit_pars, pini
;{{{
  compile_opt hidden
  return, self.pini
end;}}}

pro fatcalc::set_initial_fit_pars, pini
;{{{
  compile_opt hidden
  self.pini = pini
end;}}}

function fatcalc::get_B0_field
;{{{
  compile_opt hidden
  return, self.sess.B0
end;}}}

pro fatcalc::set_B0_field, field
;{{{
  compile_opt hidden
  self.sess.B0 = float(field)
end;}}}

pro fatcalc::savemaps
;{{{
  compile_opt hidden
  on_error, 2
  x_voxel = (self->getinfo('pixel_spacing_x'))[0]
  y_voxel = (self->getinfo('pixel_spacing_y'))[0]
  z_voxel = (self->getinfo('slice_thickness'))[0]
  z_voxel += (self->getinfo('slice_gap'))[0]

  vox = [x_voxel, y_voxel, z_voxel]

  ; file basename
  fbase = self.sess.datafile
  ; get rid of extension
  fbase = strmid(fbase, 0, strlen(fbase)-4)

  map = self->getmap(/AW)
  if n_elements(map) gt 1 then begin
    ff = filepath(fbase + '_Wfrac.hdr', root=self.sess.datadir)
    writeanz, map, ff, voxel=vox, /over
  endif
  
  map = self->getmap(/AF)
  if n_elements(map) gt 1 then begin
    ff = filepath(fbase + '_Ffrac.hdr', root=self.sess.datadir)
    writeanz, map, ff, voxel=vox, /over
  endif

  map = self->getmap(/T2SW)
  if n_elements(map) gt 1 then begin
    ff = filepath(fbase + '_T2sW.hdr', root=self.sess.datadir)
    writeanz, map, ff, voxel=vox, /over
  endif

  map = self->getmap(/T2SF)
  if n_elements(map) gt 1 then begin
    ff = filepath(fbase + '_T2sF.hdr', root=self.sess.datadir)
    writeanz, map, ff, voxel=vox, /over
  endif
end ;}}}

pro fatcalc::loaddata, parfile, get_file=get_file
;{{{
  compile_opt hidden
  on_error, 2
  data = parrec(parfile, info=info, /float, /flipY, $
                  get_file=keyword_set(get_file))
  if n_elements(data) le 1 then message, 'failed to load data'

  ; take nb of slices and echoes from image info part 
  nb_slices = fix(max(info.slice_number))
  nb_echoes = fix(max(info.echo_number))
  self.dim = [info.recon_res, nb_slices, nb_echoes]

  ; sort mag/real/imag
  m_index = where(info.image_type_mr eq 0, cn)
  mag = reform(data[*,*,m_index], self.dim)
  r_index = where(info.image_type_mr eq 1, cn)
  real = reform(data[*,*,r_index], self.dim)
  i_index = where(info.image_type_mr eq 2, cn)
  imag = reform(data[*,*,i_index], self.dim)
 
;    if info.recon_res[0] gt 128 then begin
;        info.recon_res = info.recon_res/2
;        self.dim = [info.recon_res, info.nb_slices, info.nb_echoes]
;        mag = rebin(mag, self.dim)
;        real = rebin(real, self.dim)
;        imag = rebin(imag, self.dim)
;    endif

  timing = info.echo_time[m_index]
  timing = timing[uniq(timing)]

  ; data loaded
  self->free_all_pointers
  self.data = ptr_new(mag,/no_copy)
  self.cmpx = ptr_new(complex(real,imag),/no_copy)
  self.info = ptr_new(info,/no_copy)
  self.time_vec = ptr_new(timing,/no_copy)
  self.sess.reconstructed = 0
  self.sess.datafile = file_basename(parfile)
  self.sess.datadir = file_dirname(parfile)
end ;}}}

pro fatcalc::save_session, name, data=data
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(name) eq 0 then begin
    caldat, systime(/julian), mm, dd, yy
    nn = "fatcalc_session_"
    name = string(nn, yy,mm,dd, format='(%"%s_%04d%02d%02d")' )
  endif
  if self.sess.savname eq '' then self.sess.savname = name
  rootdir = programrootdir()
  sav = filepath(self.sess.savname+'.sav', root=rootdir)
  session = self.sess
  if keyword_set(data) AND self->status(/data) then begin
    data = *self.data
    info = *self.info
    save, session, data, info, filename=sav, /compress
  endif else $
    save, session, filename=sav
end ;}}}

pro fatcalc::restore_session, name, status=status
;{{{
  compile_opt hidden
  on_error, 2
  status = 'bad'
  if n_params() lt 1 then message, "No session file provided"
  restore, name
  self.sess.savfile = name
  self.sess.datadir = session.datadir
  self.sess.datafile = session.datafile
  if n_elements(data) gt 0 then begin
    ptr_free, self.data
    self.data = ptr_new(data, /no_copy)
  endif
  if n_elements(info) gt 0 then begin
    ptr_free, self.info
    self.info = ptr_new(info, /no_copy)
  endif
  status = 'ok'
end ;}}}

function fatcalc::init, session_sav=sav, group_leader=wid
;{{{
  compile_opt hidden
  on_error, 2
  if n_elements(sav) gt 0 then begin
    self->restore_session, sav, status=status
    if status ne 'ok' then begin
      message, "Failed to restore: " + session
      return, 0
    endif
  endif
  self.sess.B0 = 1.5
  self.sess.gammabar = 42.5729
  self.sess.wfs_ppm = 3.35e-6
  if n_elements(wid) gt 0 then $
    if widget_info(wid,/valid_id) eq 1 then self.guitlb=wid
  return, 1
end ; }}}

pro fatcalc::free_all_pointers
;;{{{
  compile_opt hidden
  class = {FATCALC}
  for i=0, n_tags(class)-1 do begin
    if size(self.(i), /tname) NE 'POINTER' then continue
    if ptr_valid(self.(i)) then ptr_free, self.(i)
  endfor
end ; }}}

pro fatcalc::cleanup
  compile_opt hidden
  self->free_all_pointers
end

pro fatcalc__define
  sess = { FATCALCSESS, $
    savfile: "",  $
    datadir: "",  $
    datafile: "", $
    reconstructed:0b, $
    wfs_ppm : 3.35e-6, $ ; water fat shift
    B0 : 1.5,          $ ; main field
    gammabar : 42.5729 $ ; [MHz/T]
  }

    class = { FATCALC,$
      data : ptr_new(), $ ; pointer to magnitude data
      dim : intarr(4),  $ ; image dimensions
      cmpx : ptr_new(), $ ; complex data
      info : ptr_new(), $ ; pointer to data info struct
      time_vec : ptr_new(), $
      mask : ptr_new(), $ ; body mask
      dB_map : ptr_new(), $ ; deltaB map
      ph0_map : ptr_new(),$ ; initial phase
      pini : fltarr(3,2), $ ; initial fit parameters
      fit_res : ptr_new(), $ ; fitted parameters
      fit_err : ptr_new(), $ ; estimated errors on fitted parameters
      fit_sigma : ptr_new(), $ ; sqrt(total((y-yfit)^2)/dof)
      fit_bad : ptr_new(), $ ; possible outliers
      sess : sess, $
      guitlb : -1L $ ; TLB ID for dialogs
   }
end
