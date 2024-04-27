; normal individual
; L1 C1 L2 C2 L3 C3 L1 C1 L2 C2 L3 C3 ...
;  0  1  2  3  4  5  6  7  8  9 10 11 ... index
;  0  0  1  1  2  2  3  3  4  4  5  5 ... pair index

function get_initials, s
; 'john smith' -> 'js'
;{{{
  if strlen(s) eq 0 then return, '' 
  ii=strsplit(s, ' ')
  return, strjoin(strmid(s,ii,1), '')
end;}}}

pro process_data, file
;{{{
  calc = obj_new('rpicalc')
  calc->SetProperty, datafile=file
  calc->LoadData
  pars = calc -> GetRPIPars(xres=xres, yres=yres, $
          n_slices=n_slices, n_phases=n_phases)
  ; for results files
  dir = file_dirname(file)
  pat_ini = get_initials(pars.patient)
  vox = [pars.fov/xres, pars.fov/yres, pars.slice_thick+pars.slice_gap]
  
  ; brain mask (needed?)
  msk = calc->brainmask(/redo, noise=25)
  fout = filepath(pat_ini+'_brain_mask', root=dir)
  writeanz, msk, fout, voxel=vox, /over

  ; normal subraction
  calc -> SetProperty, label_type='normal', control_type='individual'
  calc->Subtract
  p = calc->_getptr(/lica)
  fout = filepath(pat_ini+'_lica_n', root=dir)
  writeanz, *p, fout, voxel=vox, /over
  ; M0 for lica (control)
  idx = calc->Lblindex('lica','control')
  m0 = fix(round(calc->getM0(idx)))
  fout = filepath(pat_ini+'_lica_n_m0', root=dir)
  writeanz, m0, fout, voxel=vox, /over

  p = calc->_getptr(/rica)
  fout = filepath(pat_ini+'_rica_n', root=dir)
  writeanz, *p, fout, voxel=vox, /over
  ; M0 for rica (control)
  idx = calc->Lblindex('rica','control')
  m0 = fix(round(calc->getM0(idx)))
  fout = filepath(pat_ini+'_rica_n_m0', root=dir)
  writeanz, m0, fout, voxel=vox, /over

  p = calc->_getptr(/post)
  fout = filepath(pat_ini+'_post_n', root=dir)
  writeanz, *p, fout, voxel=vox, /over
  ; M0 for rica (control)
  idx = calc->Lblindex('post','control')
  m0 = fix(round(calc->getM0(idx)))
  fout = filepath(pat_ini+'_post_n_m0', root=dir)
  writeanz, m0, fout, voxel=vox, /over

  ; fake subraction
  calc -> SetProperty, label_type='normal', control_type='fake_rot'
  calc->Subtract
  p = calc->_getptr(/lica)
  fout = filepath(pat_ini+'_lica_r', root=dir)
  writeanz, *p, fout, voxel=vox, /over
  p = calc->_getptr(/rica)
  fout = filepath(pat_ini+'_rica_r', root=dir)
  writeanz, *p, fout, voxel=vox, /over
  p = calc->_getptr(/post)
  fout = filepath(pat_ini+'_post_r', root=dir)
  writeanz, *p, fout, voxel=vox, /over

  ; M0 for rotating (average control)
  idx = calc->Lblindex('lica','control')
  m0 = fix(round(calc->getM0(idx)))
  fout = filepath(pat_ini+'_ave_r_m0', root=dir)
  writeanz, m0, fout, voxel=vox, /over

  print, "Done with " + file
  obj_destroy, calc
end ;}}}

pro show_results, folder
;{{{

  f_mask = file_search(folder, '*_brain_mask.hdr')
  f_lica_n = file_search(folder, '*_lica_n.hdr')
  f_lica_r = file_search(folder, '*_lica_r.hdr')
  f_rica_n = file_search(folder, '*_rica_n.hdr')
  f_rica_r = file_search(folder, '*_rica_r.hdr')
  f_post_n = file_search(folder, '*_post_n.hdr')
  f_post_r = file_search(folder, '*_post_r.hdr')

  mask = reverse(readanz(f_mask), 2)
  lica_n = reverse(readanz(f_lica_n), 2)
  lica_r = reverse(readanz(f_lica_r), 2)
  rica_n = reverse(readanz(f_rica_n), 2)
  rica_r = reverse(readanz(f_rica_r), 2)
  post_n = reverse(readanz(f_post_n), 2)
  post_r = reverse(readanz(f_post_r), 2)
  sz = size(lica_n, /dimen)
  for i=0, sz[3]-1 do begin
    lica_n[*,*,*,i] *= mask
    lica_r[*,*,*,i] *= mask
    rica_n[*,*,*,i] *= mask
    rica_r[*,*,*,i] *= mask
    post_n[*,*,*,i] *= mask
    post_r[*,*,*,i] *= mask
  endfor

  bb = mr_bbox(mask[*,*,0]) ; bounding box (for cropping)
  ws = bb[*,1] - bb[*,0] + [1,1] ; cropped size

  dsp = bytarr(3, 64, 64)
  last_path = strlowcase(file_basename(folder))
  resu_path = file_dirname(folder) + '/proc/png'
  resu_png = filepath(last_path+'.png', root=resu_path)

  ph = [0,1,2,3,4,5,6,7] ; phases to display
  ph_str = string(ph*300 + 50)
  n_ph = n_elements(ph)
  sl = 3 ; slice to display
  ; display in two rows:
  ;     normal subtraction
  ;     rotating subtraction
  sc = 3
  dm_max = 10.
  window, xsize=ws[0]*sc*n_ph, ysize=ws[1]*sc*2
  for i=0, n_ph-1 do begin
    dsp[0,*,*] = bytscl(lica_n[*,*,sl,ph[i]], min=0, max=dm_max)
    dsp[1,*,*] = bytscl(rica_n[*,*,sl,ph[i]], min=0, max=dm_max)
    dsp[2,*,*] = bytscl(post_n[*,*,sl,ph[i]], min=0, max=dm_max)
    crop = arrex(dsp, [0,bb[*,0]], [2,bb[*,1]] )
    tv, congrid(crop, 3, ws[0]*sc, ws[1]*sc, /interp), /true, i
    dsp[0,*,*] = bytscl(lica_r[*,*,sl,ph[i]], min=0, max=dm_max)
    dsp[1,*,*] = bytscl(rica_r[*,*,sl,ph[i]], min=0, max=dm_max)
    dsp[2,*,*] = bytscl(post_r[*,*,sl,ph[i]], min=0, max=dm_max)
    crop = arrex(dsp, [0,bb[*,0]], [2,bb[*,1]] )
    tv, congrid(crop, 3, ws[0]*sc, ws[1]*sc, /interp), /true, i+n_ph
    xyouts, ws[0]*sc*(0.5+i), ws[1]*sc, ph_str[i], /device, font=0, align=0.5
  endfor
  ;sshot = tvrd(/true)
  ;write_png, resu_png, sshot
  ;wdelete, !d.window
end ;}}}

function make_reg_mask, region, ph_idx
; region: lica|rica|post (all slices and phases)
; ph_idx: phases to average 
;{{{
  sz = size(region, /dimen)
  if n_elements(ph_idx) eq 0 then ph_idx = indgen(sz[3])
  av = avrg(region[*,*,*,ph_idx], 4)
  msk = av gt 2
  ;mm_k = bytarr(3,3) & mm_k[1,1]=1
  for i=0, sz[2]-1 do begin
    m = label_blobs(msk[*,*,i], minsize=6) gt 0
    msk[*,*,i] = m
    ;msk[*,*,i] = dilate(m, mm_kern(3,/rect))
    ;msk[*,*,i] = erode(m, mm_kern(3,/rect))
  endfor
  return, msk
end ;}}}

function calc_diff, folder
;{{{
  ;{{{ 
  f_mask = file_search(folder, '*_brain_mask.hdr')
  f_lica_n = file_search(folder, '*_lica_n.hdr')
  f_lica_n_m0 = file_search(folder, '*_lica_n_m0.hdr')
  f_lica_r = file_search(folder, '*_lica_r.hdr')
  f_rica_n = file_search(folder, '*_rica_n.hdr')
  f_rica_n_m0 = file_search(folder, '*_rica_n_m0.hdr')
  f_rica_r = file_search(folder, '*_rica_r.hdr')
  f_post_n = file_search(folder, '*_post_n.hdr')
  f_post_r = file_search(folder, '*_post_r.hdr')
  f_post_n_m0 = file_search(folder, '*_post_n_m0.hdr')
  f_ave_r_m0 = file_search(folder, '*_ave_r_m0.hdr')

  mask = reverse(readanz(f_mask), 2)
  roi = mask
  roi[*,*,0] = 0
  roi = erode(roi, mm_kern(5))
  lica_n = reverse(readanz(f_lica_n), 2)
  lica_r = reverse(readanz(f_lica_r), 2)
  rica_n = reverse(readanz(f_rica_n), 2)
  rica_r = reverse(readanz(f_rica_r), 2)
  post_n = reverse(readanz(f_post_n), 2)
  post_r = reverse(readanz(f_post_r), 2)
  lica_n_m0 = reverse(readanz(f_lica_n_m0), 2)
  rica_n_m0 = reverse(readanz(f_rica_n_m0), 2)
  post_n_m0 = reverse(readanz(f_post_n_m0), 2)
  ave_r_m0 = reverse(readanz(f_ave_r_m0), 2)
  ;}}} 

  ph_idx = [1,2,3,4,5,6]
  msk_lica = make_reg_mask(lica_n, ph_idx)*roi
  msk_rica = make_reg_mask(rica_n, ph_idx)*roi
  msk_post = make_reg_mask(post_n, ph_idx)*roi
;  n_lica = total(msk_lica)
;  n_rica = total(msk_rica)
;  n_lat = 2*(n_lica-n_rica)/(n_lica+n_rica)
;  msk_lica = make_reg_mask(lica_r, ph_idx)*mask
;  msk_rica = make_reg_mask(rica_r, ph_idx)*mask
;  msk_post = make_reg_mask(post_r, ph_idx)*mask
;  n_lica = total(msk_lica)
;  n_rica = total(msk_rica)
;  r_lat = 2*(n_lica-n_rica)/(n_lica+n_rica)
;
;  return, [n_lat, r_lat]

  wil = where(msk_lica eq 1, cnl)
  wir = where(msk_rica eq 1, cnr)
  wip = where(msk_post eq 1, cnp)

  resu = fltarr(6, 13)-10000
  for i=0, 12 do begin
    if cnl gt 0 then begin
      n=lica_n[*,*,*,i]
      r=lica_r[*,*,*,i]
      resu[0,i] = mean( n[wil]/lica_n_m0[wil] ) * 100
      resu[1,i] = mean( r[wil]/ave_r_m0[wil] ) * 100
    endif else print, folder, ' empty lica mask'
    if cnr gt 0 then begin
      n=rica_n[*,*,*,i]
      r=rica_r[*,*,*,i]
      resu[2,i] = mean( n[wir]/lica_n_m0[wir] ) * 100
      resu[3,i] = mean( r[wir]/ave_r_m0[wir] ) * 100
    endif else print, folder, ' empty rica mask'
    if cnp gt 0 then begin
      n=post_n[*,*,*,i]
      r=post_r[*,*,*,i]
      resu[4,i] = mean( n[wip]/lica_n_m0[wip] ) * 100
      resu[5,i] = mean( r[wip]/ave_r_m0[wip] ) * 100
    endif else print, folder, ' empty post mask'
  endfor

  return, resu
end ;}}}

; [ivz] 2009-10-21: new code
function rpi_eval_subj_list
;{{{
  Nsubj = 30
  idx = indgen(Nsubj)
  rnd = randomu(seed, Nsubj)
  out = strarr(2*Nsubj)
  for i=0, Nsubj-1 do begin
    if rnd[i] le 0.5 then begin
      out[i] = strtrim(idx[i],2)+'norm'
      out[i+Nsubj] = strtrim(idx[i],2)+'rot'
    endif else begin
      out[i] = strtrim(idx[i],2)+'rot'
      out[i+Nsubj] = strtrim(idx[i],2)+'norm'
    endelse
  endfor
  return, out
end ;}}}

function make_rpi_rgb, lica, rica, post, phase_list=phase_list, slice_list=slice_list, $
  scale=scale, max_deltaM=max_deltaM, anatomy_img=anat
;{{{

  sz = size(lica, /dimen)
  nx=sz[0] & ny = sz[1] & nz = sz[2] & nph = sz[3]

  if n_elements(phase_list) eq 0 then phase_list = indgen(nph)
  if n_elements(slice_list) eq 0 then slice_list = indgen(nz)
  if n_elements(scale) eq 0 then scale = 256.0/nx ; enough for one slice
  if n_elements(max_deltaM) eq 0 then max_deltaM = 10.0 ; [a.u]

  add_anat = n_elements(anat) gt 0 ? 1b : 0b

  n_ph = n_elements(phase_list)
  n_sl = n_elements(slice_list)
  ; all slices from each phase are displayed as one image in two rows
  w_xsize = (n_sl+1)/2 * nx * scale ; 1st div must be integer !
  w_ysize = 2 * ny * scale 
  window, xsize=w_xsize, ysize=w_ysize, /pixmap

  dsp = bytarr(3, nx, ny)
  ; results buffer
  if add_anat then $
    out = bytarr(n_ph+1, 3, w_xsize, w_ysize) $
  else out = bytarr(n_ph, 3, w_xsize, w_ysize)

  for phi=0, n_ph-1 do begin
    for sli=0, n_sl-1 do begin
      ph = phase_list[phi]
      sl = slice_list[sli]
      dsp[0,*,*] = bytscl(lica[*,*,sl,ph], min=0, max=max_deltaM)
      dsp[1,*,*] = bytscl(rica[*,*,sl,ph], min=0, max=max_deltaM)
      dsp[2,*,*] = bytscl(post[*,*,sl,ph], min=0, max=max_deltaM)
      tv, congrid(dsp, 3, nx*scale, ny*scale, /interp), /true, sli
      snap = tvrd(/true)
    endfor
      out[phi,*,*,*] = snap
  endfor
  if add_anat then begin
    sz = size(anat, /dimen)
    ; make nice bytscale
    hh = histogram(anat, nbins=100, location=hx)
    hh_int = total(hh, /cumul)
    iw = where(hh_int ge max(hh_int)*0.99)
    if iw[0] ne -1 then max_anat = hx[iw[0]] $
    else max_anat = max(anat)
  print, max_anat
    scale_anat = scale * nx / sz[0]
    ; TODO: check nb of slices in anat
    for sli=0, n_sl-1 do begin
      dsp = bytscl(anat[*,*,slice_list[sli]], max=max_anat)
      tv, congrid(dsp, sz[0]*scale_anat,sz[1]*scale_anat,/interp), sli
    endfor
    snap = tvrd(/true)
    out[n_ph,*,*,*] = snap
  endif
  wdelete, !d.window
  return, out
end ;}}}

pro show_map, map, phase
;{{{
  on_error, 2
  if n_elements(map) eq 0 then return
  if n_elements(phase) eq 0 then phase=0
  s=size(map,/dimen)
  phase =  phase < (s[0]-1)
  window, xsize=s[2], ysize=s[3], /free
  tv, reform(map[phase,*,*,*]),/true
end ;}}}

pro save_bitmaps, map, dir, names
;{{{
  on_error, 2
  if n_elements(map) eq 0 then begin
    print, 'empty map'
    return
  endif
  if file_test(dir, /directory) ne 1 then begin
    print, "bad directory: ", dir
    return
  endif
  s = size(map,/dimen)
  if s[0] ne n_elements(names) then begin
    print, "nb of provided names is different from nb of images in the map"
    return
  endif
  for j=0, s[0]-1 do begin
    file = filepath(names[j], root=dir)
    write_png, file, reform(map[j,*,*,*])
  endfor
end ;}}}

;pro proc_all, rnd_seed
  raw_data_dir = '/export/data/asl/ksh/rpi_asl_ismrm10/RPI_file'
  rpi_maps_dir = '/export/data/asl/ksh/rpi_asl_ismrm10/rpi_maps'
  if file_test(rpi_maps_dir, /directory) ne 1 then file_mkdir, rpi_maps_dir
  ; which phases and slices to save
  phase_list = indgen(11)+1 ; skip 1st phase
  slice_list = reverse(indgen(6)+1) ; skip 1st slice, sup. to inf. order
  ; bitmap file names
  map_file_names = 'rpi_ph'+string(phase_list, format='(I02)')+'.png'
  map_file_names = [map_file_names, 't2w.png']
  ; final bitmap destination is <rpi_maps_dir>/<subjXX>/<map_file_names>
  
  rpi_list = file_search(raw_data_dir, 'RPI*.PAR', count=Nsubj)
  rnd_list = randomu(rnd_seed, Nsubj) ; N random nbs ]0;1]
  rnd_log = strarr(Nsubj*2)
  for j=0, Nsubj-1 do begin
  ;for j=0, 1 do begin
    subj_dir_full = file_dirname(rpi_list[j])
    subj_dir = file_basename(subj_dir_full)
    ; get t2w image
    t2w_file = file_search(subj_dir_full, 'T2*.PAR', count=Nt2)
    if Nt2 gt 0 then t2w = parrec(t2w_file,/flipY)
    ; do subtraction
    calc = obj_new('rpicalc')
    calc->SetProperty, datafile=rpi_list[j]
    calc->LoadData
    ; normal subraction
    calc -> SetProperty, label_type='normal', control_type='individual'
    calc -> Subtract
    lica = reverse(*(calc->_getptr(/lica)), 2)
    rica = reverse(*(calc->_getptr(/rica)), 2)
    post = reverse(*(calc->_getptr(/post)), 2)
    ; generate color map
    maps_n = make_rpi_rgb(lica, rica, post, $
      phase_list=phase_list, slice_list=slice_list, max_deltaM=10.0, $
      anatomy_img=t2w)
    ; rotating subraction
    calc -> SetProperty, label_type='normal', control_type='fake_rot'
    calc -> Subtract
    lica = reverse(*(calc->_getptr(/lica)), 2)
    rica = reverse(*(calc->_getptr(/rica)), 2)
    post = reverse(*(calc->_getptr(/post)), 2)
    maps_r = make_rpi_rgb(lica, rica, post, $
      phase_list=phase_list, slice_list=slice_list, max_deltaM=10.0, $
      anatomy_img=t2w)
    ; kill calc obj
    obj_destroy, calc
    ; save maps
    if rnd_list[j] le 0.5 then begin
      ; save normal substraction as j and rotated as j+Nsubj
      rnd_log[j] = 'N'
      rnd_log[j+Nsubj] = 'R'
      dir_n = 'subj'+string(j+1, format='(I02)')
      dir_n =  rpi_maps_dir + path_sep() + dir_n
      if ~file_test(dir_n) then file_mkdir, dir_n
      dir_r = 'subj'+string(j+1+Nsubj, format='(I02)')
      dir_r =  rpi_maps_dir + path_sep() + dir_r
      if ~file_test(dir_r) then file_mkdir, dir_r
      ;print, dir_n
      ;print, dir_r
      save_bitmaps, maps_n, dir_n, map_file_names 
      save_bitmaps, maps_r, dir_r, map_file_names
    endif else begin
      ; save normal substraction as j+Nsubj and rotated as j
      rnd_log[j] = 'R'
      rnd_log[j+Nsubj] = 'N'
      dir_n = 'subj'+string(j+1+Nsubj, format='(I02)')
      dir_n =  rpi_maps_dir + path_sep() + dir_n
      if ~file_test(dir_n) then file_mkdir, dir_n
      dir_r = 'subj'+string(j+1, format='(I02)')
      dir_r =  rpi_maps_dir + path_sep() + dir_r
      if ~file_test(dir_r) then file_mkdir, dir_r
      ;print, dir_n 
      ;print, dir_r
      save_bitmaps, maps_n, dir_n, map_file_names 
      save_bitmaps, maps_r, dir_r, map_file_names
    endelse
  endfor
  ; log file for randomizaton record
  log_file = filepath('randomization_log.txt', root=rpi_maps_dir)
  openw, fp, log_file, /get_lun
  for j=0, n_elements(rnd_log)-1 do $
    printf, fp, string(j+1, rnd_log[j], format='(I02, A4)')
  free_lun, fp
end
