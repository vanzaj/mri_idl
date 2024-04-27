;+
; NAME:
;   PARREC
;
; PURPOSE:
;   PAR/REC reader for V3, 4, 4.1 and 4.2
;   all data is read into a 3D array
;   no image sorting is performed
;
; USAGE: 
;   img = parrec("par|rec file" [, info=info, $
;          /floatpoint, /flipX, /flipY, /no_data] )
;
; KEYWORDS:
;   info               - par info (general and image part) [output as stuct]
;   floatpoint         - output will contain FP = (PV * RS + RI)/(RS * SS)
;   flipX              - reverse first dimension (flip right-left)
;   flipY              - reverse second dimension (flip anterior-posterior)
;   no_data            - don't read data from rec file
;
; DEPENDECY:
;   hashtable__define.pro (Craig Markwardt)
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2005-03-22 ivz original 
;   2005-03-23 ivz added mr_types as types
;   2005-04-04 ivz added flipX and flipY
;   2005-09-05 ivz major rewrite (support for both PAR V3 and V4)
;   2007-04-11 ivz major rewrite now using hashtable (support for V4.1) 
;   2007-11-08 ivz added support for V4.2
;   2009-10-25 ivz added hash_info keyword switch (save info as struct or obj)
;                  added hash_null_value keyword
;-
; Copyright (C) 2005-2009, Ivan Zimine
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


; convert a string to a numerical value
; (or array, if multiple values separated by splitchar (defaults to ' '))
function parrec_str2val, str, splitchar=splitchar, float=float, int16=int16
    on_error, 2
    if size(str,/tname) ne 'STRING' then $
        message, "input must be a string"
    if n_elements(str) gt 1 then $
        message, "input must be a scalar string"
    if n_elements(splitchar) eq 0 then splitchar=' '
    ftype = keyword_set(float) ? 1.0 : 1.0d
    itype = keyword_set(int16) ? 1 : 1L
    ; get rid of leading & trailing spaces
    str = strtrim(str, 2)
    try = strsplit(str, splitchar, /extract)
    if n_elements(try) eq 1 then try=try[0]
    ; definitely not a number
    if total(stregex(try, "[^0-9.eEdD+-]", /boolean)) gt 0 then return, str
    ; float or integer
    if total(stregex(str, "[.eEdD]",/boolean)) gt 0 then $
		return, try * ftype $
    else return, try * itype
end

; general header info
; return 2xN string array (short_tag, PAR_exact_name)
function parrec_headtags 
    tags = [$
        ["pat_name",    "Patient name"],       $
        ["exam_name",   "Examination name"],   $
        ["protocol",    "Protocol name"],      $
        ["exam_date",   "Examination date/time"], $
        ["acq_nr",      "Acquisition nr"], $
        ["rec_nr",      "Reconstruction nr"], $
        ["scan_duration", "Scan Duration"],    $
        ["nb_phases",   "Max. number of cardiac phases"], $
        ["nb_echoes",   "Max. number of echoes"], $
        ["nb_slices",   "Max. number of slices/locations"], $
        ["nb_dynamics", "Max. number of dynamics"], $
        ["nb_mixes",    "Max. number of mixes"], $
        ["pat_position","Patient position"],   $
        ["prep_dir",    "Preparation direction"], $
        ["technique",   "Technique"],   $ 
        ["bits_pix",    "Image pixel size"],   $    ; V3
        ["scan_res",    "Scan resolution"],    $
        ["scan_pc",     "Scan percentage"],    $    ; V3
        ["recon_res",   "Recon resolution"],   $    ; V3
        ["NAV",         "Number of averages"], $    ; V3
        ["TR",          "Repetition time"],    $
        ["FOV",         "FOV (ap,fh,rl)"],     $
        ["slice_th",    "Slice thickness"],    $    ; V3
        ["slice_gap",   "Slice gap"],          $    ; V3
        ["water_fat_shift", "Water Fat shift"],$
        ["angulation",  "Angulation midslice"],$
        ["offcenter", "Off Centre midslice"],  $
        ["diffusion", "Diffusion         <0=no 1=yes>"], $
        ["nb_diff_vals", "Max. number of diffusion values"], $ ; V 4.1
        ["nb_diff_grad", "Max. number of gradient orients"], $ ; V 4.1
        ["nb_asl_lbl", "Number of label types   <0=no ASL>"] $ ; V 4.2
    ]
    return, tags
end;------------------ parrec_headtags()

; image info
; return 1d array (column names) 
function parrec_imgtags, version 
    if n_elements(version) eq 0 then version=4.0

    tags = [$
        "slice_number", $
        "echo_number", $
        "dynamic_scan_number", $
        "cardiac_phase_number", $
        "image_type_mr", $
        "scanning_sequence", $
        "index_in_REC_file"]

    if version gt 3.0 then begin
        tags = [tags, $
            "image_pixel_size", $
            "scan_percentage", $
            "recon_resolution_x", $
            "recon_resolution_y"]
    endif

    tags = [tags, $
        "rescale_intercept", $
        "rescale_slope", $
        "scale_slope", $
        "window_center", $
        "window_width", $
        "image_angulation_ap", $
        "image_angulation_fh", $
        "image_angulation_rl", $
        "image_offcentre_ap", $
        "image_offcentre_fh", $
        "image_offcentre_rl" ]

    if version gt 3.0 then $
        tags = [tags, "slice_thickness", "slice_gap"]

    tags = [tags, $
        "image_display_orientation", $
        "slice_orientation", $
        "fmri_status_indication", $
        "image_type_ed_es", $
        "pixel_spacing_x", $
        "pixel_spacing_y", $
        "echo_time", $
        "dyn_scan_begin_time", $
        "trigger_time", $
        "diffusion_b_factor" ]

    if version gt 3.0 then $
        tags = [tags, "number_of_averages"]

    tags = [tags, "image_flip_angle"]

    if version eq 3.0 then return, tags

    tags = [tags, $
        "cardiac_frequency", $
        "minimum_RR_interval", $
        "maximum_RR_interval", $
        "TURBO_factor", $
        "Inversion_delay" ]

    if version eq 4.0 then return, tags

    tags = [tags, $
        "diffusion_b_value_number", $
        "gradient_orientation_number", $
        "contrast_type", $
        "diffusion_anisotropy_type", $
        "diffusion_ap", $
        "diffusion_fh", $
        "diffusion_rl" ]

    if version eq 4.1 then return, tags

    tags = [tags, $
        "label_type_asl" ]

    return, tags
end

; main
function parrec, file, get_file=get_file, info=inf, no_data=no_data, $
  floatpoint=fp, dispvalue=dv, flipx=flipX, flipy=flipY, $
  hash_info = hash_info, hash_null_value=hash_null_value, debug=debug

  catch, error
  if error ne 0 then begin
    catch, /cancel
    if keyword_set(debug) then begin
      help, data
      print, !error_state.msg
    endif
    if obj_valid(head) then obj_destroy, head
    return, -1
  endif

  ; check keywords
  get_file = keyword_set(get_file)
  no_data=keyword_set(no_data)
  debug=keyword_set(debug)
  if n_elements(hash_null_value) eq 0 then hash_null_value=0L

  if n_elements(file) eq 0 and (get_file eq 0) then $
    message, "No input file"

  if get_file then begin
    ok = dialog_pickfile(filter=['*.par','*.PAR'],/read)
    ; cancel
    if ok eq '' then return, -1
    file = ok
    cd, file_dirname(file)
  endif

  ; make par and rec filenames based on extension
  if strpos(file, '.par') gt -1 then begin
    parfile = file
    recfile = strmid(file, 0, strlen(file)-4) + '.rec'
  endif

  if strpos(file, '.PAR') gt -1 then begin
    parfile = file
    recfile = strmid(file, 0, strlen(file)-4) + '.REC'
  endif

  if strpos(file, '.rec') gt -1 then begin
    recfile = file
    parfile = strmid(file, 0, strlen(file)-4) + '.par'
  endif

  if strpos(file, '.REC') gt -1 then begin
    recfile = file
    parfile = strmid(file, 0, strlen(file)-4) + '.PAR'
  endif

  if n_elements(parfile) eq 0 then $
    message, "wrong extension in " + file

  if not file_test(parfile, /read) then $
    message, "can't read " + parfile

  ; compressed REC ?
  compressed=0b
  if file_test(recfile+'.gz',/read) then begin
    compressed=1b
    recfile=recfile+'.gz'
  endif

  if not file_test(recfile, /read) then $
    message, "can't read " + recfile

  ; read PAR file as a binary
  openr, fid, parfile, /get_lun
  fs = fstat(fid)
  buf = bytarr(fs.size)
  readu, fid, buf
  free_lun, fid

  ; look for a line break in the first 100 chars
  ; linebreaks (CR = 0x0D, LF = 0x0A)
  line = string(buf[0:99])
  linebr = string([13b,10b]) ; win (CR+LF)
  if strpos(line, linebr) eq -1 then $
    linebr = string(10b) ; unix (LF)
  if strpos(line, linebr) eq -1 then $
    linebr = string(13b) ; mac (CR)
  if strpos(line, linebr) eq -1 then begin
    message, "couldn't determine line break character"
  endif

  ; split on line breaks
  buf = strsplit(string(buf), linebr, /extract)

  ; par version
  si = (where(strmatch(buf[0:10], "# * Research image export tool *") eq 1))[0]
  if si gt -1 then begin
    line = buf[si]
    ; split on white spaces
    ver = strsplit(line, '\ \ *', /regex, /extract)
    ver = ver[n_elements(ver)-1] ; V3, V4, V4.1 ...
    version = float(strmid(ver, 1, strlen(ver)-1))
    if version gt 4.2 then $
      message, ver + " may not be fully supported",/info
  endif else begin
    message, "Failed to get PAR/REC version, assuming V4",/info
    version = 4.0
  endelse
  if debug then print, version, format='(%"version: %4.1f")'

  ; general info buffer start/end line index
  si = (where(strmatch(buf, "# === GENERAL INFORMATION ===*") eq 1))[0]
  ei = (where(strmatch(buf, "# === PIXEL VALUES ===*") eq 1))[0]
  if (si gt -1) and (ei gt -1) then $
    gen_buf = buf[si+2:ei-2] $
  else message, "failed to get GENERAL INFORMATION"

  ; image info buffer start/end line index
  si = (where(strmatch(buf, "# === IMAGE INFORMATION ===*") eq 1))[0]
  ei = n_elements(buf)-1
  if si gt -1 then img_buf = buf[si+2:ei-1] $
  else message, "failed to get IMAGE INFORMATION"

  n_imgs = n_elements(img_buf)

  head = obj_new('hashtable', null_value=hash_null_value)
  tags = parrec_headtags()

  head->add, 'version', version

  ; parse general info
  for i=0, n_elements(tags[0,*])-1 do begin
    par_string = '.    '+tags[1,i]+'*'
    match = where(strmatch(gen_buf, par_string) eq 1)
    if match[0] gt -1 then begin
      line = gen_buf[match[0]]
      len = strlen(line)
      col_pos = strpos(line, ':')
      par_val = strtrim(strmid(line, col_pos+1, len-col_pos), 2)
      head->add, tags[0,i], parrec_str2val(par_val, /int16, /float)
    endif
  endfor

  ; separate date / time
  date_time = head->get('exam_date')
  try = strtrim(strsplit(date_time,'/',/extract), 2)
  if (n_elements(try) eq 2) and (try[0] ne '') then begin
    head->remove, 'exam_date'
    head->add, 'exam_date', try[0]
    head->add, 'exam_time', try[1]
  endif else begin ; fake but valid date/time
    head->remove, 'exam_date'
    head->add, 'exam_date', '1901.01.01'
    head->add, 'exam_time', '00:00:00'
  endelse

  ; backward compatibility with V3
  ss = strsplit(img_buf[0], '\ \ *', /regex, /extract)
  if version gt 3 then begin
    head->add, 'bits_pix',  fix(ss[7])
    head->add, 'scan_pc',   fix(ss[8])
    head->add, 'recon_res', [fix(ss[9]), fix(ss[10])]
    head->add, 'slice_th',  float(ss[22])
    head->add, 'slice_gap', float(ss[23])
    head->add, 'NAV',       fix(ss[34])
  endif

  ; image info tags
  imgtags = parrec_imgtags(version)

  ; table for all image info values
  img_info = fltarr(n_elements(imgtags), n_imgs)

  for i=0, n_imgs-1 do begin
    ss = strsplit(img_buf[i], '\ \ *', /regex, /extract)
    img_info[*,i] = float(ss)
  endfor

  ; save image info into header
  for i=0, n_elements(imgtags)-1 do begin
    if head->iscontained(imgtags[i]) then head->remove, imgtags[i]
    head->add, imgtags[i], reform(img_info[i,*])
  endfor

  ; don't read rec
  if keyword_set(no_data) then begin
    inf = head->struct()
    obj_destroy, head
    return, 0
  endif 

  ; scaling
  r_int   = head->get('rescale_intercept')
  r_slope = head->get('rescale_slope')
  s_slope = head->get('scale_slope')

  ; reading data from REC
  bpp = head->get('bits_pix')
  case bpp of
    8 : data_type = 1
   16 : data_type = 2
   else : message, "unsupported bits/pixel: ", bpp
  endcase

  dims = [head->get('recon_res'), n_imgs]
  data = make_array(dimension=dims, type=data_type,/nozero)
  openr, fid, recfile, /get_lun, /swap_if_big_endian, compress=compressed
  readu, fid, data
  free_lun, fid

  ; stored pixel value -> float value
  if keyword_set(dv) then fp=1b ; force float point conversion
  if keyword_set(fp) then begin
    data = float(data)
    for i=0, n_imgs-1 do begin
      ; display value ?
      if keyword_set(dv) then begin
        data[*,*,i] = data[*,*,i] * r_slope[i] + r_int[i]
        continue
      endif
      s1 = 1.0/s_slope[i]
      s2 = 1.0/(r_slope[i] * s_slope[i])
      if r_int[i] eq 0.0 then data[*,*,i] = data[*,*,i] * s1 $
      else data[*,*,i] = (data[*,*,i] * r_slope[i] + r_int[i])*s2
    endfor
  endif

  if keyword_set(flipX) then data = reverse(temporary(data),1)
  if keyword_set(flipY) then data = reverse(temporary(data),2)

  ; return info as object or structure
  if keyword_set(hash_info) then inf = head $
  else begin
    inf = head->struct()
    obj_destroy, head
  endelse

  matrix = size(data, /dimensions)
  return, data

end
