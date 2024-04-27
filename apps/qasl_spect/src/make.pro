pro make, target

  if n_elements(target) eq 0 then target='kbe_qasl_spect'

  files = [$
      "programrootdir", $
      "vcolorbar__define", $
      "hashtable__define", $
      "anzhead_struct", $
      "readanz", $
      "writeanz", $
      "parrec", $
      "ph_xroi", $
      "ph_baseobj__define", $
      "ph_container__define", $
      "sthmatrix", $
      "ph_imgreg__define", $
      "slice_stack__define", $
      "voldisp__define", $
      "xmanager"  $
  ]

  ; pros
  for j=0, n_elements(files)-1 do $
    resolve_routine, files[j], /either, /compile_full_file

  ; compile app
  resolve_routine, 'read_spect_dcm', /either, /compile_full_file
  resolve_routine, 'kbe_get_input_files', /either, /compile_full_file
  resolve_routine, 'kbe_qasl_spect', /compile_full_file

  resolve_all, /continue_on_error

  target = filepath(target+'.sav', root_dir='..')
  save, /routines, filename=target
end
