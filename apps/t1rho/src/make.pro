pro make, target

  if n_elements(target) eq 0 then target='t1rho_jp'

  files = [$
      "programrootdir", $
      "vcolorbar__define", $
      "hashtable__define", $
      "progressbar__define", $
      "anzhead_struct", $
      "writeanz", $
      "parrec", $
      "ph_xroi", $
      "obj_valid_isa", $
      "ph_baseobj__define", $
      "ph_container__define", $
      ;"sthmatrix", $
      ;"ph_`imgreg__define", $
      "slice_stack__define", $
      "voldisp__define", $
      "xmanager"  $
  ]


  for j=0, n_elements(files)-1 do $
   resolve_routine, files[j], /either, /compile_full_file


  resolve_routine, "t1rho_jp", /compile_full_file
  resolve_all, /continue_on_error


  ; create target in the parent directory
  target = filepath("t1rho_jp.sav", root_dir='..')

  save, /routines, filename=target
end
