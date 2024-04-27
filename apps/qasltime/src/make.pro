pro make, target

  if n_elements(target) eq 0 then target='qasltime'

  files = [$
    'avrg', $
    'programrootdir', $
    'hashtable__define', $
    'parrec', $
    'gammavar', $
    'mpfitfun', $
    'obj_valid_isa', $
    'anzhead_struct', $
    'writeanz', $
    'xmanager', $
    'qaslcalc__define' $
  ]

  for j=0, n_elements(files)-1 do $
    resolve_routine, files[j], /either, /compile_full_file

  resolve_routine, target, /compile_full_file
  resolve_all,/continue_on_error

  target=filepath(target+'.sav', root_dir="..")
  save, /routines, filename=target
end

