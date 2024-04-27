pro make, target

  if n_elements(target) eq 0 then target='fatfrac'

  files = [$
    'fsc_field', $
    'fsc_color', $
    'progressbar__define', $
    'hashtable__define', $
    'xcolors', $
    'parrec', $
    'mpfitfun', $
    'minmax', $
    'anzhead_struct', $
    'writeanz', $
    'fatcalc__define', $
    'xmanager' $
    ]

  for j=0, n_elements(files)-1 do $
    resolve_routine, files[j], /either, /compile_full_file

  resolve_routine, target, /compile_full_file
  resolve_all,/continue_on_error

  target=filepath(target+'.sav', root_dir="..")
  save, /routines, filename=target
end

