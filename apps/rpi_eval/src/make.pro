pro make, target

  if n_elements(target) eq 0 then target='rpi_eval'

  files = [ $
    'error_message', $
    'programrootdir', $
    'wind', $
    'slide_trueimg',  $
    'xmanager'  $
  ]
  for j=0, n_elements(files)-1 do $
    resolve_routine, files[j],/compile_full_file, /either

  resolve_routine,'rpi_eval',/compile_full_file 
  resolve_all,/continue_on_error

  target = filepath(target+'.sav', root_dir='..')
  save, /routines, filename=target
end
