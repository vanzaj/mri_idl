pro make, target

  if n_elements(target) eq 0 then target='rpiview'

  files = [$
    'avrg', $ 
    'arrex', $
    'fsc_droplist', $
    'fsc_field', $
    'error_message', $
	'programrootdir', $
    'mm_kern', $
    'g_smooth', $
    'imgrange', $
    'mrimask', $
    'structtable', $
    'parrec', $
    'hashtable__define', $
    'ivzerror__define', $
    'rpicalc__define' $
    ]

  for j=0, n_elements(files)-1 do $
    resolve_routine, files[j], /either, /compile_full_file

  resolve_routine, 'xmanager', /compile_full_file
  resolve_routine, target, /compile_full_file
  resolve_all, /continue_on_error

  target=filepath(target+'.sav', root_dir="..")
  save, file=target, /routines
end

