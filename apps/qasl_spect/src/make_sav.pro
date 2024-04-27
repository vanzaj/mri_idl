.full_reset_session

pros = [$
    "programrootdir", $
    "parrec", $
    "ph_xroi", $
    "ph_baseobj__define", $
    "ph_container__define", $
    "vcolorbar__define", $
    "slice_stack__define", $
    "voldisp__define" $
]

; pros
for fi=0, n_elements(pros)-1 do resolve_routine, pros[fi], /either, /compile_full

; compile app
resolve_routine, 'XManager', /compile_full_file
resolve_routine, 'kbe_get_input_files', /either, /compile_full_file
resolve_routine, 'kbe_qasl_spect', /compile_full_file

resolve_all, /continue_on_error
save, /routines, filename='kbe_qasl_spect.sav'
exit

