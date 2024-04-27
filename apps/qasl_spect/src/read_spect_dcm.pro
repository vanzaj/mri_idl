;+
; USAGE:
;   im = read_spect_dcm('filename', [info=info])
;
; PURPOSE: 
;   read multi-frame reconstructed (SCIEMENS NM IP2) SPECT dicom
;
; OUTPUT:
;   image data
;   parameters via info keyword
; 
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;
;   2008-02-15 ivz Original
;-

function read_spect_dcm, infile, voxel=voxel, get_file=get_file

  catch, error
  if error ne 0 then begin
      if obj_valid(dcm) then obj_destroy, dcm
      help, /last_message, output=out
      ok = dialog_message(out, /error)
      return, -1
  endif

  if (n_elements(infile) eq 0) or keyword_set(get_file) then begin
      infile = dialog_pickfile(filter='*.IMA', /read)
      if infile eq '' then return, -1
  endif

  dcm = obj_new('IDLffDICOM')
  r = dcm->read(infile)
  if r[0] eq 0 then message, 'Failed to read ' + infile

  voxel = [1.0, 1.0, 1.0]

  ;; PixelSpacing
  val = dcm->getvalue('0028'x,'0030'x,/no_copy)
  if size(val, /Tname) eq 'POINTER' then $
    voxel[0:1] = float(strsplit(*val[0], '\',/extract))

  ;; SpacingBetweenSlices
  ;val = dcm->getvalue('0018'x,'0088'x,/no_copy)
  ;; SliceThickness
  val = dcm->getvalue('0018'x,'0050'x,/no_copy)
  if size(val, /Tname) eq 'POINTER' then $
    voxel[2] = float(*val[0])
  
  ;; data
  val = dcm->getvalue('7FE0'x,'0010'x, /no_copy)

  n_tot = n_elements(val)
  dims = size(*val[0], /dimen)

  spect = make_array(dimension=[dims, n_tot], type=2, /nozero)

  for j=0, n_tot-1 do spect[*,*,j] = *val[j]

  obj_destroy, dcm

  return, reverse(spect, 2)

end
