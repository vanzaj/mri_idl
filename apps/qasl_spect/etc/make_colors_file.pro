; create custom color table file
; mainly from colors1.tbl

pro make_colors_file
  cd, programrootdir(), current=old_dir

  loadct, get_names=names
  idx = [0, 3, 8, 1, 13, 4]

  nct = n_elements(idx)
  bnames = bytarr(32, nct)

  openw, lun, 'z_colors.tbl', /get_lun

  writeu, lun, byte(nct)
  for i=0, nct-1 do begin
    loadct, idx[i], /silent
    tvlct, r, g, b, /get
    writeu, lun, r
    writeu, lun, g
    writeu, lun, b
    bnames[0,i] = byte(names[idx[i]])
  endfor
  writeu, lun, bnames

  free_lun, lun
  cd, old_dir
end
