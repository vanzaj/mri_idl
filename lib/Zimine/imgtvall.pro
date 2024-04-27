;+
; NAME:
;   IMGTVALL 
;
; PURPOSE:  
;   Quick display of all slices of a 3D volume in a new display window
;   ( for j=0, n_slices-1 do tv, vol[*,*,j], j )
;
; USAGE: 
;   IMGTVALL, vol [, scale=scale, grid=grid, win_size=win_size, range=range,$
;             crop=crop, bbox=bbox, title=title ]
;
;     vol       3D array [dimX, dimY, dimZ]
;     scale     changes slice display size to [dimX,dimY]*scale  
;     grig      2el vector for slices arrangement on the display
;     win_size  whole display window size 
;     range     image values range for bytscl
;     crop      auto-cropping all slices (only tested with axial images)
;     bbox      bound box for manual cropping
;     title     window title
;   
; DEPENDENCY:  
;   arrex.pro
;   mr_bbox.pro
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2000-09-02 ivz Original
;-
; Copyright (C) 2000, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

pro imgtvall, ima, scale=scale, grid=grid, win_size=win_size, range=range,$
      crop=crop, bbox=bbox, title=title

    on_error, 2

    if n_params() eq 0 then begin
        print, 'usage: imgtvall, vol, scale=scale, grid=grid'
        return
    endif

    vol=reform(ima)
    s = size(vol)
    if s[0] lt 2 or s[0] gt 3 then message, 'first parameter must be 2D or 3D '
    nz = (s[0] eq 3) ? s[3] : 1
    nx = s[1] & ny = s[2]

    if n_elements(bbox) gt 1 then begin
        vol = arrex(vol, bbox[*,0], bbox[*,1])
        nx = bbox[0,1]-bbox[0,0]+1
        ny = bbox[1,1]-bbox[1,0]+1
        crop=0
    endif

    if keyword_set(crop) then begin
        bbox = mr_bbox(vol, noise=1, enlarge=5)
        vol = arrex(vol, bbox[*,0], bbox[*,1])
        nx = bbox[0,1]-bbox[0,0]+1
        ny = bbox[1,1]-bbox[1,0]+1
    endif

    if n_elements(scale) gt 0 then begin
        scale=abs(scale)
        nx = round(nx * scale) & ny = round(ny * scale)
    endif else scale=1.0

    ; max display X size
    if n_elements(win_size) eq 0 then win_size=(nx*nz) < 900

    if n_elements(grid) eq 0 then begin
        gx = win_size / nx
        gy = nz / gx
        if gx*gy lt nz then gy=gy+1
        grid = [gx, gy]
    endif else begin
        gx = grid[0] & gy = grid[1]
        if gx*nx gt win_size or gy*ny gt win_size then $
            message, "grid/scale doesnt fit into win_size"
    endelse

    case n_elements(range) of
        0 : rr = minmax(vol)
        1 : rr = [0, range]
        2 : rr = range
        else : message, 'incorrect range' 
    endcase

    if n_elements(title) gt 0 then $
      window, xsize=gx*nx, ysize=gy*ny, /free, title=title $
    else window, xsize=gx*nx, ysize=gy*ny, /free
    for i=0, nz-1 do $
        if scale ne 1.0 then $
            tv, bytscl(congrid(vol[*,*,i], nx, ny,/interp), min=rr[0], max=rr[1]), i $
        else tv, bytscl(vol[*,*,i], min=rr[0], max=rr[1]), i 
end

