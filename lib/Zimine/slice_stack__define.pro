;+
; OBJECT: slice_stack
;
; simple object containing 3D volume (stack of contiguous slices)
; (currently only TRA orientation of data in the stack is supported)
;
; PROPERTIES:
;   voxel  - 3el voxel size
;   palette   - reference to IDLgrPalette
;   win_level - window level [0.5 = img_max-img_min/2]
;   win_width - window width [1 = img_max-img_min, actual width can be > 1]
;
; MODIFICATIONS:
; Written: Ivan Zimine <ivan.zimine@philips.com> 
; 2008-03-27 ivz : Original
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get a slice out of stack
; data in stack assumed in TRA orientation
function slice_stack::slice, nb, view_ori=view_ori, bytscl=bytscl, true=true
;{{{
  compile_opt idl2,hidden
  if n_elements(nb) eq 0 then nb = 0
  d = self.dims
  if n_elements(view_ori) eq 0 then view_ori='TRA'
  case strupcase(view_ori) of
    'TRA': sl = (*self.data)[*, *, 0 > nb < (d[2]-1) ]
    'SAG': sl = (*self.data)[0 > nb < (d[0]-1), *, * ]
    'COR': sl = (*self.data)[*, 0 > nb < (d[1]-1), * ]
  endcase

  ; raw slice (no scaling)
  if (n_elements(bytscl) eq 0) AND $
     (n_elements(true) eq 0) then return, sl

  scmin = self.imin + self.range * (self.win_level - self.win_width/2)
  scmax = self.imin + self.range * (self.win_level + self.win_width/2)
  sl = bytscl(reform(sl), min=scmin, max=scmax)

  if keyword_set(bytscl) then return, sl

  true = byte([3,size(sl,/dimen)])
  ; assume grayscale if no palette
  if ~obj_valid(self.palette) then begin
    r = indgen(256)
    g = r & b = r
  endif else $
    self.palette->GetProperty, red_values=r, blue_values=b, green_values=g 
    
  true[0,*] = r[sl]
  true[1,*] = g[sl]
  true[2,*] = b[sl]
  return, true
end ; }}}

; get value at [x,y,z] location
function slice_stack::value, x,y,z
;{{{
  compile_opt idl2,hidden
  if n_elements(x) eq 0 or n_elements(y) eq 0 or n_elements(z) eq 0 then $
    return, !values.f_nan
  if (x lt 0) or (x ge self.dims[0]) or $
     (y lt 0) or (y ge self.dims[1]) or $
     (z lt 0) or (z ge self.dims[2]) then return, !values.f_nan
  return, (*self.data)[x,y,z]
end ; }}}

;;;;;;;;;;;;;;;;;;;; get/set properties

pro slice_stack::setproperty, voxel=voxel, orient=orient, $
  win_level=win_level, win_width=win_width, palette=palette, $
  _REF_EXTRA=extra
;{{{
  compile_opt hidden

  if n_elements(orient) gt 0 then self.orient=strtrim(orient,2)
  if n_elements(win_level) gt 0 then self.win_level = 0.01 > win_level < 0.99
  if n_elements(win_width) gt 0 then self.win_width = win_width; 0.01 > win_width < 1.0
  nv = n_elements(voxel)
  if nv gt 0 then begin
    case nv of 
      1: self.voxel = replicate(voxel,3)
      2: self.voxel = [voxel, 1.0]
      else : self.voxel = voxel[0:2]
    endcase
  endif
  if obj_valid_isa(palette, 'IDLgrPalette') then self.palette = palette

  self->ph_BaseObj::SetProperty, _STRICT_EXTRA=extra
end ; }}}

pro slice_stack::GetProperty, voxel=voxel, orient=orient, $
  imin=imin, imax=imax, win_level=win_level, win_width=win_width, $
  palette=palette, _REF_EXTRA=extra
;{{{
  compile_opt hidden

  if n_elements(extra) NE 0 then $
    self->ph_BaseObj::GetProperty, _STRICT_EXTRA=extra

  voxel = self.voxel
  orient = self.orient
  imin = self.imin
  imax = self.imax
  win_level = self.win_level
  win_width = self.win_width
  palette = obj_valid(self.palette) ? self.palette : obj_new()
end ; }}}

; get voxel size
function slice_stack::voxel
;{{{
  compile_opt hidden
  return, self.voxel
end ; }}}

function slice_stack::fov
;{{{
  compile_opt hidden
  return, self.dims*self.voxel
end ; }}}

function slice_stack::dim, view_ori
;{{{
  compile_opt hidden

  if n_elements(view_ori) eq 0 then return, self.dims
  case view_ori of
    'SAG' : ret=self.dims[0]
    'COR' : ret=self.dims[1]
    'TRA' : ret=self.dims[2]
    else : ret=0
  endcase
  return, ret
end ; }}}

; get [pointer to] pixel data 
function slice_stack::data, pointer=pointer, min=imin, max=imax
;{{{
  compile_opt hidden
  if ~ptr_valid(self.data) then return, -1
  imin = self.imin
  imax = self.imax
  if keyword_set(pointer) then return, self.data $
  else return, *self.data
end ; }}}

function slice_stack::init, vol, voxel=vox, orient=orient, $
  palette=palette, win_level=win_level, win_width=win_width, $
  no_copy=no_copy, _extra=extra
;{{{
  compile_opt hidden

  on_error, 2

  ok = self->ph_BaseObj::init(_extra=extra)

  if n_elements(vol) eq 0 then return, 0
  vol = reform(vol) ; protect against input as data[0,*,*,*] 
  dims = size(vol)
  if (dims[0] lt 2) or (dims[0] gt 3) then return, 0

  if n_elements(vox) eq 0 then vox=replicate(1.0, 3)
  if n_elements(orient) eq 0 then orient='TRA'
  if n_elements(name) eq 0 then name='noname'

  if dims[0] eq 2 then vol = reform(vol, [dims[1:2],1])
  self.dims = size(vol, /dimen)
  self.imin = min(vol, max=imax)
  self.imax = imax
  self.range = self.imax-self.imin
  self.data = ptr_new(vol, no_copy=keyword_set(no_copy))
  self.voxel = vox

  ;if n_elements(win_level) eq 0 then self.win_level = 0.5
  ;if n_elements(win_width) eq 0 then self.win_width = 1.0
  self.win_level = (n_elements(win_level) gt 0) ? win_level : 0.5
  self.win_width = (n_elements(win_width) gt 0) ? win_width : 1.0

  if obj_valid_isa(palette, 'IDLgrPalette') then self.palette=palette

  self.orient=strtrim(orient,2)
  return, 1
end ; init }}}

pro slice_stack::cleanup
;{{{ 
  compile_opt hidden
  ptr_free, self.data
  self->ph_BaseObj::Cleanup
end ; cleanup }}}

pro slice_stack__define
  compile_opt idl2, hidden
  obj = { SLICE_STACK,  $
    inherits ph_BaseObj,$
    data: ptr_new(),    $ ; 3D data array 
    dims: intarr(3),    $ ; xyz size
    voxel:fltarr(3),    $ ; xyz voxel size [mm]
    imin: 0.0,          $ ; data min
    imax: 0.0,          $ ; data max
    range: 0.0,         $ ; (max-min)
    palette: obj_new(), $ ; reference to palette object (no cleanup)
    win_level: 0.0,     $ ; window level
    win_width: 0.0,     $ ; window width
    orient: ''          $ ; TRA/SAG/COR
  }
end

