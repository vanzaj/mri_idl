;+
; NAME:
;   OBJ_VALID_ISA
;
; PURPOSE:
;   check if 1st parameter is a valid instance of a classname given in 2d
;   parameter
;
; USAGE:
;   ret = obj_valid_isa(objectRef, className)
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2007-04-11 ivz Original
;-
; Copyright (C) 2007, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function obj_valid_isa, obj, classname

  on_error, 2

  nobj = n_elements(obj)
  ncls = n_elements(classname)

  ret = (nobj gt 1) ? bytarr(nobj) : 0b

  ; no input
  if (nobj eq 0) or (ncls eq 0) then return, ret

  ; multiple objects, single classname
  if (nobj gt 1) and (ncls eq 1) then $
    classes = replicate(classname, nobj) $
  else classes = classname

  ncls = n_elements (classes)
  
  ; at this point nb of obj and classes should be the same
  if nobj ne ncls then return, ret

  for oi = 0, nobj - 1 do $
    if obj_valid (obj[oi]) then $
      if obj_isa(obj[oi], classes[oi]) then ret[oi] = 1b

  return, ret
end

