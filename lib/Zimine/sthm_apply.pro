;+
; NAME:
;   STHM_APPLY
;
; PURPOSE:
;   Apply homogeneous spatial transformation matrix to a list of points
;
; USAGE: 
;   new = sthm_apply(trans, pts)
;     trans - 4x4 transformation matrix
;     pts   - 3,n array of coordinates
; 
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2005-09-16 ivz Original
;-
; Copyright (C) 2005, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function sthm_apply, TM, pts, homo=homo
    on_error, 2
    if n_params() ne 2 then message, "Must be called with 2 parameters"
    if (n_elements(pts) mod 3) ne 0 then $
        message, "Second parameter must be (3,n) array"
    n_pts = n_elements(pts) / 3
    ; transform to homogeneous coordinates
    pts_h = dblarr(4,n_pts)+1
    pts_h[0,0] = pts
    pts_h = transpose(TM ## transpose(pts_h))
    ; return unnormalized homogeneous coordinates 
    if keyword_set(homo) then return, pts_h
    ; renormalize
    pts_h = pts_h / rebin(pts_h[3,*], 4, n_pts)
    return, pts_h[0:2,*]
end

