;+
; NAME:
;   STHMATRIX
;
; PURPOSE:
;   Compute spatial transformation homogeneous matrix
;
; USAGE:
;   mat = STHMATRIX(translate=trans, rotate=angles, scale=scale, $
;                   order=ord, left=left, zero=zero)
;     trans    (dx,dy,dz)
;     angles   (ax, ay, az) [deg]
;     scale    (sx,sy,sz)
;     order    rotation order ('xyz','zxy',...)
;     left     left-hand rotations (angles *= -1)
;     zero     force elements below zero to 0.0
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


function sthmatrix, translate=trans, rotate=angles, scale=scale, $
        order=ord, left=left, zero=zero, help=help
    
    catch, error
    if error ne 0 then begin
        print, !error_state.msg
        return, -1
    endif

    if keyword_set(help) then begin
        print, "Usage: M = sthmatrix(translate=trans, rotate=ang, scale=sc, order=ord, left=left, zero=zero)"
        print, "    trans  - 3el vector (dx,dy,dz)"
        print, "    ang    - 3el vectror (a_x, a_y, a_z) in degrees"
        print, "    order  - rotation order (default: 'xyz')"
        print, "    left   - left hand rotations"
        print, "    zero   - smaller values will be set to 0.0"
        return, -1
    endif
    
    if n_elements(trans) eq 0 then trans = replicate(0.0, 3)
    if n_elements(angles) eq 0 then angles = replicate(0.0, 3)
    if n_elements(trans) ne 3 then message, "translation must be a 3el vector"
    if n_elements(angles) ne 3 then message, "rotation must be a 3el vector"
    if n_elements(ord) eq 0 then ord='xyz'
    if n_elements(scale) eq 0 then scale = 1.0
    if n_elements(scale) eq 1 then scale = replicate(scale, 3)
    if n_elements(scale) ne 3 then message, "scale must be a 3el vector"
    if keyword_set(left) then angles= -1 * angles

    ; save original transformation
    T0 = !p.t
    t3d,/reset
    t3d, scale=scale
    case strlowcase(ord) of
        'xyz' : begin
            t3d, rotate=angles ; default IDL order
        end    
        'xzy' : begin
            t3d, rotate=[angles[0], 0, 0]
            t3d, rotate=[0, 0, angles[2]]
            t3d, rotate=[0, angles[1], 0]
        end
        'yxz' : begin
            t3d, rotate=[0, angles[1], 0]
            t3d, rotate=[angles[0], 0, 0]
            t3d, rotate=[0, 0, angles[2]]
        end
        'yzx' : begin
            t3d, rotate=[0, angles[1], 0]
            t3d, rotate=[0, 0, angles[2]]
            t3d, rotate=[angles[0], 0, 0]
        end
        'zxy' : begin
            t3d, rotate=[0, 0, angles[2]]
            t3d, rotate=[angles[0], 0, 0]
            t3d, rotate=[0, angles[1], 0]
        end
        'zyx' : begin
            t3d, rotate=[0, 0, angles[2]]
            t3d, rotate=[0, angles[1], 0]
            t3d, rotate=[angles[0], 0, 0]
        end
        else : message, "incorrect rotation order"
    endcase

    t3d, translate=trans
    
    T = !p.t
    !p.t = T0
    if n_elements(zero) gt 0 then begin
        if abs(zero) gt 1e-3 then message, 'WARNING: zero = '+strtrim(zero,2), /info
        wh = where(abs(T) lt zero, cn)
        if cn gt 0 then T[wh] = 0.0
    endif
    return, T
end

