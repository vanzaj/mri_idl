;+
; NAME:
;   MM_KERN
;
; PURPOSE:
;   Create 2D or 3D morphological kernel (disc or square, sphere or cube)
;
; USAGE:  
;   kern = mm_kern(kernel_size, [/rect, /k3d])
;     Returns a kernel of specified size (diameter in pixels)
;     unless /RECT is set (returns a square kernel).
;     K3D keyword switches 3D mode
;
; NOTE:
;   It is usually a bad idea to use even sized kernels
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2005-04-06 ivz Original
;-
; Copyright (C) 2005, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
 
function mm_kern, s, rect=rect, k3d=k3d
    on_error, 2
    if n_elements(s) eq 0 then s=3

    if n_elements(s) gt 1 then begin
        message, "Non square kernel size is not implemented", /info 
        s = s[0]
    end

    if (s mod 2)+1 then message, "Even sided kernel! Check results.", /info

    ; 2D kernel
    if not keyword_set(k3d) then begin
        if keyword_set(rect) then k = replicate(1b,s,s) $
        else k =  shift(dist(s),s/2,s/2) le s/2 
        return, k
    endif 

    ; cube
    if keyword_set(rect) then return, replicate(1b, s, s, s)
    
    ; sphere
    x = indgen(s) - s/2
    xx = rebin(x, s, s, s)
    yy = transpose(xx, [1,0,2])
    zz = transpose(xx, [2,1,0])

    return, sqrt(xx^2. + yy^2. + zz^2.) le s/2
end

