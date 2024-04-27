;+
; NAME:
;   G_SMOOTH
;
; PURPOSE:  
;   Gaussian smoothing of 2D/3D image
;
; USAGE: 
;   result = G_SMOOTh(image, fwhm, ksize [,okern=k])
;     image - array to be smoothed
;     fwhm  - "desired resolution in pixels" (must be > 1)
;             original pixel intensity is assumed to come from
;             gaussian distribution with FWHM=1 
;             (scalar or 2el vector)
;     ksize - kernel size, (scalar or 2el vector)
;     okern - computed kernel (output)
;
; HISTORY:
;    Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;    2001-06-26 ivz Original
;-
; Copyright (C) 2001, Ivan Zimine 
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function g_smooth, image, fwhm, ksize, okern=okern, overwrite=over, help=help

    on_error, 2

    if n_params() lt 1 or keyword_set(help) then begin
        print, "usage: res = g_smooth(img, fwhm, ksize)"
        return, -1
    endif

    ndims = size(image, /n_dimensions)
    if (ndims lt 2) or (ndims gt 3) then $
        Message, "parameter must be a 2D or 3D image"

    dtname = size(image, /tname)
    if dtname ne "FLOAT" and dtname ne "DOUBLE" then begin
        print, "WARNING: integer arithmetic, check results"
    endif
    
    ; default fwhm & ksize
    if N_Elements(fwhm) eq 0 then fwhm = 2.
    if N_Elements(ksize) eq 0 then ksize = 3

    if N_Elements(fwhm) eq 1 then fwhm = [fwhm[0],fwhm[0]]
    if N_Elements(ksize) eq 1 then ksize = [ksize[0],ksize[0]]
    
    if abs(fwhm[0]) le 1 or abs(fwhm[1]) le 1 then $
        message, "FWHM must be greater than 1"

    if abs(ksize[0]) le 1 or abs(ksize[1]) le 1 then $
        message, "Kernel size must be greater than 1"

    ksize = abs(ksize)
    xc = float(ksize[0])/2
    yc = float(ksize[1])/2
    
    kx = findgen(ksize[0]) + 0.5 - xc
    ky = findgen(ksize[1]) + 0.5 - xc

    sigma_x = float(abs(fwhm[0]))/sqrt(8*alog(2))
    sigma_y = float(abs(fwhm[1]))/sqrt(8*alog(2))

    kx = exp(-0.5 * (kx/sigma_x)^2) / (sqrt(2.0 * !pi)*sigma_x)
    ky = exp(-0.5 * (ky/sigma_y)^2) / (sqrt(2.0 * !pi)*sigma_y)

    kx = kx # (intarr(ksize[1])+1) ; replicate kx y times
    ky = ky # (intarr(ksize[0])+1) ; replicate ky x times

    k = kx * transpose(ky)
    k = k/min(k) ; scale the kernel so that smallest element is 1

    if arg_present(okern) then okern=k

    scale = total(k)
    
    copy = Keyword_Set(over) ? temporary(image) : image
    
    if ndims eq 2 then $
        return, convol(copy, k, scale, /edge_truncate)

    for i=0, (size(copy))[3]-1 do $
        copy[*,*,i] = convol(copy[*,*,i], k, scale, /edge_truncate)

    return, copy
end

