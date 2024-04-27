;+
; NAME:
;   MRIMASK
;
; PURPOSE:
;   Create a binary mask of image (pixels with intensity above noise level)
;
; USAGE: 
;   mask = mrimask(im [, KERN=kern, /FILLHOLES, OTHRES=othres])
;
; ALGORITHM:
;   automatic threshold (10% of max intensity holding 99% of total energy)
;   get rid of small outliers with erode -> dilate
;   fill holes inside the mask
;
; DEPENDENCY:
;   g_smooth.pro
;   imgrange.pro
;   mm_kern.pro
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2001-06-17 ivz Original
;   2005-04-06 ivz Morph kernel via mm_kern()
;-
; Copyright (C) 2001, 2005, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function mrimask, im, KERNEL=kern, FILLHOLES=fillholes $
        ,OTHRES=othres, NOISE=noise, CONTOUR=cnt, SMOOTH=smooth, HELP=help

    On_Error, 2
 
    if N_Params() eq 0 or Keyword_Set(help) then begin
       print, "Usage: mask = mrimask(image)"
       print, "KEYWORDS:"
       print, "     KERNEL: morphological operator array"
       print, "  FILLHOLES: flag to fill holes in the mask"
       print, "      NOISE: noise level relative to max (0-100, default 10%)"
       print, "     OTHRES: computed threshold (output)"
       return, 0
    endif
 
    imSize = size(im)
    if (imSize[0] lt 2) or (imSize[0] gt 3) then $
       Message, "First parameter must be a 2D or 3D image"

    nz = (imSize[0] eq 3) ? imSize[3] : 1
    
    if n_elements(noise) eq 0 then noise=10
    
    if (noise ge 100) or (noise le 0) then begin
        print, "noise must be in the (0,100) range"
        return, -1
    endif

    ims = float(im)
    ; smooth image a bit
    if keyword_set(smooth) then ims = g_smooth(ims, 3, 3)

    imin = Min(ims, MAX=imax)

    ; get the image range holding 99% of total image energy
    rng = imgrange(ims, 0, 99)
    othres = 0.01*float(noise) * rng[1]

    mask = ims gt othres

    ; default kernel
    ; 3x3 or 5x5 disk 
    if N_Elements(kern) eq 0 then begin
        if nz gt 10 then kern = mm_kern(5) $
        else kern = mm_kern(3)
    endif

    for i=0, nz-1 do $
        mask[*,*,i] = morph_open(mask[*,*,i], kern)

    if keyword_set(fillholes) then begin
        for i=0, nz-1 do begin
            msk = mask[*,*,i]
            msk = dilate(msk, kern)
            roi = search2d(msk, 0,0,0,0)
            msk[*] = 1
            msk[roi] = 0
            mask[*,*,i] = erode(msk,kern)
        endfor
   endif

   if arg_present(cnt) then begin
       cnt = mask
       kk = mm_kern(3)
       for i=0, nz-1 do $
           cnt[*,*,i] = dilate(mask[*,*,i], kk)-mask[*,*,i]
   endif

   return, mask
end

