;+
; NAME:
;   MR_BBOX
;
; PURPOSE:
;   Compute a bounding box around an MR image
;   (useful for image cropping using ARREX)
;
; ALGO:
;    get the image mask
;    get x and y profiles of the mask
;    cut profiles profile above 1 (prof = prof < 1)
;    derive cutted profile
;    1 is the low bound, -1 is the high bound
;
;    for 3D image, recursive call with mip_z and mip_y
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;   2005-03-19 ivz Original
;-
; Copyright (C) 2005, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


function mr_bbox, im, ENLARGE=enlr, NOISE=noise, HELP=help

    on_error, 2

    if N_elements(im) eq 0 or Keyword_Set(help) then begin
        print, "Compute a bounding box around an image"
        print, "Usage: bb = mr_bbox(im, [enlarge=enlarge, noise=noise])"
        print, "    im          2D or 3D array"
        print, "    enlarge     pixels to add to the bbox in each direction"
        print, "    noise       noise level for mrimask()"
        return, -1
    endif

    if N_elements(enlr) gt 0 then epc=enlr

    s = size(im)

    case s[0] of
        2 : begin
            msk = mrimask(im, NOISE=noise, /fill)
            if total(msk) eq 0 then message, "Error: empty mask"
            xprof = total(msk, 2) < 1
            xprof = xprof - shift(xprof,1)
            xa = (where(xprof eq 1) - 1) > 0
            xb = (where(xprof eq -1)) < (s[1]-1)
            yprof = total(msk, 1) < 1
            yprof = yprof - shift(yprof,1)
            ya = (where(yprof eq 1) - 1) > 0
            yb = (where(yprof eq -1)) < (s[2]-1)
            if n_elements(epc) gt 0 then begin
                if n_elements(epc) eq 1 then epc=[epc,epc]
                xa = (xa - epc[0]) > 0
                xb = (xb + epc[0]) < (s[1]-1)
                ya = (ya - epc[1]) > 0
                yb = (yb + epc[1]) < (s[2]-1)
            endif
            res = [[xa,ya],[xb,yb]]
        end ; 2D
        3: begin
            if s[3] lt 10 then $
                message, "Small nb. of slices. Check results", /info
            if n_elements(epc) eq 1 then epc=[epc,epc]
            xy = mr_bbox(max(im,dim=3), ENLARGE=epc, NOISE=noise)
            if n_elements(epc) eq 3 then epc=[epc[0],epc[2]]
            xz = mr_bbox(max(im,dim=2), ENLARGE=epc, NOISE=noise)
            res = [[xy[0,0], xy[1,0], xz[1,0]], $
                   [xy[0,1], xy[1,1], xz[1,1]]]

        end ; 3D
        else : $
            message, "Input must be 2D or 3D"
    endcase
    return, res
end

