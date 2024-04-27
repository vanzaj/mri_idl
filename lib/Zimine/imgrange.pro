;+
; NAME:
;   IMGRANGE
;
; PURPOSE:
;   Find image intensity range which holds between lower and upper bounds (%)
;   of total signal energy
;
; USAGE: 
;   range = imgrange(image, [low, high, sigma=sigma])
;
; ALGORITHM:
;   compute a histogram with 100 bins and the bins array
;   compute signal energy in each bin and cumulative sum of this energy
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2001-06-01 ivz Original
;-
; Copyright (C) 2001, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 


; computes image energy between two intensities
function img_E, im, low, high
    compile_opt  hidden
    catch, error
    if error ne 0 then begin
        catch,/cancel
        print, "!error_state.msg"
        return, -1
    endif
    if n_elements(low) eq 0 then low = min(im)
    if n_elements(high) eq 0 then high = max(im)
    ii = where(im ge low and im le high, cnt)
    res = 0.0
    if cnt gt 0 then res = total(im[ii])
    return, res
end

function imgrange, im, E_low, E_high, SIGMA=sigma, HELP=help

    On_Error, 2
 
    if N_Params() lt 1 or Keyword_Set(help) then $
        Message, "usage: res = imgrange(img [,low,high, sigma=sigma])"
 
    imSize = size(im)
    if (imSize[0] lt 2) or (imSize[0] gt 3) then $
       Message, "First parameter must be a 2D or 3D image"

    if N_Elements(E_low) eq 0 then E_low=0 $
    else E_low=float(E_low[0]) > 0
    
    if N_Elements(E_high) eq 0 then E_high=100 $
    else E_high=float(E_high[0]) < 100

    if E_low gt E_high then $
        Message, "E_low GT E_high"

    if N_Elements(sigma) eq 0 then sigma = 0.1 ; 0.1% precision
    
    nbins = 100
    hist = histogram(im, nbins=nbins, omin=imin, omax=imax, reverse_ind=r) 
    binsize = (imax - imin)/(nbins - 1)
    binarr = imin + lindgen(nbins)*binsize

    Ebins = fltarr(nbins) ; signal energy in each bin
    for i=0L, nbins-1 do $
        Ebins[i] = (r[i] ne r[i+1]) ? total(im[r[r[i]:r[i+1]-1]]) : 0.0

    Ecumul = total(Ebins, /cumulative)
    
    E_tot = Ecumul[nbins-1]
    
    ; get the low limit
    llim = imin
    if E_low gt 0 then begin
        idx = where(Ecumul le E_tot * E_low / 100, cnt)
        if cnt gt 0 then llim = binarr[idx[cnt-1]] else llim = binarr[0]
        ; low threshold is between llim and llim + binsize - 1
        while (E_low - img_E(im,0,llim)/E_tot * 100) GT sigma do $
            llim = llim+1
    endif 

    ; get the high limit
    hlim = imax
    if E_high lt 100 then begin
        idx = where(Ecumul ge E_tot * E_high / 100, cnt)
        if cnt gt 0 then hlim = binarr[idx[0]] else hlim = binarr[nbins-1]
        while (E_high - img_E(im,0,hlim)/E_tot * 100) GT sigma do $
            hlim = hlim+1
    endif 

    return, [llim, hlim]
end

