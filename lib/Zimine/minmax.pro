;+
; NAME:
;   MINMAX
;
; PURPOSE:
;   Compute MIN, MAX & RANGE of an array
; 
; USAGE: 
;   res = minmax(arr [, NAN=nan], RANGE=rng)
;     res[0] = min(arr)
;     res[1] = max(arr)
;     rng = max-min
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2000-07-20 ivz Original
;-
; Copyright (C) 2000, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function minmax, array, NAN=nan, RANGE=rng
  on_error, 2
  amin = min( array, MAX=amax, NAN=nan)
  rng = amax-amin
  return, [ amin, amax ]
end

