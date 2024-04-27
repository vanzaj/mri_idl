;+
; NAME:
;   MEANSTDEV
;
; PURPOSE:
;   Compute MEAN, STDDEV of an array
;   shortcut for MOMENT()
; 
; USAGE:
;   res = meanstd(arr [, NAN=nan, /DOUBLE])
;     res[0] = mean(arr)
;     res[1] = stdev(arr)
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2000-07-20 ivz original
;-
; Copyright (C) 2000, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function meanstdev, array, NAN=nan, DOUBLE=double
  on_error,2
  res = moment(array, NAN=nan, DOUBLE=keyword_set(double))
  return, [res[0], sqrt(res[1])]
end

