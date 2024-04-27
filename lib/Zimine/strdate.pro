;+
; NAME:
;   STRDATE
; 
; PURPOSE:
;   Return current date-time as 'yyyy-mm-dd hh:mm:ss'
;
; USAGE:
;   result = STRDATE( [ /no_time ] )
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2007-01-29 ivz  Original
;   2008-04-02 ivz  Added NO_TIME keyword
;-
; Copyright (C) 2007, 2008, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function strdate, no_time=no_time
  caldat, systime(/julian), M, D, Y, hh, mm, ss
  out = string(Y,M,D, format='(%"%4d-%02d-%02d")')
  if keyword_set(no_time) then return, out
  out += string(hh,mm,ss, format='(%" %02d:%02d:%02d")')
  return, out
end
