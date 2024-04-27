;+
; NAME:
;   GAMMAVAR
;
; PURPOSE:  
;   Compute gamma variate function
;   
; USAGE: 
;   result = GAMMAVAR(t, [p0, p1, p2])
;     t - array of time values (independent variable)
;     P - function parameters
;
; NOTES:
;   Hemodynamic (BOLD) response model
;   hr = k * (t))^p1 * exp(-(t)/p2) 
;
;   Some Typical parameters
;   p1 = 8.184, p2 = 0.44
;   p1 = 8.6, p2 = 0.547 (Cohen97)
;
;   time-to-peak = p1*p2
;   hr_max = (p1*p2)^p1 * exp(-p1)
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2000-11-07 ivz Original
;-
; Copyright (C) 2000, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function gammavar, t, P
  scale = double(p[1]*p[2])^p[1] * exp(-p[1])
  h = P[0]/scale * double(t)^(P[1]) * exp((-t)/P[2])
  return, h
end

