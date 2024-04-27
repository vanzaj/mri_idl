;+
; NAME:
;   FILE_LIST
;
; PURPOSE: 
;   Generate a numbered file list
;
; USAGE: 
;   fls = file_list(prefix, start, end, [suffix, WIDTH=width])
;
; EXAMPLE: 
;   fls = file_list('frame', 1, 10, '.gif')
;   will produce frame01.gif frame02.gif ... frame10.gif
;
;   Number of padding zeros will be determined automatically or
;   can be set via WIDTH keyword
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com>
;
;   2000-09-25 ivz Original
;   2000-09-26 ivz Added WIDTH Keyword
;-
; Copyright (C) 2000, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function file_list, pref, si, ei, suffix, width=width, help=help

  on_error, 2

  if N_Params() lt 3 or Keyword_Set(help) then begin
    print, "Usage: fls = file_list(prefix, start, end, [suffix, WIDTH=width])"
    return, ''
  endif

  if size(pref, /type) ne 7 then begin
    print, "First parameter must be a string"
    return, ''
  endif

  if (si lt 0) or (ei le si) then begin
    print, "Second parameter must be positive and "
    print, "Third parameter must be greater than the second"
    return, ''
  endif

  n_fls = ei - si + 1
  form = '(I2.2)'

  if n_fls gt 99 then form='(I3.3)'

  if n_fls gt 999 then begin
    ans = ''
    read,'Sure you want to generate '+strtrim(n_fls,2)+'names ? (y/n): ', ans
    if StrUpCase(ans) ne 'Y' then return, ''
    form='(I4.4)'
  endif

  if N_Elements(width) eq 1 then begin
    if width gt 0 then begin
      jj=strtrim(width,2)
      form='(I'+jj+'.'+jj+')'
    endif
  endif
  
  fls = pref + string(indgen(ei-si+1)+si, format=form)
  if (N_Elements(suffix) eq 1) and (size(suffix, /type) eq 7) then $
    fls = fls+suffix

  return, fls
end

