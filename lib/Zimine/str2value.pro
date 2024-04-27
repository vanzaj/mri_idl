;+
; NAME:
;   STR2VALUE
; 
; PURPOSE:
;   convert a string to a numerical value
;   (or array, if multiple values separated by splitchar (defaults to ' '))
;
;   valid numbers are [+-]?[0-9]+.?[0-9]*[eEdD]?[+-]?[0-9]{0,3}
;   if input has other characters, function returns original input
;
;   default convertion of integers to LONG and floats as DOUBLE
;   but can be changed via keywords
;
; USAGE:
;   result = str2value( str [, splitchar=splitchar, float=float, int16=int16])
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2007-01-31 ivz Original  
;-
; Copyright (C) 2007, Ivan Zimine
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function str2value, str, splitchar=splitchar, float=float, int16=int16
    catch, error
    if error ne 0 then begin
        catch, /cancel
        help, /last_message, output=err
        if n_elements(err) eq 0 then err = !error_state.msg
        ok = dialog_message(err,/error,title='str2value')
        return, str
    endif

    if size(str,/tname) ne 'STRING' then $
        message, "input must be a string"
    if n_elements(str) gt 1 then $
        message, "input must be a scalar string"
    if n_elements(splitchar) eq 0 then splitchar=' '
    ftype = keyword_set(float) ? 1.0 : 1.0d
    itype = keyword_set(int16) ? 1 : 1L
    ; get rid of leading & trailing spaces
    str = strtrim(str, 2)
    try = strsplit(str, splitchar, /extract)
    if n_elements(try) eq 1 then try=try[0]
    ; definitely not a number
    if total(stregex(try, "[^0-9.eEdD+-]", /boolean)) gt 0 then return, str
    ; float or integer
    if total(stregex(str, "[.eEdD]",/boolean)) gt 0 then $
		return, try * ftype $
    else return, try * itype
end

