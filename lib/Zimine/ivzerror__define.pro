;+
; CLASS:
;   IVZERROR
; 
; PURPOSE:
;
;   very simple error reporting object
;   any object inheriting this one gets the ErrMsg method for consistent
;   error handline
;
;   catch, error
;   if error ne 0 then begin
;     self->ErrMsg
;     return
;   endif
;
; CONSTRUCTOR:
;   Parameters: non
;   Keywords:   title=title, logfile=logfilename, /txterror, /no_syserr, /trace
;   By default, errors are reported via dialog_message()
;
; METHODS:
;   self->ErrSetup, title=title, logfile=logfile, /txterror, /no_syserr, /trace
;   self->ErrMsg, extramessage, /log
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2004-11-04 ivz original
;-
; Copyright (C) 2004, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function IVZerror::init, txterror=txterror, no_syserr=no_syserr, $
        trace=trace, title=title, logfile=logfile, dialog_parent=parent
    
    self.err_txt = keyword_set(txterror) ? 1b : 0b ; default no
    self.err_sys = keyword_set(no_syserr) ? 0b : 1b ; default yes
    self.err_calls = keyword_set(trace) ? 1b : 0b ; default no
    self.err_title = obj_class(self)
    if n_elements(logfile) gt 0 then begin
        if size(logfile, /type) eq 7 then self.err_log = logfile $
        else message, "Incorrect logfilename", /info, /continue
    endif
    if n_elements(title) gt 0 then self.err_title=title
    if n_elements(parent) eq 0 then parent=0L
    if widget_info(parent,/valid_id) then self.err_parent=parent
    return, 1
end ; of init

pro IVZerror::clearlog
    if self.err_log eq '' then return
    if file_test(self.err_log, /write) then begin
        openw, lun, self.err_log, /get_lun
        close, lun
    endif
end

function IVZerror::logfile
    return, self.err_log
end

pro IVZerror::ErrSetup, txterror=txterror, no_syserr=no_syserr, $
        trace=trace, title=title, logfile=logfile, dialog_parent=parent
    self.err_txt = keyword_set(txterror) ? 1b : self.err_txt
    self.err_sys = keyword_set(no_syserr) ? 0b : self.err_sys
    self.err_calls = keyword_set(trace) ? 1b : self.err_calls
    self.err_title = (n_elements(title) gt 0) ? title : self.err_title
    if n_elements(logfile) gt 0 then begin
        if size(logfile, /type) eq 7 then self.err_log = logfile $
        else message, "Incorrect logfilename", /info, /continue
    endif
    if n_elements(parent) eq 0 then parent=0L
    if widget_info(parent,/valid_id) then self.err_parent=parent
end ; of ErrSetup

pro IVZerror::ErrMsg, mesg, log=log
    if (n_elements(mesg) eq 0) and (self.err_sys eq 0) then return 
    self.err_msg = n_elements(mesg) gt 0 ? string(mesg) : ''
    help, /last_message, output=err
    ; which file and line 
    line = ''
    if n_elements(err) gt 1 then begin
        if stregex(err[1], '^. Execution halted at:.*') ne -1 then begin
            line = strsplit(err[1], 'at:',/regex,/extract)
            if n_elements(line) gt 1 then $
                line = strsplit(line[1], '  *',/regex,/extract) $
            else line=''
            if n_elements(line) eq 3 then begin
                file = file_basename(line[2])
                line = ' (' + file + ' line:' + line[1] + ')'
            endif 
        endif 
    endif 
    sys_error = err[0]+line
    if self.err_sys then self.err_msg = self.err_msg + ' ' +sys_error
    help, calls=stack ; call stack
    if self.err_txt then begin
        message, self.err_msg, /info, /reset
        if self.err_calls then begin
			print, "Call stack:"
			for i=0, n_elements(stack)-1 do $
				print, "    " + stack[i]
		endif
        return
    endif
    if self.err_calls then $
        ok = dialog_message([self.err_msg, stack], /error, $
                title=self.err_title, $
                dialog_parent=self.err_parent) $
    else $
        ok = dialog_message(self.err_msg, /error, title=self.err_title, $
                dialog_parent=self.err_parent)
    if keyword_set(log) and (self.err_log ne '') then begin
        caldat, systime(/julian), m, d, y, hh, mm, ss
        time = string(y,m,d,hh,mm,ss,format='(%"%4d-%02d-%02d %02d:%02d:%02d")')
        openw, lun, self.err_log, /get_lun, /append
        printf, lun, time, self.err_title, 'ERROR', self.err_msg, $
            format='(A19,A20,A10,": ",A)'
        if self.err_calls then begin
            printf, lun, 'Call stack:'
            for i=0, n_elements(stack)-1 do $
				printf, lun, "    " + stack[i]
        endif
        free_lun, lun
    endif
end ; of ErrMsg

; Report is for sending information messages to screen or to a log file
pro IVZerror::Report, text
    if n_elements(text) eq 0 then return
    if strlen(text) eq 0 then return
    caldat, systime(/julian), m, d, y, hh, mm, ss
    time = string(y,m,d,hh,mm,ss,format='(%"%4d-%02d-%02d %02d:%02d:%02d")')
    if file_test(self.err_log,/write) then $
        openw, lun, self.err_log, /get_lun, /append $
    else lun=-1
    
    printf, lun, time, self.err_title, 'INFO', text, $
        format='(A19,A20,A10,": ",A)'
    if lun gt -1 then free_lun, lun
    return
end ; of Report


pro IVZerror::cleanup
;nothing to clean
end

pro IVZerror__define
    obj = {IVZerror,   $
        err_title: '', $ ; title for dialog_message()
        err_parent: 0L,$ ; dialog_parent
        err_msg: '',   $ ; error message
        err_log: '',   $ ; log file name 
        err_sys: 1b,   $ ; print system errors (default yes)
        err_calls: 0b, $ ; show call stack (default no)
        err_txt: 0b    $ ; print errors instead of showning in dialog
    }
end

