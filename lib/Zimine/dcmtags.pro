;+
; NAME:
;   DCMTAGS
;
; PURPOSE:
;   Print tags values from a DICOM file to stdout or file
;
; USAGE: 
;   DCMTAGS, dcmfile, tags, [outfile='name', /silent]" 
;     tags    string array with elements like '(0018,1020)'
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2001-05-01 ivz original
;-
; Copyright (C) 2001, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 


PRO dcmtags, dcmfile, tags, outfile=outfile, silent=silent, help=help

    catch, error
    if error ne 0 then begin
        catch, /cancel
        message, !error_state.msg, /continue
        if obj_valid(dci) then obj_destroy, dci
        return
    endif   
    
    if Keyword_Set(help) then begin
        print, "usage: dcmtags, dcmfile, tags, [outfile='name', /silent]"
 	    return
    endif
 	 
    if N_elements(dcmfile) eq 0 then $
        dcmfile = Dialog_PickFile(/read)
 
    if dcmfile eq '' then begin 
        print, "  no input file specified" 
 	    return
    endif
   
    if N_Elements(tags) eq 0 then begin
        print, "no tags specified"
        return
    endif
     
    ; write to file ?
    if N_elements(outfile) eq 0 then out = 0 else out = 1
    ; write to stdin ?
    if Keyword_Set(silent) then verb = 0 else verb = 1
 
    ; no stdout and output file
    ; user knows what he's doing
    if verb eq 0 and out eq 0 then $
        return
 
    ; process tags
    ntags = n_elements(tags)
    tgrp = intarr(ntags)
    telm = intarr(ntags)
    for i=0, ntags-1 do begin
        c='' & grp=0 & elm=0
        reads, tags[i], format='( A1, Z, ",", Z, A1 )', c, grp, elm, c
        tgrp[i] = grp & telm[i] = elm
    endfor
 
    dci = obj_new('IDLffDICOM')
    stat = dci-> read(dcmfile)
 
    if stat eq 0 then begin 
       message, "Couldn't read file: " + dcmfile, /continue
       obj_destroy, dci
       return
    endif 
    
    if out then openw, lun, outfile, /get_lun

    for i=0, ntags-1 do begin
        ; check for pixel data tag (7FE0,0010)
        if (tgrp[i] eq 32736) and (telm[i] eq 16) then begin
            print, "skipping PixelData"
            continue
	    endif
        val = dci->GetValue(tgrp[i], telm[i], /no_copy)
        desc = dci->GetDescription(tgrp[i], telm[i])
        if size(desc,/tname) eq "STRING" then begin
            desc = StrMid(desc[0], StrPos(desc[0], " "))
            desc = String(StrCompress(desc,/Remove_All), format='(A15)')
        endif else desc='#'+String("UNKNOWN", format='(A14)')
        if ptr_valid(val[0]) then begin
            if verb then print, desc + ": " + strtrim(*val[0],2)    
            if out then printf, lun, desc + ": " + strtrim(*val[0],2)    
        endif else begin
            if verb then print, desc + ": " + tags[i] + ' undefined'
            if out then printf, lun, desc + ": " + tags[i] + ' undefined'
        endelse
    endfor

    if out then free_lun, lun
    obj_destroy, dci
    return
END 

