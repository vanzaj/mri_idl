;+
; NAME:
;   IMGTV
; 
; PURPOSE:
;   Shortcut for quick display of a 2D image or slice from 3/4D dataset 
;   in the current window (if non is available, 256x256 window is created).
;   Resizes to fit the window
;
; USAGE: 
;   IMGTV, image [, slice, frame], [/NORESIZE, /NOBYTSCL]
;
; NOTE: 
;   No device checking for color and other stuff
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   1999-08-30 ivz Original
;-
; Copyright (C) 1999, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

PRO imgtv, image, slice, frame, NORESIZE=noresize, NOBYTSCL=nobytscl, NOINTERP=nointerp

    On_Error, 2

    IF N_Elements(image) EQ 0 THEN Message, "First parameter must be 2,3,4D image"

    image = Reform(Temporary(image))
    s = Size(image)
    IF s[0] LT 2 OR s[0] GT 4 THEN Message, "First parameter must be 2,3,4D image"

    IF N_Elements(slice) EQ 0 THEN slice = 0
    IF N_Elements(frame) EQ 0 THEN frame = 0

    IF !D.Window EQ -1 THEN Window, XSIZE=256, YSIZE=256,  /free

    CASE s[0] OF  
        2 : img = image
        3 : BEGIN 
            IF slice GT s[3]-1 THEN Message, "Incorrect slice parameter"
            img = image[*,*,slice]
        END 
        4 : BEGIN  
            IF frame GT s[4]-1 THEN Message, "Incorrect frame parameter"
            IF slice GT s[3]-1 THEN Message, "Incorrect slice parameter"
            img = image[*,*,slice,frame]
        END 
    ENDCASE 
       
    interp = Keyword_Set(nointerp) ? 0 : 1

    IF NOT Keyword_Set(noresize) THEN $
        img = congrid(temporary(img), !D.X_Size, !D.Y_Size,interp=interp)
            
    IF NOT Keyword_Set(nobytscl) THEN $
        img = bytscl(temporary(img))
        
    si = size(img, /dimensions)
    TV, img, (!D.X_Size - si[0])/2, (!D.Y_Size - si[1])/2, _Extra=extra
END 

