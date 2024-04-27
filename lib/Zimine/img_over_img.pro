;+
; NAME:
;   IMG_OVER_IMG
;
; PURPOSE:  
;   Display two gray-scale images (2D or 3D) in a resizable window
;   using R and G channels of a true-color image
;   Useful to check image misalignment
;
; USAGE: 
;   IMG_OVER_IMG, im1, im2
;     with 3D images, the last dimention is slices
;
; DEPENDENCY:
;   saveimage.pro
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2003-02-23 ivz original (based on Draw_Resize.pro from D. Fanning)
;   2005-03-25 ivz changed to keep original image aspect ratio
;                  added File menu 
;-
; Copyright (C) 2003, 2005, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

pro ioi_tv, img
    on_error, 2
    s = size(img)
    xs=s[1] & ys=s[2]
    nxs = round(float(!d.x_size)) ; new X size
    nys = round(nxs * float(ys)/xs) ; new Y size
    x0 = abs(nxs-!d.x_size)/2 ; x offset (not needed)
    y0 = abs(nys-!d.y_size)/2 ; y offset
    erase
    tv, congrid(img, nxs, nys,3,/interp), x0, y0, true=3
end ;----------------------------------------------------------------

pro ioi_slide, ev
    Widget_Control, ev.top, Get_Uvalue=inf, /no_copy
    sl = ev.value
    (*inf.pbuf)[*,*,0] = bytscl(congrid((*inf.pim1)[*,*,sl], inf.nx, inf.ny,/interp))
    (*inf.pbuf)[*,*,1] = bytscl(congrid((*inf.pim2)[*,*,sl], inf.nx, inf.ny,/interp))
    wset, inf.wid
    ioi_tv, *inf.pbuf
    Widget_Control, ev.top, Set_Uvalue=inf, /no_copy
end ;----------------------------------------------------------------

pro ioi_tlb_resize, event

    Widget_Control, event.top, Get_UValue=inf, /No_Copy

   ; Resize the draw widget.
    Widget_Control, inf.drawID, Draw_XSize=event.x, Draw_YSize=event.y
    Widget_Control, inf.slideID, scr_Xsize = event.x
    
    WSet, inf.wid
    ioi_tv, *inf.pbuf

    Widget_Control, event.top, Set_UValue=inf, /No_Copy
end ;----------------------------------------------------------------

pro ioi_save, ev
    Widget_Control, event.top, Get_UValue=inf, /No_Copy
    wset, inf.wid
    f = dialog_pickfile(dialog_parent=ev.top, /write, $
            filt=['*.png','*.jpg','*.tif'])
    if f eq "" then return
    ; file extension
    ext = strmid(f,strpos(f,'.',/reverse_search),strlen(f))
    case strlowcase(ext) of
        '.jpg' : saveimage, f, /jpeg, /quiet
        '.tif' : saveimage, f, /tiff, /quiet
        '.png' : saveimage, f, /png, /quiet
        else : err = dialog_message("Unsupported format: <"+ext+">")
    endcase
    Widget_Control, event.top, Set_UValue=inf, /No_Copy
end ;----------------------------------------------------------------


pro ioi_exit, ev
    widget_control, ev.top,/destroy
end ;----------------------------------------------------------------

pro ioi_cleanup, tlb
    Widget_Control, tlb, Get_UValue=info, /No_Copy
    if N_Elements(info) eq 0 then return
    Ptr_Free, info.pim1, info.pim2, info.pbuf
end ;----------------------------------------------------------------


pro Img_Over_Img, im1, im2

    on_error, 2

    if n_params() ne 2 then $
        message, "must be called with two image parameters"

    is1 = size(im1)
    is2 = size(im2)

    if is1[0] ne is2[0] then message, "unequal image dimensions"
    if (is1[0] lt 2) or (is1[0] gt 3) then message, "images must be 2D or 3D"


    nx = is1[1] > is2[1]
    ny = is1[2] > is2[2]
    nz = (is1[0] eq 3) ? is1[3] : 1

    buf = bytarr(nx, ny, 3)
    buf[*,*,0] = bytscl(congrid(im1[*,*,0], nx, ny,/interp))
    buf[*,*,1] = bytscl(congrid(im2[*,*,0], nx, ny,/interp))

   ; Create the widgets. Make sure the TLB returns resize events.
    tlb = Widget_Base(Title='img-over-img', /Column, /TLB_Size_Events, Mbar=tlm)

    menu = Widget_Button(tlm, Value='File', /menu)
    btn = Widget_Button(menu, Value="Save", Event_Pro='ioi_save')
    btn = Widget_Button(menu, Value="Exit", /Separator, Event_Pro='ioi_exit')
    
    drawID = Widget_Draw(tlb, XSize=400, YSize=400)
    
    slideID = Widget_Slider(tlb, xsize=400, Event_Pro="ioi_slide") 


    Widget_Control, tlb, /Realize

    ;slider only if 3D image 
    if nz gt 1 then Widget_Control, slideID, Set_Slider_Max=(nz-1) $
    else Widget_Control, slideID, Sensitive=0

    Widget_Control, drawID, Get_Value=wid
    WSet, wid

    ioi_tv, buf

    info = { pim1:Ptr_New(im1), $
             pim2:Ptr_New(im2), $
             pbuf : ptr_new(buf, /no_copy), $
             nx:nx, ny:ny, $
             wid:wid, drawID:drawID, slideID:slideID }
             
    Widget_Control, tlb, Set_UValue=info, /No_Copy

    XManager, 'img_over_img', tlb, Event_Handler='ioi_tlb_resize', $
       Cleanup='ioi_cleanup', /No_Block
end

