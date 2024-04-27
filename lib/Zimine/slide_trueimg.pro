;+
; NAME:
;   SLIDE_TRUEIMG 
;
; PURPOSE:  
;   Simple GUI app to display of a stack of true-color images
;
; USAGE: 
;   slide_trueimg, stack
;     stack is 4D array of pixel-interleaved 
;           true-color images ( [3, xres, yres, nimgs] )
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2006-10-26 ivz Original
;-
; Copyright (C) 2006, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

pro sti_tv, img
    on_error, 2
    s = size(img)
    xs=s[2] & ys=s[3]
    nxs = round(float(!d.x_size)) ; new X size
    nys = round(nxs * float(ys)/xs) ; new Y size
    x0 = abs(nxs-!d.x_size)/2 ; x offset (not needed)
    y0 = abs(nys-!d.y_size)/2 ; y offset
    erase
    tv, congrid(img, 3, nxs, nys,/interp), x0, y0, true=1
end ;----------------------------------------------------------------

pro sti_slide, ev
    Widget_Control, ev.top, Get_Uvalue=inf, /no_copy
    inf.frame = ev.value
    wset, inf.wid
    sti_tv, (*inf.pim)[*,*,*,inf.frame]
    Widget_Control, ev.top, Set_Uvalue=inf, /no_copy
end ;----------------------------------------------------------------

pro sti_redraw, ev
    Widget_Control, ev.top, Get_Uvalue=inf, /no_copy
    wset, inf.wid
    sti_tv, (*inf.pim)[*,*,*,inf.frame]
    Widget_Control, ev.top, Set_Uvalue=inf, /no_copy
end ;----------------------------------------------------------------

pro sti_tlb_resize, event

    Widget_Control, event.top, Get_UValue=inf, /No_Copy

   ; Resize the draw widget.
    Widget_Control, inf.drawID, Draw_XSize=event.x, Draw_YSize=event.y
    Widget_Control, inf.slideID, scr_Xsize = event.x
    
    WSet, inf.wid
    sti_tv, (*inf.pim)[*,*,*,inf.frame]

    Widget_Control, event.top, Set_UValue=inf, /No_Copy
end ;----------------------------------------------------------------


pro sti_exit, ev
    widget_control, ev.top,/destroy
end ;----------------------------------------------------------------


pro sti_cleanup, tlb
    Widget_Control, tlb, Get_UValue=inf, /No_Copy
    if N_Elements(inf) eq 0 then return
    Ptr_Free, inf.pim
end ;----------------------------------------------------------------


pro Slide_TrueImg, im, title=title, group_leader=group
    on_error, 2

    if n_params() ne 1 then $
        message, "must be called with one image parameters"

    if n_elements(title) eq 0 then title = "Slide True"
    if n_elements(group) eq 0 then group = 0L

    is = size(im)

    if (is[0] lt 3) or (is[0] gt 4) then message, "not a true color image(s)"
    true = (where(is[1:3] eq 3))[0] + 1

    case true of
        1 : begin
            nx = is[2]
            ny = is[3]
        end
        else : message, "only pixel interleaved mode supported (true=1)"
    endcase

    nz = (is[0] eq 4) ? is[4] : 1

   ; Create the widgets. Make sure the TLB returns resize events.
    tlb = Widget_Base(Title=title,/Column, /TLB_Size_Events, Mbar=tlm)
    drawID = Widget_Draw(tlb, XSize=nx, YSize=ny, /expose_events, $
        event_pro='sti_redraw')
    slideID = Widget_Slider(tlb, xsize=nx, Event_Pro="sti_slide") 

    ; buttons
    ctb = Widget_base(tlb, /row)
    btn = Widget_Button(ctb, Value="Exit", /Separator, Event_Pro='sti_exit')

    Widget_Control, tlb, /Realize

    ; set group leader 
    if n_elements(group) gt 0 then $
        if widget_info(long(group),/valid_id) then $
            widget_control, tlb, group_leader=group

    ;slider only if 4D image 
    if nz gt 1 then Widget_Control, slideID, Set_Slider_Max=(nz-1) $
    else Widget_Control, slideID, Sensitive=0

    Widget_Control, drawID, Get_Value=wid
    WSet, wid

    sti_tv, im[*,*,*,0]

    info = { pim:Ptr_New(im), $
             nx:nx, ny:ny, true:true, frame:0, $
             wid:wid, drawID:drawID, slideID:slideID }
             
    Widget_Control, tlb, Set_UValue=info, /No_Copy

    XManager, 'img_over_img', tlb, Event_Handler='sti_tlb_resize', $
       Cleanup='sti_cleanup', /No_Block, group_leader=group
end

