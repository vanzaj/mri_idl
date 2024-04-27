;+
; NAME:
;   IMG_VIEW
;
; PURPOSE:
;   2D, 3D or 4D image display utility
;   resizable window, zoom & contrast control
;   limited ROI function
;
; USAGE:
;   IMG_VIEW, image [, GROUP_LEADER=group, 
;       BOTTOM=bottom, NCOLORS=ncolors, 
;       NOTIFYID=notifyID, TITLE=title, XSIZE=xsize, YSIZE=ysize]
;
;      image - 2D,3D or 4D data array
;          dimensions - [x, y, slices, dynamics]
;      Keywords:
;        GROUP_LEADER - parent widget ID
;        BOTTOM, NCOLORS - color table keys
;        XSIZE, YSIZE - initial draw window size
;        TITLE - widget title
;        NOTIFYID - widget IDs that need to receive {IMG_VIEW} event
;      
;
; DEPENDENCY:
;   fsc_field.pro - http://www.dfanning.com/programs/xcolors.pro
;   xcolors.pro   - http://www.dfanning.com/programs/fsc_field.pro
;   saveimage.pro - http://cimss.ssec.wisc.edu/~gumley/idl/saveimage.pro
;
; NOTES:
;   DRAW widget has two Event Handlers: one for processing of "normal" events and
;   another for "roi" events. "ROI" handler is activated via "ROI" menu 
;   (mouse pointer motion events are desactivated at the same time).
;   next mouse button "PRESS" event stores original x,y values and 
;   reactivates motion events. When button is "RELEASED" we compute ROI
;   indexes, show some results using DIALOAG_MESSAGE and switch back to
;   "normal" draw widget event handler.
;
;   INFO is stored in TLB's Uvalue as a pointer. This is done to imitate
;   /NO_COPY effect in Widget_Control,Get_Uvalue=... calls but without
;   the side effect of unsetting the TLB's uvalue while in event handler.
;   This allows using extra routines with TLB as single parameter which
;   in turn allows access to all the info data. In addition,
;   Widget_Control, Set_Uvalue=... at the end of event handlers is not needed.
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2001-03-19 ivz original
;   2001-03-22 ivz Added color support
;              Added widget notification support
;              The event { IMG_VIEW, ID:n, TOP:n, HANDLER:n, X:n, Y:n, Z:n } 
;              is send when mouse button is pressed in the draw area.
;              X, Y, Z are selected pixel/voxel coordinates in the data space
;   2001-05-10 ivz Clear display window before image redraw (using pixmap)
;                  Reload colors at each image redraw
;   2001-09-21 ivz info is stored in TLB's uvalue as pointer
;   2001-09-24 ivz Added ROI (without CW_DEFROI)
;   2006-11-08 ivz Support for 4D image array
;-
; Copyright (C) 2001, 2006, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 


pro img_view_coordconv, x, y, tlb
; converts device coordinates to image coordinates

    Widget_Control, tlb, get_uvalue=p_inf

    ws = (*p_inf).wid.winSize ; window dimensions
    is = ((*p_inf).vsize)[0:1]; original image x,y dimensions
    zs = round(is * (*p_inf).zoom) ; zoomed image dimensions

    if (!order eq 1) then y = ws[1] - y - 1

    dx = (ws[0] - zs[0])/2
    dy = (ws[1] - zs[1])/2

    x = 0 > (x - dx) < (zs[0]-1)
    y = 0 > (y - dy) < (zs[1]-1)

    x = round(x/float(zs[0]) * is[0]) < (is[0]-1)
    y = round(y/float(zs[1]) * is[1]) < (is[1]-1)
end
;-------------------------------------------------- 


pro img_view_tv, img, BOX=box, _Extra=extra
; displays an image in the center of a window
    on_error, 2
    s=size(img, /dimensions)
    win_size=[!d.x_size, !d.y_size]
    dx = (win_size[0] - s[0])/2
    dy = (win_size[1] - s[1])/2
    tv, img, dx, dy, _Extra=extra
    if keyword_set(box) then $
        plots, dx + [0,0,s[0],s[0],0], dy+[0,s[1],s[1],0,0], /device
end
;-------------------------------------------------- 


pro img_view_newimg, tlb
; gets new slice and interpolates to zoomed dimensions
    Widget_Control, tlb, get_uvalue=p_inf

    ima = (*(*p_inf).p_vol)[*,*,(*p_inf).slice, (*p_inf).dyn]
    if (*p_inf).zoom ne 1.0 then begin
        xs = round((*p_inf).vsize[0] * (*p_inf).zoom)
        ys = round((*p_inf).vsize[1] * (*p_inf).zoom)
        if ptr_valid((*p_inf).p_img) then ptr_free, (*p_inf).p_img
        (*p_inf).p_img = ptr_new(congrid(ima, xs, ys, interp=(*p_inf).interp))
    endif else begin
        if ptr_valid((*p_inf).p_img) then ptr_free, (*p_inf).p_img
        (*p_inf).p_img = ptr_new(ima)
    endelse
end
;-------------------------------------------------- 


pro img_view_redraw, tlb
; draw window update
    Widget_Control, tlb, get_uvalue=p_inf

    img = BytScl(*(*p_inf).p_img, min=(*p_inf).r1[0], max=(*p_inf).r1[1], $
                 top=(*p_inf).ncolors) + (*p_inf).bottom

    ; draw on pixmap
    wset, (*p_inf).wid.pixID
    erase
    img_view_tv, img, BOX=(*p_inf).showbox

    ; colors protection
    tvlct, *(*p_inf).p_ctbl1, (*p_inf).bottom

    ; copy pixmap to window
    wset, (*p_inf).wid.winID
    device, copy=[ 0,0,(*p_inf).wid.winSize[0], (*p_inf).wid.winSize[1], $
                   0, 0, (*p_inf).wid.pixID ]
end
;-------------------------------------------------- 


pro img_view_TLB_Resize, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf

    ; compute new draw widget size
    ; based only on Y size of TLB 
    ; (X size is problematic because of the menu)
    ; 64 pixels is the minimum allowed size
    ysize = (float(ev.y) - (*p_inf).wid.ctrSize[1]) > 64
    xsize = ysize / (*p_inf).wid.winSize[1] * (*p_inf).wid.winSize[0] 

    (*p_inf).wid.winSize = [fix(xsize), fix(ysize)]

    ; make new pixmap
    wdelete, (*p_inf).wid.pixID
    window, /free, /pixmap, xsize=fix(xsize), ysize=fix(ysize)
    (*p_inf).wid.pixID = !d.window

    Widget_Control, (*p_inf).wid.draw, Draw_Xsize=fix(xsize), Draw_Ysize=fix(ysize)
    Widget_Control, (*p_inf).wid.sl_slide, Scr_XSize=fix(xsize-(*p_inf).wid.sl_lbl_xsize)
    if Widget_Info((*p_inf).wid.dy_slide, /valid_id) then $
        Widget_Control, (*p_inf).wid.dy_slide, Scr_XSize=fix(xsize-(*p_inf).wid.sl_lbl_xsize)
    img_view_redraw, ev.top
end
;-------------------------------------------------- 


pro img_view_RefMin, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf
    (*p_inf).r1[0] = *ev.value
    img_view_redraw, ev.top
end
;-------------------------------------------------- 


pro img_view_RefMax, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf
    (*p_inf).r1[1] = *ev.value
    img_view_redraw, ev.top
end
;-------------------------------------------------- 


pro img_view_slider, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf
    Widget_Control, ev.id, Get_Uvalue=slide_name

    if slide_name eq 'dyn' then (*p_inf).dyn = ev.value $
    else (*p_inf).slice = ev.value

    img_view_newimg, ev.top
    img_view_redraw, ev.top
end
;-------------------------------------------------- 


pro img_view_ZoomIn, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf
    (*p_inf).zoom = (*p_inf).zoom + 0.2
    str = string((*p_inf).zoom, format="('Zoom: ', F3.1)")
    Widget_Control, (*p_inf).wid.zoom, Set_Value=str

    img_view_newimg, ev.top
    img_view_redraw, ev.top
end
;-------------------------------------------------- 


pro img_view_ZoomOut, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf
    (*p_inf).zoom = ((*p_inf).zoom - 0.2) > 0.2
    str = string((*p_inf).zoom, format="('Zoom: ', F3.1)")
    Widget_Control, (*p_inf).wid.zoom, Set_Value=str

    img_view_newimg, ev.top
    img_view_redraw, ev.top
end
;-------------------------------------------------- 

pro img_view_SetInterp, ev
    Widget_Control, ev.top, Get_Uvalue=p_inf
    (*p_inf).interp = ev.select
    img_view_newimg, ev.top
    img_view_redraw, ev.top
end
;-------------------------------------------------- 

pro img_view_sshot, ev

    Widget_Control, ev.top, Get_Uvalue=p_inf
    wset, (*p_inf).wid.winID

    fname = Dialog_Pickfile(Dialog_Parent=ev.top, Title="Save as")
    if fname eq "" then Return
    ; get file extension
    pos = stregex(fname, "(\.jpg|\.tiff|\.tif|\.png)$")
    if pos eq -1 then begin
        ok = Dialog_Message("Support for JPG, TIF or PNG formats only", $
                            Dialog_Parent=ev.top, /Error)
        Return
    endif

    ext = StrMid(fname, pos, StrLen(fname)-1)

    case StrLowCase(ext) of
        ".jpg" : saveimage, fname, /JPEG, Quality=85, /Quiet
        ".png" : saveimage, fname, /PNG, /Quiet
        ".tif" : saveimage, fname, /TIFF, /Quiet
        ".tiff" : saveimage, fname, /TIFF, /Quiet
    endcase

end 
;-------------------------------------------------- 


pro img_view_colors, ev

    Widget_Control, ev.top, Get_UValue=p_inf

    ncolors = (*p_inf).ncolors
    bottom = (*p_inf).bottom

    etype = Tag_Names(ev, /Structure_Name)

    if etype eq "WIDGET_BUTTON" then begin
        Widget_Control, ev.id, Get_Value=btn_name
        case StrUpCase(btn_name) of
            "LOAD NEW": begin
                XColors, Ncolors=ncolors, Bottom=bottom, Title = 'img_view Colors',$ 
                    Group_Leader=ev.top, NotifyID=[ev.id, ev.top]
            end
            "LOAD ORIGINAL": begin
                tvlct, *(*p_inf).p_ctbl0, bottom
                img_view_redraw, ev.top
            end
            "SHOW BOX": begin
                (*p_inf).showbox = 1b
                Widget_Control, ev.id, Set_Value="Show Box *"
                img_view_redraw, ev.top
            END
            "SHOW BOX *": begin
                (*p_inf).showbox = 0b
                Widget_Control, ev.id, Set_Value="Show Box"
                img_view_redraw, ev.top
            END
        endcase
    endif else $
    if etype eq "XCOLORS_LOAD" then begin
        r = ev.r[ bottom : ncolors-1+bottom ]
        g = ev.g[ bottom : ncolors-1+bottom ]
        b = ev.b[ bottom : ncolors-1+bottom ]
        if Ptr_Valid( (*p_inf).p_ctbl1 ) then Ptr_Free, (*p_inf).p_ctbl1
        (*p_inf).p_ctbl1 = Ptr_New([[r], [g], [b]])
        img_view_redraw, ev.top
endif

end
;--------------------------------------------------


pro img_view_roi, ev
    Widget_Control, ev.top, Get_UValue=p_inf

    Widget_Control, ev.id, Get_Value=btn_name

        ; get ROI type from button name 
    (*p_inf).roi.type = StrUpCase(btn_name)

        ; disable motion events
    Widget_Control, (*p_inf).wid.draw, Draw_Motion_Events=0

        ; switch to "pencil" cursor
    if !D.Window gt -1 then Device, Cursor_Standard=86

        ; switch to another event handler
    Widget_Control, (*p_inf).wid.draw, Event_Pro="img_view_DrawRoi"
end
;--------------------------------------------------

pro img_view_DrawRoi, ev
    ; only up, down and motion events
    if ev.type gt 2 then return
    Widget_Control, ev.top, Get_UValue=p_inf

    names = ["PRESS", "RELEASE", "MOTION", "VIEWPORT", "EXPOSE"]
    event = names[ev.type]

    case event of
        "PRESS": begin
            (*p_inf).roi.x0 = ev.x
            (*p_inf).roi.y0 = ev.y
            Widget_Control, (*p_inf).wid.draw, Draw_Motion_Events=1
        end
        "MOTION": begin
            (*p_inf).roi.x1 = ev.x
            (*p_inf).roi.y1 = ev.y
            case (*p_inf).roi.type of
                "RECTANGLE" : begin
                    x0 = (*p_inf).roi.x0
                    y0 = (*p_inf).roi.y0
                    x1 = (*p_inf).roi.x1
                    y1 = (*p_inf).roi.y1
                    xcrd = [x0, x1, x1, x0, x0] 
                    ycrd = [y0, y0, y1, y1, y0] 
                end
                "ELLIPSE": begin
                    x0 = (*p_inf).roi.x0
                    y0 = (*p_inf).roi.y0
                    x1 = (*p_inf).roi.x1
                    y1 = (*p_inf).roi.y1
                    xc = (x0 + x1)/2
                    yc = (y0 + y1)/2
                    a = float(abs(x1 - x0))
                    b = float(abs(y1 - y0))
                    xcrd = round(xc + a/2*cos(2./!radeg*FindGen(180)))
                    ycrd = round(yc + b/2*sin(2./!radeg*FindGen(180)))
                end
                else:
            endcase
            img_view_redraw, ev.top
            plots, xcrd, ycrd, /device
        end
        "RELEASE": begin
            Widget_Control, (*p_inf).wid.draw, Clear_Events=1
            (*p_inf).roi.x1 = ev.x
            (*p_inf).roi.y1 = ev.y
            case (*p_inf).roi.type of
                "RECTANGLE": begin
                    x0 = (*p_inf).roi.x0
                    y0 = (*p_inf).roi.y0
                    x1 = (*p_inf).roi.x1
                    y1 = (*p_inf).roi.y1
                    img_view_coordconv, x0, y0, ev.top
                    img_view_coordconv, x1, y1, ev.top
                    xcrd = [x0, x1, x1, x0, x0] 
                    ycrd = [y0, y0, y1, y1, y0] 
                end
                "ELLIPSE" : begin
                    x0 = (*p_inf).roi.x0
                    y0 = (*p_inf).roi.y0
                    x1 = (*p_inf).roi.x1
                    y1 = (*p_inf).roi.y1
                    img_view_coordconv, x0, y0, ev.top
                    img_view_coordconv, x1, y1, ev.top
                    xc = (x0 + x1)/2
                    yc = (y0 + y1)/2
                    a = float(abs(x1 - x0))
                    b = float(abs(y1 - y0))
                    xcrd = round(xc + a/2*cos(2./!radeg*FindGen(180)))
                    ycrd = round(yc + b/2*sin(2./!radeg*FindGen(180)))
                end
            endcase
            is = long(((*p_inf).vsize)[0:1])
            roi = polyfillv(xcrd, ycrd, is[0], is[1])
            if roi[0] ne -1 then begin
                img = (*(*p_inf).p_vol)[*,*,(*p_inf).slice, (*p_inf).dyn]
                img = reform(img, is[0]*is[1],/over)
                res = moment(img[roi])
                mesg = ["ROI results", $
                        "size: " + String(N_Elements(roi), format='(I10)'), $
                        "mean: " + String(res[0], format='(F10.2)'), $
                        "stdev:" + String(sqrt(res[1]), format='(F10.2)') $
                        ]
                ok = Dialog_Message(mesg, Dialog_Parent=ev.top, /Info)
            endif else begin
                mesg = "Problem with ROI selection"
                ok = Dialog_Message(mesg, Dialog_Parent=ev.top, /Error)
            endelse
            img_view_redraw, ev.top
                ; restore normal event handler
            Widget_Control, (*p_inf).wid.draw, Event_Pro="img_view_Draw"
                ; switch to default cursor type
            Device, Cursor_Standard=34
        end
    endcase
end
;--------------------------------------------------


pro img_view_Draw, ev

    names = ["PRESS", "RELEASE", "MOTION", "VIEWPORT", "EXPOSE"]
    event = names[ev.type]

    Widget_Control, ev.top, Get_Uvalue=p_inf

    x = ev.x & y = ev.y
    z = (*p_inf).slice
    d = (*p_inf).dyn

    img_view_coordconv, x, y, ev.top

    ; data value
    val = (*(*p_inf).p_vol)[x,y,z,d]

    case event of
        "MOTION" : begin
            str = string(x, y, val, format="('(',i3,',',i3,')',': ', f7.2)")
            Widget_Control, (*p_inf).wid.stat, Set_Value=str
        end
        "PRESS" : begin
            s = size((*p_inf).notifyID)
            if s[0] eq 1 then count=0 else count=s[2]-1
            for j=0, count do begin
                if Widget_Info((*p_inf).notifyID[0,j], /Valid_Id) then begin
                    thisEvent = { IMG_VIEW, $
                                  ID : (*p_inf).notifyID[0,j], $
                                  TOP: (*p_inf).notifyID[1,j], $
                                  HANDLER:0L, $
                                  X:long(x), Y:long(y), Z:long(z) }
                    Widget_Control, (*p_inf).notifyID[0,j], Send_Event=thisEvent
                endif
            endfor
        end
        else:
    endcase
end
;-------------------------------------------------- 


pro img_view_Quit, ev
    ; default cursor
    if !D.Window gt -1 then Device, Cursor_Standard=34
    Widget_Control, ev.top, /Destroy
end
;-------------------------------------------------- 


pro img_view_Cleanup, tlb
    Widget_Control, tlb, Get_UValue=p_inf
    ; restore the original color table
    tvlct, *(*p_inf).p_ctbl0, (*p_inf).bottom
    Ptr_Free, (*p_inf).p_vol, (*p_inf).p_img
    Ptr_Free, (*p_inf).p_ctbl0, (*p_inf).p_ctbl1
    Ptr_Free, p_inf
end
;-------------------------------------------------- 


;================================================== 
; Main program 
;================================================== 
pro img_view, vol, $
    Title=title, $
    Group_Leader=group, $
    Ncolors=ncolors, $
    Bottom=bottom, $
    XSize=xsize, $ ; initial DRAW window XSIZE
    YSize=ysize, $ ; initial DRAW window YSIZE
    NotifyID=notifyID

    on_error, 2 ; return to caller

    if N_Params() eq 0 then begin
        Print, "Usage: img_view, Image "
        Return
    endif

    if N_Elements(vol) eq 0 then begin
        Print, "Image parameter required"
        Return
    endif

    if N_Elements(title)   eq 0 then title = "img_view"
    if N_Elements(group)   eq 0 then group = 0L
    if N_Elements(bottom)  eq 0 then bottom=0
    if N_Elements(ncolors) eq 0 then ncolors=(!D.N_Colors < (256-bottom))
    if N_Elements(xsize)   eq 0 then xsize=256 else xsize=fix(abs(xsize))
    if N_Elements(ysize)   eq 0 then ysize=256 else ysize=fix(abs(ysize))
    if N_Elements(notifyID) eq 0 then notifyID = [-1L, -1L]

    ; image dimension array
    s = size(vol)
    case s[0] of
        2: vsize = [s[1:2], 1, 1]
        3: vsize = [s[1:3], 1]
        4: vsize = [s[1:4]]
        else : message, "Image parameter must be 2,3 or 4D array" 
    endcase

    ; display window size
    winSize = [xsize, ysize] 
    zoom = min(float(winSize)/vsize[0:1])

    ; Top Level Base with Top Level Menu
    tlb = Widget_Base(Title=title, MBar=tlm, /TLB_Size_Events, /Row)

    mFile = Widget_Button(tlm, Value="File", /Menu)
    mb = Widget_Button(mFile, Value="Screen Shot", Event_Pro="img_view_sshot")
    mb = Widget_Button(mFile, Value="Quit", /separator, Event_Pro="img_view_quit")

    mColor = Widget_Button(tlm, Value="Colors", /Menu)
    mb = Widget_Button(mColor, Value="Load New", Event_Pro="img_view_colors")
    mb = Widget_Button(mColor, Value="Load Original", Event_Pro="img_view_colors")
    mb = Widget_Button(mColor, Value="Show Box",/separator, Event_Pro="img_view_colors")


    mRoi = Widget_Button(tlm, Value="ROI", /Menu)
    mb = Widget_Button(mRoi, Value="Rectangle", Event_Pro="img_view_roi")
    mb = Widget_Button(mRoi, Value="Ellipse", Event_Pro="img_view_roi")

    ; display window & status bar
    cont = Widget_Base(tlb, /column)
    drawID = Widget_Draw(cont, Xsize=winSize[0], Ysize=winSize[1], $
                         /Button_Events, /Motion_Events, Retain=2, $
                         Event_Pro="img_view_draw")
    statID = Widget_Label(cont, Value="( 0, 0):", /Dynamic_Resize, /Align_Left)

    ; slices slider
    jnk = Widget_Base(cont,/row,/base_align_right)
    sl_lblID = Widget_Label(jnk, Value="Slice:")
    sl_slideID = Widget_Slider(jnk, Min=0, Max=1>(vsize[2]-1), XSize=winSize[0],$
                Event_Pro="img_view_slider", sensitive=(vsize[2] gt 1), uvalue="slice") 

    ; dynamics slider
    if vsize[3] gt 1 then begin
        jnk = Widget_Base(cont,/row,/base_align_right)
        dy_lblID = Widget_Label(jnk, Value="Dyns: ")
        dy_slideID = Widget_Slider(jnk, Min=0, Max=1>(vsize[3]-1), XSize=winSize[0],$
                    Event_Pro="img_view_slider", sensitive=(vsize[2] gt 1), uvalue='dyn') 
    endif else dy_slideID = -1L

    ; Controls container (another container)
    cont = Widget_Base(tlb, /column)

    ; Slice control
    flag = vsize[2] gt 1
    cont1 = Widget_Base(cont, /Column, Sensitive=flag)
       
    ; Zoom control
    cont1 = Widget_Base(cont, /Column)
    val = 'Zoom: ' + string(zoom, format='(F3.1)')
    zoomID = Widget_Label(cont1, Value=val, /Dynamic_Resize)
    cont2 = Widget_Base(cont, /Row,  /Align_Center)
    btn = Widget_Button(cont2, Value=" - ", Event_Pro="img_view_ZoomOut")
    btn = Widget_Button(cont2, Value=" + ", Event_Pro="img_view_ZoomIn")
    cont3 = Widget_Base(cont, /nonexclusive)
    btn = Widget_Button(cont3, Value="Interpol", Event_Pro="img_view_SetInterp")

    ; image min/max used in bytscl(img)
    ; FSC_FIELD doesn't like BYTEs
    imin = min(vol, max=imax)
    if size(imin, /tname) eq "BYTE" then begin
        imin = fix(imin)
        imax = fix(imax)
    endif

    fld = FSC_Field(cont, Title="Min:", Value=imin, /CR_ONLY, $
                    Xsize=4, Event_Pro="img_view_refmin")
    fld = FSC_Field(cont, Title="Max:", Value=imax, /CR_ONLY, $
                    Xsize=4, Event_Pro="img_view_refmax")

    Widget_Control, tlb, /realize

    ; set group leader
    if N_Elements(group) gt 0 then $
        if Widget_Info(Long(group), /Valid_ID) then $
            Widget_Control, tlb, Group_Leader=group

    ; Create empty pixmap
    window, /free, /pixmap, xsize=winSize[0], ysize=winSize[1]
    pixID = !d.window

    ; Get drawable window index
    Widget_Control, DrawID, Get_Value = winID
    WSet, winID

    Widget_Control, tlb, Tlb_Set_Title=title + ' (' + strtrim(winID, 2) + ')'

    ; sliders widths - labels
    g1 = widget_info(sl_lblID, /geometry)
    sl_lbl_xsize = g1.xsize
    Widget_Control, sl_slideID, Scr_XSize=fix(winSize[0]-sl_lbl_xsize)
    if vsize[3] gt 1 then begin
        g1 = widget_info(dy_lblID, /geometry)
        sl_lbl_xsize = g1.xsize > sl_lbl_xsize
        Widget_Control, dy_slideID, Scr_XSize=fix(winSize[0]-sl_lbl_xsize)
    endif

    ; screen size of current tlb
    geom = widget_info(tlb, /geometry)
    tlbSize = fix([geom.xsize, geom.ysize])
    ctrSize = tlbSize - winSize ; size of control base (slices, zoom btns)

    ; get current colors
    tvlct, r, g, b, /Get
    r = r[bottom : bottom+ncolors-1]
    g = g[bottom : bottom+ncolors-1]
    b = b[bottom : bottom+ncolors-1]

    ; widget IDs needed in event handlers
    wid = { draw : drawID, $
            winSize : winSize,  $ ; draw window size
            ctrSize : ctrSize,  $ ; control base size
            sl_lbl_xsize : sl_lbl_xsize, $ ; sliders label xsize
            winID : winID,  $ ; window ID number of draw widget
            pixID : pixID,  $ ; pixmap ID number
            stat : statID, $
            sl_slide: sl_slideID, $
            dy_slide: dy_slideID, $
            zoom : zoomID  $
          }

    ; ROI structure
    roi = { type: "", $ ; string holding roi type to be drawn
            x0:-1, y0:-1, $
            x1:-1, y1:-1  $
          }

    info = { $
             p_vol : Ptr_New(vol), $ ; original volume
             p_img : Ptr_New(),  $ ; current slice image (zoomed + scaled)
             vsize : vsize,$ ; image dimensions
             r0    : [imin,imax], $ ; original values range
             r1    : [imin,imax], $ ; display values range
             slice : 0,    $ ; current slice
             dyn   : 0,    $ ; current dynamic
             zoom  : zoom,  $ ; current zoom
             interp: 0b,    $
             wid   : wid , $ ; structure with widget IDs
             roi   : roi,  $ 
             showbox : 0b, $ ; draw a box with image dimensions
             p_ctbl0 : Ptr_New([[r],[g],[b]]), $ ; original color table
             p_ctbl1 : Ptr_New([[r],[g],[b]]), $ ; working color table
             ncolors : ncolors, $
             bottom : bottom, $
             notifyID : notifyID $
           }

    Widget_Control, tlb, Set_UValue=Ptr_New(info, /no_copy)

    img_view_newimg, tlb
    img_view_redraw, tlb

    XManager, "img_view", tlb, Event_Handler="img_view_tlb_resize", $
        /No_Block, Cleanup="img_view_cleanup", Group_Leader=group
END

