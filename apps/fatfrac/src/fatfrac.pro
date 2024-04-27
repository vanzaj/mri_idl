;+
; NAME:
;   FATFRAC
;
; PURPOSE:
;   GUI for fat fraction calculator (fatcalc)
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com> (ivz)
;
;   2008-03-18 ivz Added 3D support
;   2008-10-07 ivz Fixed reconstruction of subset of echoes/slices (in calc obj)
;   2008-10-08 ivz Added B0 option (v.0.4.5)
;-

; local copy of Fanning's normalize function 
Function FatFrac_Normalize, range, position=position 
;{{{
On_Error, 2
IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0] ELSE $
    position=Float(position)
range = Float(range)

scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

RETURN, scale
END ;---------------------------------------------- }}}

; error reporting
PRO FatFrac_ErrorMsg 
;{{{
catch,/cancel
help, /last_message, output=err
if n_elements(err) eq 0 then err = !error_state.msg
ok = dialog_message(err,/error,title='FatFrac')
END ;---------------------------------------------- }}}

PRO FatFrac_Load, ev
;{{{
catch, error
if error ne 0 then begin
    FatFrac_ErrorMsg
    return
endif
file = dialog_pickfile(filter=['*.par','*.PAR'],/read, $
        get_path=path, dialog_parent=ev.top)
if file eq '' then return
Widget_Control, ev.top, Get_UValue=pin
cd, path
(*pin).calc->LoadData, file
im = (*pin).calc->rawimage(echo=0)
si = size(im,/dimensions)
(*pin).ogr.im->SetProperty, data=bytscl(im), dimensions=si, interp=1
(*pin).ogr.im->GetProperty, Xrange=xr, Yrange=yr
xcc = FatFrac_normalize(xr) & ycc = FatFrac_normalize(yr)
(*pin).ogr.im->SetProperty, Xcoord_Conv=xcc, Ycoord_Conv=ycc

; enable slice slider
;nb_slices = (*pin).calc->getinfo('nb_slices')
nb_slices = ( (*pin).calc->getdim() )[2]
wdg = widget_info(ev.top, find_by_uname='slide/slice')
if nb_slices gt 1 then $
    Widget_Control, wdg, Set_Slider_Max=nb_slices-1, /Sensitive $
else $
    Widget_Control, wdg, Set_Slider_Max=1, Sensitive=0

; enable echo slider (no check non multi-echo data)
;nb_echoes = (*pin).calc->getinfo('nb_echoes')
nb_echoes = ( (*pin).calc->getdim() )[3]
wdg = widget_info(ev.top, find_by_uname='slide/echo')
Widget_Control, wdg, Set_Slider_Max=nb_echoes-1, /Sensitive

wdg = widget_info(ev.top, find_by_uname='max_echo')
Widget_Control, wdg, Set_Value=strtrim(nb_echoes,2)
(*pin).var.max_echo = nb_echoes

if ptr_valid((*pin).im) then ptr_free, (*pin).im
(*pin).im = ptr_new(im,/no_copy)

(*pin).ogr.win->Draw, (*pin).ogr.scene
;FatFrac_Redraw, ev.top
END ;--------------------------------------------}}}

PRO FatFrac_Save, ev
; {{{
catch, error
if error ne 0 then begin
    FatFrac_ErrorMsg
    return
endif
Widget_Control, ev.top, Get_UValue=pin

Widget_Control, ev.id, Get_Value=btn_name

; screen shot
if strpos(btn_name, 'png') gt -1 then begin 
    f = dialog_pickfile(dialog_parent=ev.top, /write, $
                    filter=['*.png', '*.jpg', '*.tif'])
    if f eq '' then return ; canceled
    ; update the screen
    FatFrac_Redraw, ev.top
    (*pin).ogr.win->GetProperty, image_data=img
    ; save based on file extension
    ext = strupcase(strmid(f, strlen(f)-3, 3))
    case ext of
        'PNG': write_png, f, img
        'JPG': write_jpeg, f, img, true=1
        'TIF': write_tiff, f, reverse(img,3), compression=1
        else : message, "Please use png, jpg or tif file extension"
    endcase
endif else begin 
; save maps in Analyze format
    if (*pin).calc->status(/fitted) eq 0 then begin
        ok = dialog_message('Noting to save yet...', /info, $
            dialog_parent=ev.top)
        return
    endif
    (*pin).calc -> savemaps
endelse

END ;--------------------------------------------}}}

PRO FatFrac_Colors, ev
; {{{
Widget_Control, ev.top, Get_UValue=pin
etype = Tag_Names(ev ,/Structure_Name)
case etype of
"WIDGET_BUTTON" : begin
    Xcolors, Group_Leader=ev.top, NotifyID=[ev.id, ev.top]
end
"XCOLORS_LOAD" : begin
    (*pin).ogr.im->GetProperty, Palette=pal
    pal->SetProperty, Red_Values=ev.r, Green_Values=ev.g, Blue_Values=ev.b
    FatFrac_Redraw, ev.top
end
endcase
END ;--------------------------------------------}}}

PRO FatFrac_Slider, ev
;{{{
catch, error
if error ne 0 then begin
    FatFrac_ErrorMsg
    return
endif
Widget_Control, ev.top, Get_UValue=pin
slider = widget_info(ev.id, /uname)
if slider eq 'slide/slice' then $
    (*pin).var.slice = ev.value $
else (*pin).var.echo = ev.value
FatFrac_Redraw, ev.top
END ;--------------------------------------------}}}

PRO FatFrac_Switch_ImType, ev
; {{{
Widget_Control, ev.top, Get_UValue=pin
Widget_Control, ev.id, Get_UValue=im_type
(*pin).var.im_type = im_type
FatFrac_Redraw, ev.top
END ;--------------------------------------------}}}

PRO FatFrac_ReconCpx, ev
; {{{
Widget_Control, ev.top, Get_UValue=pin
(*pin).calc->rec_all
FatFrac_Redraw, ev.top
END ;--------------------------------------------}}}

PRO FatFrac_FitAll, ev
; {{{
catch, error
if error ne 0 then begin
    FatFrac_ErrorMsg
    return
endif
Widget_Control, ev.top, Get_UValue=pin
if (*pin).calc->status(/data) eq 0 then message,'no data loaded'
Widget_Control, ev.id, Get_Value=btn_name
widget_control, /hourglass
errmsg = ''
(*pin).calc->fit_in_mask, errmsg=errmsg, $
    np=(*pin).var.np_fit, use_real=(*pin).var.use_real, max_echo=(*pin).var.max_echo
if errmsg[0] ne '' then $
    ok = dialog_message(errmsg, dialog_parent=ev.top, /info)
if (*pin).calc->status(/fitted) eq 1 then begin
    (*pin).var.im_type = 'AW'
    FatFrac_Redraw, ev.top
endif
widget_control, hourglass=0
END ;--------------------------------------------}}}

PRO FatFrac_Switch_FitRes, ev
; {{{
Widget_Control, ev.top, Get_UValue=pin
Widget_Control, ev.id, Get_UValue=fpar
if (*pin).var.np_fit eq 3 and fpar eq 'T2SF' then fpar = 'T2SW' 
(*pin).var.im_type = fpar
FatFrac_Redraw, ev.top
END ;--------------------------------------------}}}

PRO FatFrac_SetB0, ev
; {{{
  Widget_Control, ev.top, Get_UValue=pin
  Widget_Control, ev.id, Get_Value=list
  field=list[ev.index]
  (*pin).calc->set_B0_field, float(field)
END ;--------------------------------------------}}}

PRO FatFrac_FitParameters, ev
; {{{
Widget_Control, ev.top, Get_UValue=pin
;Widget_Control, ev.id, Get_UValue=fpar
fpar = widget_info(ev.id, /uname)
pini = (*pin).calc->get_initial_fit_pars()
case fpar of
    'use_real' : begin 
        btn_on = widget_info(ev.id,/button_set)
        (*pin).var.use_real = btn_on
        widget_control, widget_info(ev.top, find_by_uname='base/neg_real'), $
            map=btn_on
    end
    'max_echo' : begin
        input = fix(*ev.value)
        n_ec = ( (*pin).calc->getdim() )[3] ; nb of echoes in the data
        if (input lt 3) or (input gt n_ec) then begin
            msg = "Max echo must be > 3 and <= " + strmid(n_ec,1)
            ok = dialog_message(msg, /info, dialog_parent=ev.top)
            ; reset to previous max nb of echoes
            ev.object -> Set_Value, (*pin).var.max_echo
        endif
        (*pin).var.max_echo = input
    end
    'four_pars' : (*pin).var.np_fit = widget_info(ev.id,/button_set) ? 4 : 3
    'ff+' : pini[0,0] = *ev.value
    't2sw+' : pini[1,0] = *ev.value
    't2sf+' : pini[2,0] = *ev.value
    'ff-' : pini[0,1] = *ev.value
    't2sw-' : pini[1,1] = *ev.value
    't2sf-' : pini[2,1] = *ev.value
    else : ; do nothing
endcase
(*pin).calc->set_initial_fit_pars, pini
;print, (*pin).var.np_fit
END ;--------------------------------------------}}}

; default Draw events
PRO FatFrac_Draw_Main, ev
;{{{
catch, error
if error ne 0 then begin
    FatFrac_ErrorMsg
    return
endif

Widget_Control, ev.top, Get_UValue=pin

; process events only when inside the view
hit=(*pin).ogr.Win->Select((*pin).ogr.View, [ev.x,ev.y], dimensions=[1,1])
if not obj_valid(hit) then return

eventNames = ['DOWN','UP','MOTION','SCROLL','EXPOSE','KEY', 'MOD']
btnNames = ['NONE','LEFT','MIDDLE','RIGHT']

case eventNames[ev.type] of
    'EXPOSE' : FatFrac_Redraw, ev.top
    'DOWN' : begin
        if ev.clicks eq 2 then begin
            FatFrac_Redraw, ev.top
            return
        endif
        hit = (*pin).ogr.Win->PickData( (*pin).ogr.View, $
                        (*pin).ogr.Im, [ev.x, ev.y], xyz)
        if hit ne -1 then begin
            (*pin).dsp.xyz = xyz
            FatFrac_PlotSig, ev.top
        endif
    end ; Mouse DOWN
    'UP' : begin
        ;print, 'press:', ev.press, 'release:', ev.release
    end ; Mouse UP
    'MOTION' : begin
        hit = (*pin).ogr.Win->PickData((*pin).ogr.View, (*pin).ogr.Im, [ev.x, ev.y], xyz)
        if hit ne -1 then begin
            x=floor(xyz[0]) & y=floor(xyz[1])
            if ptr_valid((*pin).im) then $
                val = (*(*pin).im)[x,y] $
            else val = -9999
            str = string(x,y, format='(%"(%d,%d): ")')
            if abs(val) gt 1e5 then str += string(val, format='(E0.2)') $
            else str += strtrim(val,2)
            wdg = widget_info(ev.top, find_by_uname='lbl/status')
            Widget_Control, wdg, set_value=str
        endif
    end ; MOTION
    'KEY' : begin
        if ev.press then begin
        key = strlowcase(string(ev.ch))
        if key eq '+' then begin
            sc = ((*pin).var.scale+0.2) < 5
            loc = (1.0 - sc)/2
            (*pin).ogr.View->SetProperty, Location=[loc,loc], dimension=[sc,sc]
            (*pin).var.scale=sc
            ;(*pin).ogr.Win->Draw, (*pin).ogr.Scene
            FatFrac_Redraw, ev.top
        endif
        if key eq '-' then begin
            sc = ((*pin).var.scale-0.2) > 0.1
            loc = (1.0 - sc)/2
            (*pin).ogr.View->SetProperty, Location=[loc,loc], dimension=[sc,sc]
            (*pin).var.scale=sc
            ;(*pin).ogr.Win->Draw, (*pin).ogr.Scene
            FatFrac_Redraw, ev.top
        endif
        ;print, 'key', key
        ;print, 'mod', ev.modifiers
        endif
    end ; KEY
    'MOD' : ;print, ev.modifiers
    else : ; do noting
endcase
END ;--------------------------------------------}}}

PRO FatFrac_PlotSig, tlb
;{{{
Widget_Control, tlb, Get_Uvalue=pin
if not (*pin).calc->status(/data) then return
xyz = (*pin).dsp.xyz
x=floor(xyz[0]) & y=floor(xyz[1]) & z=(*pin).var.slice
tc = (*pin).calc->timecourse(x,y,z)
rtc = (*pin).calc->timecourse(x,y,z,/real)
t_ptr = (*pin).calc->ptrref(/time)
wset, (*pin).dsp.pwin
n_pts = n_elements(*t_ptr)
ii = indgen(n_pts/2)*2

np = (*pin).var.np_fit 
mec = (*pin).var.max_echo 
pfit = (*pin).calc->fit_pixel_wf(x, y, z, $
    use_real=(*pin).var.use_real, np=np, max_echo=mec, fit_info=fit_info)
if n_elements(pfit) gt 0 then $
fit_tc = (np eq 3) ? fatcalc_fit_wf3par(*t_ptr, pfit) : $
        fatcalc_fit_wf4par(*t_ptr, pfit)

; load colors
col_back = fsc_color('black', 1)
col_axe = fsc_color('white', 2)
col_real = fsc_color('white', 3)
col_even = fsc_color('cyan', 4)
col_odd = fsc_color('yellow', 5)
col_fit = fsc_color('red', 6)

yr = minmax([tc,rtc])
yr = yr + (yr[1]-yr[0])*[-0.05,0.05]
plot, *t_ptr, rtc, yrange=minmax([tc,rtc]), font=2, $
    xtitle='time [ms]', charsize=0.8, background=col_back, color=col_axe, /nodata
oplot, *t_ptr, rtc, linestyle=2, color=col_real
oplot, (*t_ptr)[ii], tc[ii], psym=5, color=col_even
oplot, (*t_ptr)[ii+1], tc[ii+1], psym=6, color=col_odd
if n_elements(fit_tc) gt 0 then begin
    oplot, (*t_ptr)[0:mec-1], fit_tc[0:mec-1], thick=2, color=col_fit
    wdg = widget_info(tlb, find_by_uname='text/fitres')
    widget_control, wdg, set_value=fit_info
endif


;p_fit = (*pin).calc->fit_water_t2s(x,y, yfit=yfit)
;p_fit = (*pin).calc->fit_wf4par(x,y, yfit=yfit)
;oplot, *t_ptr, yfit

;print, p_fit
END ;--------------------------------------------}}}

PRO FatFrac_Redraw, tlb
;{{{
Widget_Control, tlb, Get_Uvalue=pin
if (*pin).calc->status(/data) eq 0 then begin
    (*pin).ogr.Win->Draw, (*pin).ogr.Scene
    return
endif
sl = (*pin).var.slice
ec = (*pin).var.echo
im_type = (*pin).var.im_type
fit_res = ['AW','AF','T2SW','T2SF','chi2']
;print, im_type
if total(strmatch(fit_res, im_type)) gt 0 then $
    if (*pin).calc->status(/fitted) eq 0 then return

slide_on = 1
; disable slide/echo when viewing maps
if total(strmatch([fit_res,'dB'], im_type)) gt 0 then slide_on = 0
wdg = widget_info(tlb, find_by_uname='slide/echo')
widget_control, wdg, sensitive=slide_on

case im_type of
    'R' : im = (*pin).calc->rawimage(slice=sl, echo=ec, /real) 
    'I' : im = (*pin).calc->rawimage(slice=sl, echo=ec,/imag)
    'M0' : im = (*pin).calc->rawimage(slice=sl, echo=ec) 
    'PH' : im = atan((*pin).calc->rawimage(slice=sl, echo=ec,/complex),/phase)
    'dB' : begin
        if (*pin).calc->status(/dB) eq 0 then (*pin).calc->deltaB
        ; getmap should use sl=1 for 1st slice !!!
        im = (*pin).calc->getmap(slice=sl+1, /dB)
    end
    'mask' : begin
        if (*pin).calc->status(/mask) eq 0 then msk=(*pin).calc->newmask() 
        im = (*pin).calc->getmap(slice=sl+1, /mask) 
    end
    'AW' : im = (*pin).calc->getmap(slice=sl+1, /AW)
    'AF' : im = (*pin).calc->getmap(slice=sl+1, /AF)
    'T2SW' : im = (*pin).calc->getmap(slice=sl+1, /T2Sw)
    'T2SF' : im = (*pin).calc->getmap(slice=sl+1, /T2Sf)
    'chi2' : im = (*pin).calc->getmap(slice=sl+1, /chi2)
    else : im = (*pin).calc->rawimage(slice=sl+1, echo=ec) 
endcase
(*pin).ogr.Im->SetProperty, Data=bytscl(im)
(*(*pin).im)[0,0] = im
(*pin).ogr.Win->Draw, (*pin).ogr.Scene
END ;--------------------------------------------}}}

PRO FatFrac_TLB_resize, ev
;{{{
Widget_Control, ev.top, Get_UValue=pin
(*pin).ogr.Win->SetProperty, Dimension=[ev.x, ev.y]
(*pin).ogr.Win->Draw, (*pin).ogr.Scene
END ;--------------------------------------------}}}

PRO FatFrac_Quit, ev
;{{{
Widget_Control, ev.top, /Destroy
END ;--------------------------------------------}}}

PRO FatFrac_Cleanup, tlb
;{{{
Widget_Control, tlb, Get_UValue=pin
if ptr_valid(pin) EQ 1 then begin
    cd, (*pin).var.orig_path
    device, decomposed=(*pin).var.decomposed
    Obj_Destroy, (*pin).calc
    Obj_Destroy, (*pin).ogr.container
    ptr_free, (*pin).im
    ptr_free, pin
endif
END ;--------------------------------------------}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;                      Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
PRO FatFrac, XSize=xsize, YSize=ysize, Title=title, $
    Group_Leader=group, calc=calc

forward_function fatcalc_fit_wf3par, fatcalc_fit_wf4par

Catch, error
if error ne 0 then begin
    Catch, /cancel
    print, !error_state.msg
    if Obj_Valid(oCont) then Obj_Destroy, oCont
    RETURN
endif

version = '0.4.6'

cd, current=orig_path

if strlowcase(!version.os_family) eq 'windows' then cd, "C:\"

; indexed colors for plot window
device, get_decomposed=decomposed
device, decomposed=0

image = bytarr(128,128)
imgSize = size(image, /dimensions)

; grObjects {{{
; container for easy clean up.
oCont = Obj_New('IDL_Container')

oPal = Obj_New('IDLgrPalette')
oPal->LoadCT, 0
oCont->Add, oPal

oIm = Obj_New('IDLgrImage', bytscl(image[*,*,0]), Palette=oPal, dimensions=imgSize)
oIm->GetProperty, Xrange=xr, Yrange=yr
oIm->SetProperty, Xcoord_Conv=FatFrac_Normalize(xr), Ycoord_Conv=FatFrac_Normalize(yr)

oMod = Obj_New('IDLgrModel')
oMod->Add, oIm


oView = Obj_New('IDLgrView', color=[127,127,127], viewplane=[0,0,1,1],  $
    location=[0,0], dimensions=[1,1], units=3)
    
oView->Add, oMod

oScene = Obj_New('IDLgrScene', Color=[127,127,127])
oScene->Add, oView
oCont->Add, oScene
;}}}

IF N_Elements(xsize) EQ 0 THEN xsize = 400
IF N_Elements(ysize) EQ 0 THEN ysize = 400
IF N_Elements(title) EQ 0 THEN title = 'FatFrac'
title += ' (v.' + version + ')'

    ; Creat the widgets for this program.

tlb = Widget_Base(row=1, title=title, $;TLB_Size_Events=1, $
    mbar=tlm, /base_align_left)
filer = Widget_Button(tlm, Value='File')
loader = Widget_Button(filer, Value='Load', Event_Pro='FatFrac_Load')
saver = Widget_Button(filer, Value='Save', /menu)
sav = Widget_Button(saver, Value='maps (Analyze)', Event_Pro='FatFrac_Save')
sav = Widget_Button(saver, Value='screen (.png, .tif)', Event_Pro='FatFrac_Save')
quiter = Widget_Button(filer, Value='Quit', Event_Pro='FatFrac_Quit',/separator)
imager = Widget_Button(tlm, Value='Image')
colorer = Widget_Button(imager, Value='Colors', Event_Pro='FatFrac_Colors')

; image
pb = widget_base(tlb, /column,/base_align_left, xpad=0, ypad=0)
jnk = widget_base(pb,/column, xpad=0, ypad=0)
drawID = Widget_Draw(jnk, XSize=xsize, YSize=ysize, Graphics_Level=2, $
   /Button_Events, /Motion_Events, Keyboard_Events=2, /Expose_Events,$
   Event_Pro='FatFrac_Draw_Main', Retain=0, uname='draw/img')

jnk = widget_base(pb,/row, /base_align_bottom, xpad=0, ypad=0)
wdg = widget_label(jnk, Value='Slice:')
geo = widget_info(wdg, /geometry)
slideID = widget_slider(jnk, Xsize=Xsize-geo.scr_xsize, Min=0, Max=1, $
    sensitive=0, Event_Pro='FatFrac_Slider', uname='slide/slice')

jnk = widget_base(pb,/row, /base_align_bottom, xpad=0,ypad=0)
wdg = widget_label(jnk, Value='Echo:')
geo = widget_info(wdg, /geometry)
slideID = widget_slider(jnk, Xsize=Xsize-geo.scr_xsize, Min=0, Max=1, $
    sensitive=0, Event_Pro='FatFrac_Slider', uname='slide/echo')

sbarID = Widget_Label(pb, Value='()', /dynamic_resize, $
  uname='lbl/status', /sunken_frame)


; controls
cb = widget_base(tlb, /column)

rb = widget_base(cb, /row,/frame)
rad = widget_base(rb,/row,/exclusive, uname='image_types')
mbtnID = widget_button(rad, value='Magn', uvalue='M0', $
    Event_Pro='FatFrac_Switch_ImType')
imID= widget_button(rad, value='Real', uvalue='R', $
    Event_Pro='FatFrac_Switch_ImType')
imID = widget_button(rad, value='Imag', uvalue='I', $
    Event_Pro='FatFrac_Switch_ImType')
imID = widget_button(rad, value='Phase', uvalue='PH', $
    Event_Pro='FatFrac_Switch_ImType')
imID = widget_button(rad, value='dB', uvalue='dB', $
    Event_Pro='FatFrac_Switch_ImType')
imID = widget_button(rad, value='mask', uvalue='mask', $
    Event_Pro='FatFrac_Switch_ImType')
imID = widget_button(rb, value='Recon', Event_Pro='FatFrac_ReconCpx')

; 4 par fit
rb = widget_base(cb, /row,/frame)
rad = widget_base(rb,/row,/exclusive, uname='maps_types')
wbtnID = widget_button(rad, value='water', uvalue='AW', $
    Event_Pro='FatFrac_Switch_FitRes')
imID= widget_button(rad, value='fat', uvalue='AF', $
    Event_Pro='FatFrac_Switch_FitRes')
imID = widget_button(rad, value='T2*w', uvalue='T2SW', $
    Event_Pro='FatFrac_Switch_FitRes')
imID = widget_button(rad, value='T2*f', uvalue='T2SF', $
    Event_Pro='FatFrac_Switch_FitRes')
imID = widget_button(rad, value='chi2', uvalue='chi2', $
    Event_Pro='FatFrac_Switch_FitRes')

; fit parameters

pini = [[0.25,30.,20],[0.75,30.,20]]

fp = widget_base(cb, /row)
checkb = widget_base(fp,/row,/nonexclusive)
riID = widget_button(checkb, value='Use Real image', uname='use_real', $
    Event_Pro='FatFrac_FitParameters')
hiID = widget_button(checkb, value='4 parameter fit', uname='four_pars', $
    Event_Pro='FatFrac_FitParameters')

wid = FSC_Field(fp, Title='Max echo:', value=-1, /cr_only, $
    name='max_echo', xsize=4, event_pro='FatFrac_FitParameters')
imID = widget_button(fp, value='Fit All', Event_Pro='FatFrac_FitAll')

pb1 = widget_base(cb, /row, /base_align_center)
a1 = FSC_Field(pb1, Title='FF:', value=pini[0,0],/cr_only, $
    name='ff+', xsize=4, event_pro='FatFrac_FitParameters')
t2swID = FSC_Field(pb1, Title='T2*w:', value=pini[1,0],/cr_only, $
    name='t2sw+', xsize=4, event_pro='FatFrac_FitParameters')
t2sfID = FSC_Field(pb1, Title='T2*f:', value=pini[2,0],/cr_only, $
    name='t2sf+', xsize=4, event_pro='FatFrac_FitParameters')
b0ID = widget_droplist(pb1, Title=' B0:', value=['1.5','3.0'], $
    event_pro='FatFrac_SetB0')

pb_neg_base = widget_base(cb, /row, uname='base/neg_real')
a1 = FSC_Field(pb_neg_base, Title='FF:', value=pini[0,1],/cr_only, $
    name='ff-', xsize=4, event_pro='FatFrac_FitParameters')
t2swID = FSC_Field(pb_neg_base, Title='T2*w:', value=pini[1,1],/cr_only, $
    name='t2sw-', xsize=4, event_pro='FatFrac_FitParameters')
t2sfID = FSC_Field(pb_neg_base, Title='T2*f:', value=pini[2,1],/cr_only, $
    name='t2sf-', xsize=4, event_pro='FatFrac_FitParameters')
lbl = widget_label(pb_neg_base, value='(for negative real)')

plotID = Widget_Draw(cb, XSize=xsize, YSize=floor(xsize*2./4), $
    uname='draw/plot')

fitresID = widget_text(cb, xsize=40, ysize=4, value='', $
    uname='text/fitres')


    ; Realize the widgets. Get the object window.
Widget_Control, tlb, /Realize
Widget_Control, mbtnID, set_button=1
Widget_Control, wbtnID, set_button=1
Widget_Control, pb_neg_base, map=0
Widget_Control, drawID, Get_Value=oWin
Widget_Control, plotID, Get_Value=pwin
oCont->Add, oWin

; resize sBar to drawID size
geo = widget_info(drawID, /geometry)
widget_control, sbarID, xsize=geo.xsize

    ; Create info structure for program information.
oGr = { im : oIm, model: $
    oMod, view: oView, scene: oScene, $
    container: oCont, win: oWin}

var = { scale: 1.0, slice: 0, echo:0, $
    im_type:'M0', np_fit:3, use_real:0b, max_echo:-1, $
    orig_path:orig_path, decomposed:decomposed}

dsp = { left_down : 0b, right_down : 0, key:'', $
        x0:0, y0:0, xyz:fltarr(3), $
        pwin : pwin $
      }

calc = obj_new('fatcalc', group_leader=tlb)
calc->set_initial_fit_pars, pini
calc->set_B0_field, 1.5
info = { calc : calc,$
         im : ptr_new(), $ ; current image 
         ogr  : oGr, $ ; graphic objects
         var  : var, $ ; variables
         dsp  : dsp  $ ; display variables
        }
Widget_Control, tlb, Set_UValue=ptr_new(info, /No_Copy)

    ; warning message
warn_txt = ["CAUTION - Investigation Device.",$
            "Limited by Federal Law to investigational use only."]
ok = dialog_message(warn_txt, dialog_parent=tlb, /information)

XManager, 'fatfrac', tlb, /No_Block, Cleanup='FatFrac_Cleanup', $
    Event_Handler="FatFrac_TLB_resize", Group_Leader=group

END
