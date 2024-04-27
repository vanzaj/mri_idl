;+
; NAME:
;     NXMOVIE
;
; PURPOSE:
;     This program is a simplified version of XINTERANIMATE. It is written
;     to illustrate the proper way to write an animation loop in a widget
;     program using the WIDGET_TIMER functionality.
;
; CATEGORY:
;     Widgets.
;
; CALLING SEQUENCE:
;
;      NXMOVIE, images
;
; INPUTS:
;     images: A three-dimensional image array. The animation occurs over
;              the third dimension.
;
; KEYWORD PARAMETERS:
;     GROUP_LEADER:   The group leader of the program. 
;                     When the group leader dies, this program dies as well.
;
;     TITLE:   The window title of the program. The default is "< CINE >"
;
;     XSIZE:   Width of the animation window. Default is Data X size
;
;     YSIZE:   Height of the animation window. Default is Data Y size
;
;     NOBYTSCL: Skip byte scaling the data
;
; COMMON BLOCKS:
;     None.
;
; SIDE EFFECTS:
;     None.
;
; EXAMPLE:
;     To open the abnormal heart data and animate it, type:
;
;        filename = FILEPATH(SUBDIR=['examples', 'data'], 'abnorm.dat')
;        OPENR, lun, filename, /GET_LUN
;        data = BYTARR(64, 64, 15)
;        READU, lun, data
;        FREE_LUN, lun
;        data = REBIN(data, 256, 256, 15)
;
;        NXMOVIE, data
;
; MODIFICATION HISTORY:
;      Written by: David Fanning, June 96.
;      Added slider for controlling animation speed. 30 June 99.
;
;      jan 2000 - added NEXT, PREV, HOME, END  buttons for manual 
;                 frame control (Ivan Zimine <ivan.zimine@physics.unige.ch>)
;      oct 2000 - added XSIZE and YSIZE keywords (IVZ) 
;      jan 2001 - added Labels for Frame and Delay (IVZ)
;-


PRO NXMOVIE_DELAY_EVENTS, event

; Get the INFO structure.
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

; Set the delay.
info.delay = (100 - event.value) / 200.0 ; Max delay 0.5 seconds.
Widget_Control, info.delayLbl, Set_Value="Delay: "+ $
                                 string(info.delay, format='(f4.2)')
; Put INFO structure back.
WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; ***************************** of NXMOVIE_DELAY_EVENTS ***********



PRO NXMOVIE_EVENT, event

; Get the INFO structure.
WIDGET_CONTROL, event.top, GET_UVALUE=info

; What kind of event is this?
eventName = TAG_NAMES(event, /STRUCTURE_NAME)

; Code for BUTTON events.
if eventName eq 'WIDGET_BUTTON' then begin

   case event.id of

      info.start: begin

         ; If stop flag is 0, then display next frame.
         if info.stopflag eq 0 then begin
            WSET, info.wid
            TV, (*info.p_data)[*,*,info.thisFrame]
            ; Write the frame number on the image.
            str = "Frame: " + String(info.thisFrame, format='(i3)')
            Widget_Control, info.frameLbl, Set_Value=str
         endif

         ; Update frame number.
         info.thisFrame = info.thisFrame + 1
         if info.thisFrame gt (info.nframes - 1) then info.thisFrame = 0

         ; Set a timer event to get back into this event handler.
         WIDGET_CONTROL, event.ID, TIMER=0

         ; Update stop flag & deactivate all buttons
         info.stopflag = 0
         Widget_Control, info.first, sensitive=0
         Widget_Control, info.prev, sensitive=0
         Widget_Control, info.next, sensitive=0
         Widget_Control, info.last, sensitive=0
         Widget_Control, info.start, sensitive=0
      end ; of START button CASE.

      info.stp: begin
         info.stopflag = 1
         info.thisFrame = info.thisFrame - 1 

         ; Activate buttons for manual browsing
         Widget_Control, info.first, sensitive=1
         Widget_Control, info.next, sensitive=1
         Widget_Control, info.prev, sensitive=1
         Widget_Control, info.last, sensitive=1
         Widget_Control, info.start, sensitive=1

      end ; of STOP button

      info.first: begin
         info.thisFrame = 0
         WSET, info.wid
         TV, (*info.p_data)[*,*,info.thisFrame]
         str = "Frame: " + String(info.thisFrame, format='(i3)')
         Widget_Control, info.frameLbl, Set_Value=str
      end ; end of << (FIRST frame) button

      info.prev: begin
         info.thisFrame = info.thisFrame - 1
         IF info.thisFrame LT 0 THEN info.thisFrame = info.nframes-1 
         WSET, info.wid
         TV, (*info.p_data)[*,*,info.thisFrame]
         str = "Frame: " + String(info.thisFrame, format='(i3)')
         Widget_Control, info.frameLbl, Set_Value=str
      end ; end of < (PREV frame) button

      info.next: begin
         info.thisFrame = info.thisFrame + 1
         IF info.thisFrame GT (info.nframes - 1) THEN info.thisFrame = 0
         WSET, info.wid
         TV, (*info.p_data)[*,*,info.thisFrame]
         str = "Frame: " + String(info.thisFrame, format='(i3)')
         Widget_Control, info.frameLbl, Set_Value=str
      end ; end of > (NEXT frame) button

      info.last: begin
         info.thisFrame = info.nframes - 1
         WSET, info.wid
         TV, (*info.p_data)[*,*,info.thisFrame]
         str = "Frame: " + String(info.thisFrame, format='(i3)')
         Widget_Control, info.frameLbl, Set_Value=str
      end ; end of >> (LAST frame) button

      info.quit: WIDGET_CONTROL, event.top, /DESTROY
   ENDCASE

   ; Put INFO structure back.
   IF WIDGET_INFO(event.top, /VALID_ID) THEN $
      WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
   RETURN
ENDIF ; of BUTTON code

; Code for TIMER events.
IF eventName EQ 'WIDGET_TIMER' THEN BEGIN

   IF info.stopflag EQ 0 THEN BEGIN
      WSET, info.wid
      ; Delay for a moment.
      Wait, info.delay
      ; Display the image.
      TV, (*info.p_data)[*,*,info.thisFrame]
      ; Write the frame number on the image.
      str = "Frame: " + String(info.thisFrame, format='(i3)')
      Widget_Control, info.frameLbl, Set_Value=str

      ; Update frame number.
      info.thisFrame = info.thisFrame + 1
      IF info.thisFrame GT (info.nframes - 1) THEN info.thisFrame = 0

      ; Set a timer event to get back into this event handler.
      IF info.stopflag EQ 0 THEN WIDGET_CONTROL, event.ID, TIMER=0
   ENDIF

   WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
ENDIF ; of TIMER code

END ; ***************************** of NXMOVIE_EVENT ***********************

PRO NXMOVIE_CLEANUP, tlb
   WIDGET_CONTROL, tlb, GET_UVALUE=info
   Ptr_Free, info.p_data
END ; ***** of NXMOVIE_CLEANUP ********************************************* 



; ***********
;  Main prog
; ***********

PRO NXMOVIE, data, GROUP_LEADER=group, TITLE=title, $
    XSIZE=xsize, YSIZE=ysize, NOBYTESCL=nobytscl

  On_Error, 2

  IF N_Params() EQ 0 THEN Message, "Must be called with 1 parameter."

  dataSize = SIZE(data)
  IF dataSize[0] NE 3 THEN Message, 'Data parameter must be 3D.'

  IF N_Elements(title) EQ 0 THEN title=' << CINE >> '

  IF N_Elements(xsize) EQ 0 THEN xsize = 256 > dataSize[1]
  IF N_Elements(ysize) EQ 0 THEN ysize = 256 * dataSize[2]/dataSize[1]

  if keyword_set(nobytscl) then dat=data else dat=bytscl(data)

  ;; Create the widgets.
  tlb = Widget_Base(COLUMN=1, TITLE=title, /Base_Align_Center)
  drawID = Widget_Draw(tlb, XSIZE=xsize, YSIZE=ysize)


  junk = widget_Base(tlb, row=2, /align_center)

    frameLBL = Widget_Label(junk, Value="Frame: 0", /dynamic_resize, $
                            XSize=xsize/2, /align_left)
    delayLBL = Widget_Label(junk, Value="Delay: 0.25", /dynamic_resize,$
                            XSize=xsize/2, /align_right)
    delayID = Widget_Slider(junk, /Suppress_Value, XSize=xsize, $
                            Max=100, Min=0, Value=50, $
                            Event_Pro='NXMovie_Delay_Events')

  junk= Widget_Base(tlb, row=1, align_center=1, SPACE=10)

  junk1 = Widget_Base(junk, column=1, frame=1)
    start = Widget_Button(junk1, Value=' Start ')
    stp = Widget_Button(junk1, Value=' Stop ')

  junk1= Widget_Base(junk, column=2, frame=1)
  ; go to first frame button
  first = Widget_Button(junk1, Value=" Home ")
  ; go to previous frame button
  prev = Widget_Button(junk1,  Value=" Prev ")
  ; go to last frame button
  last = Widget_Button(junk1,  Value="  End ")
  ; go to next frame button
  next = Widget_Button(junk1,  Value=" Next ")

  junk2= Widget_Base(tlb, row=1, align_center=1)
    quit = Widget_Button(junk2, Value=' Quit ')

  ; Realize and make draw widget active graphics window.
  WIDGET_CONTROL, tlb, /REALIZE
  WIDGET_CONTROL, drawID, GET_VALUE=wid
  WSET, wid

  TV, Congrid(dat[*,*,0], xsize, ysize, /interp)

  if (xsize ne dataSize[1]) or (ysize ne dataSize[2]) then $
    p_data = Ptr_New(Congrid(dat, xsize, ysize, dataSize[3],/interp)) $
  else p_data = Ptr_New(dat)

  ; Create "INFO" structure. Store in TLB's UValue.
  info = {start:start, $         ; ID of START button.
          stp:stp, $             ; ID of STOP button.
          quit:quit, $           ; ID of QUIT button.
          first:first, $         ; ID of HOME btn
          prev:prev, $           ; ID of PREV btn
          next:next, $           ; ID of NEXT btn
          last:last, $           ; ID of END btn
          frameLbl: frameLbl, $  ; ID of frame Label
          delayLbl: delayLbl, $  ; ID of delay Label
          wid:wid, $             ; window index number of draw widget.
          p_data:p_data, $       ; pointer to data to animate.
          delay:0.25, $          ; next frame delay time (seconds)
          stopflag:0, $          ; stop animation flag.
          nframes:dataSize[3], $ ; total number of animation frames.
          thisFrame:0 }          ; current animation frame.

  WIDGET_CONTROL, tlb, SET_UVALUE=info, /NO_COPY
  XMANAGER, 'nxmovie', tlb, GROUP_LEADER=group, /no_block, Cleanup='NXMOVIE_CLEANUP'
END

