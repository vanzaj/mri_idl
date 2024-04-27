;+
; NAME:
;   rec2anz
; 
; PURPOSE:
;   Graphical interface around parrec() and writeanz
;   to convert between Philips PAR/REC (V3 and V4) to Analyze 7.5
;
; DEPENDENCY:
;   anzhead_struct.pro
;   file_list.pro
;   parrec.pro
;   writeanz.pro
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2006-09-06 ivz Original
;   2007-06-05 ivz Apdated for Kyushu Univ (for Dr. Yoshiura)
;-
; Copyright (C) 2006, 2007, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

pro rec2anz_src, ev
    widget_control, ev.top, get_uvalue=pinf
    widget_control, ev.id, get_uvalue=uval

    case uval of
    'btn' : begin
        ;if (*pinf).dest_dir ne '' then cd, (*pinf).dest_dir
        file = dialog_pickfile(dialog_parent=ev.top, /read, $
            get_path=path, filter=['*.par','*.PAR'])
        if file eq '' then return
        file = file_basename(file)
    end
    'txt' : begin
        widget_control, ev.id, get_value=val
        if file_test(val[0]) eq 0 then return
        path = file_dirname(val[0])
        file = file_basename(val[0])
    end
    endcase

    ; read the data
    ff = filepath(file, root_dir=path)
    ;im = parrec(ff, info=parinfo, matrix=matrix, voxel=vox)
    im = parrec(ff, info=parinfo)
    if n_elements(im) eq 1 then begin
        ok = dialog_message("Failed to read "+ff, /error, $
                dialog_parent=ev.top)
        return
    endif

    ; new parrec returns 3D array and assumes that slices have been ordered
    ; reform to 4D if nb_slices NE total_nb_images
    sz = size(im)
    n_tot = sz[3]
    n_sl = parinfo.nb_slices
    if n_tot ne n_sl then begin
      n_dyns = n_tot/n_sl
      im = reform(im, [sz[1],sz[2],n_sl,n_dyns], /over)
    endif else n_dyns = 1

    ; get image dimensions
    matrix = size(im, /dimen)

    ; get voxel info
    vx = parinfo.pixel_spacing_x[0]
    vy = parinfo.pixel_spacing_y[0]
    if parinfo.version GT 3 then $
      vz = parinfo.slice_thickness[0] + parinfo.slice_gap[0] $
    else vz = parinfo.slice_th + parinfo.slice_gap
    voxel = [vx,vy,vz]

    ; defaults for analyze file name
    widget_control, (*pinf).wids.rec_id, set_value = ff
    if (*pinf).dest_dir eq '' then $
        widget_control, (*pinf).wids.dst_id, set_value = path
    prefix = file_basename(file_basename(file, '.par'), '.PAR')
    widget_control, (*pinf).wids.ana_id, set_value = prefix

	; image orientation
	ori_list = parinfo.slice_orientation
	ori_uniq = ori_list[uniq(ori_list[sort(ori_list)])]
	if n_elements(ori_uniq) gt 1 then begin
		ok = dialog_message(["Detected multiple image orientations", "verify results"], $
				/warning, dialog_parent=ev.top)
	endif

	; axial RECs are saved as R->L, A->P, I->S (LPH)
	; flip Y, becase we need P->A (LAS = SPM2)
	if ori_uniq[0] eq 1 then im = reverse(temporary(im),2)

	; PAR: TRA=1, SAG=2, COR=3  -> ANZ: TRA=0, COR=1, SAG=2
	ori = ([0,2,1])[ori_uniq[0]-1]
	widget_control, (*pinf).wids.ori_id, set_droplist_select=ori

    ; check image order
    ; warn if dynamic / diffusion comes before slices (export w/o sort option on)
    if (parinfo.nb_slices gt 1) then begin
        if (parinfo.slice_number[0] eq parinfo.slice_number[1]) then begin
            msg = ['Data appears incorrectly sorted!', $
                   'Convertion may result in incorrectly ordered images.', $
                   'Consider reexporting with Sort option on.']
            ok = dialog_message(msg, /info, dialog_parent=ev.top)
        endif
    endif

    ; activate split if dynamic study
    if (parinfo.nb_dynamics gt 1) or (parinfo.diffusion eq 1) then $
        widget_control, (*pinf).wids.split_id, sensitive=1 $
    else widget_control, (*pinf).wids.split_id, sensitive=0

    ; activate Fix B0 if diffusion V4.1
    if (parinfo.diffusion eq 1) and (parinfo.version eq 4.1) then $
        widget_control, (*pinf).wids.fixb0_id, sensitive=1 $
    else widget_control, (*pinf).wids.fixb0_id, sensitive=0

    info = "Name:  " + parinfo.pat_name
    info = [info, "Exam:  "+parinfo.exam_name]
    info = [info, "Protocol:  "+parinfo.protocol]
    ss = string(matrix[0:2], format='(3(i4, :, " x "))')
    info = [info, "Matrix: " + ss]
    info = [info, "Dynamics: " + strtrim(n_dyns,2)]
    ss = string(voxel, format='(3(f4.2, :," x "))')
    info = [info, "Voxel: " + ss]
    info = [info, "TR:  " + string(parinfo.TR, format='(f7.2)')]
    widget_control, (*pinf).wids.info_id, set_value = info

    (*pinf).parfile = ff
    (*pinf).dest_dir = path
    (*pinf).prefix = prefix
    (*pinf).n_dyns = n_dyns
    (*pinf).voxel = voxel
    (*pinf).tr = parinfo.TR
    if ptr_valid((*pinf).data) then ptr_free, (*pinf).data
    (*pinf).data = ptr_new(im,/no_copy)
    if ptr_valid((*pinf).parinfo) then ptr_free, (*pinf).parinfo
    (*pinf).parinfo = ptr_new(parinfo,/no_copy)
end
;--------------------------------------------------

pro rec2anz_dst, ev
    widget_control, ev.top, get_uvalue=pinf
    widget_control, ev.id, get_uvalue=uval
    case uval of
    'btn' : begin
        path = dialog_pickfile(dialog_parent=ev.top, /directory)
        if path eq '' then return
        widget_control, (*pinf).wids.dst_id, set_value=path
        (*pinf).dest_dir = path
    end
    'txt' : begin
        widget_control, ev.id, get_value=val
        tdir = val[0]
        if tdir eq '' then return
        if file_test(tdir, /directory) eq 0 then begin
            ans = dialog_message(["Directory "+tdir+" doesn't exist", $
                    "Create new directory ?",tdir], /Question, dialog_parent=ev.top)
            if strlowcase(ans) eq 'no' then begin
                widget_control, (*pinf).wids.dst_id, set_value=(*pinf).dest_dir
                return
            endif
            file_mkdir, tdir
        endif
        (*pinf).dest_dir = tdir
    end ; of txt
    'prefix' : begin
        widget_control, ev.id, get_value=val
        prefix = val[0]
        if prefix eq '' then return
        (*pinf).prefix = prefix
    end
    endcase
end
;--------------------------------------------------

pro rec2anz_go, ev
    widget_control, ev.top, get_uvalue=pinf
    widget_control, ev.id, get_value=name
    if strlowcase(name) eq 'exit' then begin
        cd, (*pinf).orig_dir
        widget_control, ev.top, /destroy
        return
    endif

    if not ptr_valid((*pinf).data) then begin
        ok = dialog_message("Nothing to convert!", /info, dialog_parent=ev.top)
        return
    endif

    widget_control, (*pinf).wids.dst_id, get_value=path
    (*pinf).dest_dir = path[0]

    if file_test((*pinf).dest_dir,/directory) eq 0 then begin
        ok = dialog_message("Invalid destination directory!", /error, $
                    dialog_parent=ev.top)
        return
    endif

    widget_control, (*pinf).wids.ana_id, get_value=prefix
    (*pinf).prefix = file_basename(file_basename(prefix[0], '.hdr'), '.img')

    if (*pinf).prefix eq '' then begin
        ok = dialog_message("Empty prefix!", /error, $
                    dialog_parent=ev.top)
        return
    endif

    ; if split dyns, generate a list of output file names
    split = widget_info((*pinf).wids.split_id, /button_set)

    if (*pinf).n_dyns gt 1 and split then $
        file = file_list((*pinf).prefix+'_', 1, (*pinf).n_dyns) $
    else file = (*pinf).prefix

    ; in V4.1 B0 image is one before last
    ; if fixB0, put it as the 1st image
    fixb0 = widget_info((*pinf).wids.fixb0_id, /button_set)
    if fixb0 then begin
        ndyns = (*pinf).n_dyns
        vol = (*(*pinf).data)[*,*,*,0] ; save 1st volume
        tmp = vol
        (*(*pinf).data)[*,*,*,0] = (*(*pinf).data)[*,*,*,ndyns-2] ; B0 as 1st
        for i=1, ndyns-2 do begin
            tmp = (*(*pinf).data)[*,*,*,i] ; save current
            (*(*pinf).data)[*,*,*,i] = vol ; replace it with previous
            vol = tmp                      ; reassign for next cycle
        endfor
    endif

    cd, (*pinf).dest_dir

    test = file_search((*pinf).prefix+'*.hdr', count=count)
    if count gt 0 then begin
        mesg = (*pinf).prefix + '*.hdr found in ' + (*pinf).dest_dir
        ans = dialog_message([mesg, 'Overwrite?'], /question, $
                    dialog_parent=ev.top)
        if strlowcase(ans) eq 'no' then return
    endif

    desc = (*(*pinf).parinfo).protocol + ' \ ' + $
           (*(*pinf).parinfo).pat_name + ' \ ' + $
           (*(*pinf).parinfo).exam_date

    ana_ori = widget_info((*pinf).wids.ori_id, /droplist_select)

    if (*pinf).n_dyns gt 1 and split then $
        for i=0, (*pinf).n_dyns-1 do $
            writeanz, (*(*pinf).data)[*,*,*,i], file[i], orient=ana_ori, $
                voxel=(*pinf).voxel, tr=(*pinf).tr, desc=desc, /overwrite $
    else writeanz, *(*pinf).data, file, orient=ana_ori, $
        voxel=(*pinf).voxel, tr=(*pinf).tr, desc=desc, /overwrite

    cd, file_dirname((*pinf).parfile)
    (*pinf).parfile = ''
    (*pinf).prefix = ''
    ptr_free, (*pinf).data
    ptr_free, (*pinf).parinfo
    widget_control, (*pinf).wids.rec_id, set_value=''
    widget_control, (*pinf).wids.ana_id, set_value=''
    widget_control, (*pinf).wids.info_id, set_value='par info'
    widget_control, (*pinf).wids.split_id, sensitive=0, set_button=0
    widget_control, (*pinf).wids.fixb0_id, sensitive=0, set_button=0
end
;---------------------------------------------

pro rec2anz_about, ev
    widget_control, ev.top, get_uvalue=pinf
	text = [$
	(*pinf).title, $
	"Converter for PAR/REC (V3-V4.2) to Analyze (7.5)", $
	"Author: Ivan Zimine <izimine@gmail.com>", $
	"", $
	"NOTES: REC file is assumed to be sorted",$
	"       i.e. image slices should come before dynamics",$
	"Very limited support for image orientation", $
	"because of Analyze format limitations", $
	"Try to avoid converting non-axial images", $
	"Axial images in the REC file are saved following Radiological convention" $
	]
	ok = dialog_message(text, /info, dialog_parent=ev.top)
end

pro rec2anz_empty, ev
    ;
end
;---------------------------------------------
pro rec2anz_clean, tlb
    widget_control, tlb, get_uvalue=pinf
    ptr_free, (*pinf).parinfo
    ptr_free, (*pinf).data
    ptr_free, pinf
end


;main
pro rec2anz

    cd, current=orig_dir
    version = '0.8.1'

    warn_txt = ["CAUTION - Investigation Device.",$
                "Limited by Federal Law to investigational use."]

    ;title = string(format='(%"rec2anz (version %3.1f)")', version)
    title = string(format='(%"rec2anz (version %s)")', version)

    tlb = widget_base(title=title, /column, /base_align_center)

    b0 = widget_base(tlb, /row)

    b1  = widget_base(b0, /column, /frame, /base_align_left)
    lbl = widget_label(b1, value="Source (par/rec):")

    b2     = widget_base(b1, /row)
    rec_id = widget_text(b2, value="", uvalue='txt', /editable, $
                         xsize=40, event_pro='rec2anz_src')
    but = widget_button(b2, value='Browse', uvalue='btn', $
                        event_pro='rec2anz_src')

    lbl    = widget_label(b1, value="Destination Dir:")
    b2     = widget_base(b1, /row)
    dst_id = widget_text(b2, value="", uvalue='txt', /editable, $
                         xsize=40, event_pro='rec2anz_dst')
    but    = widget_button(b2, value='Browse', uvalue='btn', $
                           event_pro='rec2anz_dst')

    lbl    = widget_label(b1, value="Analyze file name:")
    ana_id = widget_text(b1, value="", uvalue="prefix", /editable, $
                         xsize=40, event_pro='rec2anz_dst')

    b2     = widget_base(b1, /row)
    jnk    = widget_base(b2, /row)
    lbl = widget_label(jnk, value="Orientation")
    vals = ['TRA', 'COR', 'SAG'];, 'TRA flip', 'COR flip', 'SAG flip']
    ori_id = widget_droplist(jnk, value=vals, event_pro='rec2anz_empty')
    neb =widget_base(b2, /nonexclusive)
    split_id = widget_button(neb, value='Split dynamics', sensitive=0, $
                             event_pro='rec2anz_empty')
    fixb0_id = widget_button(neb, value='DTI: B0 - 1st image', sensitive=0, $
                             event_pro='rec2anz_empty')

    b1      = widget_base(b0, /column, /frame, /base_align_left)

    text_size = max(strlen(warn_txt)) > 30
    warn_id = widget_text(b1, value=warn_txt, xsize=text_size, ysize=n_elements(warn_txt))
    info_id = widget_text(b1, value="par info", xsize=text_size, ysize=8)
    bb  = widget_base(b1, /align_right)
    but = widget_button(bb, value='About', event_pro='rec2anz_about')

    ; /grid_layout automagically creates buttons with same xsize
    b1 = widget_base(tlb, column=2, space=20, /grid_layout)
    but = widget_button(b1, value='Convert', event_pro='rec2anz_go')
    but = widget_button(b1, value='Exit', event_pro='rec2anz_go')


    Widget_Control, tlb, /realize

	ori = 0 ; default orientation
    Widget_Control, ori_id, set_droplist_select=ori

    wids = {rec_id:rec_id, dst_id:dst_id, ana_id:ana_id, $
            info_id:info_id, ori_id: ori_id, $
            split_id:split_id, fixb0_id:fixb0_id}

    info = {orig_dir : orig_dir, $
            parfile : '', $
            dest_dir : '', $
            prefix : '', $
            parinfo : ptr_new(), $
            n_dyns : 1, $
            voxel : fltarr(3), $
            tr : 0.0, $
            data : ptr_new(), $
            ori  : ori, $
            wids : wids, $
            title : title $
        }
    Widget_Control, tlb, set_uvalue=ptr_new(info,/no_copy)

    Xmanager, 'rec2anz', tlb, /no_block, cleanup='rec2anz_clean'
end
