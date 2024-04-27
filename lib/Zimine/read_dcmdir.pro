;+
; NAME:
;   READ_DCMDIR
;
; PURPOSE:  
;   Read data/info from a list of dicom files
;
; USAGE: 
;   files = file_search(path, 'IM_*')
;   ptr_data = read_dcmdir(files [, info=info] )
;   
;   returns a pointer to the data and info in a hashtable object
;   caller should do proper cleanup
;
; WARNING: All files must belong to the same series (same acquisition)
;   
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2007-09-15: original
;   2008-01-16: added check for SeriesInstanceUID
;-
; Copyright (C) 2007, 2008, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


; transform/clean some dicom elements info PAR equivalents
pro dcm2par_info_update, info
    catch, error
    if error ne 0 then begin
        ok = error_message(/trace)
        return
    endif
    if (obj_valid(info) eq 0) then $
        message, 'invalid info object'

    nimg = info->get('TotalImages')
    xy_res = info->get('ReconMatrix')
    xy_pix = info->get('PixelSpacing')
    fov_x = xy_res[0] * xy_pix[0]
    fov_y = xy_res[1] * xy_pix[1]

    ; patient position
    pp = info->get('PatientPosition')
    if strpos(string(pp), 'HFS') gt -1 then info->add, 'PatientPosition', 'Head First Supine', /replace $
    else info->add, 'PatientPosition', 'PRIDE_unknown_position',/replace

    ; acquisition matrix (sometimes returned as 4-el vector)
    acq_mat = info->get('AcquisitionMatrix')
    if n_elements(acq_mat) eq 4 then acq_mat = acq_mat[[0,3]]
    if n_elements(acq_mat) lt 2 then acq_mat = info->get('ReconMatrix')
    info->add, 'AcquisitionMatrix', acq_mat, /replace

    ; gap
    sl_thick = info->get('SliceThickness')
    sl_gap = abs(info->get('SpacingBtwSlices') - sl_thick)
    info->add, 'SliceGap', float(sl_gap)

    ; FOV (read, phase)
    phase_dir = info->get('PhaseEncodeDir')
    if phase_dir EQ 'COL' then fov_read = xy_res[0] * xy_pix[0] $
    else fov_read = xy_res[1] * xy_pix[1]
    fov_phase = fov_read * info->get('PercentSampling')/100
    info->add, 'FOV_read', fov_read
    info->add, 'FOV_phase', fov_phase

    ; angulations
    dircos = info->get('ImageOrientation')
        ; assuming same angulations for all images
    rowcol = reform(dircos[0,*])
    angul = awgeom_rowcol_to_ang(rowcol[0:2], rowcol[3:5], view_ori=view_ori)
    info->add, 'ImageAngulation', angul ;rh,ap,fh
    info->add, 'SliceOrientation', view_ori ;TRA/SAG/COR

    ; prep dir
    prep_dir = 'PRIDE_unknown_preparation_direction'
    case view_ori of
        'TRA' : prep_dir = (phase_dir eq 'COL') ? 'Anterior-Posterior' : 'Right-Left'
        'SAG' : prep_dir = (phase_dir eq 'COL') ? 'Feet-Head' : 'Anterior-Posterior'
        'COR' : prep_dir = (phase_dir eq 'COL') ? 'Feet-Head' : 'Right-Left'
    endcase
    info->add, 'PreparationDir', prep_dir

    ; offcenter
    imgpos = info->get('ImagePosition') ; upper-left corner
    ; shift imgpos by half fov in the direction of (row + col)
    offcenter = imgpos + (dircos[*,0:2] * fov_x/2 + dircos[*,3:5] * fov_y/2)
    info->add, 'ImageOffcenter', offcenter
    info->add, 'MidSliceOffcenter', total(offcenter, 1) / nimg

    ; how many slices & dynamics (based on view direction)
    ;
    ; ImagePosition is location of UpperLeft pixel (x,y,z)
    ; at least for pure TRA, different slices have different z,
    ; so I assume x = RL, y = AP, z = FH
    if view_ori EQ 'SAG' then sp = imgpos[*,0] $
    else if view_ori EQ 'COR' then sp = imgpos[*,1] $
    else sp = imgpos[*,2] ; TRA
    n_sl = n_elements(uniq(sp[sort(sp)]))
    n_dyn = nimg / n_sl

    info->add, 'NbSlices', n_sl
    info->add, 'NbDynamics', n_dyn

    ; FOV in slice dir
    fov_slice = (sl_thick + sl_gap) * n_sl - sl_gap
    info->add, 'FOV_slice', fov_slice

    ; FOV (rl, ap, fh)
    if view_ori EQ 'TRA' then begin
        if phase_dir EQ 'COL' then fov = [fov_read, fov_phase, fov_slice] $
        else fov = [fov_phase, fov_read, fov_slice]
    endif else if view_ori EQ 'COR' then begin
        if phase_dir EQ 'COL' then fov = [fov_read, fov_slice, fov_phase] $
        else fov = [fov_phase, fov_slice, fov_read]
    endif else begin ; SAG
        if phase_dir EQ 'COL' then fov = [fov_slice, fov_read, fov_phase] $
        else fov = [fov_slice, fov_phase, fov_read]
    endelse
    info->add, 'FOV_rl_ap_fh', fov

    ; dynamics or slices first ? (using slice position)
    dyn_order='no'
    if sp[0] eq sp[1] then dyn_order = 'yes'
    info->add, 'DynamicOrder', dyn_order
    info->add, 'WaterFatShift', 0.0
end


function read_dcmdir, files, info=info, debug=debug
    catch, error
    if error ne 0 then begin
        ok = error_message(/trace)
        if obj_valid(dcm) then obj_destroy, dcm
        if obj_valid(info) then obj_destroy, info
        return, -1
    endif

    debug = keyword_set(debug)

    if n_elements(files) eq 0 then $
        message, /noname, "Usage: data = read_dcmdir(files)"

    nf = n_elements(files)

    dcm = obj_new('IDLffDICOM')

    if (dcm -> read(files[0]) ne 1) then $
        message, "unable to read " + files[0]

    if obj_valid(info) then obj_destroy, info
    info = obj_new('hashtable', length=200, null_value=2L^30, /no_dup)

    ; general info (read only from the first file)
    tags = ['PatientsName','StudyDate', 'StudyTime',$
            'SeriesNumber','AcquisitionNumber',$
            'ScanningSequence', 'ProtocolName','SeriesDescription',$
            'SeriesInstanceUID', $
            'SoftwareVersion','ImagingFrequency','PatientPosition', $
            'RepetitionTime','EchoTime','FlipAngle','PhaseEncodeDir',$
            'SliceThickness','SpacingBtwSlices','PixelSpacing',$
            'EchoTrainLength', 'NumberOfAverages','PercentSampling',$
            'PercentPhaseFOV','BitsAllocated', 'AcquisitionMatrix' $
            ]

    manuf = strupcase(getdcmtag(dcm, 'Manufacturer'))
    info->add, 'Manufacturer', manuf

    for i=0, n_elements(tags)-1 do begin
        val = getdcmtag(dcm, tags[i])
        nv = n_elements(val)
        if manuf eq 'GE MEDICAL SYSTEMS' and nv gt 1 and $
           tags[i] eq 'BitsAllocated' then $
                info->add, tags[i], val[1] $
        else info->add, tags[i], val
    endfor

    ; get recon resolution directly from data
    v = getdcmtag(dcm, '7fe0,0010',/pointer)

    if ptr_valid(v[0]) eq 0 then $
        message, "Failed to get image data from " + files[0]

    ; GE DTI saves two images. We need the 2d one
    if manuf eq 'GE MEDICAL SYSTEMS' and n_elements(v) gt 1 then $
        img = *v[1] else img = *v[0]

    xy_res = size(img, /dimensions)
    info->add, 'ReconMatrix', xy_res
    img_type = size(img, /type)

    ; arrays
    data = make_array(dimension=[xy_res, nf], type=img_type, /nozero)
    img_num = lonarr(nf)
    img_position = dblarr(nf,3)
    img_dircos = dblarr(nf,6)
    img_wc = dblarr(nf)
    img_ww = img_wc
    r_inter = img_wc
    r_slope = img_wc + 1.0d
    dti_b_value = dblarr(nf)

    seriesUID = info->get('SeriesInstanceUID')

    ; loop over all files
    for i=0, nf-1 do begin
        if (dcm -> read(files[i]) ne 1) then $
            message, "unable to read " + files[i]
        thisUID = getdcmtag(dcm,'SeriesInstanceUID')
        if thisUID NE seriesUID then $
            message, "Multiple image series are not allowed"
        image_num = getdcmtag(dcm,'ImageNumber')
            ; in Philips DTI dicom image_num is 2el vector
        if (n_elements(image_num) GT 1) AND (strpos(manuf, 'PHILIPS') gt -1) then begin
            image_num = long(image_num[1])
            ;help, image_num
        endif

        img_num[i] = image_num
        img_position[i,*] = getdcmtag(dcm,'ImagePosition')
        img_dircos[i,*] = getdcmtag(dcm,'ImageOrientation')
        wc = getdcmtag(dcm,'WindowCenter')
        ww = getdcmtag(dcm,'WindowWidth')
        img_wc[i] = wc[0]
        img_ww[i] = ww[0]
        if strpos(manuf, 'PHILIPS') gt -1 then begin
            r_inter[i] = getdcmtag(dcm, 'RescaleIntercept')
            r_slope[i] = getdcmtag(dcm, 'RescaleSlope')
        endif
        p = getdcmtag(dcm, '7fe0,0010',/pointer)
        if strpos(manuf, 'GE MEDICAL') gt -1 then begin
            img = *p[1]
            b_val = getdcmtag(dcm, '0043,1039')
            if n_elements(b_val) gt 0 then b_val = b_val[0] else b_val = 0
        endif else begin
            img = *p[0]
            b_val = 0
        endelse
        data[*,*,i] = img
        dti_b_value[i] = b_val
    endfor

    ; resort images if needed (never sure about order of dicom files)
    sidx = sort(img_num)
    if total(abs(img_num[sidx] - img_num)) ne 0 then begin
        if debug then print, 'resorting'
        img_num = img_num[sidx]
        img_position = img_position[sidx,*]
        img_dircos = img_dircos[sidx,*]
        img_wc = img_wc[sidx]
        img_ww = img_ww[sidx]
        r_inter = r_inter[sidx]
        r_slope = r_slope[sidx]
        dti_b_value = dti_b_value[sidx]
        data = data[*,*,sidx]
    endif

    info->add, 'ImagePosition', img_position
    info->add, 'ImageOrientation', img_dircos
    info->add, 'ImageNumber', img_num
    info->add, 'WindowCenter', img_wc
    info->add, 'WindowWidth', img_ww
    info->add, 'RescaleIntercept', r_inter
    info->add, 'RescaleSlope', r_slope
    info->add, 'bValue', dti_b_value
    info->add, 'TotalImages', nf

    dcm2par_info_update, info

    obj_destroy, dcm
    return, ptr_new(data, /no_copy)
end

