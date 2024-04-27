;+
; NAME:
;   GETDCMTAG
;
; PURPOSE:  
;   Get tag's value from a valid dicom object
;
; USAGE: 
;   result = getdcmtag(obj, tag)
;   tag is specified as a string either as 'gggg,eeee' 
;   or predefined name (ex. 'StudyDate')
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   2007-01-13 ivz Original
;   2008-01-15 ivz added dcmdict_manuf
;-
; Copyright (C) 2007, 2008, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


function dcmdict, ntags=ntags 
    tags = [ $
        ['TransferSyntaxUID',  '0002,0010'], $
        ['StudyDate',          '0008,0020'], $
        ['StudyTime',          '0008,0030'], $
        ['ContentTime',        '0008,0033'], $ ; new name
        ['Modality',           '0008,0060'], $
        ['Manufacturer',       '0008,0070'], $
        ['Institution',        '0008,0080'], $
        ['StudyDescription',   '0008,1030'], $
        ['SeriesDescription',  '0008,103E'], $
        ['ManufacturerModel',  '0008,1090'], $
        ['PatientsName',       '0010,0010'], $
        ['PatientBirthDate',   '0010,0030'], $
        ['ScanningSequence',   '0018,0020'], $
        ['SequenceName',       '0018,0024'], $
        ['ProtocolName',       '0018,1030'], $
        ['SliceThickness',     '0018,0050'], $
        ['RepetitionTime',     '0018,0080'], $
        ['EchoTime',           '0018,0081'], $
        ['NumberOfAverages',   '0018,0083'], $
        ['ImagingFrequency',   '0018,0084'], $
        ['SpacingBtwSlices',   '0018,0088'], $
        ['PhaseEncodingSteps', '0018,0089'], $
        ['EchoTrainLength',    '0018,0091'], $
        ['PercentSampling',    '0018,0093'], $
        ['PercentPhaseFOV',    '0018,0094'], $
        ['PixelBandwidth',     '0018,0095'], $
        ['SoftwareVersion',    '0018,1020'], $
        ['AcquisitionMatrix',  '0018,1310'], $
        ['PhaseEncodeDir',     '0018,1312'], $
        ['FlipAngle',          '0018,1314'], $
        ['PatientPosition',    '0018,5100'], $
        ['SeriesInstanceUID',  '0020,000E'], $
        ['SeriesNumber',       '0020,0011'], $
        ['AcquisitionNumber',  '0020,0012'], $
        ['ImageNumber',        '0020,0013'], $
        ['ImagePosition',      '0020,0032'], $
        ['ImageOrientation',   '0020,0037'], $
        ['FrameOfReferenceUID','0020,0052'], $
        ['SamplesPerPixel',    '0028,0002'], $
        ['Rows',               '0028,0010'], $
        ['Columns',            '0028,0011'], $
        ['PixelSpacing',       '0028,0030'], $
        ['BitsAllocated',      '0028,0100'], $
        ['BitsStored',         '0028,0101'], $
        ['WindowCenter',       '0028,1050'], $
        ['WindowWidth',        '0028,1051'], $
        ['RescaleIntercept',   '0028,1052'], $
        ['RescaleSlope',       '0028,1053']  $
    ]
    ntags = (size(tags))[2]
    return, tags
end

; some Philips proprietary tags
function dcmdict_manuf, manuf 
    PM = [ $
        ['Diff_B_Factor',      '2001,1003'], $
        ['Diff_Direction',     '2001,1004'], $
        ['EPIfactor',          '2001,1013'], $
        ['Techique',           '2001,1020'], $
        ['WaterFatShiftPix',   '2001,1022'], $
        ['Angulation_AP',      '2005,1071'], $
        ['Angulation_FH',      '2005,1072'], $
        ['Angulation_RL',      '2005,1073'], $
        ['FOV_AP',             '2005,1074'], $
        ['FOV_FH',             '2005,1075'], $
        ['FOV_RL',             '2005,1076'], $
        ['Offcenter_AP',       '2005,1078'], $
        ['Offcenter_FH',       '2005,1079'], $
        ['Offcenter_RL',       '2005,107A']  $
    ]
end

; convert pointer array to array if possible
function ptrarr2arr, pa 
    on_error,2
    if ptr_valid(pa[0]) eq 0 then $
        message, "input must be valid pointer"
    np = n_elements(pa)
    if np eq 1 then return, *pa[0]
    ty = size(*pa[0],/type)
    dm = size(*pa[0],/dimension)
    nel0 = product(dm)
    nel0 = (nel0 eq 0) ? 1 : nel0 ; in case pointed values are not arrays
    out = make_array(dimension=[nel0,np],type=ty,/nozero)
    out[*,0] = *pa[0]
    for i=1,np-1 do begin
        nel = product(size(*pa[i],/dimension))
        nel = (nel eq 0) ? 1 : nel
        if nel ne nel0 then $
            message, "pointer array with elements of different size"
        out[*,i] = *pa[i]
    endfor
    if nel0 gt 1 then out = reform(out,[dm,np],/over)
    return,reform(out,/over)
end

function getdcmtag, oDcm, tag, pointer=pointer
    on_error, 2
    if obj_valid(oDcm) eq 0 then $
        message, 'invalid dicom object'

    if size(tag,/tname) ne 'STRING' then $
        message, "invalid tag parameter"

    ; try to match with known tags
    list = dcmdict() ; list is 2xN string array ['tagname', 'gggg,eeee']
    wh = where(strlowcase(tag) eq strlowcase(list[0,*]), cn)
    if cn gt 0 then tag = list[1, wh[0]]

    ; tag as 'group,elem'
    tt = strsplit(tag, ',', /extract)
    if n_elements(tt) ne 2 then message, "bad tag format: " + tag

    ; convert group and element from hex to decimal
    grp = 0L & elm = 0L
    reads, tt[0], format='(z)', grp
    reads, tt[1], format='(z)', elm

    val = oDcm->GetValue(grp, elm, /no_copy)

    if ptr_valid(val[0]) eq 0 then message, 'failed to get value from ' + tag

    if keyword_set(pointer) then return, val

    nval = n_elements(val)
    val = ptrarr2arr(val)
    if nval gt 1 then return, val

    typ = size(val, /tname)
    if strlowcase(typ) ne 'string' then return, val

    vr = oDcm->GetVR(grp, elm)

    ; numeric types
    wh = where(vr[0] eq ['DS','FL','FD','IS','SL','UL','US'], cn)
    if cn gt 0 then begin
        try = strsplit(val[0],'\',/extract)
        if n_elements(try) eq 1 then try = try[0]
        return, double(try)
    endif

    ; date
    if vr eq 'DA' then begin
        ; old date format
        if strpos(val[0],'.') gt -1 then return, *val[0]
        date = byte(val[0])
        sep = byte('.')
        return, string([date[0:3],sep,date[4:5],sep,date[6:7]])
    endif

    ; hhmmss.frac -> hh:mm:ss.frac
    if vr eq 'TM' then begin
        time = byte(val[0])
        sep = byte(':')
        time = [time[0:1],sep,time[2:3],sep,time[4:*]]
        return, string(time)
    endif

    ; else return as it is
    return, strtrim(val)
end

