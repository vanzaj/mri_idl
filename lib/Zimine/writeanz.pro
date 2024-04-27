;+
; NAME:
;   WRITEANZ
;
; PURPOSE:
;   Procedure to write an image in Analyze 7.5 format
;
; USAGE: 
;   WriteAnz, image, "anzfile.img" [, VOXEL=voxel ]
;     image - 2,3,4 dimensional array
;     VOXEL - 3-el array with voxel size in mm
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   1999-01-13 ivz Original
;   2000-06-24 ivz changed dbh_struct() to anzhead_struct()
;                  added basename() to handle file suffix
;   2000-08-05 ivz corrected dim[0] and pixdim[0]
;                  set cal_max, cal_min
;                  set db_name
;   2001-03-16 ivz added support for LONG and FLOAT data types
;   2006-03-21 ivz added EXP_DATE, EXP_TIME
;-
; Copyright (C) 1999-2001, 2006, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
; -


Pro WriteAnz, image, aFile, VOXEL=voxel, TR=tr, DESC=desc, SCALE=scale, $
    EXP_DATE=exp_date, EXP_TIME=exp_time, OVERWRITE=overwrite, ORIENT=ori, $
    ORIGIN=orig, ZIP=zip, HELP=help

  On_Error, 2

  IF N_Params() NE 2 or Keyword_set(help) THEN BEGIN
    print, "Usage: WriteAnz, image, filename, VOXEL=voxel, TR=TR, "
    print, "    ORIENT=ori, SCALE=scale, ORIGIN=orig,"
    print, "    DESC=desc, EXP_DATE=exp_date, EXP_TIME=exp_time, /OVERWRITE"
    print, ""
    print, "    ORIENT should be an integer between 0 and 5"
    print, "    SCALE and ORIGIN are SPM modifs"
    print, "    DESC, EXP_DATE, EXP_TIME are free format strings (80, 10, 10 chars saved)"
    Return
  ENDIF

  sep = path_sep()
  file = file_basename(aFile,".img")
  file = file_basename(file,".hdr")

  file = file_dirname(aFile) + sep + file

  compress = keyword_set(zip) ? 1 : 0

  iFile = file + '.img'
  if compress then iFile = file + '.img.gz'
  hFile = file + '.hdr'

  IF NOT Keyword_Set(overwrite) THEN BEGIN
      IF File_Test(iFile) OR File_Test(hFile) THEN BEGIN
        Print, hFile+" exists."
        ans = "n"
        Read, "Overwrite ? (y/n): ", ans
        IF StrLowCase(ans) EQ "n" THEN Return
      ENDIF
  end

  dims = Size(image) ; image dimentions
  pixdims = FltArr(4) ; pixel dimentions

  IF (dims[0] LT 2) OR (dims[0] GT 4) THEN BEGIN
    Print, "First parameter must be 2, 3, or 4 dimensional array"
    Return
  ENDIF

  type = dims[N_Elements(dims)-2] ; image data type (IDL)
  type = Byte(type)

  if (type lt 1) or (type gt 4) then begin
      Print, "Unsupported data type: ", size(image, /tname)
      Return
  endif

  IF N_Elements(voxel) EQ 0 THEN voxel = [1.0, 1.0, 1.0] ;
  IF N_Elements(tr) EQ 0 then tr = 1.0
  IF N_Elements(desc) EQ 0 THEN desc = 'nothing provided'
  IF N_Elements(scale) EQ 0 THEN scale = 1.0
  IF N_Elements(orig) EQ 0 THEN orig = fix([0,0,0])
  IF N_Elements(orig) EQ 2 THEN orig = [orig,0]
  IF N_Elements(orig) NE 3 THEN BEGIN
      Message, "Invalid origin... reset to (0,0,0)", /cont
      orig = fix([0,0,0])
  ENDIF

  voxel = Float(voxel)
  tr = abs(float(tr))
  scale = abs(scale)

  head = anzhead_struct()

  head.hk.db_name = byte(string(hFile, format='(A18)'))

  pixdim = [4.0, voxel]

  ;; pixel size
  head.dime.pixdim[0:3] = pixdim

  ; temporal resolution
  head.dime.pixdim[4] = tr

  ; scale factor
  head.dime.funused1 = scale

  if StrLen(desc) gt 80 then desc = String(format='(A80)', desc)
  head.hist.descrip = byte(desc)
  if N_Elements(exp_date) gt 0 then begin
      exp_date = String(format='(A10)', exp_date)
      head.hist.exp_date = byte(exp_date)
  endif
  if N_Elements(exp_time) gt 0 then begin
      exp_time = String(format='(A10)', exp_time)
      head.hist.exp_time = byte(exp_time)
  endif

  ;; image dimention
  CASE dims[0] OF
     2: head.dime.dim[0:4] = [4, dims[1:2], 1, 1]
     3: head.dime.dim[0:4] = [4, dims[1:3], 1]
     4: head.dime.dim[0:4] = dims[0:4]
     ELSE : BEGIN
        Message, "Nb. of dimensions too big: "+StrTrim(dim[0], 2), /continue
        Return
     ENDELSE
  ENDCASE

  ; this just works :-)
  head.dime.datatype = 2^type
  head.dime.bitpix = (2^type*4) < 32

  imax = Max(image, MIN=imin)
  head.dime.glmin = LONG(imin)
  head.dime.glmax = LONG(imax)
  head.dime.cal_max = float(imax)
  head.dime.cal_min = float(imin)

  ; origin (ala spm... save int into 2 bytes)
  oo = bytarr(6)
  oo[[0,2,4]] = fix(orig) / 256
  oo[[1,3,5]] = fix(orig) mod 256
  head.hist.originator[0:5] = oo

  ;image orientation (default transverse unflipped)
  if n_elements(ori) eq 0 then ori=0b
  if abs(ori) gt 6 then Message, "unknown orientation"
  head.hist.orient = byte(abs(ori))

  OpenW, hlun, hFile, /GET_LUN
  OpenW, ilun, iFile, /GET_LUN, COMPRESS=compress
  WriteU, hlun, head
  WriteU, ilun, image
  Free_lun, hlun, ilun

END

