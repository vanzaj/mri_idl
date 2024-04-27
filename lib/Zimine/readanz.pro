;+
; NAME:
;   READANZ
;
; PURPOSE:
;   Function to read image data in ANALYZE 7.5 format
;   returns an array with the image or -1 if error
;
; USAGE: 
;   img = ReadAnz(anzfile, [START=indx, NUM=nb_frames,
;                                SLICE=slice_num, ENDIAN=endian ])
;     anzfile - ANALYZE file name (w/o extension)
;     START  - start index (1 based) for reading multiframe data
;     NUM    - number of frames to read
;     SLICE  - slice nb. to read (1 based)
;     ENDIAN - 'little' or 'big' endianness of input data
;
; EXAMPLE:
;   img = readanz('brain3D') 
;     will read all data from brain3D.img taking data pars from brain3D.hdr
;
;   img = readanz('all_epi', start=2, num=20, slice=5)
;     will read from all_epi.img from index 1 to 20 and only slice 4
;
; DEPENDENCY: 
;   anzhead_struct.pro
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   1999-01-13 ivz Original
;   1999-03-31 ivz Error handling with Message
;   1999-09-28 ivz added SLICE keyword
;   2000-06-22 ivz changed START and NUM keywords
;   2000-07-24 ivz changed dbh_struct to anzhead_struct
;                  automatic endianness handling
;   2001-03-16 ivz added LONG and FLOAT types
;                  added memory checking (to avoid reading too much)
;   2005-08-18 ivz added ZIP keyword (! header should not be compressed)
;                        (image file size not checked using image dims in header)
;   2008-09-06 ivz removed zip, guess from file names
;-
; Copyright (C) 1999-2001, 2005, 2008 Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;- 

function ReadAnz, inFile, START=start, NUM=num, SLICE=slice, HDR=head, HELP=help
  ;; inFile = analyze file name with/without extention 
  ;; START = index of the first volume to be read (1 based)
  ;; NUM = nb of frames to read
  ;; SLICE = slice number in the volume to be read (1 based)

  On_Error, 2

  if Keyword_Set(help) then begin
    print, "USAGE: img = ReadAnz(anzfile, [START=indx, NUM=nb_frames, SLICE=slice_num])"
    print, "  anzfile - ANALYZE file name (w/o extension)"
    print, "  START  - start index (1 based) for reading multiframe data"
    print, "  NUM    - number of frames to read"
    print, "  SLICE  - slice nb. to read (1 based)"
    return, -1
  endif

  if N_Elements(inFile) eq 0 then inFile = Dialog_PickFile(filt="*.hdr")

  File = inFile

  sep = path_sep()

;  compressed = keyword_set(zip) ? 1b : 0b
;  if compressed then begin
;    hext = '.hdr' ; some troubles reading compressed header
;    iext = '.img.gz'
;  endif else begin
;    hext = '.hdr'
;    iext = '.img'
;  endelse
 
  hext = '.hdr'
  iext = '.img'
  slen = strlen(file)
  if (strpos(file, '.img.gz') gt 0) then begin
    file = strmid(file, 0, slen-7)
    iext = '.img.gz'
    compressed=1b
  endif else if (strpos(file, '.img') gt 0) then begin
    file = strmid(file, 0, slen-4)
    iext = '.img'
    compressed = 0b
  endif else if (strpos(file, '.hdr') gt 0) then begin
    file = strmid(file, 0, slen-4)
    if file_test(file+'.img.gz') then begin
      iext = '.img.gz'
      compressed = 1b
    endif else begin
      iext = '.img'
      compressed = 0b
    endelse
  endif 

  ;; get rid of extension if any
;   file = file_basename(file, iext)
;   file = file_basename(file, hext)
;   file = file_dirname(inFile) + sep + file   

  ;; check if .img file exists
  if (File_Search(file+iext))[0] eq '' then begin 
     Print, "Couldn't find "+file+ iext +" file"
     return, -1
  endif
  ;; check if .hdr file exists
  if (File_Search(file+hext))[0] eq '' then begin 
     Print, "Couldn't find "+ file+ hext +" file"
     return, -1
  endif
  
  head = anzhead_struct()

  OpenR, lun, file+hext, /get_lun
  fs = FStat(lun)
  if fs.size ne 348 then begin
     Message, "Incorrect header file size", /continue
     Free_lun, lun
     Return, -1
  endif 
  Readu, lun, head

  Free_lun, lun

  swp = 0
  ; sizeof_hdr is used as a magic number
  if head.hk.sizeof_hdr ne 348 then begin
    head = swap_endian(head)
    swp = 1
  endif

  ; stop if swapping doesn't help
  if head.hk.sizeof_hdr ne 348 then begin
    Message,"Incorrect sizeof_hdr field in the header", /continue
    Return, -1
  endif

  ; Info header
  dimx = head.dime.dim[1]
  dimy = head.dime.dim[2]
  dimz = head.dime.dim[3]
  frames = head.dime.dim[4]
  if frames eq 0 then frames = 1 ; may be not necessairy
  bpp = head.dime.bitpix
  glmax = head.dime.glmax

  case bpp of
      8: type = 1 ;; IDL byte
     16: type = (glmax GT (2L^15-1)) ? 12 : 2 ;; UINT or FIX
     32: begin
       if head.dime.datatype eq 8 then $
           type = 3 $ ; IDL long
       else $
           type = 4 ; IDL float
     end
     else: begin 
        Message, "Data must have Byte|Integer|Long|Float type", /continue
        Return, -1
     endelse
  endcase

  ; default START and NUM
  if N_Elements(start) eq 0 then start = 1
  if N_Elements(num) eq 0 then num = frames - start + 1
  if N_Elements(slice) eq 0 then slice = -1 ; read the whole volume

  ; check START and NUM
  if (start lt 1) or (start gt frames) then begin 
     Message, "Incorrect start index", /continue
     Return, -1
  endif 
  if (start + num gt frames+1) then begin 
     Message, "Incorrect nb. of volumes to read", /continue
     Return, -1
  endif 
  ;; check SLICE
  if slice gt dimz then begin 
     Message, "Incorrect slice number", /continue
     Return, -1
  endif 

  ; check memory
  mem_limit = 200; MB
  if slice eq -1 then mem = long(dimx) * dimy * dimz * num * bpp/8 $
  else mem = long(dimx) * dimy * num * bpp/8
  mem = mem / (1024l*1024) ; bytes -> MegaBytes
  if mem gt mem_limit then begin
      print, "You are about to read ", strtrim(mem,2), " MB of data"
      ans = ""
      read, "Continue ? (y/n): ", ans
      if StrUpCase(ans) ne 'Y' then $
          return, -1
  endif

  if slice eq -1 then $
      imgs = Make_Array(dimx, dimy, dimz, num, type=type) $
  else imgs = Make_Array(dimx, dimy, num, type=type)
  
  OpenR, lun, File+iext, /get_lun, swap_endian=swp, compress=compressed
  
  finfo = fstat(lun) ; get real image file size (bytes)
  fsize = long(dimx) * dimy * dimz * frames * bpp/8 

  if not compressed then begin
    if fsize ne finfo.size then begin
       Message, "Image dimensions do not match actual file size", /continue
       Free_lun, lun
       Return, -1
    endif   
  endif

  vol = Assoc(lun, Make_Array(dimx, dimy, dimz, type=type))

  j = 0
  for i=start-1, start+num-2 do begin 
     tmp = vol[i]
     if slice eq -1 then imgs[*, *, *, j] = tmp $
     else imgs[*, *, j] = tmp[*, *, slice - 1]
     j = j+1
  endfor 

  Free_Lun, lun

  return, imgs

end

