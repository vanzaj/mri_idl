;+
; NAME: ph_ImgReg__define
;
;   Processing object to perform image registions using AIR.
;   Requires references to two valid slice_stack objects to be registered.
;
; METHODS:
;
;   obj -> SetAirParameters, model=?, costf=?, t1=?, t2=?,...
;   obj -> align, ref_stack=stack1, tst_stack=stack2
;   stack3 = obj -> reslice()
;   stack3 = obj -> reslice(img_stack = stack4) ; reslice another stack
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;   
;   2008-04-30 ivz Original
;-

; local error reporting
pro ph_ImgReg::ErrMsg, err, catch_cancel=catch_cancel
  compile_opt hidden
  if n_elements(err) eq 0 then $
    help, /last_message, output=err
  if keyword_set(catch_cancel) then catch,/cancel
  if n_elements(err) eq 0 then err = !error_state.msg
  ok = dialog_message(err)
  return
end

pro ph_ImgReg::Align, ref_stack=ref_stack, tst_stack=tst_stack, status=output
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return
  endif

  if obj_valid_isa(ref_stack, 'slice_stack') then self.stack_ref = ref_stack
  if obj_valid_isa(tst_stack, 'slice_stack') then self.stack_tst = tst_stack

  self->DeleteTmpFiles
  output=''
  self->WriteTmpFile, /ref_img, /tst_img, status=output
  if strlen(output) gt 0 then $
    message, output

; align SPECT onto MRI

;opts = "-m 6 -b1 2.0 2.0 2.0 -p1 256 -p2 0 -t1 65 -t2 500 -x 1 -c 0.0001 -g transform.txt y"
;jnk = [align_bin, file_brain, file_spect, file_airmat, opts]
;align_cmd = strjoin(jnk, ' ')
;spawn, align_cmd, output1

  opt = ' -m ' + self.opt_model
  opt+= ' -x ' + self.opt_costf
  opt+= ' -c ' + self.opt_convth
  opt+= ' -t1 ' + self.opt_t1
  opt+= ' -t2 ' + self.opt_t2
  if strtrim(self.opt_b1,2) NE '' then $
    opt += ' -b1 ' + self.opt_b1
  if strtrim(self.opt_b2,2) NE '' then $
    opt += ' -b2 ' + self.opt_b2
  if strtrim(self.opt_p1,2) NE '' then $
    opt += ' -p1 ' + self.opt_p1
  if strtrim(self.opt_p2,2) NE '' then $
    opt += ' -p2 ' + self.opt_p2

  cd, self.dir_tmp
  cmd = [self.cmd_align, self.file_ref, self.file_tst, self.file_air, opt]
  cmd = strjoin(cmd, ' ')
  ;cd, current=here
  ;print, "dir: "+here
  ;print, cmd
  spawn, cmd, out, err
  output = out[0]
  stderr = err
  if file_test(self.file_air) EQ 0 then $
    output = "align failed to produce .air file"
  cd, self.dir_old
end

function ph_ImgReg::Reslice, img_stack=img_stack, new_name=new_name, status=output
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return, obj_new()
  endif

  ; do we have a new stack?
  if obj_valid_isa(img_stack, 'slice_stack') NE 0 then begin
    self.stack_tst = img_stack
    self->WriteTmpFile, /tst_img
  endif

  cd, self.dir_tmp
  if file_test(self.file_tst+'.hdr', /read) EQ 0 then $
    message, 'Test image files not available'

  opt = '-o -k -n ' + self.opt_rmode
  cmd = [self.cmd_reslice, self.file_air, self.file_res, opt]
  cmd = strjoin(cmd, ' ')
  spawn, cmd, out, err
  output = out
  stderr = err

  cd, self.dir_old
  res = self->ReadResFile(new_name=new_name)
  return, res
end

function ph_ImgReg::ManualAir, trans=trans, rota=rota, $; scale=scale$
  status=output, new_name=new_name
    
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    self.file_air = file_air_orig
    cd, self.dir_old
    return, obj_new()
  endif

  file_air_orig = self.file_air
  self.file_air = 'manual.air'
  self->_dotair_write, self.file_air, trans=trans, rota=rota

  result = self->reslice(status=output, new_name=new_name)
  self.file_air = file_air_orig
  return, result
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TmpFiles

function ph_ImgReg::RandomName, prefix=prefix, extension=extension
; based on code by Mark Hadfield
; (posted on comp.lang.idl-pvwave)
  compile_opt hidden

  if ptr_valid(self._seed) then begin
    seed = *(self._seed)
    ; 8-char random string
    rnd = StrTrim(String((randomu(seed,1))[0]*2L^30, FORMAT='(Z)'), 2)
    *(self._seed) = seed
  endif else begin
    rnd = StrTrim(String((randomu(seed,1))[0]*2L^30, FORMAT='(Z)'), 2)
    self._seed = ptr_new(seed, /no_copy)
  endelse

  if n_elements(prefix) gt 0 then pref=strtrim(prefix[0],2) $
  else pref=''
  if n_elements(extension) gt 0 then ext=strtrim(extension[0],2) $
  else ext=''

  return, pref+rnd+ext
end

function ph_ImgReg::ReadResFile, new_name=new_name
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return, res
  endif

  cd, self.dir_tmp
  res=obj_new()

  if file_test(self.file_res+'.hdr', /read) eq 0 then $
    message, 'Result files not available'

  self.stack_tst -> GetProperty, imin=tst_imin, imax=tst_imax, name=name
  p = self.stack_tst->data(/pointer)
  dtype = size(*p, /Tname)

  scale = double(self.int_max)/(tst_imax-tst_imin)
  if scale EQ 0.0 then $
    message, 'zero scale, returning'

  ; dump is 16-bit int
  dump = readanz(self.file_res, hdr=hdr)
  dump_max = max(dump)
  ; correct stupid factor of 2 
  if double(dump_max)/self.int_max LT 0.8 then $
      dump = uint(dump)*2
  vox = hdr.dime.pixdim[1:3]

  case dtype of
    'BYTE' : data = byte(round((1./scale) * dump + tst_imin))
    'INT' : data = fix(round((1./scale) * dump + tst_imin))
    'UINT' : data = uint(round((1./scale) * dump + tst_imin))
    'LONG' : data = round((1./scale) * dump + tst_imin)
    'ULONG' : data = ulong(round((1./scale) * dump + tst_imin))
    'FLOAT' : data = float((1./scale) * dump + tst_imin)
    'DOUBLE' : data = (1./scale) * dump + tst_imin
    else : message, 'Unknown data type '+dtype
  endcase

  cd, self.dir_old
  if n_elements(new_name) NE 0 then name=new_name $
  else name = name+'_REG'
  res = obj_new('slice_stack', data, voxel=vox, name=name)
  return, res
end

pro ph_ImgReg::WriteTmpFile, ref_img = ref_img, tst_img=tst_img, status=output
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return
  endif

  cd, self.dir_tmp

  ; reference image data
  if keyword_set(ref_img) then begin
    dims = strjoin(strtrim(self.stack_ref->dim(),2), ' ')
    vox = string(self.stack_ref->voxel(), format='(%"%5.2f %5.2f %5.2f")')
    cmd = strjoin([self.cmd_makehdr, self.file_ref, '1', dims, vox], ' ')
    spawn, cmd, out, err
    output = out[0]
    data = self.stack_ref->data(min=imin, max=imax)
    scale = double(self.int_max)/(imax-imin)
    data = uint(round(scale*(data-imin)))
    openw, lun, self.file_ref+'.img', /get_lun
    writeu, lun, data
    free_lun, lun
    ; update threshold
    t1 = round(scale*(float(self.opt_t1)-imin))
    self.opt_t1 = strtrim(t1,1)
  endif

  ; test image data
  if keyword_set(tst_img) then begin
    dims = strjoin(strtrim(self.stack_tst->dim(),2), ' ')
    vox = string(self.stack_tst->voxel(), format='(%"%5.2f %5.2f %5.2f")')
    cmd = strjoin([self.cmd_makehdr, self.file_tst, '1', dims, vox], ' ')
    ;print, '<'+cmd+'>'
    spawn, cmd, out, err
    output = out[0]
    data = self.stack_tst->data(min=imin, max=imax)
    scale = double(self.int_max)/(imax-imin)
    data = uint(round(scale*(data-imin)))
    openw, lun, self.file_tst+'.img', /get_lun
    writeu, lun, data
    free_lun, lun
    ; update threshold
    t2 = round(scale*(float(self.opt_t2)-imin))
    self.opt_t2 = strtrim(t2,1)
  endif

  cd, self.dir_old
end

pro ph_ImgReg::DeleteTmpFiles
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return
  endif
  cd, self.dir_tmp
  file_delete, self.file_ref + ['.hdr', '.img'], /allow_nonexistent
  file_delete, self.file_tst + ['.hdr', '.img'], /allow_nonexistent
  file_delete, self.file_res + ['.hdr', '.img'], /allow_nonexistent
  file_delete, self.file_air, /allow_nonexistent
  file_delete, 'manual.air', /allow_nonexistent
  file_delete, 'air_transform.txt', /allow_nonexistent
  cd, self.dir_old
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; read/write .air files

; structure of .air file
function ph_ImgReg::_dotair_struct 
  compile_opt hidden

  key_info = {bits:0L,$
              x_dim:0L,y_dim:0L, z_dim:0L,$ ;image dimensions
              x_s:1.D, y_s:1.D, z_s:1.D}    ;voxel sizes 

  matr = DblArr(4,4)

  char128 = Replicate(0b, 128) ; fake char array
  char116 = Replicate(0b, 116)

  dotair =  {e      : dblarr(4,4),$
             s_file : char128,    $ ; standard image (target)
             s      : key_info,   $ 
             r_file : char128,    $ ; reslice image
             r      : key_info,   $ 
             comment: char128,    $ ; comment 
             s_hash : ulong(0),   $ ; file codes (not important)
             r_hash : ulong(0),   $ 
             s_vol  : uint(0),    $ ; not used (even in AIR)
             r_vol  : uint(0),    $ 
             reserv : char116 }; info string about convergence, iterations etc

  return, dotair
end

; read .air file and return [rotations, shifts]
function ph_ImgReg::_dotair_read, file, struct=struct
  catch, error
  if error ne 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return, 0
  endif
  
  if n_elements(file) gt 0 then airfile = file
  ; if no file provided, try to get last .air file from a call to align
  if n_elements(airfile) eq 0 then airfile = self.file_air
  if n_elements(airfile) eq 0 then message, "Please provide .air input file"
  
  struct = self->_dotair_struct()
  
  OPENR, unit, filepath(airfile, root=self.dir_tmp), /GET_LUN
  READU, unit, struct
  FREE_LUN, unit
  
  sx_dim = struct.s.x_dim ;;
  sy_dim = struct.s.y_dim ; standard image dimensions
  sz_dim = struct.s.z_dim ;;
  
  sx_size = struct.s.x_s ;;
  sy_size = struct.s.y_s ; standard image voxel sizes
  sz_size = struct.s.z_s ;;
  
  rx_dim = struct.r.x_dim ;;
  ry_dim = struct.r.y_dim ; reslice image dimensions
  rz_dim = struct.r.z_dim ;;
  
  rx_size = struct.r.x_s ;;
  ry_size = struct.r.y_s ; reslice image voxel sizes
  rz_size = struct.r.z_s ;;
  
  ssize = sx_size < sy_size < sz_size ;AIR makes cubic voxels
  rsize = rx_size < ry_size < rz_size
  psc = ssize/rsize
  
  unitm = identity(4, /double)
  Cs = unitm ;shift coords to the center of image
  Cs[3,0] = -(sx_dim-1)*(sx_size/ssize)/2.0
  Cs[3,1] = -(sy_dim-1)*(sy_size/ssize)/2.0
  Cs[3,2] = -(sz_dim-1)*(sz_size/ssize)/2.0
  
  P = unitm  ; voxel size correction between the two images
  P[0,0] = psc
  P[1,1] = psc
  P[2,2] = psc
  
  Cr = unitm ;shift coords back to left|top of the image
  Cr[3,0] = (rx_dim-1)*(rx_size/rsize)/2.0
  Cr[3,1] = (ry_dim-1)*(ry_size/rsize)/2.0
  Cr[3,2] = (rz_dim-1)*(rz_size/rsize)/2.0
  
  Zr= unitm ;makes cubic voxel in reslice image 
  Zr[0,0] = rsize/rx_size
  Zr[1,1] = rsize/ry_size
  Zr[2,2] = rsize/rz_size
  
  ;matrix that has only rotations and translations (relativ to the center)
  TR = Invert(Zr##Cr)##Transpose(struct.e)##Invert(P##Cs)
  TR = double(TR)
  ; (R = Ry*Rx*Rz) angles in radians
  pitch = asin(TR[2,1])
  if abs(cos(pitch)) le 0.001 then begin ; cos(pitch) ~ 0
      if pitch gt 0.0 then pitch = 0.5*!pi else pitch = -0.5*!pi ; force 90 deg
      roll = 0.0 ; force to 0
      yaw = atan(TR[0,2], TR[0,0])
  endif else begin
      roll = atan(TR[2,0], TR[2,2])
      yaw = atan(-1*TR[0,1], TR[1,1])
  endelse
  
  pitch = pitch*180/!pi
  roll  = roll*180/!pi
  yaw   = yaw*180/!pi
  
  x_shift = -TR[3,0]
  y_shift = -TR[3,1]
  z_shift = -TR[3,2]
  
  ; round up to 4th digit
  prec=1e4
  pitch = double(round(pitch*prec))/prec
  roll  = double(round(roll*prec))/prec
  yaw   = double(round(yaw*prec))/prec
  
  return, [pitch, roll, yaw, x_shift, y_shift, z_shift]
end

; write .air file (in/out dims and voxel size are taken from self.stack_tst/ref)
; use setProperty, stack_ref = ... to change the reslice and reference data
pro ph_ImgReg::_dotair_write, file, rota=rota, trans=trans, status=status
; only for rigid body (6 parameters model)
; rotations in deg as [pitch,roll,yaw]
; translation [x,y,z] in mm
  compile_opt hidden
  catch, error
  if error ne 0 then begin
    self->ErrMsg, /catch_cancel
    cd, self.dir_old
    return
  endif

  if n_elements(file) eq 0 then begin
    file = self->RandomName(prefix='manuair', extension='.air')
    self.file_air = file
  endif 

  if n_elements(rota) eq 0 then rota = replicate(0.0, 3)
  if n_elements(trans) eq 0 then trans = replicate(0.0, 3)

  if n_elements(rota) ne 3 then message, "Rotations must be 3-el array"
  if n_elements(trans) ne 3 then message, "Translations must be 3-el array"

  struct = self->_dotair_struct()

  dim = self.stack_ref->dim()
  vox = self.stack_ref->voxel()

  struct.s_file = byte( self.file_ref )
  struct.s.x_dim = (sx_dim = dim[0])
  struct.s.y_dim = (sy_dim = dim[1])
  struct.s.z_dim = (sz_dim = dim[2])
  struct.s.x_s = (sx_size = vox[0])
  struct.s.y_s = (sy_size = vox[1])
  struct.s.z_s = (sz_size = vox[2])

  dim = self.stack_tst->dim()
  vox = self.stack_tst->voxel()
  struct.r_file = byte( self.file_tst )
  struct.r.x_dim = (rx_dim = dim[0])
  struct.r.y_dim = (ry_dim = dim[1])
  struct.r.z_dim = (rz_dim = dim[2])
  struct.r.x_s = (rx_size = vox[0])
  struct.r.y_s = (ry_size = vox[1])
  struct.r.z_s = (rz_size = vox[2])

  struct.comment = byte('Created from IDL')
  struct.s_hash = 12345
  struct.r_hash = 12345
  struct.s_vol = 0
  struct.r_vol = 0

  ssize_min = sx_size < sy_size < sz_size ;AIR makes cubic voxels
  rsize_min = rx_size < ry_size < rz_size
  psc = ssize_min/rsize_min

  unitm = identity(4, /double)
  Cs = unitm ;shift coords to the center of image
  Cs[3,0] = -(sx_dim-1)*(sx_size/ssize_min)/2.0
  Cs[3,1] = -(sy_dim-1)*(sy_size/ssize_min)/2.0
  Cs[3,2] = -(sz_dim-1)*(sz_size/ssize_min)/2.0

  P = unitm  ; voxel size correction between the two images
  P[0,0] = psc
  P[1,1] = psc
  P[2,2] = psc

  Cr = unitm ;shift coords back to left|top of the image
  Cr[3,0] = (rx_dim-1)*(rx_size/rsize_min)/2.0
  Cr[3,1] = (ry_dim-1)*(ry_size/rsize_min)/2.0
  Cr[3,2] = (rz_dim-1)*(rz_size/rsize_min)/2.0

  Zr= unitm ;makes cubic voxel in reslice image 
  Zr[0,0] = rsize_min/rx_size
  Zr[1,1] = rsize_min/ry_size
  Zr[2,2] = rsize_min/rz_size

  ; sign chosen by trial and error ...
  translate = trans * (-1) / ssize_min
  ; bug in AIR with roll (Y) ??? (left vs right rot. matrix ???)
  angles = rota * [-1,1,-1]
  TR = sthmatrix(translate=translate, rotate=angles, order='zxy')

  struct.e = transpose(Zr##Cr##TR##P##Cs)

  cd, self.dir_tmp
  openw, lun, file, /get_lun
  writeu, lun, struct
  free_lun, lun
  cd, self.dir_old
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get/Set Properties
pro ph_ImgReg::SetAirParameters, model=model, costf=costf, convth=convth,$
    t1=t1, t2=t2, b1=b1, b2=b2, p1=p1, p2=p2, rmodel=rmode

  compile_opt hidden
  if self->air_par_ok(model) then self.opt_model = model
  if self->air_par_ok(costf) then self.opt_costf = costf
  if self->air_par_ok(convth) then self.opt_convth = convth
  if self->air_par_ok(t1) then self.opt_t1 = t1
  if self->air_par_ok(t2) then self.opt_t2 = t2
  if self->air_par_ok(b1, 3) then self.opt_b1 = b1
  if self->air_par_ok(b2, 3) then self.opt_b2 = b2
  if self->air_par_ok(p1) then self.opt_p1 = p1
  if self->air_par_ok(p2) then self.opt_p2 = p2
  if self->air_par_ok(rmode) then self.opt_rmode = rmode
end

function ph_ImgReg::air_par_ok, par, nel
; (internal) check parameter validity
; parameter should be num or string
; a string with internal spaces will be split
; possible side effect: par is converted to string
  compile_opt hidden
  catch, error
  if error NE 0 then begin
    catch, /cancel
    return, 0
  endif

  if n_elements(par) eq 0 then return, 0
  ; default nb of elements
  if n_elements(nel) eq 0 then nel = 1
  ; empty string
  if (strtrim(par,2))[0] eq '' then return, 0

  if size(par, /tname) eq 'STRING' then begin
    par = strtrim(strcompress(par[0]),2)
    try = strsplit(par, ' ,', /extract)
    if n_elements(try) gt 1 then par=try
  endif

  np = n_elements(par)
  if np NE nel then return, 0

  if np gt 1 then par = strjoin(strtrim(par,1), ' ') $
  else par = strtrim(par,1)
  return, 1
end

pro ph_ImgReg::GetProperty, stack_ref=stack_ref, stack_tst=stack_tst, $
  transform=transform, dir_air=dir_air, dir_tmp=dir_tmp

  compile_opt hidden
  stack_ref = self.stack_ref
  stack_tst = self.stack_tst
  transform = self.transform
  dir_air = self.dir_air
  dir_tmp = self.dir_tmp
end

pro ph_ImgReg::SetProperty, stack_ref=stack_ref, stack_tst=stack_tst, $
  dir_tmp=dir_tmp

  compile_opt hidden
  catch, error
  if error NE 0 then begin
    self->ErrMsg, /catch_cancel
    return
  endif

  if n_elements(dir_tmp) NE 0 then $
    if file_test(dir_tmp, /directory, /write) then self.dir_tmp = dir_tmp

  if obj_valid_isa(stack_ref, 'slice_stack') then begin
    self.stack_ref = stack_ref
    self->WriteTmpFile, /ref_img
  endif

  if obj_valid_isa(stack_tst, 'slice_stack') then begin
    if ~obj_valid(self.stack_ref) then $
      message, 'Reference stack must be assigned before Test stack'
    ref_dim = self.stack_ref->dim()
    tst_dim = stack_tst->dim()
    if (ref_dim[2] EQ 1) AND (tst_dim[2] NE 1) then $
      message, 'Multi-slice Test is incompatible with single slice Reference'
    self.stack_tst = stack_tst
    self->WriteTmpFile, /tst_img
  endif
end

pro ph_ImgReg::Cleanup
  compile_opt hidden
  if ptr_valid(self._seed) then ptr_free, self._seed
  self -> DeleteTmpFiles
end

function ph_ImgReg::init, air_dir=air_dir, tmp_dir=tmp_dir

  compile_opt hidden
  On_Error, 2

  self.transform = identity(4)
  self.int_max = 2L^16-1 ; max of unsigned 16bit integer

  cd, current=cwd
  self.dir_old = cwd
  if N_Elements(air_dir) eq 0 then air_dir=cwd
  if file_test(air_dir, /directory) EQ 0 then $
    message, "Invalid AIR directory: "+air_dir

  if N_Elements(tmp_dir) eq 0 then tmp_dir=cwd
  if file_test(tmp_dir, /directory, /write) EQ 0 then $
    message, "Invalid TMP directory: "+tmp_dir

  align_cmd = 'alignlinear'
  makehdr_cmd = 'makeaheader'
  reslice_cmd = 'reslice'
  if StrUpCase(!version.os_family) EQ 'WINDOWS' then begin
    align_cmd += ".exe"
    makehdr_cmd += ".exe"
    reslice_cmd += ".exe"
  endif

  align_cmd = filepath(align_cmd, root_dir=air_dir)
  if file_test(align_cmd, /executable) eq 0 then $
    message, "Could not find alignlinear in " + air_dir

  makehdr_cmd = filepath(makehdr_cmd, root_dir=air_dir)
  if file_test(makehdr_cmd, /executable) eq 0 then $
    message, "Could not find makeaheader in " + air_dir

  reslice_cmd = filepath(reslice_cmd, root_dir=air_dir)
  if file_test(reslice_cmd, /executable) eq 0 then $
    message, "Could not find reslice in " + air_dir

  self.opt_model = '6' ; rigid body
  self.opt_costf = '1' ; ratio image
  self.opt_convth = '0.00001'
  self.opt_t1 = '100'
  self.opt_t2 = '100'
  self.opt_b1 = ''
  self.opt_b2 = ''
  self.opt_p1 = ''
  self.opt_p2 = ''
  self.opt_rmode = '1' ; trilinear

  self.dir_air = air_dir
  self.dir_tmp = tmp_dir
  self.cmd_align = '"' + align_cmd+'"'
  self.cmd_makehdr = '"' + makehdr_cmd + '"'
  self.cmd_reslice = '"' + reslice_cmd + '"'

  rnd = self->RandomName()
  self.file_ref = 'ref_' + rnd
  self.file_tst = 'tst_' + rnd
  self.file_res = 'res_' + rnd
  self.file_air = 'tst2ref_' + rnd + '.air'
  return, 1
end

pro ph_ImgReg__define

  compile_opt hidden

  obj = {ph_IMGREG, $
    _seed      : ptr_new()   ,$ ; private var for randomu calls
    transform  : dblarr(4,4) ,$ ; current transformation matrix
    stack_ref  : obj_new()   ,$ ; reference volume (no cleanup)
    stack_tst  : obj_new()   ,$ ; test volume      (no cleanup)
    int_max    : 0L          ,$ ; intensity max (for writing analyze files)
    dir_air    : ''          ,$ ; path to AIR binaries
    dir_tmp    : ''          ,$ ; directory for temp hdr/img/air files
    dir_old    : ''          ,$ ; previous working directory
    cmd_makehdr: ''          ,$ ; makeheader
    cmd_align  : ''          ,$ ; alinglinear
    cmd_reslice: ''          ,$ ; reslice
    opt_model  : ''          ,$ ; -m  (transf model)
    opt_costf  : ''          ,$ ; -x  (cost function)
    opt_convth : ''          ,$ ; -c  (convergence threshold)
    opt_t1     : ''          ,$ ; -t1 (ref img threshold)
    opt_t2     : ''          ,$ ; -t2 (test img threshold)
    opt_b1     : ''          ,$ ; -b1 (ref img smoothing)
    opt_b2     : ''          ,$ ; -b2 (tst img smoothing)
    opt_p1     : ''          ,$ ; -p1 (ref img partitions)
    opt_p2     : ''          ,$ ; -p2 (tst img partitions)
    opt_rmode  : ''          ,$ ; -n (reslice interpolation model)
    file_ref   : ''          ,$ ; tmp filename for reference image
    file_tst   : ''          ,$ ; tmp filename for test image
    file_res   : ''          ,$ ; tmp filename for result image
    file_air   : ''           $ ; tmp file for .air file
  }
end

