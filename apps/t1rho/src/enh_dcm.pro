; testing Philips enhanced dicom

pt = "/export/data/pride_jp/t1rho/Bos_enhanced_dcm/SL01"
fn = filepath("IM_0082", root_dir=pt)

dcm = obj_new('IDLffDicom')
r = dcm->read(fn)
v = dcm->getValue('7FE0'x,'0010'x, /no_copy)

ima = intarr(256,256,20)
for j=0,19 do ima[*,*,j] = *(v[j])

;obj_destroy, dcm
end

