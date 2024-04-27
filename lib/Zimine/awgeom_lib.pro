;+
; Angulations / Offcenter etc...
;
; This is NOT exactly the same as AWGEOM code
; Patient position/orientation is assumed head-first/suppine
; preparation and fat shift direction are ignored
; XYZ scanner coordinate system is NOT used
;
; Convention for slice_orientaion in PAR file
; 1 = TRA, 2 = SAG, 3 = COR
;
;
; HISTORY:
;   Author: Ivan Zimine <ivan.zimine@philips.com>
;
;   2008-10-17 ivz original
;-


; orientation of an arbitrary vector
; sign of orientation is +1 for parallel or -1 for antiparallel
; to standard positive direction

function awgeom_view_axis, vec, sign=sign
    nv = sqrt(total(vec^2))
    av = abs(vec)/nv
    if (av[1] ge av[0]) and (av[1] ge av[2]) then begin
        view = 'COR' ; P ~ y
        sign = (total(vec/nv*[0,1,0]) ge 0) ? 1 : -1
    endif else begin
        if (av[0] gt av[1]) and (av[0] ge av[2]) then begin
            view = 'SAG' ; L ~ x
            sign = (total(vec/nv*[1,0,0]) ge 0) ? 1 : -1
        endif else begin
            view = 'TRA' ; H ~ z
            sign = (total(vec/nv*[0,0,1]) ge 0) ? 1 : -1
        endelse
    endelse
    return, view
end

function awgeom_view_axis_vector, orient, negative=negative
    if n_elements(orient) eq 0 then ori = 'TRA'
    sign = keyword_set(negative) ? -1 : 1

    ; get the 1st letter
    ori = strupcase( (byte(orient))[0] )

    id = identity(3)

    ; assume TRA orientation if orient does not contain SAG or COR
    if ori eq 'S' then out = id[0,*] $
    else if ori eq 'C' then out = id[1,*] $
    else out = id[2,*]

    return, out * sign
end

; slice orientation matrix
; L= +x, P= +y, H=+z (xyz are NOT scanner coordinates)
; Tsom changes TRA slice (assumed default orientation) into COR or SAG slice
; (Tsom is not NWV -> L'P'H')
function awgeom_Tsom, ori, sgn
    if n_elements(ori) eq 0 then ori = 'TRA'
    if n_elements(sgn) eq 0 then sgn = 1
    tsom = identity(3)
    case ori of
        'COR' : tsom = [[1,0,0],[0,0,-1],[0,1,0]] ; rot_x(-90)
        'SAG' : tsom = [[0,0,-1],[-1,0,0],[0,1,0]] ; rot_y(90)rot_x(-90)
        else : ; assume TRA
    endcase
    return, float(transpose(tsom))
end

; get angulation vector (rl,ap,fh) from rotation matrix
; mat is assumed to be Trl(-a_rl) x Tap(-a_ap) x Tfh(-a_fh)
function awgeom_angulation_vec, mat
    sy = mat[2,0]
    sy = -1.0 > sy < 1.0
    ang_ap = asin(sy)
    cy = cos(ang_ap)
    ; ang_ap is close to 90 deg i.e. cos(ang_ap) =~ 0.0
    if abs(cy) lt 0.01 then begin
        ang_fh = 0.0
        if ang_ap gt 0.0 then ang_ap = 0.5*!pi else ang_ap = -0.5*!pi
        ang_rl = atan(mat[0,1], mat[0,2])
    endif else begin
        ang_rl = atan(mat[2,1], mat[2,2])
        ang_fh = atan(mat[1,0], mat[0,0])
    endelse
    ang = [ang_rl, ang_ap, ang_fh]*!radeg
    ; reset to 0 very small angles
    wh = where(abs(ang) le 1e-5, cn)
    if cn gt 0 then ang[wh] = 0.0
    return, -1*ang
end

; angulations from row, col direction cosines
function awgeom_rowcol_to_ang, row, col, view_ori=view_ori
    catch, error
    if error ne 0 then begin
        ok = error_message(/trace)
        return, float([0,0,0])
    endif

    rowcol_mat = dblarr(3,3)
    normal = crossp(double(row), col)
    rowcol_mat[0,*] = row
    rowcol_mat[1,*] = col
    rowcol_mat[2,*] = normal

    view_ori = awgeom_view_axis(normal)
    t_som = awgeom_Tsom(view_ori)
    t_ang = rowcol_mat ## transpose(t_som)
    angul = awgeom_angulation_vec(t_ang)
    return, angul
end

