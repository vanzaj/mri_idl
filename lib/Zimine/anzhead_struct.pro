;+
; NAME:
;   ANZHEAD_STRUCT
;
; PURPOSE:  
;   Create a structure of ANALYZE 7.5 file header
;   Essentially one-to-one mapping to C-struct
;
; USAGE: 
;   result = ANZHEAD_STRUCT()
;
; HISTORY:
;   Author: Ivan Zimine <izimine@gmail.com> (ivz)
;
;   1999-01-13 ivz Original
;   2000-08-05 ivz Added more defaults (from MEDx FAQ)
;-
; Copyright (C) 1999, 2000, Ivan Zimine
; This software is provided as is without any warranty whatsoever. 
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

FUNCTION AnzHead_Struct     
    Long  = 0L
    short = 0
    char  = 0b
    str3  = bytarr(3)
    str4  = bytarr(4)
    str8  = bytarr(8)
    str10 = bytarr(10)
    str18 = bytarr(18)
    str24 = bytarr(24)
    str80 = bytarr(80)

    header_key = {                  $    ;/* header_key*/      
                                    $    ;/* off + size*/
        sizeof_hdr    :   Long,     $    ;/* 0 + 4     */
        data_type     :   str10,    $    ;/* 4 + 10    */
        db_name       :   str18,    $    ;/* 14 + 18   */
        extents       :   Long,     $    ;/* 32 + 4    */
        session_error :   short,    $    ;/* 36 + 2    */
        regular       :   char,     $    ;/* 38 + 1    */
        hkey_un0      :   char      $    ;/* 39 + 1    */
    }


    image_dimension = {             $    ;/* image_dimension */
                                    $    ;/* off + size*/
        dim           :   INTARR(8),$    ;/* 0 + 16    */
        vox_units     :   str4,     $    ;/* 16 + 4    */
        cal_units     :   str8,     $    ;/* 20 + 8    */
        unused1       :   short,    $    ;/* 28 + 2    */
        datatype      :   short,    $    ;/* 30 + 2    */
                          ; 0      /*Unknown data type*/ 
                          ; 1      /*Binary (1 bit per voxel)*/ 
                          ; 2      /*Unsigned character (8 bits per voxel)*/ 
                          ; 4      /*Signed short (16 bits per voxel)*/ 
                          ; 8      /*Signed integer (32 bits per voxel)*/ 
                          ;16      /*Floating point (32 bits per voxel)*/ 
                          ;32      /*Complex (64 bits per voxel ; 2 floating point numbers) 
                          ;64      /*Double precision (64 bits per voxel)*/ 
        bitpix        :   short,    $    ;/* 32 + 2    */
                          ; 1,8,16,32 or 32
        dim_un0       :   short,    $       ;/* 34 + 2    */
        pixdim        :   FLTARR(8),$    ;/* 36 + 32   */ 
                          ;voxel dimensions (note: starts from 1)
                          ;pixdim[1] - voxel width
                          ;pixdim[2] - voxel height
                          ;pixdim[3] - slice thickness + gap
                          ;..etc
        vox_offset    :   0.0,      $    ;/* 68 + 4    */
        funused1      :   1.0,      $    ;/* 72 + 4    */
                          ; SPM scale factor (int -> float conversion)
        funused2      :   0.0,      $    ;/* 76 + 4    */
        funused3      :   0.0,      $    ;/* 80 + 4    */
        cal_max       :   1.0,      $    ;/* 84 + 4    */
        cal_min       :   1.0,      $    ;/* 88 + 4    */
        compressed    :   Long,     $    ;/* 92 + 4    */
        verified      :   Long,     $    ;/* 96 + 4    */
        glmax         :   Long,     $    ;/* 100 + 4   */
        glmin         :   Long      $    ;/* 104 + 4   */
    }    

    data_history =  {               $    ;/* data_history */
                                    $    ;/* off + size*/
        descrip       :   str80,    $    ;/* 0 + 80    */
        aux_file      :   str24,    $    ;/* 80 + 24   */
        orient        :   char,     $    ;/* 104 + 1   */
                        ; 0   transverse unflipped 
                        ; 1   coronal unflipped 
                        ; 2   sagittal unflipped 
                        ; 3   transverse flipped 
                        ; 4   coronal flipped 
                        ; 5   sagittal flipped 
        originator    :   str10,    $    ;/* 105 + 10  */
                        ; SPM99 and SPM2 abuses first 6 bytes
                        ; to store image origin as 3 short int's
        generated     :   str10,    $    ;/* 115 + 10  */
        scannum       :   str10,    $    ;/* 125 + 10  */
        patient_id    :   str10,    $    ;/* 135 + 10  */
        exp_date      :   str10,    $    ;/* 145 + 10  */
        exp_time      :   str10,    $    ;/* 155 + 10  */
        hist_un0      :   str3,     $    ;/* 165 + 3   */
        views         :   Long,     $    ;/* 168 + 4   */
        vols_added    :   Long,     $    ;/* 172 + 4   */
        start_field   :   Long,     $    ;/* 176 + 4   */
        field_skip    :   Long,     $    ;/* 180 + 4   */
        omax          :   Long,     $    ;/* 184 + 4   */
        omin          :   Long,     $    ;/* 188 + 4   */
        smax          :   Long,     $    ;/* 192 + 4   */
        smin          :   Long      $    ;/* 196 + 4   */
    }  


   dsr = {                                 $    ;/* dsr       */
                                           $    ;/* off + size*/
        hk            :   header_key,      $    ;/* 0 + 40    */
        dime          :   image_dimension, $    ;/* 40 + 108  */
        hist          :   data_history     $    ;/* 148 + 200 */
                                ;/* total=348 */
   }

   dsr.hk.sizeof_hdr = 348
   dsr.hk.extents = 16384
   dsr.hk.session_error = 0
   dsr.hk.regular = byte('r')
   dsr.hk.hkey_un0 = 0b

   dsr.dime.dim[0] = 4
   dsr.dime.dim[4] = 1
   dsr.dime.vox_units = byte('mm  ')
   dsr.dime.cal_units = byte('pixel')
   dsr.dime.pixdim[0] = 4
   dsr.dime.pixdim[4] = 0
   dsr.dime.funused1 = 1.0
   dsr.dime.vox_offset = 0.0

   dsr.hist.orient = 0b
 
   RETURN, dsr
END
