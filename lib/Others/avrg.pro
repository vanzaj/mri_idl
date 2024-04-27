function avrg, array, dim, VARIANCE=var, DOUBLE=dbl
;+
; NAME:
;       AVRG
; PURPOSE:
;       Average an array over specified dimension
; EXPLANATION:
;       Calculate the average value of an array, or calculate the average
;       value over one dimension of an array as a function of all the other
;       dimensions.
;
; CALLING SEQUENCE:
;       RESULT = AVRG( ARRAY, [ DIMENSION, STD=std, /DOUBLE ] )
;
; INPUTS:
;       ARRAY = Input array.  May be any type except string.
;
; OPTIONAL INPUT PARAMETERS:
;       DIMENSION = Optional dimension to do average over, scalar
;
; OUTPUTS:
;       The average value of the array when called with one parameter.
;
;       If DIMENSION is passed, then the result is an array with all the
;       dimensions of the input array except for the dimension specified,
;       each element of which is the average of the corresponding vector
;       in the input array.
;
;       Standard Deviation can be optionally saved via STD keyword
;
; RESTRICTIONS:
;       Dimension specified must be valid for the array passed; otherwise the
;       input array is returned as the output array.
; PROCEDURE:
;       AVG(ARRAY) = TOTAL(ARRAY)/N_ELEMENTS(ARRAY) when called with one
;       parameter.
; MODIFICATION HISTORY:
;       William Thompson        Applied Research Corporation
;       July, 1986              8201 Corporate Drive
;                               Landover, MD  20785
;       Converted to Version 2      July, 1990
;       Replace SUM call with TOTAL    W. Landsman    May, 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       22 Oct 2000 I. Zimine, added VARIANCE keyword
;-

On_Error,2

s = Size(array)

if s[0] eq 0 then Message,'Parameter must be an array'

dbl = Keyword_Set(dbl)

if N_Params() eq 1 then begin

    npts = Float(N_Elements(array))
    avg = Total(array, DOUBLE=dbl) / npts
    if Arg_Present(var) then begin
        if size(array, /tname) ne "DOUBLE" then $
            var = (Total(float(array)^2, DOUBLE=dbl) - npts*avg^2)/(npts-1)$
        else $
            var = (Total(array^2) - npts*avg^2)/(npts-1)
    endif
    
end else begin

    if ((dim ge 1) and (dim le S[0])) then begin
        npts = Float(S[dim])
        avg = Total(array,dim, double=dbl) / npts
        if Arg_Present(var) then begin
            if size(array, /type) lt 4 or size(array,/type) gt 6 then $
                ; need to cast array because of possible overflow when squaring
                var = (Total(float(array)^2, dim, DOUBLE=dbl) - npts*avg^2)/(npts-1) $
            else $
                var = (Total(array^2, dim) - npts*avg^2)/(npts-1) 
        endif
        
   end else $
      Message,'*** Dimension out of range'
endelse

Return, avg
end
