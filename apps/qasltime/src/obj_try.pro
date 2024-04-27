;
; qaslcalc tests
;

;data_path = 'G:\Images\ismrm10\good_02'
data_path = '/export/data/asl/ksh/rpi_asl_ismrm10/ASL_file/good_02'
data_file = filepath('QASL_02.PAR', root=data_path)


if ~obj_valid(calc) then begin
    calc = obj_new('qaslcalc')
    ret = calc->loadData(data_file)
    data = calc->get('data') ; data hash object
    qp = data->get('QUASAR') ; pointer to full QUASAR data set
endif

ret = calc->redoMeanVol()
ret = calc->redoMaskVol(noise=0.15)
ret = calc->redoSimpleAverage()

; check everything inside data
;tab = string(9b)
;ks = data->keys()
;for j=0, n_elements(ks)-1 do $
;  print, ks[j]+': ', tab, size(*(data->get(ks[j])), /dim)

; check deltaM at [45,24,3]
ll = reform((*qp)[45,24,3,0,*,*])
ct = reform((*qp)[45,24,3,1,*,*])
icr = calc->getimgindex()
mll = avrg(ll[[icr],*],1, var=v1)
mct = avrg(ct[[icr],*],1, var=v2)
ploterr, mct-mll, sqrt(v1+v2)

; alternative data access method
; p = calc->getData(dataTag)


;iCr = calc->getImgIndex(lowfa=ilow, non_crushed=iNCr)
;idx = indgen(7,42/7)
;; check slice #3
;;isl=4
;msk = calc->makebrainmask()
;;ii = where(msk[*,*,isl] gt 0)
;ii = where(msk gt 0)
;dat = reform(*p, [64L*64*7,2,42,13],/over)
;res = fltarr(7)s
;for i=0,6 do begin ima = avrg(dat[*,1,idx[i,*],5],3) & $
;    res[i] = mean(ima[ii]) & endfor


end
