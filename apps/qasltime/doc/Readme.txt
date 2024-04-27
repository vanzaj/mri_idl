A little help on implementation structure.


../src/qaslcalc__define.pro    ASL data analysis
../src/qasltime.pro            GUI app
../src/make.pro                compile and test


--------
qaslcalc
--------
  class = { QASLCALC, $
    name     : "",         $ ; object name 
    errObject: obj_new(),  $ ; error handling object
    errMethod: "",         $ ; error handling method
    hash_null: 0L,         $ ; hash null value
    info     : obj_new(),  $ ; hashtable contains PAR header 
    data     : obj_new(),  $ ; hash with pointers to data & results  
    extra    : ptr_new()   $ ; utility pointer
  }

ret = calc->loadData(file)
ret = calc->redoMeanVol(phase=ph, /crushed)
ret = calc->redoMask(noise=0.15) 
icr = calc->getImgIndex(non_crushed=incr, lowFa=iL)
ret = calc->redoSimpleAverage() 
ret = calc->redoSubtraction() 

; data access
ptr = calc->getData(tag)
; where tag one of: 'TIarr','QUASAR', 'meanVol', 'maskVol'
;        meanCrushLbl, meanCrushCtl, meanNonCrushLbl, meanNonCrushCtl
;        meanLowFaLbl, meanLowFaCtl, deltaMcr, deltaMncr

  ptr = dat->get('QUASAR')
  ptr = dat->get('TIarr')
  ptr = dat->get('meanVol') 
  ptr = dat->get('maskVol')
  ptr = dat->get('meanCrushLbl')
  ptr = dat->get('meanCrushCtl')



--------
qasltime
--------

GUI unames
	base/tlb
	file/open, file/save, file/exit
	base/main ; global container
	base/left, base/right  ; display and controls containers
	draw/image, draw/plot
	drop/imageType
	slider/slice, slider/phase, slider/dyn
	laber/statusbar

qasltime obj
  gui = { qaslTimeGUI, $ ; internal variables
    winID: [0L,0L], $ ; image and plot window IDs
    drawMaxSize: 0,$ ; maximum allowed draw window X size
    drawXsize: 0,  $ ; draw widget X size     
    ctlXsize:  0,  $ ; controls base X size (needed for app resizing)
    imgType: '', $ ; current image type
    imgSize: [0,0], $ ; image [xsize, ysize]
    imgRange: [0.0, 0.0], $ ; image [min, max]
    slice:  0, $ ; current slice
    phase:  0, $ ; current phase
    dyn:    0, $ ; current dynamic
    mouseX: 0, $ ; mouse click X-coord (device) 
    mouseY: 0, $ ; mouse click Y-coord (device)
    tlb:    0L $
  }

  obj = { qaslTime    ,$
    version: ''       ,$ ; prog version
    title:   ''       ,$ ; GUI title
    running: 0b       ,$ ; running flag
    debug:   0b       ,$ ; debub flag
    gui:     gui      ,$ ; gui internal variables
    calc:    obj_new(),$ ; calc object
    hash:    obj_new() $ ; hashtable object for storing 'randomly' useful data 
  }

