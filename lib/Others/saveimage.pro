PRO SAVEIMAGE, FILE, BMP=BMP, PNG=PNG, PICT=PICT, JPEG=JPEG, TIFF=TIFF, $
  QUALITY=QUALITY, DITHER=DITHER, QUIET=QUIET

;+
; NAME:
;    SAVEIMAGE
;
; PURPOSE:
;    Save the current graphics window to an output file (GIF by default).
;
;    The output formats supported are:
;    GIF   8-bit with color table,
;    BMP   8-bit with color table,
;    PNG   8-bit with color table,
;    PICT  8-bit with color table,
;    JPEG 24-bit true color,
;    TIFF 24-bit true-color.
;
;    Any conversions necessary to convert 8-bit or 24-bit images onscreen to
;    8-bit or 24-bit output files are done automatically.
;
; CATEGORY:
;    Input/Output.
;
; CALLING SEQUENCE:
;    SAVEIMAGE, FILE
;
; INPUTS:
;    FILE     Name of the output file (GIF format by default).
;    
; OPTIONAL INPUTS:
;    None.
;	
; KEYWORD PARAMETERS:
;    BMP      Set this keyword to create BMP format (8-bit with color table).
;    PNG      Set this keyword to create PNG format (8-bit with color table).
;    PICT     Set this keyword to create PICT format (8-bit with color table).
;    JPEG     Set this keyword to create JPEG format (24-bit true color).
;    TIFF     Set this keyword to create TIFF format (24-bit true color).
;    QUALITY  If set to a named variable, specifies the quality for
;             JPEG output (default 75). Ranges from 0 ("terrible") to
;             100 ("excellent"). Smaller quality values yield higher
;             compression ratios and smaller output files.
;    DITHER   Set this keyword to dither the output image when creating 8-bit
;             GIF or BMP format (default is no dithering).
;    QUIET    Set this keyword to suppress the information message
;             (default is to print an information message).
;
; OUTPUTS:
;    None.
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    The output file is overwritten if it exists.
;
; RESTRICTIONS:
;    Requires IDL 5.1 or higher.
;
; EXAMPLE:
;
;openr, 1, filepath('hurric.dat', subdir='examples/data')
;image = bytarr(440, 340)
;readu, 1, image
;close, 1
;loadct, 13
;tvscl, image
;saveimage, 'hurric.gif'
;
; MODIFICATION HISTORY:
; Liam.Gumley@ssec.wisc.edu
; $Id: saveimage.pro,v 1.12 1999/09/09 16:08:40 gumley Exp $
;
; I. Zimine. Handle !order variable. Oct 99
;-

;-------------------------------------------------------------------------------
;- CHECK INPUT
;-------------------------------------------------------------------------------

;- Check IDL version

if float(!version.release) lt 5.1 then begin
  message, 'IDL 5.1 or higher is required', /continue
  return
endif

;- Check arguments

if n_params() ne 1 then begin
  message, 'Usage: SAVEIMAGE, FILE', /continue
  return
endif
if n_elements(file) eq 0 then message, 'Argument FILE is undefined'
if n_elements(file) gt 1 then message, 'Argument FILE must be a scalar string'

;- Check keywords

output = 'GIF'
if keyword_set(bmp)  then output = 'BMP'
if keyword_Set(png)  then output = 'PNG'
if keyword_set(pict) then output = 'PICT'
if keyword_set(jpeg) then output = 'JPEG'
if keyword_set(tiff) then output = 'TIFF'
if n_elements(quality) eq 0 then quality = 75

;- Check for window capable device with an open window, and get visual depth

if (!d.flags and 256) eq 0 then message, 'Unsupported graphics device'
if !d.window lt 0 then message, 'No graphics windows are open'
device, get_visual_depth=depth

;-------------------------------------------------------------------------------
;- GET CONTENTS OF GRAPHICS WINDOW
;-------------------------------------------------------------------------------

;- Copy the contents of the current display to a pixmap

current_window = !d.window
xsize = !d.x_size
ysize = !d.y_size
window, /free, /pixmap, xsize=xsize, ysize=ysize
device, copy=[0, 0, xsize, ysize, 0, 0, current_window]

;- Read the pixmap contents into an array

if depth gt 8 then image = tvrd(true=1) else image = tvrd()

;- Delete the pixmap

wdelete, !d.window
wset, current_window

;- Get the current color table

tvlct, r, g, b, /get

;- If an 8-bit image was read, reduce the number of colors

if depth le 8 then begin
  reduce_colors, image, index
  r = r[index]
  g = g[index]
  b = b[index]
endif            

;-------------------------------------------------------------------------------
;- WRITE OUTPUT FILE
;-------------------------------------------------------------------------------

;- Save the image in 8-bit or 24-bit format

case 1 of

  ;- Save the image in 8-bit output format
  
  (output eq 'GIF')  or (output eq 'BMP') or $
  (output eq 'PICT') or (output eq 'PNG') : begin

    if depth gt 8 then begin

      ;- Convert 24-bit image to 8-bit
    
      image = color_quan(image, 1, r, g, b, colors=256, $
        dither=keyword_set(dither))

      ;- Sort the color table from darkest to brightest
      
      table_sum = total([[long(r)], [long(g)], [long(b)]], 2)
      table_index = sort(table_sum)
      image_index = sort(table_index)
      r = r[table_index]
      g = g[table_index]
      b = b[table_index]
      oldimage = image
      image[*] = image_index[temporary(oldimage)]
      
    endif
    
    ;- Check !0rder variable
    IF !order THEN image = Reverse(Temporary(image), 2)

    ;- Save the image

    case output of
      'GIF'  : write_gif,  file, image, r, g, b
      'BMP'  : write_bmp,  file, image, r, g, b
      'PNG'  : write_png,  file, image, r, g, b
      'PICT' : write_pict, file, image, r, g, b
    endcase
    
  end
  
  ;- Save the image in 24-bit output format
  
  (output eq 'JPEG') or (output eq 'TIFF') : begin

    ;- Convert 8-bit image to 24-bit
    
    if depth le 8 then begin
      dims = size(image, /dimensions)
      nx = dims[0]
      ny = dims[1]
      true = bytarr(3, nx, ny)
      true[0, *, *] = r[image]
      true[1, *, *] = g[image]
      true[2, *, *] = b[image]
      image = temporary(true)
    endif

    ;- Check !order variable

    IF !order THEN image = reverse(temporary(image), 3)

    ;- If TIFF format output, reverse image top to bottom
    
    if output eq 'TIFF' then image = reverse(temporary(image), 3)
    
    ;- Write the image
    
    case output of
      'JPEG' : write_jpeg, file, image, true=1, quality=quality
      'TIFF' : write_tiff, file, image, 1
    endcase
    
  end
  
endcase

;- Print information for the user

if not keyword_set(quiet) then $
  print, file, output, format='("Created ",a," in ",a," format")'
  
END
