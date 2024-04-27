
Ivan Zimine <izimine@gmail.com>
March 2010

This is a collection of tools that were written mostly during my PhD at Geneva
University Hospital in Switzerland (1999-2004) and post-doc at the National
Neuroscience Institute in Singapore (2004-2007). These are under BSD-lite "uti,
non abuti" license. Files without copyright/license clause were written during
my stay at Philips Healthcare in Japan (from May 2007 to March 2010).
Presumably they should be put under appropriate Philips copyright protection.


== DATA I/O ==

anzhead_struct          Header structure of ANALYZE 7.5 format
readanz                 function to read data from ANALYZE 7.5 format
writeanz                procedure to write an image in ANALYZE 7.5 format
file_list               generates numbered file names (pref+NNN+ext)

dcmtags                 print tag values from a DICOM file
getdcmtag               get a tag value for a valid IDLffDICOM object instance
read_dcmdir             read image data from a list of DICOM files
awgeom_lib              several functions to handle image orientation and
                          geometry (philips specific)

parrec                  function to read data from PAR/REC (V3-V4.2) formats
                        image data is returented as 3D array 
                        (all temporal dimension put together)

== OBJECTS ==

obj_valid_isa           check if parameter is a valid instance of a given class
ivzerror__define        simple error handling object
ph_baseobj__define      base object (mainly to be inherited from)
ph_container__define    extended IDL_Container object
ph_imgreg__define       image registration object using AIR (WIP)
ph_xroi                 modification of IDL's XROI (WIP)
slice_stack__define     object for working with 3D image stack (WIP)
voldisp__define         display object for slice_stack (very WIP)


== WIDGET PROGRAMS ==

img_over_img            displays two images using Red and Green channels
                           (useful for checking misalignment)
img_view                generic image (2D - 4D) viewer with zoom and simple ROI
nxmovie                 3D image animation (modified Fanning's xmovie)
structtable             show STRUCT variable in WIDGET_TABLE
slide_trueimg           simple animation of a stack of true-color images
wind                    'clickable' WINDOW


== UTILITES ==

g_smooth                Gaussing smoothing of an image
gammavar                Gamma variate function
imgrange                image intensities corresponding to % of signal power
imgtv                   'shortcut' for TV (fits to current window size)
imgtvall                display all slices of a 3D image in a new window
meanstdev               return [mean,stdev] of the input
minmax                  return [min,max] of the input
mm_kern                 morphological kernel generator (2D, 3D)
mr_bbox                 find bounding box of an image (tested mainly w/ AXIAL)
mrimask                 generate a mask of data above some noise threshold
sthm_apply              apply homogeneous coordinate transformation matrix 
                          to a list of points (3D)
sthmatrix               generate a homogeneous coordiante transformation matrix
str2value               convert string to number
strdate                 return current date/time as 'yyyy-mm-dd hh:mm:ss'

