#lang racket/base
;;;
;;; Cairo
;;;

; This file contains an object oriented layer on top of the raw Cairo bindings.
; The idea is to allow users to use Cairo directly without any higher-level abstractions.
; Using the Cairo classes below it is straightforward to follow Cairo tutorials
; written in C or Python. The downside of using this interface is of course, that
; it is very low-level. 

;;;
;;; Cairo Classes
;;;

; The class hiearchy for patterns:
;    https://www.cairographics.org/manual/bindings-patterns.html

(provide Context
         Matrix
         Path
         Region
         Rectangle
         RectangleInt
         Surface 
             BitmapSurface 
             ImageSurface 
             RecordingSurface
         Pattern
             SolidPattern
             SurfacePattern 
             Gradient
                 LinearGradient 
                 RadialGradient
             MeshPattern)

(provide cairo-version
         cairo-version-string)

;;;
;;; TODO
;;;

; - add methods for Path (?)


;;;
;;; Module Imports
;;;


(require racket/class
         (only-in racket/draw bitmap% make-bitmap make-platform-bitmap)
         ffi/unsafe ffi/vector ffi/unsafe/cvector ; for make-cvector*
         "bindings.rkt")

;;;
;;; Enumerations
;;;

(require (for-syntax racket/base
                     racket/match
                     racket/syntax syntax/parse #;syntax/format))

(define-syntax (define-enum stx)
  (syntax-parse stx
    [(_define-enum id spec) ; spec ::=  list of : symbol or [symbol value]
     (define symbols
       (let loop ([spec (syntax->datum #'spec)])
         (match spec
           ['()                                              '()]
           [(cons (? symbol? s) more)                        (cons s (loop more))]
           [(cons (list (? symbol? s) (? integer? i)) more)  (cons s (loop more))]
           [_ (raise-syntax-error 'define-enum "bad specification" stx)])))
     (define values
       (let loop ([spec  (syntax->datum #'spec)]
                  [value 0])
         (match spec
           ['()                                              '()]
           [(cons (? symbol? s) more)                        (cons value (loop more (+ value 1)))]
           [(cons (list (? symbol? s) (? integer? i)) more)  (cons i     (loop more (+ i 1)))]
           [_ (raise-syntax-error 'define-enum "bad specification" stx)])))
     (with-syntax ([(symbol ...)     symbols]
                   [(value ...)      values]
                   [enum-symbols     (format-id stx "~a-symbols"     #'id)]
                   [enum-values      (format-id stx "~a-values"      #'id)]
                   [enum-association (format-id stx "~a-association" #'id)]
                   [enum-value       (format-id stx "~a-value"       #'id)]
                   [enum-symbol      (format-id stx "~a-symbol"      #'id)]
                   [check-enum       (format-id stx "check-~a"       #'id)])
       (syntax/loc stx
         (begin
           (define enum-symbols           '(symbol ...))
           (define enum-values            '(value ...))
           (define enum-association       (list (cons 'symbol 'value)  ...))
           (define enum-association-rev   (list (cons 'value  'symbol) ...))           
           (define (enum-symbol? s)       (and (symbol? s) (memq s enum-symbols)))
           (define (unsafe-enum-value  s) (cdr (assq s enum-association)))
           (define (unsafe-enum-symbol s) (cdr (assq s enum-association-rev)))           
           (define (enum-value s)         (and (enum-symbol? s) (unsafe-enum-value s)))
           (define (enum-symbol int)      (unsafe-enum-symbol int))
           (define (check-enum who s [more ""])
             (unless (enum-symbol? s)
               (error who "expected one of the symbols ~a, got: ~a ~a"
                      enum-symbols s more))))))]))

;;;
;;; Enumerations
;;;

(define-enum content        ([color #x1000] [alpha #x2000] [color-alpha #x3000]))
(define-enum surface-type   (image pdf ps xlib xcb glitz quartz win32 beos directfb svg 
                                   os2 win32-printing quartz-image script qt recording
                                   vg gl drm tee xml skia subsurface cogl))
; pattern
(define-enum pattern-extend (none repeat reflect pad))                        ; enum cairo_extend_t
(define-enum pattern-filter (fast good best nearest bilinear gaussian))       ; enum cairo_filter_t
(define-enum pattern-type   (solid surface linear radial mesh raster-source)) ; enum cairo_pattern_type_t

; pen
(define-enum antialias (default none gray subpixel fast good best))
(define-enum fill-rule (winding even-odd))
(define-enum line-cap  (butt round square))
(define-enum line-join (miter round bevel))
; compositing
(define-enum operator  (clear source over in out atop dest dest-over dest-in dest-out
                              dest-atop xor add saturate multiply screen overlay
                              darken lighten color-dodge color-burn hard-light soft-light
                              difference exclusion hsl-hue hsl-saturation hsl-color
                              hsl-luminosity))
;; image formats
(define-enum image-format ([invalid -1] argb32 rgb24 a8 a1 rgb16-565 rgb30 rgb96f rgba128f))
;;  argb32     each pixel 32 bits
;;  rgb24      each pixel 32 bits, upper 8 bits unused
;;  a8         each pixel 8 bits, only alpha
;;  a1         each pixel 1 bits, only alpha (endianness!)
;;  rgb16-565  each pixel 16 bit, red upper 5, green middle 6, blue lower 5
;;  rgb30      like rgb24 but with 10 bpc


;;;
;;; Path
;;; 

; Paths are used to (implicitly) generate simple masks.
; The cairo context stores a current path, and (cr.copy-path) or (cr.copy-path-flat)
; will make a copy and wrap it in a Path object.

(define Path
  (class object%
    (init-field [path #f]) ; an _cairo_path_t
    (super-new)))


;;;
;;; Surfaces
;;;

; Surfaces represents targets that Cairo can render to.

(define Surface
  (class object%
    (init-field [surface #f])
    (super-new)

    (define/pubment (create-similar width height [content 'color-alpha])
      (check-content 'Surface.create-similar content "as content")
      (define enum (content-value content))
      (unless (and (integer? width) (integer? height) (> width 0) (> height 0))
        (error 'Surface.create-similar "expected positive, integer height and width"))
      (define raw-surface (cairo_surface_create_similar surface enum width height))
      ; Now call create-similar of the subclass with our new cairo surface
      ; in order to wrap it in an object of the correct class.
      (inner #f create-similar raw-surface content width height))

    (define/pubment (create-similar-image width height [content 'color-alpha])
      (check-content 'Surface.create-similar-image content "as content")
      (define enum (content-value content))
      (unless (and (integer? width) (integer? height) (> width 0) (> height 0))
        (error 'Surface.create-similar-image "expected positive, integer height and width"))
      (define raw-surface (cairo_surface_create_similar_image surface enum width height))
      (define format      (send this get-format))
      (new ImageSurface
           [surface raw-surface] [width width] [height height]))    
    ;; (define/public (status)
    ;;   (cairo_surface_status surface))
    (define/public (get-content)
      (define content (cairo_surface_get_content surface))
      (content-symbol content))
    (define/public (flush)
      (cairo_surface_flush surface))
    (define/public (mark-dirty)
      (cairo_surface_mark_dirty surface))
    (define/public (mark-dirty-rectangle x y width height)
      (cairo_surface_mark_dirty_rectangle surface x y width height))
    (define/public (finish)
      (cairo_surface_finish surface))   
    (define/public (get-type)
      (define enum (cairo_surface_get_type surface))
      (surface-type-symbol enum))))

;;;
;;; BitmapSurfaces
;;;

; A bitmap surface allow us to 1) draw to a regular Racket bitmap
; and 2) use bitmaps as patterns.

; make-image-surface : int int -> _cairo_surface_t
(define (make-image-surface width height #:backing-scale [backing-scale 1.0])
  (define bm (make-bitmap width height #:backing-scale backing-scale))
  (send bm get-handle))
               

(define BitmapSurface
  (class Surface
    (inherit-field surface)
    ; (define (new BitmapSurface [bitmap <a-norma-bitmap>]))
    (init-field bitmap)
    (define s  (send bitmap get-handle))
    (super-new [surface s])
    
    (define/public (get-bitmap) bitmap)

    (define/augride (create-similar raw-surface content width height)
      (check-content 'BitmapSurface.create-similar content "as content")
      (define enum (content-value content))
      ; Since we only need to wrap once, use override in subclasses of BitmapSurface
      (define alpha?  #t)                              ; todo: examine the image format
      (define bs      (send this get-backing-scale))        ; todo: correct backing scale?
      (define bm      (make-bitmap width height alpha? #:backing-scale 2.0))
      (new BitmapSurface [bitmap bm]))

    ; Unique to Bitmap Surfaces
    (define/public (get-backing-scale) (send bitmap get-backing-scale))
    (define/public (get-width)         (send bitmap get-width))
    (define/public (get-height)        (send bitmap get-height))))


;;;
;;; Image Surfaces
;;;

; Rendering to an image surface is the same as rendering to a memory buffer.

(define (stride-for-width format width)
  (define enum (image-format-value format))
  (cairo_format_stride_for_width enum width))

(define (data->cpointer data)
  (cond
    [(u32vector? data) (u32vector->cpointer data)] ; for argb32, rgb24, rgb-30
    [(u16vector? data) (u16vector->cpointer data)] ; for rgb16-565    
    [(u8vector?  data) (u8vector->cpointer data)]  ; for  a8, a1,
    [(bytes?     data) (u8vector->cpointer data)]  
    [else (error 'data->cpointer "got: ~a" data)]))

(define ImageSurface
  (class Surface
    (inherit-field surface)
    (init-field [bitmap #f]
                [width  #f] [height #f] [backing-scale 1.0]
                [format #f]
                [data   #f]) u32vector
    (super-new)
    (unless surface
      (cond
        [(is-a? bitmap bitmap%)
         (set! surface (send bitmap get-handle))
         (set! width   (send bitmap get-width))
         (set! height  (send bitmap get-height))]
        [(and data format width height) ; data contains the pixels
         (check-image-format 'ImageSurface format)
         (unless (and (integer? width) (integer? height) (> width 0) (> height 0))
           (error 'ImageSurface "both width and height must be positive integers"))
         (define stride       (stride-for-width format width))
         (define enum         (image-format-value format))
         (define data-pointer (data->cpointer data))
         (set!   surface      (cairo_image_surface_create_for_data data-pointer enum width height stride))
         (set!   bitmap       #f)]
        [(and format width height)
         (check-image-format 'ImageSurface format)
         (unless (and (integer? width) (integer? height) (> width 0) (> height 0))
           (error 'ImageSurface "both width and height must be positive integers"))
         (define enum (image-format-value format))
         (set! surface (cairo_image_surface_create enum width height))
         (set! bitmap #f)]
        [(and (not bitmap) width height)
         (set! bitmap  (make-bitmap width height #:backing-scale backing-scale))
         (set! surface (send bitmap get-handle))])
      (unless surface
        (error 'ImageSurface "expected either a bitmap or both width and height with an optional format")))
    
    (define/public (get-bitmap) bitmap)

    (define/augride (create-similar raw-surface content width height)
      (check-content 'ImageSurface.create-similar content "as content")
      (new ImageSurface [surface surface] [width width] [height height]))


    (define/public (get-data)
      ; note:     call cairo_surface_flush() before accessing data
      ;       and call cairo_durface_dirty() after modification
      (cairo_surface_flush surface)
      (define format  (get-format))
      (define pointer (cairo_image_surface_get_data surface))
      (define w       (cairo_image_surface_get_width surface))
      (define h       (cairo_image_surface_get_height surface))
      (define s       (cairo_image_surface_get_stride surface))
      (define len     (* h s)) ; num of pixels multiply with bytes pr pixel
      (case format
        [(argb32 rgb24 rgb-30) (make-cvector* pointer _uint32           (quotient (* h s) 4))]
        [(rgb16-565)           (make-cvector* pointer _uint16           (quotient (* h s) 2))]
        [(a8)                  (make-cvector* pointer _uint8                      (* h s))]
        [(a1)                  (make-cvector* pointer _uint8            (quotient (* h s) 8))]
        [else                  (error 'ImageSurface.get-data "unknown format: ~a"
                                      format)]))

    (define/public (get-format)
      (define format (cairo_image_surface_get_format surface))
      (image-format-symbol format))

    (define/public (get-width)
      (cairo_image_surface_get_width surface))
    (define/public (get-height)
      (cairo_image_surface_get_height surface))
    (define/public (get-stride)
      (cairo_image_surface_get_stride surface))
    
    ))

;;;
;;; Recording Surface
;;;

; A recording surface records the drawing operations (that is no rendering happens).
; The recording can later be "played back" onto another surface. 
; A recording surface can thus be used to draw the same object multiple times and
; possibly on multiple surfaces.

(define RecordingSurface
  (class Surface
    (inherit-field surface)
    (init-field [content 'color-alpha]
                [extents #f]) 
    (super-new)
    (define who 'RecordingSurface)
    (define enum (content-value content))
    (unless surface
      (cond
        [(is-a? extents Rectangle)
         (define the-extents (get-field rectangle extents))
         (set! surface (cairo_recording_surface_create enum the-extents))]
        [else
         (set! surface (cairo_recording_surface_create enum #f))]))

    (define/augride (create-similar raw-surface content width height)
      (check-content 'RecordingSurface.create-similar content "as content")
      (new RecordingSurface [surface raw-surface]))

    (define/public (ink-extents)
      (define-values (x y w h) (cairo_recording_surface_ink_extents surface))
      (values x y w h))

    (define/public (get-extents! rectangle)
      ; overwrites the rectangle
      (define the-rectangle (get-field rectangle rectangle))
      (define bounded? (cairo_recording_surface_get_extents surface the-rectangle)) ; the-rectangle is overwritten
      bounded?)))

;;; 
;;; Scripts
;;;

;; Like recording surfaces, but the recording is written to a file (intead of to memory).

;; TODO
;; (define Script
;;   (class object%
;;     (init-field [device #f])
;;     (super-new)

;; (define-cairo cairo_script_create                 (_cfun _string*/utf-8                      -> _cairo_device_t-pointer))
;; (define-cairo cairo_script_from_recording_surface (_cfun _cairo_device_t-pointer _cairo_surface_t -> _int))
;; (define-cairo cairo_script_get_mode               (_cfun _cairo_device_t-pointer                  -> _int))
;; (define-cairo cairo_script_set_mode               (_cfun _cairo_device_t-pointer _int             -> _void))
;; (define-cairo cairo_script_surface_create        (_cfun _cairo_device_t-pointer _int _double _double -> _cairo_surface_t))
;; (define-cairo cairo_script_surface_create_for_target (_cfun _cairo_device_t-pointer _cairo_surface_t-pointer
;;                                                             -> _cairo_surface_t))
;; (define-cairo cairo_script_write_comment           (_cfun _cairo_device_t-pointer _string/utf-8 _int -> _void))



;;;
;;; Devices
;;;

;; Devices represent the underlying rendering system.

;; TODO
;; (define Device
;;   (class object%
;;     (init-field raw-device)
;;     (super-new)
;;     (define/public (get-raw-device)
;;       raw-device)))



;;;
;;; Matrix
;;; 
         
; A matrix represents an affine transformation.

(define Matrix
  (class object%
    ;; typedef struct {
    ;;     double xx; double yx;
    ;;     double xy; double yy;
    ;;     double x0; double y0;
    ;; } cairo_matrix_t;
    (init-field [matrix #f])    
    (super-new)
    (unless matrix
      (set! matrix (make-cairo_matrix_t 1 0 0 1 0 0)))

    (define/public (->vector)
      (vector (cairo_matrix_t-xx matrix)
              (cairo_matrix_t-yx matrix)
              (cairo_matrix_t-xy matrix)
              (cairo_matrix_t-yy matrix)
              (cairo_matrix_t-x0 matrix)
              (cairo_matrix_t-y0 matrix)))
    
    (define/public (init xx yx xy yy x0 y0)
      ; x_new = xx * x + xy * y + x0;
      ; y_new = yx * x + yy * y + y0;
      (cairo_matrix_init xx yx xy yy x0 y0))
    (define/public (init-identity)
      (cairo_matrix_init_identity matrix))

    (define/public (init-translate tx ty)
      (cairo_matrix_init_translate matrix tx ty))
    (define/public (init-scale sx sy)
      (cairo_matrix_init_scale matrix sx sy))
    (define/public (init-rotate radians)
      (cairo_matrix_init_rotate matrix radians))
    
    (define/public (translate tx ty)
      (cairo_matrix_translate matrix tx ty))
    (define/public (scale sx sy)
      (cairo_matrix_scale matrix sx sy))
    (define/public (rotate radians)
      (cairo_matrix_rotate matrix radians))
    
    (define/public (invert)
      (define status (cairo_matrix_invert matrix))
      (if (zero? status)
          #t  ; the inverse existed
          #f)) ; nope
    (define/public (multiply matrix-a matrix-b)
      ; first a, then b
      (define mat-a (get-field matrix matrix-a))
      (define mat-b (get-field matrix matrix-b))
      (cairo_matrix_multiply matrix mat-a mat-b))
    (define/public (transform-distance dx dy)
      ; transforms distance vector (dx,dy) by the matrix
      ;   dx2 = dx1 * a + dy1 * c;
      ;   dy2 = dx1 * b + dy1 * d;
      (cairo_matrix_transform_distance matrix dx dy))
    (define/public (transform-point x y)
      (cairo_matrix_transform_point matrix x y))
    ))

;;;
;;; Pattern
;;;

; A pattern represents a paint source. 

(define Pattern ; todo: check more functions?
  (class object%
    (init-field [pattern #f])
    (super-new)
    (define/public (set-matrix matrix)
      (define mat (get-field matrix matrix))
      (cairo_pattern_set_matrix pattern mat)) ; user space -> pattern space
    (define/public (get-matrix)
      (define mat (cairo_pattern_get_matrix pattern))
      (new Matrix [matrix mat]))
    (define/public (get-type)
      (pattern-type-symbol (cairo_pattern_get_type pattern)))))


; A solid pattern consists of a single color.

(define SolidPattern
  (class Pattern
    (inherit-field pattern)
    (init-field red green blue [alpha #f]) ; double in [0.0;1.0]
    (set! pattern
          (if alpha
              (cairo_pattern_create_rgba red green blue alpha)
              (cairo_pattern_create_rgb  red green blue)))
    (super-new [pattern pattern])))

; A mesh pattern is the most general way of creating shaded areas.

(define MeshPattern
  (class Pattern
    (inherit-field pattern)
    (super-new)
    (unless pattern
      (set! pattern (cairo_pattern_create_mesh)))
    

    (define/public (begin-patch)
      (cairo_mesh_pattern_begin_patch pattern))
    (define/public (end-patch)
      (cairo_mesh_pattern_end_patch pattern))

    (define/public (move-to x y)
      (cairo_mesh_pattern_move_to pattern x y))
    (define/public (line-to x y)
      (cairo_mesh_pattern_line_to pattern x y))
    (define/public (curve-to x1 y1  x2 y2  x3 y3)
      (cairo_mesh_pattern_curve_to pattern x1 y1  x2 y2  x3 y3))

    (define/public (set-control-point point-num x y)
      (unless (memv point-num '(0 1 2 3))
        (error 'MeshPattern.set-control-point "expected 0, 1, 2 or 3 as point-num"))
      (cairo_mesh_pattern_set_control_point pattern point-num x y))

    (define/public (get-control-point patch-num point-num)
      (define n (cairo_mesh_pattern_get_patch_count pattern))
      (unless (<= 0 patch-num (- n 1))
        (error 'MeshPattern.get-control-point "patch-num not in range"))
      (unless (memv point-num '(0 1 2 3))
        (error 'MeshPattern.get-control-point "expected 0, 1, 2 or 3 as point-num"))
      (cairo_mesh_pattern_get_control_point pattern patch-num point-num))

    (define/public (set-corner-color-rgb corner-num r g b) ; r,g,b are doubles
      (unless (memv corner-num '(0 1 2 3))
        (error 'MeshPattern.set-corner-color-rgb "expected 0, 1, 2 or 3 as corner-num"))
      (cairo_mesh_pattern_set_corner_color_rgb pattern corner-num r g b))
    (define/public (set-corner-color-rgba corner-num r g b a) ; r,g,b,a are doubles
      (unless (memv corner-num '(0 1 2 3))
        (error 'MeshPattern.set-corner-color-rgba "expected 0, 1, 2 or 3 as corner-num"))
      (cairo_mesh_pattern_set_corner_color_rgba pattern corner-num r g b))

    (define/public (get-corner-color-rgba patch-num corner-num)
      (define n  (cairo_mesh_pattern_get_patch_count pattern))
      (unless (<= 0 patch-num (- n 1))
        (error 'MeshPattern.get-control-point "patch-num not in range"))
      (unless (memv corner-num '(0 1 2 3))
        (error 'MeshPattern.set-corner-color-rgba "expected 0, 1, 2 or 3 as corner-num"))
      (define-values (status r g b a) 
        (cairo_mesh_pattern_get_corner_color_rgba pattern patch-num corner-num))      
      (unless (zero? status)
        (error 'MeshPattern.get-corner-color-rgba "error status"))
      (values r g b a))

    (define/public (get-patch-count)
      (define-values (status count) (cairo_mesh_pattern_get_patch_count pattern))
      (if (zero? status) count #f))

    (define/public (get-path patch-num)
      (new Path [path (cairo_mesh_pattern_get_path pattern patch-num)]))
    ))

; A surface pattern represents a surface used as a source pattern.

(define SurfacePattern
  (class Pattern
    (inherit-field pattern)
    (init-field surface)
    (define sur (get-field surface surface))
    (define pat (cairo_pattern_create_for_surface sur))
    (super-new [pattern pat])
    (define/public (set-extend extend)
      (check-pattern-extend 'Pattern.set-extend extend "as extend mode")
      (cairo_pattern_set_extend pattern (pattern-extend-value extend)))
    (define/public (get-extend)
      (pattern-extend-symbol (cairo_pattern_get_extend pattern)))
    (define/public (set-filter filter)
      (check-pattern-filter 'Pattern.set-filter filter)
      (cairo_pattern_set_filter pattern (pattern-filter-value filter)))
    (define/public (get-filter filter)
      (pattern-filter-symbol (cairo_pattern_get_filter pattern)))))

; A Gradient represents a color transition.
; The subclasses LinearGradient and RadialGradient specify how the color transition
; is to be applied.

(define Gradient
  (class Pattern
    (init pattern)
    (super-new [pattern pattern])
    (define who 'Gradient.get-color-stop-rgba)    

    (define/public (add-color-stop-rgb offset red green blue)        ; offset is a double in [0.0;1.0]
      (cairo_pattern_add_color_stop_rgb (get-field pattern this) offset red green blue))
    (define/public (add-color-stop-rgba offset red green blue alpha) ; offset is a double in [0.0;1.0]
      (cairo_pattern_add_color_stop_rgba (get-field pattern this) offset red green blue alpha))
    (define/public (get-color-stop-count)
      (cairo_pattern_get_color_stop_count (get-field pattern this)))
    (define/public (get-color-stop-rgba index)
      (unless (integer? index) (error who "expected a natural number as index, got: ~a" index))
      (define-values (status offset r g b a)  (cairo_pattern_get_color_stop_rgba (get-field pattern this) index))
      (case status
        [(0)  (values offset r g b a)]
        [else (error who (cairo_status_to_string status))]))))


(define LinearGradient
  (class Gradient
    (inherit-field pattern)
    (init-field x0 y0 x1 y1)
    (super-new [pattern (cairo_pattern_create_linear x0 y0 x1 y1)])))

(define RadialGradient
  (class Gradient
    (inherit-field pattern)
    (init-field cx0 cy0 radius0 cx1 cy1 radius1)
    (set! pattern (cairo_pattern_create_radial cx0 cy0 radius0
                                               cx1 cy1 radius1))
    (super-new [pattern pattern])))

;;;
;;; Rectangle
;;;


(define Rectangle
  (class object%
    (init-field [rectangle #f] ; an _cairo_rectangle_t
                [x 0.] [y 0.] [width 0.] [height 0.])
    (super-new)
    (unless rectangle
      (cond
        [(and x y width height)
         (set! x      (exact->inexact  x))
         (set! y      (exact->inexact  y))
         (set! width  (exact->inexact  width))
         (set! height (exact->inexact  height))
         (set! rectangle (make-cairo_rectangle_t x y width height))]
        [else
         (error 'Rectangle "incorrect init-fields")]))

    (define/public (->vector)
      (vector (cairo_rectangle_t-x      rectangle)
              (cairo_rectangle_t-y      rectangle)
              (cairo_rectangle_t-width  rectangle)
              (cairo_rectangle_t-height rectangle)))))

(define RectangleInt ; since 1.10
  (class object%
    (init-field [rectangle-int #f] ; an _cairo_rectangle_int_t
                [x 0] [y 0] [width 0] [height 0])
    (super-new)
    (define who 'RectangleInt)
    (unless rectangle-int
      (cond
        [(and x y width height)
         (unless (integer? x)      (error who "use integer values for x"))
         (unless (integer? y)      (error who "use integer values for y"))
         (unless (integer? width)  (error who "use integer values for width"))
         (unless (integer? height) (error who "use integer values for height"))
           
         (set! rectangle-int (make-cairo_rectangle_int_t x y width height))]
        [else
         (error 'RectangleInt "incorrect init-fields")]))

    (define/public (->vector)
      (vector (cairo_rectangle_int_t-x      rectangle-int)
              (cairo_rectangle_int_t-y      rectangle-int)
              (cairo_rectangle_int_t-width  rectangle-int)
              (cairo_rectangle_int_t-height rectangle-int)))))


;;;
;;; Region
;;;

; Regions represents areas built of axis aligned rectangles.
; They can be used for clipping.

(define Region
  (class object%
    (init-field [region     #f]  ; an _cairo_region_t-pointer
                [rectangle  #f]  ; an RectangleInt
                [rectangles #f]) ; a list of RectangleInt
    (super-new)
    (unless region
      (set! region (cairo_region_create)))
    (when rectangle
      (create-rectangle rectangle))
    (when rectangles
      (send/apply this create-rectangles rectangles))

    (define/public (create-rectangle rectangle-int)
      (define rect-int (get-field rectangle-int rectangle-int))
      (set! region (cairo_region_create_rectangle rect-int))
      (unless (status)
        (error 'Region.create-rectangle "out of memory")))
    (define/public (status)
      (define s (cairo_region_status region))
      (case s
        [(0) #t] ; CAIRO_STATUS_SUCCESS,   no error
        [(1) #f] ; CAIRO_STATUS_NO_MEMORY, out of memory
        [else #f]))
    (define/public (create-rectangles . rectangles)
      (define count (length rectangles))
      (define rects (apply _array _cairo_rectangle_t count rectangles))
      (set! region (cairo_region_create_rectangles rects count))
      (unless (status)
        (error 'Region.create-rectangles "out of memory")))
    (define/public (copy)
      (define reg (cairo_region_copy region))
      (define Reg (new Region [region reg]))
      (unless (status)
        (error 'Region.copy "out of memory"))
      Reg)
    (define/public (get-extents) ; returns RectangleInt
      (define extents (cairo_region_get_extents region)) 
      (new RectangleInt [rectangle-int extents]))
    (define/public (number-of-rectangles)
      (cairo_region_num_rectangles region))
    (define/public (get-rectangle nth)
      (define destination        (new RectangleInt))
      (define dest-rectangle-int (get-field rectangle-int destination))
      (cond
        [(and (integer? nth) (<= 0 nth (number-of-rectangles)))
         (cairo_region_get_rectangle region nth dest-rectangle-int)]
        [else
         (error 'Rectangle.get-rectangle "extected an integer less than the number of rectangles")])
      destination)
    (define/public (empty?)
      (cairo_region_is_empty region))
    (define/public (contains-point? x y) ; since 1.10
      (define who 'Region.contains-point?)
      (unless (integer? x)      (error who "use integer values for x"))
      (unless (integer? y)      (error who "use integer values for y"))         
      (cairo_region_contains_point region x y))
    (define/public (contains-rectangle? rectangle) ; since 1.10
      (define rect (get-field rectangle rectangle ))
      (define result (cairo_region_contains_rectangle region rect))
      (case result
        [(0) 'in]          ; the rectangle is entirely inside the region
        [(1) 'out]         ; the rectangle is entirely outside the region
        [(2) 'partially]   ; the rectangle is partially inside and outside the region
        [else #f]))
    (define/public (equal? another-region) ; since 1.10
      (define another-reg (get-field region another-region))
      (cairo_region_equal region another-reg))
    (define/public (translate dx dy)
      (define who 'Region.translate)
      (unless (integer? dx) (error who "use integer values for dx"))
      (unless (integer? dy) (error who "use integer values for dy"))
      (cairo_region_translate region dx dy))
    (define/public (intersect another-region) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define another            (get-field region another-region))
      (define status             (cairo_region_intersect destination another))
      (case status
        [(0) destination-region]
        [else (error 'Region.intersect "no memory")]))
    (define/public (intersect-rectangle rectangle-int) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define rect-int           (get-field rectangle-int rectangle-int))
      (define status             (cairo_region_intersect_rectangle destination rect-int))
      (case status
        [(0) destination-region]
        [else (error 'Region.intersect-rectangle "no memory")]))
    (define/public (subtract another-region) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define another            (get-field region another-region))
      (define status             (cairo_region_subtract destination another))
      (case status
        [(0) destination-region]
        [else (error 'Region.intersect "no memory")]))
    (define/public (subtract-rectangle rectangle-int) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define rect-int           (get-field rectangle-int rectangle-int))
      (define status             (cairo_region_subtract_rectangle destination rect-int))
      (case status
        [(0) destination-region]
        [else (error 'Region.subtract-rectangle "no memory")]))
    (define/public (union another-region) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define another            (get-field region another-region))
      (define status             (cairo_region_union destination another))
      (case status
        [(0) destination-region]
        [else (error 'Region.union "no memory")]))
    (define/public (union-rectangle rectangle-int) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define rect-int           (get-field rectangle-int rectangle-int))
      (define status             (cairo_region_union_rectangle destination rect-int))
      (case status
        [(0) destination-region]
        [else (error 'Region.union-rectangle "no memory")]))
    (define/public (xor another-region) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define another            (get-field region another-region))
      (define status             (cairo_region_xor destination another))
      (case status
        [(0) destination-region]
        [else (error 'Region.xor "no memory")]))
    (define/public (xor-rectangle rectangle-int) ; since 1.10
      (define destination-region (copy))
      (define destination        (get-field region destination-region))
      (define rect-int           (get-field rectangle-int rectangle-int))
      (define status             (cairo_region_xor_rectangle destination rect-int))
      (case status
        [(0) destination-region]
        [else (error 'Region.xor-rectangle "no memory")]))      
    ))

;;;
;;; Context
;;; 

; The cairo context ties surfaces, patterns etc together.
; The context contains the current drawing state.

(define Context
  (class object%
    (init-field target) ; target is a Surface (containing a _cairo_surface_t)
    (field [context #f])
    
    (unless (is-a? target Surface)
      (error 'Context "The target must be an instance of Surface, got: ~a" target))
    (super-new)

    ;; Create a new Cairo context
    
    (define surface (get-field surface target))
    (set! context (cairo_create surface))

    ;; TODO  Set all graphics state to default values

    (define/public (status)
      (cairo_status context))
    (define/public (status-string)
      (cairo_status_to_string (cairo_status context)))
    (define/public (save)
      (cairo_save context))
    (define/public (restore)
      (cairo_restore context))
    (define/public (get-target)
      target)
    (define/public (push-group)
      (cairo_push_group context))
    (define/public (pop-group)
      (cairo_pop_group context))
    (define/public (pop-group-to_source)
      (cairo_pop_group_to_source context))
    (define/public (push-group-with-content content)
      (check-content 'push-group-with-content content "as content")
      (cairo_push_group_with_content context (content-value content)))
    (define/public (get-group-target)
      (define surface (cairo_get_group_target context))
      (and surface
           ; Can we do better and return a surface with a more precise class?
           (new Surface [surface surface])))
    (define/public (set-source-rgb red green blue)
      ; The colors are doubles in the range 0. to 1. Values are clamped.
      (cairo_set_source_rgb context red green blue))
    (define/public (set-source-rgba red green blue alpha)
      ; The colors and alpha are doubles in the range 0. to 1. Values are clamped.
      (cairo_set_source_rgba context red green blue alpha))
    (define/public (set-source source)
      (unless (is-a? source Pattern) (error 'set-source "expected a Pattern as source"))
      (define pattern (get-field pattern source))
      (cairo_set_source context pattern))
    (define/public (set-source-surface surface x y)
      (define s (get-field surface surface))
      (cairo_set_source_surface context s x y))
    (define/public (get-source)
      (define p (cairo_get_source context))
      (new Pattern [pattern p]))
    (define/public (set-antialias mode)
      (check-antialias 'Context.set-antialias mode "as the mode")
      (cairo_set_antialias context (antialias-value mode)))
    (define/public (get-antialias)
      (antialias-symbol (cairo_get_antialias context)))
    (define/public (set-dash dashes [offset 0.0])
      ; dashes is a vector of positive doubles
      ; they are the length (in userspace) of on/off-segments at the time of *stroking*
      (cairo_set_dash context dashes offset))
    (define/public (set-no-dash)
      (cairo_set_dash context #() 0.0))    
    (define/public (get-dash-count)
      (cairo_get_dash_count context))
    ;; (define/public (get-dash)  TODO
    ;;   (cairo_get_dash context ??? ???))
    (define/public (set-fill-rule fill-rule)
      (check-fill-rule 'Context.set-antialias fill-rule "as the fill rule")
      (cairo_set_fill_rule context (fill-rule-value fill-rule)))
    (define/public (get-fill-rule)
      (fill-rule-symbol (cairo_get_fill_rule context)))
    (define/public (set-line-cap cap)
      (check-line-cap 'Context.set-line-cap cap "as the line cap")      
      (cairo_set_line_cap context (line-cap-value cap)))
    (define/public (get-line-cap)
      (line-cap-symbol (cairo_get_line_cap context)))
    (define/public (set-line-join join)
      (check-line-join 'Context.set-line-join join "as the line join")            
      (cairo_set_line_join context (line-join-value join)))
    (define/public (get-line-join)
      (define join (cairo_get_line_join context))
      (line-join-symbol join))
    (define/public (set-line-width [width 2.0]) ; default in Cairo is 2.
      (cairo_set_line_width context width))
    (define/public (get-line-width) 
      (cairo_get_line_width context))
    (define/public (set-miter-limit [limit 10.0])
      (cairo_set_miter_limit context limit))
    (define/public (get-miter-limit) 
      (cairo_get_miter_limit context))
    (define/public (set-operator op)
      (check-operator 'Context.set-operator op "as the operator")
      (cairo_set_operator context (operator-value op)))
    (define/public (get-operator) 
      (operator-symbol (cairo_get_operator context)))
    (define/public (get-tolerance) 
      (cairo_get_tolerance context))
    (define/public (set-tolerance [tolerance 0.1])
      (cairo_set_tolerance context tolerance))
    (define/public (clip)
      (cairo_clip context))
    (define/public (clip-preserve)
      (cairo_clip_preserve context))
    (define/public (clip-extents)
      (cairo_clip_extents context))
    (define/public (in-clip? x y)
      (cairo_in_clip context (* 1. x) (* 1. y)))
    (define/public (reset-clip)
      (cairo_reset_clip context))
    ; TODO cairo_rectangle_list_destroy
    ; TODO cairo_copy_clip_rectangle_list
    (define/public (fill)
      (cairo_fill context))
    (define/public (fill-preserve)
      (cairo_fill_preserve context))
    (define/public (fill-extents)
      (cairo_fill_extents context))
    (define/public (in-fill? x y)
      (cairo_in_fill context x y))
    (define/public (mask pattern)
      (unless (is-a? pattern Pattern) (error 'mask "expected a Pattern as the mask"))
      (define p (get-field pattern pattern))      
      (cairo_mask context p))
    (define/public (mask-surface surface surface-x surface-y)
      (unless (is-a? surface Surface) (error 'mask-surface "expected a Surface"))
      (define s (get-field surface surface))
      (cairo_mask_surface context s surface-x surface-y))
    (define/public (paint)
      (cairo_paint context))
    (define/public (paint-with-alpha alpha)
      (cairo_paint_with_alpha context alpha))
    (define/public (stroke)
      (cairo_stroke context))
    (define/public (stroke-preserve)
      (cairo_stroke_preserve context))
    (define/public (stroke-extents)
      (cairo_stroke_extents context))
    (define/public (in-stroke? x y)
      (cairo_in_stroke context x y))
    (define/public (copy-page)
      (cairo_copy_page context))
    (define/public (show-page)
      (cairo_show_page context))
    ;;
    ;; PATHS
    ;;  https://www.cairographics.org/manual/cairo-Paths.html
    ;;
    (define/public (copy-path)
      (new Path [path (cairo_copy_path context)]))
    (define/public (copy-path-flat)
      (new Path [path (cairo_copy_path_flat context)]))
    (define/public (append-path path)
      (unless (is-a? path Path) (error 'append-path "expected a Path"))
      (define p (get-field path path))
      (set-cairo_path_t-status! p 0) ; set status to SUCCESS
      (cairo_append_path context p))

    (define/public (has-current-point?)
      (cairo_has_current_point context))
    (define/public (get-current-point)
      (cairo_get_current_point context))
    (define/public (new-path)
      (cairo_new_path context))
    (define/public (new-sub-path)
      (cairo_new_sub_path context))
    (define/public (new-close-path)
      (cairo_close_path context))
    (define/public (arc xc yc radius angle1 angle2)
      (cairo_arc context xc yc radius angle1 angle2))
    (define/public (arc-negative xc yc radius angle1 angle2)
      (cairo_arc_negative context xc yc radius angle1 angle2))
    (define/public (curve-to x1 y1 x2 y2 x3 y3)
      (cairo_curve_to context x1 y1  x2 y2  x3 y3))
    (define/public (line-to x y)
      (cairo_line_to context x y))
    (define/public (move-to x y)
      (cairo_move_to context x y))
    (define/public (rectangle x y w h)
      (cairo_rectangle context x y w h))
    ;; TODO cairo_glyph_path
    ;; TOTO cairo_text_path
    (define/public (rel-curve-to dx1 dy1 dx2 dy2 dx3 dy3)
      (cairo_rel_curve_to context dx1 dy1 dx2 dy2 dx3 dy3))
    (define/public (rel-line-to dx dy)
      (cairo_rel_line_to context dx dy))
    (define/public (rel-move-to dx dy)
      (cairo_move_to context dx dy))
    (define/public (path-extents)
      (cairo_path_extents context))
    ;; Transformations
    ; These functions modity the ctm, the current transformation matrix:
    (define/public (translate tx ty)  ; user space coordinates    
      (cairo_translate context tx ty))
    (define/public (scale sx sy)      
      (cairo_scale context sx sy))
    (define/public (rotate angle)      
      (cairo_rotate context angle))
    (define/public (transform matrix)
      (define mat (get-field matrix matrix))
      (cairo_transform context mat))
    (define/public (set-matrix matrix)
      (define mat (get-field matrix matrix))
      (cairo_set_matrix context mat))
    (define/public (get-matrix)
      (define matrix (new Matrix))
      (define mat (get-field matrix matrix))
      (cairo_get_matrix context mat)
      matrix)
    (define/public (identity-matrix)
      (cairo_identity_matrix context))
    (define/public (user->device x y)
      (cairo_user_to_device context x y))
    (define/public (user->device-distance dx dy)
      (cairo_user_to_device_distance context dx dy))
    (define/public (device->user x y)
      (cairo_device_to_user context x y))
    (define/public (device->user-distance dx dy)
      (cairo_device_to_user_distance context dx dy))
    ))
