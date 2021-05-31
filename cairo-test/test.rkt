#lang racket
(require cairo
         ffi/vector
         ffi/cvector
         (only-in racket/draw
                  bitmap%
                  make-bitmap
                  make-platform-bitmap))

;;;
;;; TEST
;;;

(cairo-version) ; the version as a single integer
(cairo-version-string)


(define target  (new ImageSurface [width 100] [height 100]))
(define context (new Context      [target target]))

(send context arc 50 50 10 0 6.8)

(send context status)
(send context status-string)
(send context get-group-target)
(send context get-antialias)
(send context set-antialias 'none)
(send context get-antialias)
(send context get-dash-count)
(send context set-dash #(1 2 3 4))
(send context get-dash-count)
(send context set-no-dash)
(send context get-dash-count)

(send context get-line-cap) ; butt is default
(send context set-line-cap 'round) 
(send context get-line-cap) 
(send context set-line-cap 'square) 
(send context get-line-cap) 
(send context set-line-cap 'butt) 
(send context get-line-cap) 

(send context get-line-join) ; miter is default
(send context set-line-join 'round) 
(send context get-line-join) 
(send context set-line-join 'bevel) 
(send context get-line-join) 
(send context set-line-join 'miter) 
(send context get-line-join) 

(send context get-line-width) ; 2.0 is default
(send context set-line-width 1) 
(send context get-line-width) ; 2.
(send context set-line-width) ; 1.
(send context get-line-width) ; 2.

(send context get-miter-limit) ; 10.0 is default
(send context set-miter-limit 2) 
(send context get-miter-limit) ; 2.
(send context set-miter-limit) ; 
(send context get-miter-limit) ; 10

(send context get-operator) ; over
(send context set-operator 'hsl-color)
(send context get-operator) ; hsl-color
(send context set-operator 'over) 
(send context get-operator) ; over

(send context get-tolerance) ; 0.1 is default
(send context set-tolerance 2) 
(send context get-tolerance) ; 2.
(send context set-tolerance) ; 
(send context get-tolerance) ; 0.1

(send context clip-extents)

(send context in-clip? 200 200) ; #t
(send context clip)
(send context clip-extents)     ; 0 0 0 0
(send context in-clip? 200 200) ; #f
(send context in-clip?  50  50) ; #f

(send context reset-clip)
(send context clip-extents)

(send context fill-extents)
(send context stroke-extents)

;(send context set-source-rgb 1. 0. 0.)
;(send context paint)

; (send target get-bitmap)


(define p (send context copy-path))
(send context copy-path-flat)
(send context append-path p)

; (send context get-current-point)

(send context arc 50 50 10 0 6.8)
(send context path-extents)
(send context stroke)
(send target get-bitmap)

(send context set-operator 'color-burn)
(send context arc 50 50 10 0 6.8)
(send context fill)
(send target get-bitmap)


(define logo         (new ImageSurface   [bitmap (make-object bitmap% "racket-logo.png")]))
(define logo-context (new Context        [target  logo]))
(define logo-pattern (new SurfacePattern [surface logo]))

(define dest         (new ImageSurface   [width 400] [height 400]))
(define dest-context (new Context        [target dest]))

(define blue-pattern (new SolidPattern   [red 0] [blue 1] [green 0] [alpha 0.5]))


(send* dest-context
  (set-source logo-pattern)
  (paint)

  (set-source blue-pattern)
  (set-operator 'over)
  (rectangle 100 100 100 100)  
  (fill))

(send dest get-bitmap)

(send context get-matrix)

(define white-pattern (new SolidPattern   [red 1] [blue 1] [green 1] [alpha 1])) ; non-transparent
(define dest2         (new ImageSurface   [width 400] [height 400]))
(define dest2-context (new Context        [target dest2]))
(define mesh-pattern  (new MeshPattern))
(send* mesh-pattern
  (begin-patch)
  (move-to 100 100)
  (line-to 200 100)
  (line-to 200 200)
  (line-to 100 200)
  ;                 corner r g b
  (set-corner-color-rgb 0  1 0 0)
  (set-corner-color-rgb 1  0 1 0)
  (set-corner-color-rgb 2  0 0 1)
  (set-corner-color-rgb 3  1 1 0)
  (end-patch))
(send* dest2-context
  (set-source white-pattern)
  (paint)
  (set-source mesh-pattern)
  (paint))
(send dest2 get-bitmap)


(define rectangle1    (new RectangleInt   [x 100] [y 100] [width 400] [height 400]))
(define rectangle2    (new RectangleInt   [x 300] [y 300] [width 400] [height 400]))

(define region        (new Region         [rectangle rectangle1]))
(define region2       (new Region         [rectangle rectangle2]))

(displayln (send region contains-point?  50  50)) ; #f
(displayln (send region contains-point? 150 150)) ; #t
(displayln (send region contains-point? 350 350)) ; #t
(send region intersect region2)
(displayln (send region contains-point?  50  50)) ; #f
(displayln (send region contains-point? 150 150)) ; #f
(displayln (send region contains-point? 350 350)) ; #t

(define is (new ImageSurface
                [data   (make-u32vector (* 20 20) (- (expt 2 24) 1))] ; 40 white pixels
                [format 'argb32]
                [width  20]
                [height 20]))
(define is-data (send is get-data)) ; a cvector of u32int (due to argb32)


(send* dest2-context
  (set-source (new SurfacePattern [surface is]))
  (paint))
(send dest2 get-bitmap)

; Let's change the black pixels to blue, remember to flush and mark as dirty
; everytime the data are modified.
(send is flush)
  (for ([i 20]) (cvector-set! is-data (+ (* 20 i) i) 255))
(send is mark-dirty)
      
(send* dest2-context
  (set-source (new SurfacePattern [surface is]))
  (paint))
(send dest2 get-bitmap)

(send is get-type)


(define logo2 (new BitmapSurface   [bitmap (make-object bitmap% "racket-logo.png")]))
(define logo3 (send logo2 create-similar
                    (send logo2 get-width) (send logo2 get-height) 'color-alpha))
(send logo3 get-bitmap)


(define logo-width    (get-field width  logo))
(define logo-height   (get-field height logo))
(define logo4         (send logo create-similar logo-width logo-height))
(define dest4         (new ImageSurface   [width logo-width] [height logo-height]))
(define dest4-context (new Context        [target dest]))
(send dest4-context set-source (new SurfacePattern [surface logo4]))
(send dest4-context paint)

(define logo5         (send logo create-similar-image logo-width logo-height))
(define dest5         (new ImageSurface   [width logo-width] [height logo-height]))
(define dest5-context (new Context        [target dest]))
(send dest5-context set-source (new SurfacePattern [surface logo5]))
(send dest5-context paint)
(send dest5 get-bitmap)
