#lang racket
(require "dot-method.rkt")
(require "low-level-cairo.rkt")
(require (rename-in "dot-method.rkt"
                    [dot-app  #%app]
                    [dot-top  #%top]))

(define dest          (new ImageSurface  [width 400] [height 400]))
(define cr            (new Context       [target dest]))
(define blue          (new SolidPattern  [red 0] [blue 1] [green 0] [alpha 1]))
(define white         (new SolidPattern  [red 1] [blue 1] [green 1] [alpha 1]))
(define mesh-pattern  (new MeshPattern))

(define logo         (new ImageSurface   [bitmap (make-object bitmap% "racket-logo.png")]))
(define logo-context (new Context        [target  logo]))
(define logo-pattern (new SurfacePattern [surface logo]))

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
