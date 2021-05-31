#lang racket
(require "dot-method.rkt")
(require cairo)
(require (rename-in "dot-method.rkt"
                    [dot-app  #%app]
                    [dot-top  #%top]))

(define target   (new ImageSurface [width 100] [height 100]))
(define c        (new Context      [target target]))
(define blue     (new SolidPattern [red 0] [blue 1] [green 0] [alpha 1]))
(define white    (new SolidPattern [red 1] [blue 1] [green 1] [alpha 1]))

(define blue-red (new LinearGradient [x0 0] [y0 0] [x1 100] [y1 100]))
(blue-red.add-color-stop-rgb 0.0  0 0 1)
(blue-red.add-color-stop-rgb 1.0  1 0 0)


(c.set-source blue-red)
;(c.set-source blue)
(c.paint)

;(c.set-source white)
;(c.set-line-width 1)
;(c.set-antialias 'none)
;(c.line-to 99.5  0)
;(c.line-to 99.5 50)
;(c.stroke)

(target.get-bitmap)
(define-values (offset r g b a) (blue-red.get-color-stop-rgba 0))
(list offset r g b a)
(define-values (offset2 r2 g2 b2 a2) (blue-red.get-color-stop-rgba 1))
(list offset2 r2 g2 b2 a2)

(define rs  (new RecordingSurface [extents (new Rectangle [x 0] [y 0] [width 400] [height 400])]))
(define rsc (new Context [target rs]))
(rsc.set-line-width 1)
;; (rsc.move-to   0.5   0.5)
;; (rsc.line-to  99.5 199.5)
;; (rsc.stroke)
;; (rs.ink-extents)
(define re  (new Rectangle [x 11] [y 22] [width 33] [height 44]))
(rsc.move-to   0.5   0.5)
(rsc.line-to  99.5 199.5)
(rsc.stroke)
(rs.get-extents! re)
(re.->vector)

(define rs2 (send rs create-similar 100 100))

