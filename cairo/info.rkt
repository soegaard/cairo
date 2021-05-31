#lang info

(define collection 'multi)

(define deps '("cairo-lib"
               "cairo-test"))
(define implies '("cairo-lib"
                  "cairo-test"))

(define pkg-desc "Bindings for the 2d graphics library Cairo mathing the C api.")

(define pkg-authors '(soegaard))
