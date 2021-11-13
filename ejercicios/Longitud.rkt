#lang slideshow

;Funcion que devuelva la longitud de una lista

(define (longitud L)
  (if (null? L) 0
      (+ 1 (longitud (cdr L)))
      )
  )