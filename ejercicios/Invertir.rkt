#lang slideshow

;Definir una funcion que delvuelva una lista de entrada en orden inverso

(define (invertir L)
  (if (null? L) '()
      (append (invertir (cdr L)) (list (car L)))
      )
  )
