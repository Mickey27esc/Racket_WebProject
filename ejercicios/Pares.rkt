#lang slideshow

;Funcion que devuelva los numeros pares*/

(define (pares L)
  (if (null? L) '()
      (if (even? (car L)) (append (list (car L)) (pares (cdr L)))
          (pares (cdr L))
          )
      )
  )