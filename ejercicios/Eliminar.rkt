#lang slideshow

(define (eliminar x L)
  (if (null? L) '()
      (if (= x (car L)) (cdr L)
          (append (list (car L)) (eliminar x (cdr L)))
          )
      )
  )